-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- One round of 1024 bit skein - 80 rounds of threefish plus 21 subkey adds

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.skein_pkg.all;

-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity skein_round is 
	
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type;
		latency		: out integer  -- number of clocks from first input to valid output for debug
	);
	end skein_round;

architecture rtl of skein_round is
	
	constant TF_ROUNDS : integer := 80; 
	constant SUBKEY_ADDS :  integer := 21;
	constant EXTRA_STAGES : integer := 2;  --extra stages at the end for the optimizer to move around
	constant TF_PIPELINE_STAGES : integer := TF_ROUNDS*3 + SUBKEY_ADDS*2 + 2 + EXTRA_STAGES; -- 80 threefish rounds plus 21 subkey adds + extra stages
	
	
	type skein_pipe_array_type is array (0 to TF_PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);

	type subkey_array_type is array (0 to SUBKEY_ADDS - 1) of state_type;

	signal subkey		: subkey_array_type := (others =>(others =>(others => '0')));
	signal result_i : skein_pipe_type := skein_pipe_init;
	signal trace, trace2 : integer range 0 to TF_PIPELINE_STAGES := TF_PIPELINE_STAGES;  -- print debug trace
	
	
	type state_array_type is array (0 to TF_ROUNDS - 1) of state_type;
	signal threefish_state_retimining_array : state_array_type := (others =>(others =>(others => '0')));
	attribute retiming_backward : integer;
	-- attribute retiming_backward of threefish_state_retimining_array: signal is 1;
	
	
	
begin
	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to TF_PIPELINE_STAGES - 1;
		variable subkey_round : integer range 0 to SUBKEY_ADDS - 1;
		
	begin
		if rising_edge(clk) then			
			
			pipe := 0;  -- current pipeline stage (0 to latency)
			round := 0;  -- current threefish round (0 to 79)
			subkey_round := 0;  -- current subkey round (0 to 20)
			
			-- if skein_in.state(10) = x"00000004ECF83A53" then
				-- trace <= 0;
			-- end if;
			
			-- if skein_in.key(0) = x"BD008743310E06AA" then
				-- trace2 <= 0;
			-- end if;
			
			-- if trace < 1 then
				-- trace <= trace + 1;
				-- report "Round 2 state(" & to_string(trace) & "): " & to_hstring(skein_pipe(trace).state(0)) & " key: " & to_hstring(skein_pipe(trace).key(0)) & " tweak: " & to_hstring(skein_pipe(trace).tweak(0));
				-- report "Round 2 nonce(" & to_string(trace) & "): " & to_hstring(skein_pipe(trace).nonce) & " status: " & to_string(skein_pipe(trace).status);

			-- end if;
			
			-- if trace2 < 1 then
				-- trace2 <= trace2 + 1;
				-- report "Round 3 state(" & to_string(trace2) & "): " & to_hstring(skein_pipe(trace2).state(0)) & " key: " & to_hstring(skein_pipe(trace2).key(0)) & " tweak: " & to_hstring(skein_pipe(trace2).tweak(0));
				-- report "Round 3 nonce(" & to_string(trace2) & "): " & to_hstring(skein_pipe(trace2).nonce) & " status: " & to_string(skein_pipe(trace2).status);
			-- end if;
			
			
			-- register inputs
			skein_pipe(pipe) <= skein_in;
			pipe := pipe + 1;
			
			-- update status
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			case skein_pipe(pipe-1).status is
				when NOT_STARTED => 
					skein_pipe(pipe).status <= A_IN_PROCESS;
				when A_DONE =>
					skein_pipe(pipe).status <= B_IN_PROCESS;
				when others =>
					skein_pipe(pipe).status <= JUNK;
			end case;
			pipe := pipe + 1;
			
			-- 80 rounds of threefish with subkey adds
			for ii in 1 to TF_ROUNDS/4 loop
				
				-- generate subkey
				skein_pipe(pipe) <= skein_pipe(pipe-1);
				subkey(subkey_round) <= f_Get_Subkey(subkey_round, skein_pipe(pipe-1).tweak, skein_pipe(pipe-1).key);
				pipe := pipe + 1;
				
				--add subkey to the state
				skein_pipe(pipe) <= skein_pipe(pipe-1);
				skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, subkey(subkey_round));
				pipe := pipe + 1;
				subkey_round := subkey_round + 1;
				
				-- four threefish rounds
				for jj in 1 to 4 loop
					skein_pipe(pipe) <= skein_pipe(pipe - 1);
					skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
					--threefish_state_retimining_array(round) <= f_threefish_1(skein_pipe(pipe-1).state, round);
					pipe := pipe + 1;
					skein_pipe(pipe) <= skein_pipe(pipe - 1); 
					--skein_pipe(pipe).state <= threefish_state_retimining_array(round);
					pipe := pipe + 1;
					round := round + 1;
				end loop;
			end loop;
			
			-- generate final subkey
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			subkey(subkey_round) <= f_Get_Subkey(subkey_round, skein_pipe(pipe-1).tweak, skein_pipe(pipe-1).key);
			pipe := pipe + 1;
			
			--add final subkey to the state
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, subkey(subkey_round));
			
			-- update status
			case skein_pipe(pipe-1).status is
				when A_IN_PROCESS => 
					skein_pipe(pipe).status <= A_DONE;
				when B_IN_PROCESS =>
					skein_pipe(pipe).status <= B_DONE;
				when others =>
					skein_pipe(pipe).status <= JUNK;
			end case;
			
			for ii in 1 to EXTRA_STAGES loop
				pipe := pipe + 1;
				skein_pipe(pipe) <= skein_pipe(pipe-1);
			end loop;
			
			-- final output
			result_i <= skein_pipe(pipe);
			latency <= pipe + round;
			
		end if;
	end process;
	
	skein_out <= result_i;
	
end rtl;
		
