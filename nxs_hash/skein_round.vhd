-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- Two subkey add and eight rounds of threefish

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
	generic
	(
		SUBKEY_BASE_ROUND : integer range 0 to 18 := 0;  -- starting subkey round.  we use this to limit the span of subkey rounds to only what is possible
		TWEAK : tweak_type := T2  
	);
	
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type
	);
	end skein_round;

architecture rtl of skein_round is
	
	constant TF_ROUNDS : integer := 8; 
	constant SUBKEY_ADDS :  integer := 2;
	constant TF_PIPELINE_STAGES : integer := TF_ROUNDS + SUBKEY_ADDS*2;
	
	
	type skein_pipe_array_type is array (0 to TF_PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);

	type subkey_array_type is array (0 to SUBKEY_ADDS - 1) of state_type;

	signal subkey		: subkey_array_type := (others =>(others =>(others => '0')));
	signal result_i : skein_pipe_type := skein_pipe_init;
	signal trace, trace2 : integer range 0 to TF_PIPELINE_STAGES := TF_PIPELINE_STAGES;  -- print debug trace
	
	
	type state_array_type is array (0 to TF_ROUNDS - 1) of state_type;
	--signal threefish_state_retimining_array : state_array_type := (others =>(others =>(others => '0')));
	--attribute retiming_backward : integer;
	-- attribute retiming_backward of threefish_state_retimining_array: signal is 1;
	--attribute use_dsp : string;
	--attribute use_dsp of xxx : signal is "yes";
	
	--type subkey_round_list_type is (r0,r2);
	signal subkey_round_state : unsigned(FOLD_RATIO_NUM_BITS-1 downto 0) := (others => '1'); --one bit for half size, two bits for quarter size, constant 0 for full size
	
	
begin

	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to TF_PIPELINE_STAGES;
		variable subkey_round : integer range 0 to 19;
		variable temp_subkey : state_type;
		
	begin
		if rising_edge(clk) then	
		
			for ii in 0 to TF_PIPELINE_STAGES-1 loop
				if skein_pipe(ii).nonce = x"00000004ECF83A53" then
					--report " skein_pipe " & to_string(ii) & " state: " & to_hstring(skein_pipe(ii).state(0)) & " key: " & to_hstring(skein_pipe(ii).key(0)) & " loop count " & to_string(skein_pipe(ii).loop_count);
				end if;
			end loop;
		
			-- iterate through the subkey base rounds 
			subkey_round_state <= subkey_round_state + 1;
			
			round := 0;  -- current threefish round (0 to 7)
		
			-- generate first subkey
			--subkey_round := SUBKEY_BASE_ROUND + 2* to_integer(skein_in.loop_count);--to_integer(subkey_round_state);
			subkey_round := SUBKEY_BASE_ROUND + 2*to_integer(subkey_round_state);

			skein_pipe(0) <= skein_in;
			temp_subkey := f_Get_Subkey(subkey_round, TWEAK, skein_in.key);
			subkey(0) <= temp_subkey;
			
			if skein_in.nonce = x"00000004ECF83A53" then
				--report skein_round'INSTANCE_NAME & "subkey " & to_string(subkey_round) & " : " & to_hstring(temp_subkey(0)) & " tweak " & to_hstring(TWEAK(0)) & " key " & to_hstring(skein_in.key(0));
			end if;
			pipe := 1;
			
			--add subkey to the state
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, subkey(0));
			pipe := pipe + 1;
			subkey_round := subkey_round + 1;
			
			-- four threefish rounds
			for jj in 1 to 4 loop
				skein_pipe(pipe) <= skein_pipe(pipe - 1);
				skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
				--threefish_state_retimining_array(round) <= f_threefish_1(skein_pipe(pipe-1).state, round);
				--pipe := pipe + 1;
				--skein_pipe(pipe) <= skein_pipe(pipe - 1); 
				--skein_pipe(pipe).state <= threefish_state_retimining_array(round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			
			-- generate next subkey
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			temp_subkey := f_Get_Subkey(subkey_round, TWEAK, skein_pipe(pipe - 1).key);
			subkey(1) <= temp_subkey;
			
			if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
				--report skein_round'INSTANCE_NAME & "subkey " & to_string(subkey_round) & " : " & to_hstring(temp_subkey(0)) & " tweak " & to_hstring(TWEAK(0)) & " key " & to_hstring(skein_pipe(pipe-1).key(0));
			end if;
			pipe := pipe + 1;
			
			--add subkey to the state
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, subkey(1));
			pipe := pipe + 1;
			
			-- four threefish rounds
			for jj in 1 to 4 loop
				skein_pipe(pipe) <= skein_pipe(pipe - 1);
				skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
				--threefish_state_retimining_array(round) <= f_threefish_1(skein_pipe(pipe-1).state, round);
				--pipe := pipe + 1;
				--skein_pipe(pipe) <= skein_pipe(pipe - 1); 
				--skein_pipe(pipe).state <= threefish_state_retimining_array(round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			
			--skein_pipe(pipe-1).loop_count <= skein_pipe(pipe-1).loop_count + 1;
			
		end if;
	end process;
	
	skein_out <= skein_pipe(TF_PIPELINE_STAGES-1);
	
end rtl;
		
