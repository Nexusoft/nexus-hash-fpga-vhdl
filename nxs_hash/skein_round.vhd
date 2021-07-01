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
	
	type state_array_type is array (0 to TF_ROUNDS - 1) of state_type;
	
	
begin

	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to TF_PIPELINE_STAGES;
		variable subkey_round : integer range 0 to 20;
		variable temp_subkey, temp_subkey_2 : state_type;
		variable temp_subkey_key : key_type;
		
	begin
		if rising_edge(clk) then	
			-- debug messages
			for ii in 0 to TF_PIPELINE_STAGES-1 loop
				if skein_pipe(ii).nonce = x"00000004ECF83A53" then
					--report " skein_pipe " & to_string(ii) & " state: " & to_hstring(skein_pipe(ii).state(0)) & " next key: " & to_hstring(skein_pipe(ii).key(0)) & " next subkey round " & to_string(skein_pipe(ii).subkey_round);
				end if;
			end loop;
		
			
			round := 0;  -- current threefish round (0 to 7)
	
			for ii in 0 to 15 loop
				temp_subkey(ii) := skein_in.key(ii);
			end loop;
			
			if skein_in.nonce = x"00000004ECF83A53" then
				--report " subkey round " & to_string(skein_in.subkey_round) & ": " & to_hstring(temp_subkey(0));
			end if;

			-- register incoming data and add the first subkey.  The incoming key is the first subkey.
			skein_pipe(0).nonce <= skein_in.nonce;
			skein_pipe(0).state <= f_State_Add(skein_in.state, temp_subkey);
			subkey_round := skein_in.subkey_round + 1;
			skein_pipe(0).subkey_round <= subkey_round;
			-- generate and save the next subkey
			temp_subkey_key := f_Get_Next_Subkey(subkey_round, TWEAK, skein_in.key);
			skein_pipe(0).key <= temp_subkey_key;
			pipe := 1;
			
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
			-- extra round to make this match skein 2.  Do we need it?
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			pipe := pipe + 1;
			
			for ii in 0 to 15 loop
				temp_subkey(ii) := skein_pipe(pipe-1).key(ii);
			end loop;
			
			if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
				--report " subkey round " & to_string(skein_pipe(pipe-1).subkey_round) & ": " & to_hstring(temp_subkey(0));
			end if;
			
			--add next subkey to the state
			skein_pipe(pipe).nonce <= skein_pipe(pipe-1).nonce;
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
			subkey_round := skein_pipe(pipe-1).subkey_round + 1;
			skein_pipe(pipe).subkey_round <= subkey_round;
			-- generate and save the next subkey
			temp_subkey_key := f_Get_Next_Subkey(subkey_round, TWEAK, skein_pipe(pipe-1).key);
			skein_pipe(pipe).key <= temp_subkey_key;
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
			
			--extra stages if needed
			for ii in pipe to TF_PIPELINE_STAGES-1 loop
				skein_pipe(ii) <= skein_pipe(ii-1);
			end loop;
			
		end if;
	end process;
	
	skein_out <= skein_pipe(TF_PIPELINE_STAGES-1);
	
end rtl;
		
