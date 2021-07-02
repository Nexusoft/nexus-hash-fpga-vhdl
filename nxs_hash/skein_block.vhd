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
entity skein_block is 
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type
	);
	end skein_block;

architecture rtl of skein_block is
	
	constant TF_ROUNDS : integer := 80; 
	constant SUBKEY_ADDS :  integer := 20;  --excludes the final subkey add
	constant PIPELINE_STAGES : integer := TF_ROUNDS + SUBKEY_ADDS + 2;  -- must be a multiple of 2 if used in half sized design
	
	type skein_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);
	
begin
	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to PIPELINE_STAGES;
		variable subkey_round : integer range 0 to 21;
		variable temp_subkey : state_type;
		variable temp_subkey_key : key_type;
		variable tweak : tweak_type;
		
	begin
		if rising_edge(clk) then	
			-- debug messages
			for ii in 0 to PIPELINE_STAGES-1 loop
				if skein_pipe(ii).nonce = x"00000004ECF83A53" then
					--report " skein_pipe " & to_string(ii) & " state: " & to_hstring(skein_pipe(ii).state(0)) & " next subkey: " & to_hstring(skein_pipe(ii).key(0));-- & " next subkey round " & to_string(skein_pipe(ii).subkey_round);
				end if;
			end loop;
			
			pipe := 0;
			-- in skein 3 we need to make a key from the previous state
			skein_pipe(pipe) <= skein_in;
			if skein_in.loop_count = 1 then
				skein_pipe(pipe).key <= f_Get_First_Subkey(T3,f_make_key(skein_in.state));
				skein_pipe(pipe).state <= (others =>(others => '0'));  -- in round 3 the message is all zeros
			end if;
			-- in every other case we use the key we get
			pipe := pipe + 1;
			
			for ii in 0 to 15 loop
				temp_subkey(ii) := skein_pipe(pipe-1).key(ii);
			end loop;
			
			if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
				--report " subkey round 0 or 10 " & to_hstring(temp_subkey(0));
			end if;

			-- add the first subkey.  The incoming key is the first subkey.
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
			subkey_round := 1;
			-- Use tweak two for the first two passes through the block.  
			tweak := T2 when skein_pipe(pipe-1).loop_count = 0 else T3;
			-- generate and save the next subkey
			temp_subkey_key := f_Get_Next_Subkey(subkey_round, tweak, skein_pipe(pipe-1).key);
			skein_pipe(pipe).key <= temp_subkey_key;
			pipe := pipe + 1;
			subkey_round := 2;
			round := 0;  -- current threefish round

			-- first four threefish rounds
			for jj in 1 to 4 loop
				skein_pipe(pipe) <= skein_pipe(pipe - 1);
				skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			
			for ii in 1 to SUBKEY_ADDS - 1 loop
				-- the next subkey is stored in the previous key
				for jj in 0 to 15 loop
					temp_subkey(jj) := skein_pipe(pipe-1).key(jj);
				end loop;
			
				if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
					--report " subkey round " & to_string(skein_pipe(pipe-1).subkey_round) & ": " & to_hstring(temp_subkey(0));
				end if;
			
				--add next subkey to the state
				skein_pipe(pipe) <= skein_pipe(pipe-1);
				skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
				-- generate and save the next subkey
				tweak := T2 when skein_pipe(pipe-1).loop_count = 0 else T3;
				temp_subkey_key := f_Get_Next_Subkey(subkey_round, tweak, skein_pipe(pipe-1).key);
				skein_pipe(pipe).key <= temp_subkey_key;
				pipe := pipe + 1;
				subkey_round := subkey_round + 1;
				-- four threefish rounds
				for jj in 1 to 4 loop
					skein_pipe(pipe) <= skein_pipe(pipe - 1);
					skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
					pipe := pipe + 1;
					round := round + 1;
				end loop;
			end loop;
			-- add the final subkey which was calculated in the last loop above and stored in the key
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			for jj in 0 to 15 loop
				temp_subkey(jj) := skein_pipe(pipe-1).key(jj);
			end loop;
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
			pipe := pipe + 1;
			
			--any extra stages
			for ii in pipe to PIPELINE_STAGES-1 loop
				skein_pipe(ii) <= skein_pipe(ii-1);
			end loop;
			
			skein_pipe(PIPELINE_STAGES-1).loop_count <= (skein_pipe(PIPELINE_STAGES-2).loop_count + 1) mod FOLD_RATIO;
			
		end if;
	end process;
	
	skein_out <= skein_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
