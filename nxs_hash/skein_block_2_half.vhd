-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- Half of a complete skein round 2 block.  10 subkey adds and 40 threefish with optional final subkey add.  The key is fixed for the round. 

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
entity skein_block_2_half is 
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_2_pipe_type
	);
	end skein_block_2_half;

architecture rtl of skein_block_2_half is
	
	constant TF_ROUNDS : integer := 40; 
	constant SUBKEY_ADDS :  integer := 10;
	constant PIPELINE_STAGES : integer := TF_ROUNDS + SUBKEY_ADDS + 2;  -- must match the other skein_block_half
	
	type skein_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of skein_2_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_2_pipe_init);
		
	
begin

	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to PIPELINE_STAGES;
		variable subkey_round : integer range 0 to 20;
		variable subkey_round_offset : integer range 0 to 11;
		variable temp_subkey : state_type;
		variable temp_subkey_key : key_type;
		variable tweak : tweak_type;
		
	begin
		if rising_edge(clk) then	
			-- debug messages
			for ii in 0 to PIPELINE_STAGES-1 loop
				if skein_pipe(ii).nonce = x"00000004ECF83A53" then
					--report " skein_pipe " & to_string(ii) & " state: " & to_hstring(skein_pipe(ii).state(0));
				end if;
			end loop;
			
			pipe := 0;
			skein_pipe(pipe).state <= skein_in.state;
			skein_pipe(pipe).nonce <= skein_in.nonce;
			skein_pipe(pipe).loop_count <= skein_in.loop_count;
			pipe := pipe + 1;
			subkey_round_offset := 0 when skein_pipe(pipe-1).loop_count = 0 else 10;
			subkey_round := 0;
			temp_subkey := f_Get_Subkey(subkey_round + subkey_round_offset, T2, skein_in.key);
			subkey_round := 1;
			if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
				--report " subkey round 0 or 10 " & to_hstring(temp_subkey(0));
			end if;

			skein_pipe(pipe) <= skein_pipe(pipe-1);
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
			pipe := pipe + 1;
			round := 0;  -- current threefish round

			-- first four threefish rounds
			for jj in 1 to 4 loop
				skein_pipe(pipe) <= skein_pipe(pipe - 1);
				skein_pipe(pipe).state <= f_threefish_1(skein_pipe(pipe-1).state, round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			
			for ii in 1 to SUBKEY_ADDS - 1 loop
				if skein_pipe(pipe-1).nonce = x"00000004ECF83A53" then
					--report " subkey round " & to_string(skein_pipe(pipe-1).subkey_round) & ": " & to_hstring(temp_subkey(0));
				end if;
				subkey_round_offset := 0 when skein_pipe(pipe-1).loop_count = 0 else 10;
				temp_subkey := f_Get_Subkey(subkey_round + subkey_round_offset, T2, skein_in.key);
				--add next subkey to the state
				skein_pipe(pipe) <= skein_pipe(pipe-1);
				skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
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
			-- add the final subkey the second time through
			skein_pipe(pipe) <= skein_pipe(pipe-1);
			case skein_pipe(pipe-1).loop_count is
			when 1 =>
				temp_subkey := f_Get_Subkey(20, T2, skein_in.key);
			when others =>
				temp_subkey := (others => (others => '0'));
			end case;
			skein_pipe(pipe).state <= f_State_Add(skein_pipe(pipe-1).state, temp_subkey);
			pipe := pipe + 1;
			
			--any extra stages
			for ii in pipe to PIPELINE_STAGES-1 loop
				skein_pipe(ii) <= skein_pipe(ii-1);
			end loop;
			
			--skein_pipe(PIPELINE_STAGES-1).loop_count <= (skein_pipe(PIPELINE_STAGES-2).loop_count + 1);
			
		end if;
	end process;
	
	skein_out <= skein_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
