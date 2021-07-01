-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- Two subkey add and eight rounds of threefish for skein round 2.  This round uses precalculated subkeys. 

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
entity skein_round_2 is 
	
	port
	(
		clk			: in std_logic;
		state_in	: in state_type;
		subkey_1_in	: state_type;
		subkey_2_in	: state_type;
		subkey_3_in	: state_type;
		subkey_4_in	: state_type;
		nonce_in	: in unsigned(63 downto 0);
		state_out	: out state_type;
		nonce_out	: out unsigned(63 downto 0)
	);
	end skein_round_2;

architecture rtl of skein_round_2 is
	
	constant TF_ROUNDS : integer := 8; 
	constant SUBKEY_ADDS :  integer := 2;
	constant PIPELINE_STAGES : integer := TF_ROUNDS + SUBKEY_ADDS*2;  -- must be the same length as skein round 3
	
	type state_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of state_type;
	type nonce_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of unsigned(63 downto 0);

	signal state_pipe : state_pipe_array_type := (others =>(others =>(others => '0')));
	signal nonce_pipe : nonce_pipe_array_type := (others =>(others => '0'));
	signal subkey_round_state : unsigned(FOLD_RATIO_NUM_BITS-1 downto 0) := (others => '1'); --one bit for half size, two bits for quarter size, constant 0 for full size


begin

	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
		variable pipe : integer range 0 to PIPELINE_STAGES;
		variable temp_subkey : state_type;

		
	begin
		if rising_edge(clk) then	
		
			subkey_round_state <= subkey_round_state + 1;
		
			-- then nonces just pass through
			nonce_pipe(0) <= nonce_in;
			for ii in 1 to PIPELINE_STAGES-1 loop
				nonce_pipe(ii) <= nonce_pipe(ii-1);
			end loop;
			
			--debug messages
			for ii in 0 to PIPELINE_STAGES-1 loop
				if nonce_pipe(ii) = x"00000004ECF83A53" then
					--report " pipe " & to_string(ii) & " state: " & to_hstring(state_pipe(ii)(0)) & " subkey A: " & to_hstring(subkey_1_in(0)) & " subkey B: " & to_hstring(subkey_2_in(0));
				end if;
			end loop;

			round := 0;  -- current threefish round (0 to 7)
			pipe := 0;
			temp_subkey := subkey_1_in when subkey_round_state = 0 else subkey_3_in;
			--add subkey to the state
			state_pipe(pipe) <= f_State_Add(state_in, temp_subkey);
			pipe := pipe + 1;
			
			-- four threefish rounds
			for jj in 1 to 4 loop
				state_pipe(pipe) <= f_threefish_1(state_pipe(pipe-1), round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			state_pipe(pipe) <= state_pipe(pipe-1);
			pipe := pipe + 1;
			
			temp_subkey := subkey_2_in when subkey_round_state = 0 else subkey_4_in;
			--add subkey to the state
			state_pipe(pipe) <= f_State_Add(state_pipe(pipe-1), temp_subkey);
			pipe := pipe + 1;
			
			-- four threefish rounds
			for jj in 1 to 4 loop
				state_pipe(pipe) <= f_threefish_1(state_pipe(pipe-1), round);
				pipe := pipe + 1;
				round := round + 1;
			end loop;
			
			--extra stages to match skein_3 length if needed
			for ii in pipe to PIPELINE_STAGES-1 loop
				state_pipe(ii) <= state_pipe(ii-1);
			end loop;
						
		end if;
	end process;
	
	state_out <= state_pipe(PIPELINE_STAGES-1);
	nonce_out <= nonce_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
