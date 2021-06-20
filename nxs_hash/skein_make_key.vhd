-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- create a skein key from the input state with pipelining.
-- if second pass through skein (state = A_DONE) this does something, otherwise it just passes the data through untouched.

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
entity skein_make_key is 
	
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type;
		message_in	: in state_type
	);
	end skein_make_key;

architecture rtl of skein_make_key is

	constant PIPELINE_STAGES : integer := 18; 
	type skein_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);
	signal result : skein_pipe_type := skein_pipe_init;
	signal state_i : state_type := (others => (others => '0'));

begin
	
	process(clk)
		variable pipe : integer range 0 to PIPELINE_STAGES;
		variable temp_message: state_type := (others => (others => '0'));
	begin
		if rising_edge(clk) then			
			-- register inputs
			skein_pipe(0) <= skein_in;
			temp_message := message_in;
			temp_message(10) := skein_in.nonce;
			state_i <= f_State_XOR(skein_in.state, temp_message);
			-- on the second pass through, xor the state with the message and calculate the new key
			if skein_in.status = A_DONE then
				skein_pipe(0).key(16) <= C240;
			end if;
			skein_pipe(1) <= skein_pipe(0);
			if skein_pipe(0).status = A_DONE then
				skein_pipe(1).state <= (others =>(others => '0'));  -- in round 3 the message is all zeros
			end if;
			pipe := 2;
			for ii in 0 to 15 loop
				if skein_pipe(0).status = A_DONE then
					skein_pipe(1).key(ii) <= state_i(ii);  -- the xor'd state becomes the new key
				end if;
				skein_pipe(pipe) <= skein_pipe(pipe-1);
				if skein_pipe(pipe-1).status = A_DONE then
					--xor last key word with each key word one pipeline stage at a time
					skein_pipe(pipe).key(16) <= skein_pipe(pipe-1).key(16) XOR skein_pipe(pipe-1).key(ii);
				end if;
				pipe := pipe + 1;
			end loop;
			result <= skein_pipe(pipe-1);
		end if;
	end process;
	
	skein_out <= result;
	
end rtl;
		
