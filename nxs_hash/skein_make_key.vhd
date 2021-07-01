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
		state_in	: in state_type;
		message_in	: in state_type;
		nonce_in	: in unsigned(63 downto 0);
		key_out		: out key_type;
		nonce_out	: out unsigned(63 downto 0)
	);
	end skein_make_key;

architecture rtl of skein_make_key is

	constant PIPELINE_STAGES : integer := 8; -- this must be a multiple of the fold. 
	type skein_key_array_type is array (0 to PIPELINE_STAGES - 1) of key_type;
	type nonce_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of unsigned(63 downto 0);
	signal nonce_pipe : nonce_pipe_array_type := (others => (others => '0'));
	signal key_pipe : skein_key_array_type := (others => (others => (others => '0')));
	signal state_i : state_type := (others => (others => '0'));

begin
	
	process(clk)
		variable pipe : integer range 0 to PIPELINE_STAGES;
		variable temp_message: state_type := (others => (others => '0'));
		variable jj : integer range 0 to 15;
	begin
		if rising_edge(clk) then		

			for ii in 0 to PIPELINE_STAGES-1 loop
				if nonce_pipe(ii) = x"00000004ECF83A53" then
					--report " skein make key " & to_string(ii) & " key: " & to_hstring(key_pipe(ii)(16));
				end if;
			end loop;
			
			-- the nonce passes through
			nonce_pipe(0) <= nonce_in;
			for ii in 1 to PIPELINE_STAGES-1 loop
				nonce_pipe(ii) <= nonce_pipe(ii-1);
			end loop;
			
			temp_message := message_in;
			temp_message(10) := nonce_in;
			state_i <= f_State_XOR(state_in, temp_message);
			key_pipe(1)(16) <= C240;
			for ii in 0 to 15 loop
				key_pipe(1)(ii) <= state_i(ii);  -- the xor'd state becomes the new key
			end loop;
			pipe := 2;
			for ii in 0 to 3 loop
				jj := ii*4;
				key_pipe(pipe) <= key_pipe(pipe-1);
				--xor last key word with each key word four at a time
				key_pipe(pipe)(16) <= key_pipe(pipe-1)(16) XOR key_pipe(pipe-1)(jj) XOR key_pipe(pipe-1)(jj+1) XOR key_pipe(pipe-1)(jj+2) XOR key_pipe(pipe-1)(jj+3);
				pipe := pipe + 1;
			end loop;
			-- extra stages to make the pipeline a multiple of the fold
			for ii in pipe to PIPELINE_STAGES-1 loop
				key_pipe(ii) <= key_pipe(ii-1);
			end loop;
			
		end if;
	end process;
	
	key_out <= key_pipe(PIPELINE_STAGES-1);
	nonce_out <= nonce_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
