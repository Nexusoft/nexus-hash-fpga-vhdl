-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- strips the flags and escape characters from a message
-- inspired by https://eli.thegreenplace.net/2009/08/12/framing-in-serial-communications

-- Message format:
-- <START><ADDRESS><MESSAGE_BYTE_0><MESSAGE_BYTE_1>...<MESSAGE_BYTE_N><END>


-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.uart_pkg.all;



-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity uart_destuffer is 
	port
	(
		clk		: in std_logic;  
		data_in : in byte_type;
		valid_in : in std_logic;
		address_out : out byte_type;
		data_out: out rx_message_buffer_type;
		valid_out	: out std_logic
	);
end uart_destuffer;



architecture beh of uart_destuffer is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal valid_i : std_logic := '0';
	signal address_i : byte_type := (others => '0');
	signal data_out_i : rx_message_buffer_type := (others => (others => '0'));
	type destuffer_state_machine_type is (IDLE, ADDRESS, MESSAGE);
	signal destuffer_state : destuffer_state_machine_type := IDLE;
	signal escaped : std_logic := '0';
	signal message_pointer : integer range 0 to UART_RX_BUFFER_SIZE := 0;

begin

	valid_out <= valid_i;
	address_out <= address_i;
	data_out <= data_out_i;

	-- parse incoming uart data
	process(clk)
		begin
			if rising_edge(clk) then
				if valid_in = '1' then
					case destuffer_state is
						when IDLE => 
							-- search for the start flag
							if data_in = START_FLAG then
								destuffer_state <= ADDRESS;
								escaped <= '0';
								valid_i <= '0';
								message_pointer <= 0;
							end if;
						when ADDRESS =>
							-- search for the start flag
							if data_in = START_FLAG then
								destuffer_state <= ADDRESS;
								escaped <= '0';
							elsif data_in = END_FLAG then
								destuffer_state <= IDLE;
								escaped <= '0';
							else
								-- get the address byte.  Check for an escape character. 
								if escaped = '1' then
									-- if the previous character was an escape, then apply the escape mask to the next character and use it
									address_i <= data_in xor ESCAPE_MASK;
									escaped <= '0';
									destuffer_state <= MESSAGE;
								else
									--check for escape character
									if data_in = ESCAPE then
										escaped <= '1';
									else
										-- the normal (unescaped) case.  Keep this byte as the address.
										address_i <= data_in;
										escaped <= '0';
										destuffer_state <= MESSAGE;
									end if;
								end if;
							end if;
						when MESSAGE =>
							-- handle unexpected start flag
							if data_in = START_FLAG then
								destuffer_state <= ADDRESS;
								message_pointer <= 0;
								escaped <= '0';
								valid_i <= '0';
							-- check for end flag
							elsif data_in = END_FLAG then
								destuffer_state <= IDLE;
								message_pointer <= 0;
								valid_i <= '1';  -- set the valid flag high.  This should only be high for one clock cycle.
							else
								-- Check for an escape character. 
								if escaped = '1' then
									-- if the previous character was an escape, then apply the escape mask to the next character and use it
									data_out_i(message_pointer) <= data_in xor ESCAPE_MASK;
									escaped <= '0';
									if message_pointer < UART_RX_BUFFER_SIZE then
										message_pointer <= message_pointer + 1;
									end if;
									
								else
									--check for escape character
									if data_in = ESCAPE then
										escaped <= '1';
									else
										-- the normal (unescaped) case.  Keep this byte.
										data_out_i(message_pointer) <= data_in;
										escaped <= '0';
										if message_pointer < UART_RX_BUFFER_SIZE then
											message_pointer <= message_pointer + 1;
										end if;
									end if;
								end if;
							end if;
						when others =>
					end case;
				else
					valid_i <= '0'; -- ensure valid only pulses high for one clock cycle
				end if;
			end if;
		end process;
end beh;
		
