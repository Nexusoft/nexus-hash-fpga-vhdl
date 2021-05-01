-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- adds flags and escape characters to a message in preparation for sending
-- inspired by https://eli.thegreenplace.net/2009/08/12/framing-in-serial-communications


-- Input message format
--<number of bytes excluding the address><address><data byte 0><data byte 1>...<data byte N>
-- Output Message byte stream format:
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
entity uart_stuffer is 
	port
	(
		clk		: in std_logic;  
		ready_out : out std_logic;
		data_to_send : in tx_message_buffer_type;
		valid_in : in std_logic;
		byte_stream_out : out byte_type;
		valid_out	: out std_logic
	);
end uart_stuffer;



architecture beh of uart_stuffer is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal valid_out_i, ready_out_i : std_logic := '0';
	signal address_i : byte_type := (others => '0');
	signal data_out_i : byte_type := (others => '0');
	signal data_in : tx_message_buffer_type;
	type stuffer_state_machine_type is (IDLE, START, ADDRESS, MESSAGE, END_STATE);
	signal stuffer_state : stuffer_state_machine_type := IDLE;
	signal escaped : std_logic := '0';
	signal numBytes, bytePointer : integer range 0 to UART_TX_BUFFER_SIZE-1 := 0;  -- number of bytes of the data only
	constant DATA_OFFSET : integer := 2; -- start position in bytes of the data in the incoming message

begin

	valid_out <= valid_out_i;
	byte_stream_out <= data_out_i;
	ready_out <= ready_out_i;

	-- check for available messages and process
	process(clk)
		begin
			if rising_edge(clk) then
				case stuffer_state is
					when IDLE =>
						valid_out_i <= '0';
						-- reset the byte pointer
						bytePointer <= DATA_OFFSET;
						if valid_in = '1' and ready_out_i = '1' then
							-- new message is detected
							stuffer_state <= START;
							-- we are no longer ready to accept new data
							ready_out_i <= '0';
							-- latch the data
							data_in <= data_to_send;
						else	
							-- Nothing new.  Continue waiting for the next message
							ready_out_i <= '1';
						end if;
					when START =>
						-- get the address and numBytes
						numBytes <= to_integer(unsigned(data_in(0)));
						address_i <= data_in(1);
						-- start a new frame by sending a start flag
						data_out_i <= START_FLAG;
						valid_out_i <= '1';
						stuffer_state <= ADDRESS;
					when ADDRESS =>
						-- check if we are escaped
						if escaped = '1' then
							-- xor the address and send it
							data_out_i <= address_i xor ESCAPE_MASK;
							valid_out_i <= '1';
							stuffer_state <= MESSAGE;
							escaped <= '0';
						else
							-- check if the address needs to be escaped
							if escape_check(address_i) = '1' then
								-- send the escape character
								data_out_i <= ESCAPE;
								valid_out_i <= '1';
								escaped <= '1';
							else
								-- the normal (unescaped) case.  Send the address
								data_out_i <= address_i;
								valid_out_i <= '1';
								stuffer_state <= MESSAGE;
							end if;
						end if;
					when MESSAGE =>
						-- check if we are escaped
						if escaped = '1' then
							-- xor the message data and send it
							data_out_i <= data_in(bytePointer) xor ESCAPE_MASK;
							valid_out_i <= '1';
							bytePointer <= bytePointer + 1;
							escaped <= '0';
						else
							-- check if we've reached the end of the message
							if bytePointer > numBytes + DATA_OFFSET - 1 then
								valid_out_i <= '0';
								-- reset the bytePointer
								bytePointer <= DATA_OFFSET;
								stuffer_state <= END_STATE;
							elsif escape_check(data_in(bytePointer)) = '1' then -- check if the data needs to be escaped	
								-- send the escape character
								data_out_i <= ESCAPE;
								valid_out_i <= '1';
								escaped <= '1';
							else
								-- the normal (unescaped) case.  Send the data
								data_out_i <= data_in(bytePointer);
								valid_out_i <= '1';
								bytePointer <= bytePointer + 1;
							end if;
						end if;
					when END_STATE =>
						-- finish the frame by sending an end flag
						data_out_i <= END_FLAG;
						valid_out_i <= '1';
						stuffer_state <= IDLE;
						
					when others =>
				end case;
			end if;
		end process;
end beh;
		
