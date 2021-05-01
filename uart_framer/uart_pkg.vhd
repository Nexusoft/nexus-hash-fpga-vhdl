-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- package file for serial communication link

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.skein_pkg.all;



package uart_pkg is
	
	subtype byte_type is std_logic_vector(7 downto 0);
	constant UART_RX_BUFFER_SIZE : integer := 2*1088/8;  -- Size of the receive side uart buffer in bytes for destuffed messages.  This must be large enough to hold a 1088 bit skein key in ascii format.
	constant KEY2_RX_BYTE_COUNT : integer := 2*1088/8; -- number of bytes in ascii version of the key
	constant MESSAGE2_RX_BYTE_COUNT : integer := 2*1024/8; -- size of the ascii message;
	type rx_message_buffer_type is ARRAY(0 to UART_RX_BUFFER_SIZE - 1) of byte_type; 
	constant UART_TX_BUFFER_SIZE : integer := 32;  -- Width of the transmit side message buffer in bytes.  This holds stuffed messages (includes frame flags).  Big enough to hold a nonce in ascii format plus flags. 
	type tx_message_buffer_type is ARRAY(0 to UART_TX_BUFFER_SIZE - 1) of byte_type; 
	constant UART_TX_MESSAGE_FIFO_WIDTH : integer := UART_TX_BUFFER_SIZE * 8;  --width of the tx message fifo in bits
	subtype tx_message_buffer_slv_type is std_logic_vector(UART_TX_MESSAGE_FIFO_WIDTH - 1 downto 0);
	constant TX_BUFFER_MESSAGE_SIZE_LIMIT : integer := 8;  -- maximum tx message payload in bytes
	
	-----------------------------------------------------------------------------------------------
	-- registers
	-----------------------------------------------------------------------------------------------
	-- test register - must be first register
	-- write only
	constant PING_REG_ADDR					: byte_type := x"30"; -- ascii 0

	-- signature register - always returns ascii "ABCD1234" or hex 0x4142434431323334
	-- read only
	constant SIGNATURE_REG_ADDR				: byte_type := x"31";

	-- skein input registers
	-- write only
	constant SKEIN_KEY2_ADDR				: byte_type := x"32";
	constant SKEIN_MESSAGE2_ADDR			: byte_type := x"33";
	
	-- results register
	-- read only
	constant NONCE_ADDR						: byte_type := x"34";
	constant FOUND_ADDR						: byte_type := x"35";
	 
	-- enable hashing
	-- write only
	constant ENABLE_HASHING_ADDR			: byte_type := x"36";
	constant ENABLE_HASHING					: byte_type := x"31"; -- ascii 1
	constant DISABLE_HASHING				: byte_type := x"30"; -- ascii 0
	
	
	-- debug registers
	-- read only
	constant DIFF_28_COUNT_ADDR				: byte_type := x"37";
	-- write only
	constant HASH_TEST						: byte_type := x"38";
	
	-- for response to ping
	constant ACK							: byte_type := x"4B"; -- ascii "K"
	
	-- difficulty
	-- write only
	constant BITS_ADDR						: byte_type := x"39";
	
	
	-- special characters
	-- start flag
	constant START_FLAG 	: byte_type := x"7B"; -- ascii {
	constant ESCAPE			: byte_type := x"5C"; -- ascii \
	constant END_FLAG		: byte_type := x"7D"; -- ascii }
	constant ESCAPE_MASK	: byte_type := x"20";
	
	-- used to convert text to ascii bytes in simulation
	type T is array(CHARACTER range NUL to DEL) of byte_type; 
	constant CHAR2STD: T := 
    ("00000000", "00000001", "00000010", "00000011", 
     "00000100", "00000101", "00000110", "00000111", 
     "00001000", "00001001", "00001010", "00001011", 
     "00001100", "00001101", "00001110", "00001111", 
     "00010000", "00010001", "00010010", "00010011", 
     "00010100", "00010101", "00010110", "00010111", 
     "00011000", "00011001", "00011010", "00011011", 
     "00011100", "00011101", "00011110", "00011111", 
     "00100000", "00100001", "00100010", "00100011", 
     "00100100", "00100101", "00100110", "00100111", 
     "00101000", "00101001", "00101010", "00101011", 
     "00101100", "00101101", "00101110", "00101111", 
     "00110000", "00110001", "00110010", "00110011", 
     "00110100", "00110101", "00110110", "00110111", 
     "00111000", "00111001", "00111010", "00111011", 
     "00111100", "00111101", "00111110", "00111111", 
     "01000000", "01000001", "01000010", "01000011", 
     "01000100", "01000101", "01000110", "01000111", 
     "01001000", "01001001", "01001010", "01001011", 
     "01001100", "01001101", "01001110", "01001111", 
     "01010000", "01010001", "01010010", "01010011", 
     "01010100", "01010101", "01010110", "01010111", 
     "01011000", "01011001", "01011010", "01011011", 
     "01011100", "01011101", "01011110", "01011111", 
     "01100000", "01100001", "01100010", "01100011", 
     "01100100", "01100101", "01100110", "01100111", 
     "01101000", "01101001", "01101010", "01101011", 
     "01101100", "01101101", "01101110", "01101111", 
     "01110000", "01110001", "01110010", "01110011", 
     "01110100", "01110101", "01110110", "01110111", 
     "01111000", "01111001", "01111010", "01111011", 
     "01111100", "01111101", "01111110", "01111111"); 
	 
	 type CH is array(integer range 32 to 126) of character;
	 constant BYTE2CHAR : CH  := 
	 (	' ', '!', '"', '#', '$', '%', '&', ''', 
		'(', ')', '*', '+', ',', '-', '.', '/', 
		'0', '1', '2', '3', '4', '5', '6', '7', 
		'8', '9', ':', ';', '<', '=', '>', '?', 

		'@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 
		'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 
		'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 
		'X', 'Y', 'Z', '[', '\', ']', '^', '_', 

		'`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 
		'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 
		'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 
		'x', 'y', 'z', '{', '|', '}', '~');
	 
	 
	
	-- function prototypes
	function unpack_key2(rxBuf : in rx_message_buffer_type) return key_type;
	function unpack_message2(rxBuf : in rx_message_buffer_type) return state_type;
	function unpack_bitsTarget(rxBuf : in rx_message_buffer_type) return unsigned;
	function bin_to_ascii(x : in std_logic_vector(3 downto 0)) return byte_type;
	function ascii_to_bin(x : in byte_type) return std_logic_vector;
	function tx_message_buffer_array_to_slv(msg : in tx_message_buffer_type) return tx_message_buffer_slv_type;
	function tx_message_buffer_slv_to_array(msg_slv : in tx_message_buffer_slv_type) return tx_message_buffer_type;
	function pack_nonce(nonce : in unsigned(63 downto 0)) return tx_message_buffer_slv_type;
	function pack_activity_counter(activity_counter : in unsigned(31 downto 0)) return tx_message_buffer_slv_type;
	function pack_found_counter(found_counter : in unsigned(31 downto 0)) return tx_message_buffer_slv_type;
	function pack_ack return tx_message_buffer_slv_type;
	function pack_signature return tx_message_buffer_slv_type;
	function escape_check(x : in byte_type) return std_logic;
	
end package uart_pkg;

package body uart_pkg is

	-- parse the key sent from the host
	function unpack_key2(rxBuf : in rx_message_buffer_type) return key_type is
		variable key : key_type := (others => (others => '0'));
		variable kk : integer range 0 to 256;
		begin
			for ii in 0 to 16 loop -- words
				kk := ii*16; -- 16 ascii bytes per output word
				for jj in 8 downto 1 loop -- bytes
					key(ii)(8*jj-1 downto 8*jj-4) := unsigned(ascii_to_bin(rxBuf(14-2*(jj-1)+kk)));
					key(ii)(8*jj-5 downto 8*jj-8) := unsigned(ascii_to_bin(rxBuf(14-2*(jj-1)+kk+1)));
				end loop;
			end loop;
			return key;
		end function unpack_key2;
		
	-- parse the skein message sent from the host
	function unpack_message2(rxBuf : in rx_message_buffer_type) return state_type is
		variable message : state_type := (others => (others => '0'));
		variable kk : integer range 0 to 240;
		begin
			for ii in 0 to 15 loop -- words
				kk := ii*16;
				for jj in 8 downto 1 loop -- bytes
					message(ii)(8*jj-1 downto 8*jj-4) := unsigned(ascii_to_bin(rxBuf(14-2*(jj-1)+kk)));
					message(ii)(8*jj-5 downto 8*jj-8) := unsigned(ascii_to_bin(rxBuf(14-2*(jj-1)+kk+1)));
				end loop;
			end loop;
			return message;
		end function unpack_message2;
	
	function unpack_bitsTarget(rxBuf : in rx_message_buffer_type) return unsigned is
		variable bits : unsigned(31 downto 0);
		begin
			bits(31 downto 28) := unsigned(ascii_to_bin(rxBuf(0)));
			bits(27 downto 24) := unsigned(ascii_to_bin(rxBuf(1)));
			bits(23 downto 20) := unsigned(ascii_to_bin(rxBuf(2)));
			bits(19 downto 16) := unsigned(ascii_to_bin(rxBuf(3)));
			bits(15 downto 12) := unsigned(ascii_to_bin(rxBuf(4)));
            bits(11 downto 8) := unsigned(ascii_to_bin(rxBuf(5)));
            bits(7 downto 4) := unsigned(ascii_to_bin(rxBuf(6)));
            bits(3 downto 0) := unsigned(ascii_to_bin(rxBuf(7)));
			
			return bits;
		end function unpack_bitsTarget;

	-- convert hex character from ascii byte to binary
	function ascii_to_bin(x : in byte_type) return std_logic_vector is
		variable x_bin : std_logic_vector(3 downto 0);
		begin
			case x is
				when x"30" => -- "0"
					x_bin := "0000";
				when x"31" => -- "1"
					x_bin := "0001";
				when x"32" => 
					x_bin := "0010";
				when x"33" => 
					x_bin := "0011";
				when x"34" => 
					x_bin := "0100";
				when x"35" => 
					x_bin := "0101";
				when x"36" => 
					x_bin := "0110";
				when x"37" => 
					x_bin := "0111";
				when x"38" => 
					x_bin := "1000";
				when x"39" => -- "9"
					x_bin := "1001";
				when x"41" =>  -- "A"
					x_bin := "1010";
				when x"42" =>  -- "B"
					x_bin := "1011";
				when x"43" => 
					x_bin := "1100";
				when x"44" => 
					x_bin := "1101";
				when x"45" => 
					x_bin := "1110";
				when x"46" => -- "F"
					x_bin := "1111";					
				when others =>
					x_bin := "0000";
			end case;
			return x_bin;
		end function ascii_to_bin;
		
	-- convert a 4 bit binary number to an ascii hex character
	function bin_to_ascii(x : in std_logic_vector(3 downto 0)) return byte_type is
		variable x_ascii : byte_type;
		begin
			case x is
				when "0000" => -- "0"
					x_ascii := x"30";
				when "0001" => -- "1"
					x_ascii := x"31";
				when "0010" =>
					x_ascii := x"32";
				when "0011" =>
					x_ascii := x"33";
				when "0100" =>
					x_ascii := x"34";
				when "0101" =>
					x_ascii := x"35";
				when "0110" =>
					x_ascii := x"36";
				when "0111" =>
					x_ascii := x"37";
				when "1000" =>
					x_ascii := x"38";
				when "1001" =>
					x_ascii := x"39";
				when "1010" => -- "A"
					x_ascii := x"41";
				when "1011" =>
					x_ascii := x"42";
				when "1100" =>
					x_ascii := x"43";
				when "1101" =>
					x_ascii := x"44";
				when "1110" =>
					x_ascii := x"45";
				when "1111" => -- "F"
					x_ascii := x"46";
				when others =>
					x_ascii := x"58"; -- ascii "X"
			end case;
			return x_ascii;
		end function bin_to_ascii;
	
	-- convert array of bytes to std_logic_vector
	function tx_message_buffer_array_to_slv(msg : in tx_message_buffer_type) return tx_message_buffer_slv_type is
		variable msg_slv : tx_message_buffer_slv_type := (others => '0');
		variable topBit : integer range 0 to UART_TX_MESSAGE_FIFO_WIDTH-1;
		begin
			for ii in 0 to UART_TX_BUFFER_SIZE-1 loop
				topBit := UART_TX_MESSAGE_FIFO_WIDTH-1 - ii*8;
				msg_slv(topBit downto topBit - 7) := msg(ii);
			end loop;
			return msg_slv;
		end function tx_message_buffer_array_to_slv;
    -- reverse conversion
    function tx_message_buffer_slv_to_array(msg_slv : in tx_message_buffer_slv_type) return tx_message_buffer_type is
            variable msg : tx_message_buffer_type := (others => (others =>'0'));
            variable topBit : integer range 0 to UART_TX_MESSAGE_FIFO_WIDTH-1;
            begin
                for ii in 0 to UART_TX_BUFFER_SIZE-1 loop
                    topBit := UART_TX_MESSAGE_FIFO_WIDTH-1 - ii*8;
                    msg(ii) := msg_slv(topBit downto topBit - 7);
                end loop;
                return msg;
            end function tx_message_buffer_slv_to_array;


	--packing functions converts data to be sent to a tx_message_buffer format which is
	--<number of bytes excluding the address><address><data byte 0><data byte 1>...<data byte N>

	function pack_nonce(nonce : in unsigned(63 downto 0)) return tx_message_buffer_slv_type is
		variable data_out : tx_message_buffer_type;
		constant DATA_LEN : integer := 8; -- number of bytes of the data to send
		variable jj : integer range 0 to 2*DATA_LEN; -- alternate index to increment by 2
		begin
			data_out(0) := std_logic_vector(to_unsigned(DATA_LEN*2, 8));  -- the first byte is the length of the payload in bytes after converting to ascii.  
			data_out(1) := NONCE_ADDR;  -- the second byte is the address
			for ii in 1 to DATA_LEN loop 
				jj := ii*2;-- each byte of binary data requires 2 bytes of ascii data
				data_out(jj) := bin_to_ascii(std_logic_vector(nonce(63-(ii-1)*8 downto 63-(ii-1)*8-3))); -- convert 4 bits to ascii byte
				data_out(jj+1) := bin_to_ascii(std_logic_vector(nonce(63-(ii-1)*8-4 downto 63-(ii-1)*8-7))); -- convert next 4 bits to ascii byte
			end loop;  
			return tx_message_buffer_array_to_slv(data_out);
		end function pack_nonce;
		
	function pack_activity_counter(activity_counter : in unsigned(31 downto 0)) return tx_message_buffer_slv_type is
		variable data_out : tx_message_buffer_type;
		constant DATA_LEN : integer := 4; -- number of bytes of the data to send
		variable jj : integer range 0 to 2*DATA_LEN; -- alternate index to increment by 2
		begin
			data_out(0) := std_logic_vector(to_unsigned(DATA_LEN*2, 8));  -- the first byte is the length of the payload in bytes after converting to ascii.  
			data_out(1) := DIFF_28_COUNT_ADDR;  -- the second byte is the address
			for ii in 1 to DATA_LEN loop 
				jj := ii*2;-- each byte of binary data requires 2 bytes of ascii data
				data_out(jj) := bin_to_ascii(std_logic_vector(activity_counter(31-(ii-1)*8 downto 31-(ii-1)*8-3))); -- convert 4 bits to ascii byte
				data_out(jj+1) := bin_to_ascii(std_logic_vector(activity_counter(31-(ii-1)*8-4 downto 31-(ii-1)*8-7))); -- convert next 4 bits to ascii byte
			end loop;  
			return tx_message_buffer_array_to_slv(data_out);
		end function pack_activity_counter;
	
	
	
	function pack_ack return tx_message_buffer_slv_type is
	variable data_out : tx_message_buffer_type;
		constant DATA_LEN : integer := 0; -- number of bytes of the data to send
		begin
			data_out(0) := std_logic_vector(to_unsigned(DATA_LEN,8));  -- the first byte is the length of the data in bytes.  
			data_out(1) := ACK;  -- the second byte is the address
			return tx_message_buffer_array_to_slv(data_out);
		end function pack_ack;
		
	function pack_found_counter(found_counter : in unsigned(31 downto 0)) return tx_message_buffer_slv_type is
	-- return the 32 bit found count. 
		variable data_out : tx_message_buffer_type;
		constant DATA_LEN : integer := 4; -- number of bytes of the data to send
		variable jj : integer range 0 to 2*DATA_LEN; -- alternate index to increment by 2
		begin
			data_out(0) := std_logic_vector(to_unsigned(DATA_LEN*2, 8));  -- the first byte is the length of the payload in bytes after converting to ascii.  
			data_out(1) := FOUND_ADDR;  -- the second byte is the address
			for ii in 1 to DATA_LEN loop 
				jj := ii*2;-- each byte of binary data requires 2 bytes of ascii data
				data_out(jj) := bin_to_ascii(std_logic_vector(found_counter(31-(ii-1)*8 downto 31-(ii-1)*8-3))); -- convert 4 bits to ascii byte
				data_out(jj+1) := bin_to_ascii(std_logic_vector(found_counter(31-(ii-1)*8-4 downto 31-(ii-1)*8-7))); -- convert next 4 bits to ascii byte
			end loop;  
			return tx_message_buffer_array_to_slv(data_out);
		end function pack_found_counter;
	
	function pack_signature return tx_message_buffer_slv_type is
	variable data_out : tx_message_buffer_type;
		constant DATA_LEN : integer := 8; -- number of bytes of the data to send
		begin
			data_out(0) := std_logic_vector(to_unsigned(DATA_LEN,8));  -- the first byte is the length of the data in bytes.  
			data_out(1) := SIGNATURE_REG_ADDR;  -- the second byte is the address
			data_out(2) := x"41";
			data_out(3) := x"42";
			data_out(4) := x"43";
			data_out(5) := x"44";
			data_out(6) := x"31";
            data_out(7) := x"32";
            data_out(8) := x"33";
            data_out(9) := x"34";
			return tx_message_buffer_array_to_slv(data_out);
		end function pack_signature;

	function escape_check(x : in byte_type) return std_logic is 
		-- check if a byte matches one of the special characters.  If it does return '1'.
		variable matches : std_logic := '0';
		begin
			if x = START_FLAG or x = ESCAPE or x = END_FLAG then
				matches := '1';
			end if;
			return matches;
		end function escape_check;
		
		
		
end package body uart_pkg;
