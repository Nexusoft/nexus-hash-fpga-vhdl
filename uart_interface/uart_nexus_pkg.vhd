-------------------------------------------------------------------------------
-- Andrew Hatstat
-------------------------------------------------------------------------------
-- package file for the nexus standard serial communication link for FPGA mining

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
	constant PARITY_BIT : string := "none";
	constant DEFAULT_BAUD_RATE : integer := 230400;
	constant KEY2_WORD_COUNT : integer := 17;
	constant MESSAGE2_WORD_COUNT : integer := 11;
	constant KEY2_BYTE_COUNT : integer := KEY2_WORD_COUNT * 8;
	constant MESSAGE2_BYTE_COUNT : integer := MESSAGE2_WORD_COUNT * 8;
	constant WORK_PACKAGE_SIZE_BYTES : integer := KEY2_BYTE_COUNT + MESSAGE2_BYTE_COUNT;  -- Total size of the work package in bytes
	constant WORK_PACKAGE_SIZE_BITS : integer := WORK_PACKAGE_SIZE_BYTES * 8;

	--type rx_message_buffer_type is ARRAY(0 to WORK_PACKAGE_SIZE_BYTES - 1) of byte_type; 
	constant UART_TX_BUFFER_SIZE : integer := 8;  -- Width of the transmit side message buffer in bytes. Must hold one nonce.
	type tx_message_buffer_type is ARRAY(0 to UART_TX_BUFFER_SIZE - 1) of byte_type; 
	constant UART_TX_MESSAGE_FIFO_WIDTH : integer := UART_TX_BUFFER_SIZE * 8;  --width of the tx message fifo in bits
	subtype tx_message_buffer_slv_type is std_logic_vector(UART_TX_MESSAGE_FIFO_WIDTH - 1 downto 0);
	
	
end package uart_pkg;

package body uart_pkg is

	
		
end package body uart_pkg;
