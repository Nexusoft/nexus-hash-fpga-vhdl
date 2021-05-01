-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- encodes and decodes a string of bytes for sending and receiving binary data over a uart.
-- the encoder adds a start and end byte and adds escape characters as needed to the message.
-- the decoder removes the flags and parses the message
-- inspired by https://eli.thegreenplace.net/2009/08/12/framing-in-serial-communications

-- Message format:
-- <START><ADDRESS><MESSAGE_BYTE_0><MESSAGE_BYTE_1>...<MESSAGE_BYTE_N><END>
-- the behavior (read, write, or do something) is determined by the address.


-- Requests to send from the fpga are buffered in a fifo.  
-- Received data from the fifo are buffered in a fifo.
-- Register writes are done in this module.  


-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.uart_pkg.all;
use work.skein_pkg.all;



-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity uart_framer is 
	generic (
        CLK_FREQ    : integer := 50e6;   -- set system clock frequency in Hz
        BAUD_RATE   : integer := 115200; -- baud rate value
        PARITY_BIT  : string  := "none"  -- legal values: "none", "even", "odd", "mark", "space"
    );
	port
	(
		clk		: in std_logic;  
		reset	: in std_logic;
		-- uart interface
		uart_txd : out std_logic;
		uart_rxd : in std_logic;
		--user inputs to the framer
		nonce : in unsigned(63 downto 0);
		nonce_found : in std_logic;
		found_counter : in unsigned(31 downto 0);
		-- debug inputs
		activity_counter : in unsigned(31 downto 0);
		--user outputs from the framer
		skein_key2 : out key_type;
		skein_message2 : out state_type;
		bits_target : out unsigned(31 downto 0);
		hash_enable : out std_logic
		
	);
end uart_framer;



architecture beh of uart_framer is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	
	-- received message destuffer
	signal received_message_addr : byte_type;
	signal received_message_buffer : rx_message_buffer_type;
	signal destuffer_valid_in, received_message_valid : std_logic;
	
	
	-- tx message fifo
	signal tx_message_fifo_din, tx_message_fifo_dout : tx_message_buffer_slv_type;
	signal tx_message_fifo_wr_en, tx_message_fifo_rd_en, tx_message_fifo_empty : std_logic;
	
	-- tx message stuffer
	signal stuffer_data_in : tx_message_buffer_type; -- data to be stuffed
	signal stuffer_data_out : byte_type;
	signal stuffer_valid_in, stuffer_valid_out, stuffer_ready : std_logic := '0';
	
	-- uart and byte wide fifos
	signal uart_rx_data, uart_rx_fifo_dout, uart_tx_data, uart_tx_fifo_dout    : byte_type;
    signal valid, busy, frame_err, uart_rx_fifo_empty, uart_tx_fifo_empty, send, uart_tx_rd_en, uart_rx_rd_en, uart_tx_wr_en   : std_logic;	
	
	-- test values
	constant expected_nonce : unsigned (63 downto 0) := x"00000004ECF83A53";
	constant initial_nonce : unsigned (63 downto 0) := expected_nonce - 000000000; 
	constant KEY2_TEST : key_type := (x"88AC877DCC3D9F5B", x"0CE6F8BE43BA5FA4", x"65AF0210816973D7", x"ED0029D7DD5FA14C", x"F0D019FEEBA16EC7", x"4D0E1BD4235DAE9F", x"D510716EA14E5A89", x"59ADF0434406532B", x"F58F5153DCFE34A4", x"494553ABAEAC4A2B", x"05574FFA0116287B", x"B5F44CE1F0D31096", x"56B3513630FDD8DB", x"A06515574CDD6BD3", x"A7447037B78D79EC", x"8EDB8035835CFB1F", x"BBF337D66036F952");
	constant MESSAGE2_TEST : state_type := (x"6D3A302500000902", x"14B8EC919EA8A234", x"7C414429A6160DF5", x"2294C73850B42243", x"EBF6BE905FD49A41", x"88F75004AA5B07BE", x"43C26A3140193ED0", x"FC4207CD30FD1C4F", x"0000000231F5A458", x"7B032ED8001EDF6C", initial_nonce, x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000");
	

	
	-----------------------------------------------------------------------------------------------
	-- registers
	-----------------------------------------------------------------------------------------------
	
	-- skein input registers
	signal skein_key2_reg		: key_type;
	signal skein_message2_reg	: state_type;
	
	-- difficulty register
	signal bits_reg : unsigned (31 downto 0) := x"7D00FFFF";  -- 32 bits default difficulty
	
	-- results register
	signal nonce_reg	: unsigned(63 downto 0) := (others => '0'); 
	signal found_reg	: std_logic := '0';
	signal nonce_found_i  : std_logic := '0';
	
	-- enable register
	signal hash_enable_reg	: std_logic := '0';
	 
	--found counter
	signal found_counter_i : unsigned(31 downto 0) := (others => '0'); 
	
	-- generic fifo
	component fifo is
	generic
	(
		FIFO_WIDTH : natural := 1024;
		FIFO_DEPTH : natural := 128
    );
	port 
	(
		clk		: in std_logic;
		srst	: in std_logic;
		din		: in std_logic_vector(FIFO_WIDTH-1 downto 0); -- data in
		wr_en	: in std_logic; -- write enable
		rd_en	: in  std_logic;
		dout 	: out std_logic_vector(FIFO_WIDTH-1 downto 0);
		full	: out std_logic;
		empty   : out std_logic
	);
	end component fifo;

    -- COMPONENT uart_fifo
    -- PORT (
        -- clk : IN STD_LOGIC;
        -- srst : IN STD_LOGIC;
        -- din : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
        -- wr_en : IN STD_LOGIC;
        -- rd_en : IN STD_LOGIC;
        -- dout : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
        -- full : OUT STD_LOGIC;
        -- empty : OUT STD_LOGIC;
        -- wr_rst_busy : OUT STD_LOGIC;
        -- rd_rst_busy : OUT STD_LOGIC
    -- );
    -- END COMPONENT;
	
	-- COMPONENT tx_message_fifo
	-- PORT (
		-- clk : IN STD_LOGIC;
		-- din : IN STD_LOGIC_VECTOR(255 DOWNTO 0);
		-- wr_en : IN STD_LOGIC;
		-- rd_en : IN STD_LOGIC;
		-- dout : OUT STD_LOGIC_VECTOR(255 DOWNTO 0);
		-- full : OUT STD_LOGIC;
		-- empty : OUT STD_LOGIC
	-- );
	-- END COMPONENT;

	
	
	
begin
	
	-- Monitor for a received message and take appropriate action.
	-- Also if we found a nonce, send it.
	-- All messages to be sent over the uart happen in this process. 
	
	process(clk)
		begin
			if rising_edge(clk) then
				if received_message_valid = '1' then -- a message was received.  There is no queue here so we must handle it right away.
					-- take action based on the address
					case received_message_addr is
						when PING_REG_ADDR => -- we got a ping
							-- respond with ACK
							tx_message_fifo_din <= pack_ack;
							tx_message_fifo_wr_en <= '1';
						when SIGNATURE_REG_ADDR => 
							-- respond with signature
							tx_message_fifo_din <= pack_signature;
							tx_message_fifo_wr_en <= '1';
						when ENABLE_HASHING_ADDR =>
							-- enable or disable hashing
							if received_message_buffer(0) = ENABLE_HASHING then
								hash_enable_reg <= '1';
							elsif received_message_buffer(0) = DISABLE_HASHING then
								hash_enable_reg <= '0';
							end if;
							-- respond with ACK
							tx_message_fifo_din <= pack_ack;
							tx_message_fifo_wr_en <= '1';
						when SKEIN_KEY2_ADDR =>
							-- convert to key_type and save it
							skein_key2_reg <= unpack_key2(received_message_buffer);
							-- respond with ACK
							tx_message_fifo_din <= pack_ack;
							tx_message_fifo_wr_en <= '1';
						when SKEIN_MESSAGE2_ADDR =>
							-- save message2 from user
							skein_message2_reg <= unpack_message2(received_message_buffer);
							-- respond with ACK
							tx_message_fifo_din <= pack_ack;
							tx_message_fifo_wr_en <= '1';
						when BITS_ADDR =>
							-- save the bits target difficulty from user
							bits_reg <= unpack_bitsTarget(received_message_buffer);
							-- respond with ACK
							tx_message_fifo_din <= pack_ack;
							tx_message_fifo_wr_en <= '1';
						when NONCE_ADDR =>
							-- read the current nonce counter value
							tx_message_fifo_din <= pack_nonce(nonce_reg);
							tx_message_fifo_wr_en <= '1';
						when FOUND_ADDR =>
							--send the found counter
							tx_message_fifo_din <= pack_found_counter(found_counter);
							tx_message_fifo_wr_en <= '1';
						when DIFF_28_COUNT_ADDR =>
							-- send the activity counter
							tx_message_fifo_din <= pack_activity_counter(activity_counter);
							tx_message_fifo_wr_en <= '1';
						when HASH_TEST =>
							-- load test data and start hashing.  
							skein_message2_reg <= MESSAGE2_TEST;
							skein_key2_reg <= KEY2_TEST;
							hash_enable_reg <= '1';
						when others =>
					end case;
				elsif found_reg = '1' then -- a nonce was found.  Yay.  Send it to the host.
					tx_message_fifo_din <= pack_nonce(nonce_reg);
					tx_message_fifo_wr_en <= '1';
					-- our work here is done.  reset the found flag
					found_reg <= '0';
				else
					-- nothing new to send
					tx_message_fifo_wr_en <= '0';
				end if;
				
				-- monitor for increase in the found counter which indicates a nonce was found
				--nonce_found_i <= nonce_found;
				found_counter_i <= found_counter;
				-- look for a change in count 
				if found_counter_i < found_counter then
					found_reg <= '1';
					-- store the nonce value to the internal register
					nonce_reg <= nonce;
				end if;
			end if;		
	end process;
	
	-- expose the registers
	skein_key2 <= skein_key2_reg;
	skein_message2 <= skein_message2_reg;
	hash_enable <= hash_enable_reg;
	bits_target <= bits_reg;

	-- receive side message flow from high level down

	-- parse the received byte stream, remove escape characters, break the message into its parts
	uart_destuffer_i : entity work.uart_destuffer
	port map (
		clk	=> clk,
		data_in => uart_rx_fifo_dout,
		valid_in => destuffer_valid_in,
		address_out => received_message_addr,
		data_out => received_message_buffer,
		valid_out => received_message_valid  --asserts when we have received a complete message.
	);
	
	destuffer_valid_in <= not uart_rx_fifo_empty;
	uart_rx_rd_en <= destuffer_valid_in;
	
	-- Buffer the received byte stream from the uart
	uart_rx_fifo : fifo
	generic map
	(
	FIFO_WIDTH => 8,
	FIFO_DEPTH => 512
	)
	PORT MAP (
		clk => clk,
		srst => reset,
		din => uart_rx_data,
		wr_en => valid,
		rd_en => uart_rx_rd_en,
		dout => uart_rx_fifo_dout,
		full => open,
		empty => uart_rx_fifo_empty
	);
	
	-- Transmit side message flow from high level to low.  There are two fifos.  The first is a high level message queue.  The second is a byte queue that feeds the uart.
	
	
	-- this fifo is the top level queue for all messages ready to be stuffed and sent over the uart
	tx_message_fifo_i : fifo
	generic map
	(
	FIFO_WIDTH => 256,
	FIFO_DEPTH => 64
	)
	PORT MAP (
		clk => clk,
		srst => reset,
		din => tx_message_fifo_din,
		wr_en => tx_message_fifo_wr_en,
		rd_en => tx_message_fifo_rd_en,
		dout => tx_message_fifo_dout,
		full => open,
		empty => tx_message_fifo_empty
	);
	
	stuffer_valid_in <= not tx_message_fifo_empty;
	-- TODO function to convert std_logic_vector to tx buffer?
	stuffer_data_in <= tx_message_buffer_slv_to_array(tx_message_fifo_dout);
	tx_message_fifo_rd_en <= stuffer_ready and stuffer_valid_in;
	
	-- convert an outgoing message to an escaped byte stream and put it in the byte stream queue
	uart_stuffer_i : entity work.uart_stuffer
	port map (
		clk	=> clk,
		ready_out => stuffer_ready,
		data_to_send => stuffer_data_in,
		valid_in => stuffer_valid_in,
		byte_stream_out => stuffer_data_out,
		valid_out => stuffer_valid_out
		
	);
	
	uart_tx_data <= stuffer_data_out;
	uart_tx_wr_en <= stuffer_valid_out;
	
	-- uart byte stream transmit buffer
	uart_tx_fifo : fifo
	generic map
	(
	FIFO_WIDTH => 8,
	FIFO_DEPTH => 512
	)
	PORT MAP (
		clk => clk,
		srst => reset,
		din => uart_tx_data,
		wr_en => uart_tx_wr_en,
		rd_en => uart_tx_rd_en,
		dout => uart_tx_fifo_dout,
		full => open,
		empty => uart_tx_fifo_empty
	);
	
	-- send the next byte when data is available in the queue and the uart is not busy
	send <= not uart_tx_fifo_empty and not busy;
	uart_tx_rd_en <= send;
	
	-- the uart
	uart_i: entity work.UART
    generic map (
        CLK_FREQ    => CLK_FREQ,
        BAUD_RATE   => BAUD_RATE,
        PARITY_BIT  => PARITY_BIT
    )
    port map (
        CLK         => clk,
        RST         => reset,
        -- UART INTERFACE
        UART_TXD    => uart_txd,
        UART_RXD    => uart_rxd,
        -- USER DATA OUTPUT INTERFACE
        DATA_OUT    => uart_rx_data,
        DATA_VLD    => valid,
        FRAME_ERROR => frame_err,
        -- USER DATA INPUT INTERFACE
        DATA_IN     => uart_tx_fifo_dout,
        DATA_SEND   => send,
        BUSY        => busy
    );
	
	--loopback test
	-- uart_tx_data <= uart_rx_fifo_dout;
	-- send <= not uart_tx_fifo_empty and not busy;
	-- uart_rx_rd_en <= not uart_rx_fifo_empty;
	-- uart_tx_wr_en <= uart_rx_rd_en;
	-- uart_tx_rd_en <= send;
	
	
	
	

	
	
end beh;
		
