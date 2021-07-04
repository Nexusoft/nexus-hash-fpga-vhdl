-------------------------------------------------------------------------------
-- Andrew Hatstat
-------------------------------------------------------------------------------
-- implements the Nexus FPGA hash miner UART interface 

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;

library std;
use std.textio.all;

library work;
use work.uart_pkg.all;
use work.skein_pkg.all;


-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity uart_nexus_interface is 
	generic (
        CLK_FREQ    : integer := 50e6;   -- set system clock frequency in Hz
		BAUD_RATE   : integer := DEFAULT_BAUD_RATE  -- baud rate value
    );
	port
	(
		clk		: in std_logic;  
		reset	: in std_logic;
		-- uart interface
		uart_txd : out std_logic;  -- tx to the host
		uart_rxd : in std_logic;  -- rx from the host
		-- inputs from the hash block
		nonce : in unsigned(63 downto 0);
		nonce_found : in std_logic;
		-- outputs from the uart
		skein_key2 : out key_type;
		skein_message2 : out state_type;
		new_work: out std_logic
		
	);
end uart_nexus_interface;


architecture beh of uart_nexus_interface is

	type rx_register is (KEY2, MESSAGE2, DONE);  -- sections of the work package
    type tx_state_type is (SENDING, IDLE);  -- sections of the work package
	constant TIMEOUT : integer := 10;	--ms
	constant TIMEOUT_COUNT_LIMIT : integer := CLK_FREQ * TIMEOUT / 1000;

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	
	-- received messages
	signal register_byte_index : integer range 0 to 7 := 0;
	signal register_word_index : integer range 0 to KEY2_WORD_COUNT := 0;
	signal timeout_counter : integer range 0 to TIMEOUT_COUNT_LIMIT := 0;
	signal rx_data_available : std_logic := '0';
	signal current_rx_register : rx_register := KEY2;
	
	-- nonces to transmit
	signal sent_byte_count : integer range 0 to 8 := 0;
	signal nonce_slv : std_logic_vector(63 downto 0) := (others => '0');  --nonce to send as a std_logic_vector
	signal tx_state : tx_state_type := IDLE;
	
	-- tx message fifo (nonce fifo)
	signal tx_message_fifo_din, tx_message_fifo_dout : tx_message_buffer_slv_type := (others => '0');
	signal tx_message_fifo_wr_en, tx_message_fifo_rd_en, tx_message_fifo_empty, tx_message_fifo_full : std_logic := '0';
	
	-- uart and byte wide fifos
	signal uart_rx_data, uart_rx_fifo_dout, uart_tx_data, uart_tx_fifo_dout : byte_type := (others => '0');
    signal valid, busy, frame_err, uart_tx_fifo_empty, uart_tx_fifo_full, send, uart_tx_rd_en, uart_tx_wr_en  : std_logic := '0';
	
	-- test values
	constant expected_nonce : unsigned (63 downto 0) := x"00000004ECF83A53";
	constant initial_nonce : unsigned (63 downto 0) := expected_nonce - 000000000; 
	constant KEY2_TEST : key_type := (x"88AC877DCC3D9F5B", x"0CE6F8BE43BA5FA4", x"65AF0210816973D7", x"ED0029D7DD5FA14C", x"F0D019FEEBA16EC7", x"4D0E1BD4235DAE9F", x"D510716EA14E5A89", x"59ADF0434406532B", x"F58F5153DCFE34A4", x"494553ABAEAC4A2B", x"05574FFA0116287B", x"B5F44CE1F0D31096", x"56B3513630FDD8DB", x"A06515574CDD6BD3", x"A7447037B78D79EC", x"8EDB8035835CFB1F", x"BBF337D66036F952");
	constant MESSAGE2_TEST : state_type := (x"6D3A302500000902", x"14B8EC919EA8A234", x"7C414429A6160DF5", x"2294C73850B42243", x"EBF6BE905FD49A41", x"88F75004AA5B07BE", x"43C26A3140193ED0", x"FC4207CD30FD1C4F", x"0000000231F5A458", x"7B032ED8001EDF6C", initial_nonce, x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000");
	
	-----------------------------------------------------------------------------------------------
	-- registers
	-----------------------------------------------------------------------------------------------
	
	-- skein input registers
	signal skein_key2_reg		: key_type := (others => (others => '0'));
	signal skein_message2_reg	: state_type := (others => (others => '0'));
	signal new_work_i	: std_logic := '0';  -- goes high when new work package was received from the uart and key2 and message2 are valid. 
	
	
    
begin
	
	-- Monitor for a received message and move it into the key2 and message2 registers.  
	-- Resets after a timeout period to recover from spurious or incomplete data received by the uart. 
	process(clk)
		variable got_new_work : std_logic := '0';
		variable starting_nonce : std_logic_vector(63 downto 0) := (others => '0');
		variable temp_word : std_logic_vector (63 downto 0) := (others => '0');
		begin
			if rising_edge(clk) then
				-- rx state machine
				new_work_i <= '0';
				got_new_work := '0';
				if rx_data_available = '1' then -- a byte was received from the host.
					-- report "RX Fifo data out: " & to_hstring(uart_rx_fifo_dout);
					-- reset the timeout counter
					timeout_counter <= 0;
					case current_rx_register is
						when KEY2 =>
							if register_byte_index = 7 then
								register_byte_index <= 0;
								register_word_index <= register_word_index + 1;
							else
								register_byte_index <= register_byte_index + 1;
							end if;
							temp_word := std_logic_vector(skein_key2_reg(register_word_index));
							temp_word(7 + 8*register_byte_index downto 8*register_byte_index) := uart_rx_fifo_dout;  -- receive least significant bytes first
							skein_key2_reg(register_word_index) <= unsigned(temp_word);
							if register_word_index = KEY2_WORD_COUNT - 1 and register_byte_index = 7 then
								current_rx_register <= MESSAGE2;
								register_byte_index <= 0;
								register_word_index <= 0;
								report "Read Key2";
							end if;
						when MESSAGE2 =>
							if register_byte_index = 7 then
								register_byte_index <= 0;
								register_word_index <= register_word_index + 1;
							else
								register_byte_index <= register_byte_index + 1;
							end if;
							temp_word := std_logic_vector(skein_message2_reg(register_word_index));
							temp_word(7 + 8*register_byte_index downto 8*register_byte_index) := uart_rx_fifo_dout;
							skein_message2_reg(register_word_index) <= unsigned(temp_word);
							if register_word_index = MESSAGE2_WORD_COUNT - 1 and register_byte_index = 7 then
								current_rx_register <= DONE;
								register_byte_index <= 0;
								register_word_index <= 0;
								new_work_i <= '1';
								got_new_work := '1';
								starting_nonce := temp_word;
								report "Read Message2";
							end if;
						when DONE =>
							-- ignore the byte
							report "Received Byte Dropped: "; -- & to_hstring(uart_rx_fifo_dout); 
						when others =>
					end case;
				else
					if timeout_counter < TIMEOUT_COUNT_LIMIT then
						timeout_counter <= timeout_counter + 1;
					else
						-- timeout occurred, reset the rx state machine
						timeout_counter <= 0;
						register_byte_index <= 0;
						register_word_index <= 0;
						new_work_i <= '0';
						current_rx_register <= KEY2;
						report "Rx Timeout.";
					end if;
				end if;
				-- stuff nonces into the nonce tx buffer
				if nonce_found = '1' then
					if tx_message_fifo_full = '0' then
						tx_message_fifo_wr_en <= '1';
						tx_message_fifo_din <= std_logic_vector(nonce);
					end if;
				elsif got_new_work = '1' then
				-- send a fake nonce out to acknowlege we got the new work
					if tx_message_fifo_full = '0' then
						tx_message_fifo_wr_en <= '1';
						tx_message_fifo_din <= std_logic_vector(unsigned(starting_nonce) - 1);
						-- report "Starting nonce - 1 = " & to_hstring(unsigned(starting_nonce) - 1);
					end if;
				else
					tx_message_fifo_wr_en <= '0';
				end if;
			end if;		
	end process;
	
	-- expose the registers
	skein_key2 <= skein_key2_reg;
	skein_message2 <= skein_message2_reg;
	new_work <= new_work_i;

	-- read directly from the UART.  No need to buffer. 
	rx_data_available <= valid;
	uart_rx_fifo_dout <= uart_rx_data;
	
	-- Transmit side message flow from high level to low.  There are two fifos.  The first holds nonces ready to send.  The second is a byte queue that feeds the uart.
	
	-- nonce transmit buffer
	nonce_fifo_i : entity work.fifo
	generic map
	(
	FIFO_WIDTH => 64,
	FIFO_DEPTH => 16
	)
	PORT MAP (
		clk => clk,
		srst => reset,
		din => tx_message_fifo_din,
		wr_en => tx_message_fifo_wr_en,
		rd_en => tx_message_fifo_rd_en,
		dout => tx_message_fifo_dout,
		full => tx_message_fifo_full,
		empty => tx_message_fifo_empty
	);
	
	-- move bytes from nonce buffer to uart tx buffer
	process(clk)
		begin
			if rising_edge(clk) then
				uart_tx_wr_en <= '0';
				tx_message_fifo_rd_en <= '0';
				case tx_state is
					when IDLE => 
						if tx_message_fifo_empty = '0' then
							nonce_slv <= tx_message_fifo_dout;
							tx_message_fifo_rd_en <= '1';
							tx_state <= SENDING;
							sent_byte_count <= 0;
						end if;
					when SENDING =>
						if sent_byte_count >= UART_TX_BUFFER_SIZE then
							-- we are done with this message
							tx_state <= IDLE;
						else
							if uart_tx_fifo_full = '0' then
								uart_tx_data <= nonce_slv(7 + 8*sent_byte_count downto 8*sent_byte_count);  -- send least significant bytes first
								uart_tx_wr_en <= '1';
								sent_byte_count <= sent_byte_count + 1;
							end if;
						end if;
					when others =>
				end case;
			end if;
	end process;
				
	
	-- uart byte stream transmit buffer
	uart_tx_fifo : entity work.fifo
	generic map
	(
	FIFO_WIDTH => 8,
	FIFO_DEPTH => 16
	)
	PORT MAP (
		clk => clk,
		srst => reset,
		din => uart_tx_data,
		wr_en => uart_tx_wr_en,
		rd_en => uart_tx_rd_en,
		dout => uart_tx_fifo_dout,
		full => uart_tx_fifo_full,
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
	
	
end beh;
		
