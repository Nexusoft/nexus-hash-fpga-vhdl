-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Simulation test bench for nxs hash fpga with uart interface.

Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.env.finish;

library std;
use std.textio.all;

library work;
use work.uart_pkg.all;
use work.skein_pkg.all;

entity nexus_uart_tb is
end nexus_uart_tb;

architecture beh of nexus_uart_tb is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal clk50, clk250 : std_logic;
	signal reset, hash_reset: std_logic;
	signal locked, uart_rxd, uart_txd : std_logic;
	signal tb_uart_txd, tb_uart_rxd, tb_rx_valid, tb_busy, tb_send : std_logic;
	signal tb_uart_rx_data, tb_uart_tx_data : byte_type;
	
	--Caution!! these may be adjusted to wild values to make simulation run faster.  
	constant clk50_period  : time := 20 ns;  -- nominal 20ns.  ok to leave this alone
	constant clk50_freq : integer := 20e5; -- nominally 50e6.  Set to 20e5 to speed up simulation
	constant clk250_period  : time := 400 ns; -- nominal value is 4ns set to 400ns to speed up uart simulation.
	
	
	signal found : std_logic := '0';
	signal nonce : unsigned (63 downto 0) := (others => '0'); 
	signal key2 : key_type := (others => (others => '0'));
	signal message2 : state_type := (others => (others => '0'));
	signal hash_enable : std_logic := '0';
	signal activity_counter, found_counter : unsigned(31 downto 0);
	signal bitsTarget : unsigned (31 downto 0);  

	
	constant MAX_LINE_LENGTH : integer := 2048;
	signal lineLength : integer range 0 to MAX_LINE_LENGTH :=0;
    signal i : integer range 0 to MAX_LINE_LENGTH := 0;
	
	type state_type is (PROCESSING,STOP);
    signal state : state_type;
	
	signal rxString, txString : string(1 to 128);
	signal ii : integer range 1 to 64 := 1;
	signal reported : std_logic := '0';
	
	

begin

	-- test uart - simulates the host
	uart_i: entity work.UART
    generic map (
        CLK_FREQ    => clk50_freq,
        BAUD_RATE   => 115200,
        PARITY_BIT  => "none"
    )
    port map (
        CLK         => clk50,
        RST         => reset,
        -- UART INTERFACE
        UART_TXD    => tb_uart_txd, -- serial data sent to the UUT
        UART_RXD    => tb_uart_rxd, -- serial data received from the UUT
        -- USER DATA OUTPUT INTERFACE
        DATA_OUT    => tb_uart_rx_data, -- byte data received from the UUT
        DATA_VLD    => tb_rx_valid,
        FRAME_ERROR => open,
        -- USER DATA INPUT INTERFACE
        DATA_IN     => tb_uart_tx_data, -- byte data sent to the UUT.  Inject test data here.
        DATA_SEND   => tb_send,
        BUSY        => tb_busy
    );

	-- connect the simulated host uart to the system uart
	uart_rxd <= tb_uart_txd;
	tb_uart_rxd <= uart_txd;
	
	-- communication interface and registers
	uart_framer_i: entity work.uart_framer
	 generic map (
        CLK_FREQ    => clk50_freq,
        BAUD_RATE   => 115200,
        PARITY_BIT  => "none"
    )
    port map (
        clk         => clk50,
        reset         => reset,
        -- UART INTERFACE
        uart_txd    => uart_txd,  -- tx to the host
        uart_rxd    => uart_rxd,  -- rx from the host
        --user inputs to the framer
        nonce => nonce,
        nonce_found => found,
		found_counter => found_counter,
		-- debug inputs
		activity_counter => activity_counter,
        --user outputs from the framer
        skein_key2 => key2,
        skein_message2 => message2,
		bits_target => bitsTarget,
        hash_enable => hash_enable
    );
	
	-- main hash block
	nxs_hash: entity work.nxs_hash
	port map
	(
		clk => clk250,
		reset => hash_reset,
		key2 => key2,
		message2 => message2,
		bits_target => bitsTarget,
		nonce => nonce,
		found => found,
		found_counter => found_counter,
		activity_counter => activity_counter
	);
	hash_reset <= reset or not hash_enable;
	
	
	clk50_process : process
	begin
		clk50 <= '0';
		wait for clk50_period/2;
		clk50 <= '1';
		wait for clk50_period/2;
	end process;
	
	clk250_process : process
	begin
		clk250 <= '0';
		wait for clk250_period/2;
		clk250 <= '1';
		wait for clk250_period/2;
	end process;
	
	reset_process : process
	begin
		reset <= '1';
		wait for clk50_period;
		reset <= '0';
		wait;
	end process;

	test_uart_send : process(reset, clk50)
        file filein : text open read_mode is "sim_uart_in.txt";
        --file fileout : text open write_mode is "sim_uart_out.txt";
        variable line_in,line_out : line;
        variable ch : character;
        

	    begin
		if reset = '1' then
			lineLength <= 0;
			i <= 1;
		elsif rising_edge(clk50) then
			case state is
                when PROCESSING =>
					if lineLength = 0 or i > lineLength then -- did we just start or reach the end of the previous line?
						tb_send <= '0'; -- don't send anything
						if endfile(filein) then  -- check for end of file
							FILE_CLOSE(filein);
							--FILE_CLOSE(fileout);
							report "Done sending data.  Look for the nonce to come back.";
							state <= STOP;
						else
							readline(filein,line_in); -- read the next line
							lineLength <= line_in'length; -- number of characters in the line
							i <= 1; -- set character index to the first character in the line
						end if;
					elsif tb_busy = '0' and tb_send <= '0' then	-- ready to send.  check tb_send = '0' to ensure we don't try to send back to back bytes.  
						tb_uart_tx_data <= CHAR2STD(line_in(i)); -- send the ascii charcter via the test uart
						tb_send <= '1';
						i <= i+1;
						report "Sent: " & line_in(i);
					else
						tb_send <= '0'; -- don't send anything	
					end if;
				when STOP =>
					
					--finish;
					null;
			end case;
		
		end if;
	end process;
	
	test_uart_receive : process(reset, clk50)
	
	begin
		if reset = '1' then
			ii <= 1;
		elsif rising_edge(clk50) then
			if tb_rx_valid = '1' then
				rxString(ii) <= BYTE2CHAR(to_integer(unsigned(tb_uart_rx_data)));
				ii <= ii +1;
				reported <= '0';
			end if;
			if ii>1 and rxString(ii-1) = '}' and reported = '0' then -- last character of the return message
				report "Received: " & rxString;
				reported <= '1';
				rxString <= (others => ' ');
				--assert false report "Simulation completed" severity failure;
			end if;
		end if;
	end process;

end beh;
