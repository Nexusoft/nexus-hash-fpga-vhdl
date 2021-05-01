--------------------------------------------------------------------------------
-- PROJECT: SIMPLE UART FOR FPGA
--------------------------------------------------------------------------------
-- MODULE:  TESTBANCH OF UART LOOPBACK EXAMPLE TOP MODULE
-- AUTHORS: Jakub Cabal <jakubcabal@gmail.com>
-- lICENSE: The MIT License (MIT)
-- WEBSITE: https://github.com/jakubcabal/uart_for_fpga
--------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

library work;
use work.uart_pkg.all;
use work.skein_pkg.all;

entity UART_LOOPBACK_TB is
end UART_LOOPBACK_TB;

architecture FULL of UART_LOOPBACK_TB is

	signal CLK           : std_logic := '0';
	signal RST_N         : std_logic := '0';
	signal reset		: std_logic := '1';
	signal tx_uart       : std_logic;
	signal rx_uart       : std_logic := '1';
    signal busy          : std_logic;
    signal frame_error   : std_logic;
	signal nonce, expected_nonce, initial_nonce : unsigned (63 downto 0);
	signal key2 : key_type;
	signal message2 : state_type;
	signal found, hash_enable : std_logic := '0';
	signal activity_counter : unsigned(31 downto 0);

  	constant clk_period  : time := 20 ns;
	constant uart_period : time := 8680.56 ns;
	constant byte1  : std_logic_vector(7 downto 0) := START_FLAG;
	constant byte2 : std_logic_vector(7 downto 0) := SKEIN_KEY2_ADDR; -- address
	constant byte3 : std_logic_vector(7 downto 0) := x"31"; 
	constant byte4 : std_logic_vector(7 downto 0) := x"32";
	constant byte5 : std_logic_vector(7 downto 0) := x"33";
	constant byte6 : std_logic_vector(7 downto 0) := END_FLAG;
	constant byte7 : std_logic_vector(7 downto 0) := START_FLAG; 
    constant byte8 : std_logic_vector(7 downto 0) := ENABLE_HASHING_ADDR; -- address
    constant byte9 : std_logic_vector(7 downto 0) := ENABLE_HASHING;
    constant byte10 : std_logic_vector(7 downto 0) := END_FLAG;
	
	

begin

	-- utt: entity work.UART_LOOPBACK
    -- generic map (
        -- CLK_FREQ    => 50e6,
        -- BAUD_RATE   => 115200,
        -- PARITY_BIT  => "none"
    -- )
    -- port map (
        -- CLK         => CLK,
        -- RST_N       => RST_N,
        -- -- UART INTERFACE
        -- UART_TXD    => tx_uart,
        -- UART_RXD    => rx_uart,
        -- -- DEBUG INTERFACE
        -- BUSY        => busy,
        -- FRAME_ERR   => frame_error
    -- );

	
	uart_framer_i: entity work.uart_framer
    port map (
        clk         => CLK,
        reset         => reset,
        -- UART INTERFACE
        uart_txd    => tx_uart,
        uart_rxd    => rx_uart,
        --inputs to the framer
        nonce    => nonce,
        nonce_found    => found,
        -- debug inputs
        activity_counter => activity_counter,
        --user outputs from the framer
		skein_key2   => key2,
		skein_message2 => message2,
		hash_enable  => hash_enable
    );
	
	
	reset <= not RST_N;
	
	clk_process : process
	begin
		CLK <= '0';
		wait for clk_period/2;
		CLK <= '1';
		wait for clk_period/2;
	end process;

	test_rx_uart : process
	begin
		rx_uart <= '1';
		RST_N <= '0';
		wait for 100 ns;
    	RST_N <= '1';

		wait for uart_period;

		rx_uart <= '0'; -- start bit
		wait for uart_period;

		for i in 0 to (byte1'LENGTH-1) loop
		    rx_uart <= byte1(i); -- data bits
		    wait for uart_period;
		end loop;

		rx_uart <= '1'; -- stop bit
		wait for uart_period;

		rx_uart <= '0'; -- start bit
		wait for uart_period;

		for i in 0 to (byte2'LENGTH-1) loop
  			rx_uart <= byte2(i); -- data bits
        	wait for uart_period;
		end loop;

		rx_uart <= '1'; -- stop bit
		wait for uart_period;

		rx_uart <= '0'; -- start bit
		wait for uart_period;

		for i in 0 to (byte3'LENGTH-1) loop
			rx_uart <= byte3(i); -- data bits
			wait for uart_period;
		end loop;

		rx_uart <= '1'; -- stop bit
		wait for uart_period;

		rx_uart <= '0'; -- start bit
		wait for uart_period;

		for i in 0 to (byte4'LENGTH-1) loop
			rx_uart <= byte4(i); -- data bits
			wait for uart_period;
		end loop;

		rx_uart <= '1'; -- stop bit
		wait for uart_period;
		
		rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte5'LENGTH-1) loop
            rx_uart <= byte5(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;
        
        rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte6'LENGTH-1) loop
            rx_uart <= byte6(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;

        rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte7'LENGTH-1) loop
            rx_uart <= byte7(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;
        
        rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte8'LENGTH-1) loop
            rx_uart <= byte8(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;
 
        rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte9'LENGTH-1) loop
            rx_uart <= byte9(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;

        rx_uart <= '0'; -- start bit
        wait for uart_period;

        for i in 0 to (byte10'LENGTH-1) loop
            rx_uart <= byte10(i); -- data bits
            wait for uart_period;
        end loop;

        rx_uart <= '1'; -- stop bit
        wait for uart_period;

		wait;

	end process;

end FULL;
