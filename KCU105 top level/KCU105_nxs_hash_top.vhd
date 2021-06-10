-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- top level FPGA for nxs hashing on kcu105 demo board

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
entity KCU105_nxs_hash_top is 
	port
	(
		CLK_125MHZ_P		: in std_logic;  -- input clock
		CLK_125MHZ_N		: in std_logic;
		CPU_RESET		    : in  std_logic; -- active high asynchronous reset
        -- UART INTERFACE
        USB_UART_TX   : in std_logic;  -- labeled from the perspective of the USB UART transceiver chip
        USB_UART_RX   : out  std_logic;
        -- DEBUG INTERFACE
        GPIO_LED_0_LS       : out std_logic;
		GPIO_LED_1_LS       : out std_logic;
		GPIO_LED_2_LS       : out std_logic;
		GPIO_LED_3_LS       : out std_logic;
		GPIO_LED_4_LS       : out std_logic;
        GPIO_LED_5_LS  		: out std_logic
	);
end KCU105_nxs_hash_top;

architecture beh of KCU105_nxs_hash_top is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal clk_sys, clk_hash : std_logic;
	signal reset_i, reset_ii, reset, reset_async, hash_reset : std_logic;
	signal locked, uart_rxd, uart_txd : std_logic;
	signal blinkCounter : unsigned(31 downto 0);
	
	signal found, new_work, found_count_increased : std_logic := '0';
	signal nonce : unsigned (63 downto 0) := (others => '0'); 
	signal key2 : key_type := (others => (others => '0'));
	signal message2 : state_type := (others => (others => '0'));  
	signal hash_enable : std_logic := '0';
	signal found_counter, found_counter_i : unsigned(31 downto 0) := (others => '0');
	
	

	--xilinx pll
	component clk_wiz_0
	port
	 (-- Clock in ports
	  -- Clock out ports
	  clk_out_sys          : out    std_logic;
	  clk_out_hash         : out    std_logic;
	  -- Status and control signals
	  reset             : in     std_logic;
	  locked            : out    std_logic;
	  clk_in1_p         : in     std_logic;
	  clk_in1_n         : in     std_logic
	 );
	end component;


begin
		
		
	reset_async <= CPU_RESET;
	GPIO_LED_0_LS <= blinkCounter(24);
	GPIO_LED_1_LS <= uart_rxd;
	GPIO_LED_2_LS <= uart_txd;
	GPIO_LED_3_LS <= hash_enable;
	GPIO_LED_4_LS <= found_counter(0); 
	GPIO_LED_5_LS <= found; 
	USB_UART_RX <= uart_txd;  --swap rx and tx
	uart_rxd <= USB_UART_TX;

	--xilinx
	 genpll: clk_wiz_0
	 port map ( 
		 clk_out_sys => clk_sys,
		 clk_out_hash => clk_hash,
		 reset => reset_async,
		 locked => locked,
		 clk_in1_p => CLK_125MHZ_P,
		 clk_in1_n => CLK_125MHZ_N
	 );
	
	
	-- reset synchronize
	process(clk_hash)
	begin
		if rising_edge(clk_hash) then
			reset_i <= reset_async;
			reset_ii <= reset_i;
			reset <= reset_ii;
		end if;
	end process;
	
	-- reset the hash_block when new work is delivered
	hash_reset <= reset_ii or new_work;
	found_count_increased <= '1' when found_counter > found_counter_i else '0';
	
	
	process(clk_sys)
	begin
		if rising_edge(clk_sys) then
			blinkCounter <= blinkCounter + 1; -- blink LED
			found_counter_i <= found_counter;  -- when crossing clock domains from fast to slow we could miss the nonce_found signal so we monitor the change in found count instead
		end if;
	end process;
	
	
	-- communication interface and registers
	-- uart_framer_i: entity work.uart_framer
	 -- generic map (
        -- CLK_FREQ    => 50e6,
        -- BAUD_RATE   => 115200,
        -- PARITY_BIT  => "none"
    -- )
    -- port map (
        -- clk         => clk_sys,
        -- reset         => reset,
        -- -- UART INTERFACE
        -- uart_txd    => uart_txd,
        -- uart_rxd    => uart_rxd,
        -- --user inputs to the framer
        -- nonce => nonce,
        -- nonce_found => found,
		-- found_counter => found_counter,
		-- -- debug inputs
		-- activity_counter => activity_counter,
        -- --user outputs from the framer
        -- skein_key2 => key2,
        -- skein_message2 => message2,
		-- bits_target => bitsTarget,
        -- hash_enable => hash_enable
    -- );
	
	-- communication interface and registers
	uart_nexus_interface_i: entity work.uart_nexus_interface
	 generic map (
        CLK_FREQ    => 50e6,
        BAUD_RATE   => 230400
    )
    port map (
        clk         => clk_sys,
        reset         => reset,
        -- UART INTERFACE
        uart_txd    => uart_txd,  -- tx to the host
        uart_rxd    => uart_rxd,  -- rx from the host
        --user inputs to the uart interface
        nonce => nonce,
        nonce_found => found_count_increased,
        --user outputs from the uart interface
        skein_key2 => key2,
        skein_message2 => message2,
		new_work => new_work
    );
	
	
	-- main hash block
	nxs_hash: entity work.nxs_hash
	port map
	(
		clk => clk_hash,
		reset => hash_reset,
		key2 => key2,
		message2 => message2,
		nonce => nonce,
		found => found,
		found_counter => found_counter
	);
	
end beh;
		
