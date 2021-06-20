-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Simulation test bench for nxs hash with the uart interface. 
-- Read in a text file with an ascii work package and send it to the uart interface.  Hash and return a nonce when found.

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

entity nexus_hash_uart_tb is
end nexus_hash_uart_tb;

architecture beh of nexus_hash_uart_tb is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal clk50, clk250 : std_logic := '0';
	signal reset, hash_reset: std_logic := '1';
	signal uart_rxd, uart_txd : std_logic := '0';
	signal tb_uart_txd, tb_uart_rxd, tb_rx_valid, tb_busy, tb_send : std_logic := '0';
	signal tb_uart_rx_data, tb_uart_tx_data : byte_type := (others => '0');
	constant max_reset_count : integer := 15;
	signal reset_counter : integer range 0 to max_reset_count := 0;
	
	--Caution!! these may be adjusted to wild values to make simulation run faster.  
	constant clk50_period  : time := 20 ns;  -- nominal 20ns.  ok to leave this alone
	constant clk50_freq : integer := 40e5; -- nominally 50e6.  Set to 40e5 to speed up simulation
	constant clk250_period  : time := 400 ns; -- nominal value is 4ns set to 400ns to speed up uart simulation.
	constant BAUD : integer := 230400; 
	
	constant TIMEOUT : integer := 11;	--ms
	constant TIMEOUT_COUNT_LIMIT : integer := clk50_freq * TIMEOUT / 1000;
	signal timeout_counter : integer range 0 to TIMEOUT_COUNT_LIMIT := TIMEOUT_COUNT_LIMIT;

	
	signal found_count_increased, new_work : std_logic := '0';
	signal expected_nonce : unsigned (63 downto 0) := (others => '0'); 
	signal found, found_i, found_ii: std_logic := '0';
	signal nonce, nonce_i, previous_nonce : unsigned (63 downto 0) := (others => '0'); 
	signal key2 : key_type := (others => (others => '0'));
	signal message2 : state_type := (others => (others => '0'));

	signal work_package_slv : std_logic_vector(WORK_PACKAGE_SIZE_BITS - 1 downto 0) := (others => '0');
	constant MAX_LINE_LENGTH : integer := 2048;
	signal lineLength : integer range 0 to MAX_LINE_LENGTH := 0;
    signal i : integer range 0 to WORK_PACKAGE_SIZE_BYTES := 0;
	
	type state_mach_type is (PROCESSING,STOP);
    signal state : state_mach_type := PROCESSING;
	
	signal found_counter, found_counter_i : unsigned(31 downto 0) := (others => '0');
	--signal bitsTarget : unsigned (31 downto 0) := x"00000000";  
	
	signal nonce_matches : boolean := false;
	signal clock_counter : natural := 0;
	

begin

	-- test uart - simulates the host
	uart_i: entity work.UART
    generic map (
        CLK_FREQ    => clk50_freq,
        BAUD_RATE   => BAUD,
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
	uart_nexus_interface_i: entity work.uart_nexus_interface
	 generic map (
        CLK_FREQ    => clk50_freq,
        BAUD_RATE   => BAUD
    )
    port map (
        clk         => clk50,
        reset         => reset,
        -- UART INTERFACE
        uart_txd    => uart_txd,  -- tx to the host
        uart_rxd    => uart_rxd,  -- rx from the host
        --user inputs to the uart interface
        nonce => nonce_i,
        nonce_found => found_ii,
        --user outputs from the uart interface
        skein_key2 => key2,
        skein_message2 => message2,
		new_work => new_work
    );
	
	-- main hash block
	nxs_hash: entity work.nxs_hash
	port map
	(
		clk => clk250,
		reset => hash_reset,
		key2 => key2,
		message2 => message2,
		nonce => nonce,
		found => found
	);
	
	found_count_increased <= '1' when found_counter > found_counter_i else '0';
	hash_reset <= '1' when reset_counter < max_reset_count else reset;
	
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
		clock_counter <= clock_counter + 1;
	end process;
	
	reset_process : process
	begin
		reset <= '1';
		wait for clk50_period;
		wait for clk250_period;
		reset <= '0';
		wait;
	end process;
	
	-- clock_250_process : process(reset, clk250)
	-- begin
		-- if reset = '1' then
		-- elsif rising_edge(clk250) then
		-- end if;
	-- end process;
	process(clk50)
	begin
		if rising_edge(clk50) then
			found_i <= found; 
			nonce_i <= nonce;
			if found_i = '0' and found = '1' then -- assert found when it transitions from high to low to avoid multiple founds
				found_ii <= '1';
				found_counter <= found_counter + 1;
			else
				found_ii <= '0';
			end if;
		end if;
	end process;

	test_uart_send : process(reset, clk50)
        file filein : text open read_mode is "sim_nexus_work_package_in_1.txt";
        variable line_in : line;
		variable work_package : std_logic_vector(WORK_PACKAGE_SIZE_BITS - 1 downto 0);
		variable temp_byte : byte_type;
	    begin
		if reset = '1' then
			state <= PROCESSING;
			lineLength <= 0;
			i <= 0;
		elsif rising_edge(clk50) then
			case state is
                when PROCESSING =>
					if lineLength = 0 or i >= WORK_PACKAGE_SIZE_BYTES then -- did we just start or reach the end of a line?
						tb_send <= '0'; -- don't send anything
						if endfile(filein) then  -- check for end of file
							FILE_CLOSE(filein);
							report "Done sending data.";
							state <= STOP;
						elsif timeout_counter = TIMEOUT_COUNT_LIMIT then  -- wait between file lines to force a timeout between sending work packages
							readline(filein,line_in); -- read the next line
							lineLength <= line_in'length; -- number of characters in the line
							hread(line_in, work_package);  -- convert the hex string to a big standard logic vector
							work_package_slv <= work_package;
							i <= 0; -- set the index to the first byte in the slv
							timeout_counter <= 0;
						else
							timeout_counter <= timeout_counter + 1;
						end if;
					elsif tb_busy = '0' and tb_send <= '0' then	-- ready to send.  check tb_send = '0' to ensure we don't try to send back to back bytes.  
						temp_byte := work_package_slv(WORK_PACKAGE_SIZE_BITS - 1 - i*8 downto WORK_PACKAGE_SIZE_BITS - 1 - i*8 - 7); -- get the next byte
						tb_uart_tx_data <= temp_byte;
						tb_send <= '1';
						i <= i+1;
						--report "Sent byte " & to_string(i) & ": " & to_hstring(temp_byte);
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
	variable temp_byte : byte_type;
	
	begin
		if reset = '1' then
			reset_counter <= 0;
		elsif rising_edge(clk50) then
			found_counter_i <= found_counter;  -- when crossing clock domains from fast to slow we could miss the nonce_found signal so we monitor the change in found count instead
			if reset_counter < max_reset_count then
				reset_counter <= reset_counter + 1;  --hold reset more than one clock to ensure reset reaches all divided down clock domains
			end if;
			if tb_rx_valid = '1' then
				temp_byte := tb_uart_rx_data;
				report "Received: " & to_hstring(temp_byte);
			end if;
			if new_work = '1' then
				report "Work Package Received.";
				report "Key2:";
				for ii in 0 to 16 loop
					report to_hstring(Key2(ii));
				end loop;
				report "Message2:";
				for ii in 0 to 10 loop
					report to_hstring(Message2(ii));
				end loop;
				reset_counter <= 0;
			end if;
		end if;
	end process;
	
	expected_nonce <= x"00000004ECF83A53";

	nonce_matches <= nonce = expected_nonce;
	assert clock_counter < 100000 report "Reached Simulation End Time." severity FAILURE;
	
	test : process
	begin
		wait until (found = '1' and nonce_matches);
		report "found nonce " & to_hstring(nonce);
		wait for 100 us;
		finish;
	end process;

end beh;
