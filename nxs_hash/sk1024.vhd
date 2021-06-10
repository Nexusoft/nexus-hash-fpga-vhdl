-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein-Keccak 1024 bit hash for Nexus

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.skein_pkg.all;

-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity sk1024 is 
	
	port
	(
		clk			: in std_logic;
		reset		: in std_logic;
		key2		: in key_type;
		message2	: in state_type;
		read_ack	: out std_logic;  -- read acknowlege
		result		: out unsigned(31 downto 0);  -- return just the upper 32 bits of the hash
		result_valid: out std_logic
	);
	end sk1024;

architecture rtl of sk1024 is

	signal skein_result, fifo_data_out	: std_logic_vector(1023 downto 0);  
	signal skein_result_valid, fifo_empty : std_logic;
	
	signal keccak_result	: unsigned(31 downto 0);  
	signal keccak_result_valid, keccak_ready, fifo_rdreq : std_logic;
	signal keccak_reset, fifo_loaded : std_logic := '0'; 
	constant KECCAK_STARTUP_DELAY : integer := 26;  -- delay to allow fifo to fill up after reset.  Must be at least as long as one keccak round.
	signal keccak_startup_delay_counter : integer range 0 to 26 := 0; 
	signal skein_nonce_in : unsigned (63 downto 0);
	
--	--altera
--	Component fifo_1024
--	port
--	(
--		sclr		: IN STD_LOGIC ;
--		clock		: IN STD_LOGIC ;
--		data		: IN STD_LOGIC_VECTOR (1023 DOWNTO 0);
--		rdreq		: IN STD_LOGIC ;
--		wrreq		: IN STD_LOGIC ;
--		empty		: OUT STD_LOGIC;
--		full		: OUT STD_LOGIC;
--		q			: OUT STD_LOGIC_VECTOR (1023 DOWNTO 0)
--	);
--	end component fifo_1024;
	
	--xilinx
--	COMPONENT fifo_1024_wide_128_deep
--	 PORT (
--		clk : IN STD_LOGIC;
--		srst : IN STD_LOGIC;
--		din : IN STD_LOGIC_VECTOR(1023 DOWNTO 0);
--		wr_en : IN STD_LOGIC;
--		rd_en : IN STD_LOGIC;
--		dout : OUT STD_LOGIC_VECTOR(1023 DOWNTO 0);
--		full : OUT STD_LOGIC;
--		empty : OUT STD_LOGIC;
--		wr_rst_busy : OUT STD_LOGIC;
--		rd_rst_busy : OUT STD_LOGIC
--	 );
--	END COMPONENT;

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
	
	
begin

	process (clk)
	begin
		if rising_edge(clk) then
			if reset = '1' then
				fifo_loaded <= '0';
				keccak_startup_delay_counter <= 0;
			else
				-- in theory once the fifo has data in it, it should never become empty again unless there is a system reset.
				-- after reset make wait a bit to fill up the fifo sufficiently before reading from it.
				if fifo_empty = '0' then
					-- fifo has something in it.  
					if keccak_startup_delay_counter < KECCAK_STARTUP_DELAY then
						-- we are just starting up.  wait a bit.
						keccak_startup_delay_counter <= keccak_startup_delay_counter + 1;
						fifo_loaded <= '0';
					else
						-- fifo is sufficiently loaded.  ok to start reading from it.
						fifo_loaded <= '1';
					end if;
				else
					-- fifo is empty
					fifo_loaded <= '0';
				end if;
			end if;
		end if;
	end process;

	skein: entity work.skein_nxs
	port map
	(
		clk => clk,
		reset => reset,
		key2 => key2,
		message2 => message2,
		read_ack => read_ack,
		result => skein_result,
		result_valid => skein_result_valid
	);
	
	skein_nonce_in <= message2(10); -- nonce part of the input message to skein for debug.


	-- for best performance in an FPGA, I suggest instatiating the FIFO using the ip wizard or hardware specific primitives.   
	fifo_1024 : fifo
	generic map
	(
	FIFO_WIDTH => 1024,
	FIFO_DEPTH => 64
	)
    port map 
	(
		clk => clk,
		srst => reset,
		din => skein_result,
		rd_en => fifo_rdreq,
		wr_en => skein_result_valid,
		full => open,
		dout => fifo_data_out,
		empty => fifo_empty
	);

	
--	xilinx_fifo : fifo_1024_wide_128_deep
--	  PORT MAP (
--		clk => clk,
--		srst => reset,
--		din => skein_result,
--		wr_en => skein_result_valid,
--		rd_en => fifo_rdreq,
--		dout => fifo_data_out,
--		full => open,
--		empty => fifo_empty,
--		wr_rst_busy => open,
--		rd_rst_busy => open
--	  );
	
	
	-- altera_fifo_1024_inst : fifo_1024 PORT MAP (
		-- sclr	=> reset,
		-- clock	 => clk,
		-- data	 => skein_result,
		-- rdreq	 => keccak_ready,
		-- wrreq	 => skein_result_valid,
		-- empty	=> fifo_empty,
		-- full	=> open,
		-- q	 	=> fifo_data_out
	-- );
	
	
	keccak: entity work.keccak_nxs
	port map
	(
		clk => clk,
		reset => keccak_reset,
		ready => keccak_ready,
		message => fifo_data_out,
		result => keccak_result,
		result_valid => keccak_result_valid
	);
	
	fifo_rdreq <= keccak_ready and fifo_loaded and not reset;
	keccak_reset <= reset or not fifo_loaded; -- hold keccak in reset until fifo is loaded
	
	result <= keccak_result;
	result_valid <= keccak_result_valid;
	
end rtl;
		
