-- Basic configurable FIFO buffer with full and empty flags
-- bad things happen if you write to a full fifo or read from an empty fifo.  
-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity fifo is
	generic
	(
		FIFO_WIDTH : natural := 64;
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
end fifo;
 
architecture rtl of fifo is
 
	type fifo_data_type is array (0 to FIFO_DEPTH-1) of std_logic_vector(FIFO_WIDTH-1 downto 0);
	signal fifo_data : fifo_data_type := (others => (others => '0'));
	signal fifo_count : integer range 0 to FIFO_DEPTH := 0;
	signal write_index : integer range 0 to FIFO_DEPTH-1 := 0;
	signal read_index : integer range 0 to FIFO_DEPTH-1 := 0;
	signal full_i, empty_i : std_logic := '0';
	
begin
	process(clk)
	begin
		if rising_edge(clk) then
			if srst = '1' then
				fifo_count <= 0;
				write_index <= 0;
				read_index <= 0;
			else
				-- update the write index
				if wr_en = '1' then
					if write_index = FIFO_DEPTH - 1 then
						write_index <= 0;
					else
						write_index <= write_index + 1;
					end if;
				end if;
				-- update the read index
				if rd_en = '1' then
					if read_index = FIFO_DEPTH - 1 then
						read_index <= 0;
					else
						read_index <= read_index + 1;
					end if;
				end if;
				-- update the count
				if wr_en = '1' and rd_en = '0' and full_i = '0' then
					fifo_count <= fifo_count + 1;
				elsif wr_en = '0' and rd_en = '1' and empty_i = '0' then
					fifo_count <= fifo_count - 1;
				end if;
				-- write
				if wr_en = '1' then
					fifo_data(write_index) <= din;
				end if;
			end if;
		end if;
	end process;
   
   -- data out 
	dout <= fifo_data(read_index);
	full_i  <= '1' when fifo_count = FIFO_DEPTH else '0';
	empty_i <= '1' when fifo_count = 0 else '0';
	full <= full_i;
	empty <= empty_i;
	
end rtl;