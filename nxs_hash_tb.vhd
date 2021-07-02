-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Simulation test bench for nexus hash

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.finish;

library work;
use work.skein_pkg.all;

-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity nexus_hash_tb is 
end nexus_hash_tb;

architecture beh of nexus_hash_tb is

	-------------------------------------------------------------------------------
	-- signals 
	-------------------------------------------------------------------------------
	signal clk : std_logic := '0';
	signal reset : std_logic := '1';
	signal startup_done : std_logic := '0';
	signal startup_counter : integer range 0 to 3 := 0;
	
	--signal header : std_logic_vector(1727 downto 0);
	--signal result, expected_result, keccak_in : std_logic_vector(1023 downto 0);
	signal found, found_i, found_ii : std_logic := '0';
	signal nonce_matches : boolean;
	signal nonce, nonce_i, expected_nonce, initial_nonce, previous_nonce : unsigned (63 downto 0) := (others => '0');
	signal key2 : key_type;
	signal message2 : state_type;
	signal found_counter : unsigned(31 downto 0);
	-- signal bitsTarget : unsigned (31 downto 0);  
	signal clock_counter : natural := 0;
	constant total_latency : integer := 477*2 + 500;
	
	constant nonce_starting_offset : integer := 211; 

	
begin
	
	-------------------------------------------------------------------------------
	-- Generate clock
    -------------------------------------------------------------------------------
	gen_clk : process
	begin
		clk <= '0';
		wait for 10 ns;
		clk <= '1';
		wait for 10 ns;
		clock_counter <= clock_counter + 1;
	end process;
	
	gen_reset : process(clk)
	begin
		if rising_edge(clk) then
			if startup_done = '0' then
				reset <= '1';
			else
				reset <= '0';
			end if;
		end if;
	end process;
	
	-- reset on startup
	gen_startup : process(clk)
	begin
		if rising_edge(clk) then
			if startup_counter < 3 then
				startup_done <= '0';
				startup_counter <= startup_counter + 1;
			else
				startup_done <= '1';
			end if;
		end if;
	end process;
	
	
	--header <= x"04000000a793e4310d6957c758f287f462bebfbb562d25d3d8d79716a53304272d76faa68a09fc5e3a2d0005d55b1b651f401b9f482456f6c421512daf55f2f670135d02a544fad7631e4b715b0013dfc8968ee60898e8b50d8dda813e45e5e0186a3aed9e6f1d1162673e62fe393f0e9b4698c705cf93d5ca009ba2d20163540209000025303a6d34a2a89e91ecb814f50d16a62944417c4322b45038c79422419ad45f90bef6ebbe075baa0450f788d03e1940316ac2434f1cfd30cd0742fc58a4f531020000006cdf1e00d82e037b533af8ec04000000";
	--expected_result <= x"1c9a659c1a825cf5e0d6ce59ddad20ddd2ce6467de69f77f3a115b20c6818f80e63b566b81fb38b2781d7b39c49ef745ecd93aab999190d09415ac0f0edd56386d0813eb91a8f43de7e8e7de8926b0251aa1dcc1c8cf8e1f8d20b35df9b110616fdfc0eda39c25ea69bd19ba33a5e358db527591db911b272d40423f4f386fbc";
	expected_nonce <= x"00000004ECF83A53";
	-- key2 and message2 are precalculated by the software.
	-- normally the initial nonce will be zero, but for simulation set it to something close to the answer
	initial_nonce <= expected_nonce - nonce_starting_offset;
	key2 <= (x"88AC877DCC3D9F5B", x"0CE6F8BE43BA5FA4", x"65AF0210816973D7", x"ED0029D7DD5FA14C", x"F0D019FEEBA16EC7", x"4D0E1BD4235DAE9F", x"D510716EA14E5A89", x"59ADF0434406532B", x"F58F5153DCFE34A4", x"494553ABAEAC4A2B", x"05574FFA0116287B", x"B5F44CE1F0D31096", x"56B3513630FDD8DB", x"A06515574CDD6BD3", x"A7447037B78D79EC", x"8EDB8035835CFB1F", x"BBF337D66036F952");
	message2 <= (x"6D3A302500000902", x"14B8EC919EA8A234", x"7C414429A6160DF5", x"2294C73850B42243", x"EBF6BE905FD49A41", x"88F75004AA5B07BE", x"43C26A3140193ED0", x"FC4207CD30FD1C4F", x"0000000231F5A458", x"7B032ED8001EDF6C", initial_nonce, x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000", x"0000000000000000");
	-- bitsTarget <= x"7B032ED8";
	
	nxs_hash: entity work.nxs_hash
	port map
	(
		clk => clk,
		reset => reset,
		key2 => key2,
		message2 => message2,
		nonce => nonce,
		found => found
		--found_counter => found_counter
	);
	
	process(clk)
	begin
		if rising_edge(clk) then
			found_i <= found; 
			nonce_i <= nonce;
			if found_i = '1' then
				previous_nonce <= nonce_i;
			end if;
		end if;
	end process;
	found_ii <= '1' when found_i = '1' and nonce_i /= previous_nonce else '0';
	
	
	
	nonce_matches <= nonce_i = expected_nonce;
	assert clock_counter < total_latency + nonce_starting_offset*2 + 60 report "Reached Simulation End Time. " & to_string(clock_counter) severity FAILURE;
	
	test : process
	begin
		wait until (found_ii = '1' and startup_done = '1' and nonce_matches);
		report "found nonce " & to_hstring(nonce) & " at clock " & to_string(clock_counter);
		
		for jj in 1 to 50 loop
			wait until clk = '1';
		end loop;
		
		finish;
	end process;
end beh;
		
