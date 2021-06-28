-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- One round of threefish

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
entity skein_threefish is 
	generic
	(
		ROUND_BASE : integer range 0 to 79  -- we use this to limit the span of rounds to only what is possible at this pipeline stage
	);
	
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type
	);
	end skein_threefish;

architecture rtl of skein_threefish is
	
	constant TF_ROUNDS : integer := 80; 
	constant TF_PIPELINE_STAGES : integer := 1;
	
	
	type skein_pipe_array_type is array (0 to TF_PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);	
	
	--type state_array_type is array (0 to TF_ROUNDS - 1) of state_type;
	--signal threefish_state_retimining_array : state_array_type := (others =>(others =>(others => '0')));
	--attribute retiming_backward : integer;
	-- attribute retiming_backward of threefish_state_retimining_array: signal is 1;
	
	
	
begin
	
	process(clk)
		variable round : integer range 0 to TF_ROUNDS;
	begin
		if rising_edge(clk) then			
			round := ROUND_BASE;  -- current threefish round (0 to 79)
			skein_pipe(0) <= skein_in;
			skein_pipe(0).state <= f_threefish_1(skein_in.state, round);
		end if;
	end process;
	
	skein_out <= skein_pipe(0);
	
end rtl;
		
