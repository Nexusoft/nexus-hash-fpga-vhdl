-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Add the final subkey add for skein block 2

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
entity skein_2_last_subkey is 
	port
	(
		clk			: in std_logic;
		subkey_in	: in state_type;
		skein_in	: in skein_2_pipe_type;
		skein_out	: out skein_2_pipe_type
	);
	end skein_2_last_subkey;

architecture rtl of skein_2_last_subkey is
	
	constant PIPELINE_STAGES : integer := 2 + 2*(FOLD_RATIO/4);  -- must be a multiple of the fold
	type skein_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of skein_2_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_2_pipe_init);

begin

	
	process(clk)
		variable temp_subkey : state_type;
		
	begin
		if rising_edge(clk) then	
			skein_pipe(0) <= skein_in;
			skein_pipe(0).state <= f_State_Add(skein_in.state, subkey_in);	
			-- extra stages 
			for ii in 1 to PIPELINE_STAGES-1 loop
				skein_pipe(ii) <= skein_pipe(ii-1);
			end loop;
		end if;
	end process;
	
	skein_out <= skein_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
