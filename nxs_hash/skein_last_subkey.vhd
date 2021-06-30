-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- The final subkey add for skein

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
entity skein_last_subkey is 
	generic
	(
		TWEAK : tweak_type := T3  
	);
	port
	(
		clk			: in std_logic;
		skein_in	: in skein_pipe_type;
		skein_out	: out skein_pipe_type
	);
	end skein_last_subkey;

architecture rtl of skein_last_subkey is
	
	constant PIPELINE_STAGES : integer := 2 + 2*(FOLD_RATIO/4);  -- must be a multiple of the fold
	type skein_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of skein_pipe_type;
	signal skein_pipe : skein_pipe_array_type := (others => skein_pipe_init);
	signal subkey	: state_type := (others =>(others => '0'));
	
	
begin

	
	process(clk)
		variable temp_subkey : state_type;
		
	begin
		if rising_edge(clk) then	
			-- generate final subkey
			skein_pipe(0) <= skein_in;
			temp_subkey := f_Get_Subkey(20, TWEAK, skein_in.key);
			subkey <= temp_subkey;
			--add subkey to the state
			skein_pipe(1) <= skein_pipe(0);
			skein_pipe(1).state <= f_State_Add(skein_pipe(0).state, subkey);	
			
			--if skein_in.nonce = x"00000004ECF83A53" then
			--	report "final subkey 20 : " & to_hstring(temp_subkey(0)) & " tweak " & to_hstring(TWEAK(0)) & " key " & to_hstring(skein_in.key(0));
			--end if;
			-- extra stages to make the pipeline a multiple of the fold
			for ii in 2 to PIPELINE_STAGES-1 loop
				skein_pipe(ii) <= skein_pipe(ii-1);
			end loop;
			
		end if;
	end process;
	
	skein_out <= skein_pipe(PIPELINE_STAGES-1);
	
end rtl;
		
