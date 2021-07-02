-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Keccak for Nexus
-- one round of keccak with pipelining

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.keccak_pkg.all;

-------------------------------------------------------------------------------  
-- Ports
-------------------------------------------------------------------------------  
entity keccak_round is 
	-- generic
	-- (
		-- ROUND_BASE : integer range 0 to 23;  -- we use this to limit the span of rounds to only what is possible at this pipeline stage
	-- );

	port
	(
		clk			: in std_logic;
		k_in		: in keccak_pipe_type;
		k_out		: out keccak_pipe_type
		
	);
	end keccak_round;

architecture rtl of keccak_round is

	constant PIPELINE_STAGES : integer := 2;
	type keccak_pipe_array_type is array (0 to PIPELINE_STAGES - 1) of keccak_pipe_type;
	signal keccak_pipe : keccak_pipe_array_type := (others => keccak_pipe_init);
	
begin
	process(clk)
		variable round_c : std_logic_vector(63 downto 0);
	begin
		if rising_edge(clk) then
			keccak_pipe(0) <= k_in;
			keccak_pipe(0).state <= f_keccak_rho(f_keccak_theta(k_in.state));
			round_c := ROUND_CONST(keccak_pipe(0).round);
			keccak_pipe(1) <= keccak_pipe(0);
			keccak_pipe(1).state <= f_keccak_iota(f_keccak_chi(f_keccak_pi(keccak_pipe(0).state)), round_c);
			keccak_pipe(1).round <= keccak_pipe(0).round + 1;
			-- extra pipeline stages at the end for retiming
			for ii in 2 to PIPELINE_STAGES-1 loop
				keccak_pipe(ii) <= keccak_pipe(ii-1);
			end loop;
		end if;
	end process;
	
	k_out <= keccak_pipe(PIPELINE_STAGES - 1);
	
end rtl;
		
