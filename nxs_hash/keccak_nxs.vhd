-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Keccak for Nexus
-- Three rounds each at half size

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
entity keccak_nxs is 
	
	port
	(
		clk			: in std_logic;
		reset		: in std_logic;
		ready		: out std_logic; -- able to accept input
		message		: in std_logic_vector(1023 downto 0);  -- input to the hash
		result		: out unsigned(31 downto 0);  -- return the upper 32 bits of the hash result
		result_valid: out std_logic
	);
	end keccak_nxs;

architecture rtl of keccak_nxs is
	
	-- keccak pipeline types
	constant KECCAK_ROUNDS : integer := 24;  -- each super round contains 24 rounds.
	constant KECCAK_PIPELINE_STAGES : integer := KECCAK_ROUNDS;  -- two stages per round for 12 rounds.  
	constant KECCAK_TOTAL_STAGES : integer := 6 * KECCAK_PIPELINE_STAGES;
	type k_state_array_type is array (0 to KECCAK_PIPELINE_STAGES - 1) of k_state;  -- keccak state pipeline type
	type k_message_array_type is array (0 to KECCAK_PIPELINE_STAGES - 1) of k_message;  -- pipeline array for the message chunks
	type keccak_state_type is (ROUND1_A, ROUND1_B);
	
	-- keccak state pipelines
	signal k_state_pipe_1 : k_state_array_type := (others => (others => (others => (others => '0'))));
	signal k_state_pipe_2 : k_state_array_type := (others => (others => (others => (others => '0'))));
	signal k_state_pipe_3 : k_state_array_type := (others => (others => (others => (others => '0'))));


	-- message shift register
	signal k_message_pipe : k_message_array_type := (others => (others => (others => '0')));
	
	
	signal result_i : unsigned(31 downto 0) := (others => '0');
	signal result_valid_i : std_logic := '0';
	signal valid_counter : integer range 0 to KECCAK_TOTAL_STAGES := 0;
	signal pipeline_counter : integer range 0 to KECCAK_PIPELINE_STAGES := 0;
	signal keccak_state : keccak_state_type := ROUND1_A;
	--signal ready_i	: std_logic := 0;
	
	procedure SLV_to_message ( k_state_slv : in std_logic_vector(1023 downto 0);
								 k_message_1 : out k_message;
								 k_message_2 : out k_message) is
		-- convert a 1024 bit std_logic_vector into two array of words for absorbtion
		variable m1, m2 : k_message;
		begin
			-- first set of words
			for ii in 0 to 8 loop -- words
				for jj in 0 to 7 loop  -- bytes
					m1(ii)(8*jj+7 downto 8*jj) := k_state_slv(1023 - (64*ii+8*jj) downto 1023 - (64*ii+8*jj) - 7);
				end loop;
			end loop;
			-- second set of words
			for ii in 0 to 6 loop
				for jj in 0 to 7 loop
					m2(ii)(8*jj+7 downto 8*jj) := k_state_slv(1023 - 576 - (64*ii+8*jj) downto 1023 - 576 - (64*ii+8*jj) - 7);
				end loop;
			end loop;
			m2(7)(63 downto 0) := NXS_SUFFIX_1;
			m2(8)(63 downto 0) := NXS_SUFFIX_2;
			k_message_1 := m1;
			k_message_2 := m2;
			
			-- the last two words are the constant suffix
	end procedure SLV_to_message;
	
	function f_state_to_output (ks : in k_state) return unsigned is
		variable output : unsigned (31 downto 0); --(447 downto 0);
		begin
			--for ii in 0 to 6 loop
				--output(64*ii + 63 downto 64*ii) := unsigned(ks(ii / 5)(ii mod 5));
			--end loop
			output := unsigned(ks(1)(1)(63 downto 32));
			
			return output;
	end function f_state_to_output;
	
		
	function f_message_to_state (msg : in k_message) return k_state is
		variable temp_state : k_state;
		begin
			temp_state := (others => (others => (others => '0')));
			for ii in 0 to 8 loop
				temp_state(ii / 5)(ii mod 5) := msg(ii);
			end loop;
			return temp_state;
			
	end function f_message_to_state;
	
	

	function f_State_XOR ( s : in k_state; m : in k_message) return k_state is
		-- XOR a message with a state array
		variable temp_state : k_state;
		begin
			temp_state := s;
			for ii in 0 to 8 loop
				temp_state(ii / 5)(ii mod 5) := s(ii / 5)(ii mod 5) XOR m(ii);
			end loop;
		return temp_state;
	end function f_State_XOR;
	
	
begin

	process(clk)
	variable temp_state : k_state;
	variable m1, m2 : k_message;
	variable temp_round : integer range 0 to KECCAK_ROUNDS - 1;
	
	begin
		if rising_edge(clk) then
			if reset = '1' then
				valid_counter <= 0;
				result_valid_i <= '0';
				keccak_state <= ROUND1_A;
				pipeline_counter <= 0;
			else
				-- keep track of where we are in the pipeline
				if pipeline_counter < KECCAK_PIPELINE_STAGES - 1 then
					pipeline_counter <= pipeline_counter + 1;
				else
					pipeline_counter <= 0;
					-- swap states every time we go through one full round.
					if keccak_state = ROUND1_A then
						keccak_state <= ROUND1_B;
					else
						keccak_state <= ROUND1_A;
					end if;
				end if;	
			
				if valid_counter < KECCAK_TOTAL_STAGES then
					valid_counter <= valid_counter + 1;
				else
					if keccak_state = ROUND1_A then
						-- output results are valid when we are reading in new data in round 1
						result_valid_i <= '1';
					else 
						result_valid_i <= '0';
					end if;
				end if;
			end if;
			
			case keccak_state is
				when ROUND1_A => 
					-- Keccak super round 1 - first half
					-- absorb the input string into the keccak state 
					SLV_to_message(message, m1, m2);
					temp_state := f_message_to_state(m1);
					k_state_pipe_1(0) <= f_keccak_A(temp_state); -- do first round and save
					k_message_pipe(0) <= m2; -- save the rest of the message for later
					k_state_pipe_1(1) <= f_keccak_B(k_state_pipe_1(0),f_round_constant(0));
					
					-- super round 2 first half
					temp_state := f_State_XOR(k_state_pipe_1(KECCAK_PIPELINE_STAGES - 1), k_message_pipe(KECCAK_PIPELINE_STAGES - 1));
					k_state_pipe_2(0) <= f_keccak_A(temp_state);
					k_state_pipe_2(1) <= f_keccak_B(k_state_pipe_2(0),f_round_constant(0));
					
					--super round 3 first half
					k_state_pipe_3(0) <= f_keccak_A(k_state_pipe_2(KECCAK_PIPELINE_STAGES - 1));
					k_state_pipe_3(1) <= f_keccak_B(k_state_pipe_3(0),f_round_constant(0));
					
					--advance the message shift register during round A
					for ii in 1 to KECCAK_PIPELINE_STAGES - 1 loop
						k_message_pipe(ii) <= k_message_pipe(ii-1);
					end loop;
					
					
				when ROUND1_B => 
					-- super round 1 second half
					k_state_pipe_1(0) <= f_keccak_A(k_state_pipe_1(KECCAK_PIPELINE_STAGES - 1)); 				
					--k_message_pipe(0) <= k_message_pipe(KECCAK_PIPELINE_STAGES - 1);
					k_state_pipe_1(1) <= f_keccak_B(k_state_pipe_1(0),f_round_constant(KECCAK_PIPELINE_STAGES/2));
					
					-- super round 2 second half
					k_state_pipe_2(0) <= f_keccak_A(k_state_pipe_2(KECCAK_PIPELINE_STAGES - 1)); 
					k_state_pipe_2(1) <= f_keccak_B(k_state_pipe_2(0),f_round_constant(KECCAK_PIPELINE_STAGES/2));
					
					-- super round 3 second half
					k_state_pipe_3(0) <= f_keccak_A(k_state_pipe_3(KECCAK_PIPELINE_STAGES - 1)); 
					k_state_pipe_3(1) <= f_keccak_B(k_state_pipe_3(0),f_round_constant(KECCAK_PIPELINE_STAGES/2));
					
				when others =>
			end case;

			-- Iterate the rest of the rounds of keccak
			for ii in 1 to KECCAK_PIPELINE_STAGES/2 - 1 loop
				-- which round?
				if (keccak_state = ROUND1_A and ii <= pipeline_counter/2) or (keccak_state = ROUND1_B and ii > pipeline_counter/2) then 
					temp_round := ii;
				else
					temp_round := (ii + KECCAK_PIPELINE_STAGES/2);
				end if;
						
				k_state_pipe_1(2*ii) <= f_keccak_A(k_state_pipe_1(2*ii-1));
				k_state_pipe_1(2*ii+1) <= f_keccak_B(k_state_pipe_1(2*ii),f_round_constant(temp_round));
				
				k_state_pipe_2(2*ii) <= f_keccak_A(k_state_pipe_2(2*ii-1));
				k_state_pipe_2(2*ii+1) <= f_keccak_B(k_state_pipe_2(2*ii),f_round_constant(temp_round));
				
				k_state_pipe_3(2*ii) <= f_keccak_A(k_state_pipe_3(2*ii-1));
				k_state_pipe_3(2*ii+1) <= f_keccak_B(k_state_pipe_3(2*ii),f_round_constant(temp_round));
				
				
			end loop;
			result_i <= f_state_to_output(k_state_pipe_3(KECCAK_PIPELINE_STAGES - 1));
			
		end if;
	end process;

	result_valid <= result_valid_i;
	result <= result_i;
	ready <= '1' when (reset = '0') and (keccak_state = ROUND1_A) else '0';
	
	
end rtl;
		
