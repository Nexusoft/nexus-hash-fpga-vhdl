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
		read_ack	: out std_logic; 
		message		: in std_logic_vector(1023 downto 0);  -- input to the hash
		nonce_in	: in unsigned (63 downto 0);  -- nonce associated with the message for tracking purposes
		result		: out unsigned(31 downto 0);  -- return the upper 32 bits of the hash result
		nonce_out	: out unsigned(63 downto 0)
	);
	end keccak_nxs;

architecture rtl of keccak_nxs is
	
	-- keccak pipeline types
	constant KECCAK_ROUNDS : integer := 24;  -- each super round contains 24 rounds.
	
	signal result_i : unsigned(31 downto 0) := (others => '0');
	signal nonce_out_i : unsigned(63 downto 0) := (others => '0');
	
	--new method
	constant PIPELINE_STAGES_PER_ROUND : integer := 2;
	constant FOLD : integer := 2;  -- Number of clocks per hash.  Must be a factor of 24.  i.e. 1, 2, 3, 4, 6, 8, 12, 24
	constant KECCAK_ARRAY_LENGTH : integer := KECCAK_ROUNDS/FOLD+1;
	constant MESSAGE_ARRAY_LENGTH : integer := KECCAK_ARRAY_LENGTH*PIPELINE_STAGES_PER_ROUND-1;
	type keccak_array_type is array (0 to KECCAK_ARRAY_LENGTH - 1) of keccak_pipe_type;
	signal keccak_data_1 : keccak_array_type := (others => keccak_pipe_init);
	signal keccak_data_2 : keccak_array_type := (others => keccak_pipe_init);
	signal keccak_data_3 : keccak_array_type := (others => keccak_pipe_init);

	type k_message_array_type is array (0 to MESSAGE_ARRAY_LENGTH - 1) of k_message;  -- pipeline array for the message chunks
	signal k_message_pipe : k_message_array_type := (others => (others => (others => '0')));
	
	constant input_governor_count : integer := FOLD;
	signal input_governor : integer range 0 to input_governor_count - 1 := 0;
	signal read_ack_i : std_logic := '0';

begin

	
	generate_keccak_1: for ii in 1 to KECCAK_ARRAY_LENGTH-1 generate
		k_round : entity work.keccak_round
		port map
		(
			clk => clk,
			k_in => keccak_data_1(ii-1),
			k_out => keccak_data_1(ii)
		);
	end generate generate_keccak_1;
	
	generate_keccak_2: for ii in 1 to KECCAK_ARRAY_LENGTH-1 generate
		k_round : entity work.keccak_round
		port map
		(
			clk => clk,
			k_in => keccak_data_2(ii-1),
			k_out => keccak_data_2(ii)
		);
	end generate generate_keccak_2;
	
	generate_keccak_3: for ii in 1 to KECCAK_ARRAY_LENGTH-1 generate
		k_round : entity work.keccak_round
		port map
		(
			clk => clk,
			k_in => keccak_data_3(ii-1),
			k_out => keccak_data_3(ii)
		);
	end generate generate_keccak_3;
	
	
	
	process(clk)
	variable m1, m2 : k_message;
	variable temp_state : k_state;

	begin
		if rising_edge(clk) then
		
			-- manage input to keccak block 1
			if keccak_data_1(KECCAK_ARRAY_LENGTH - 1).status = KECCAK_IN_PROCESS and keccak_data_1(KECCAK_ARRAY_LENGTH - 1).round /= 24 then
				--recylce the data back into keccak block 1
				keccak_data_1(0) <= keccak_data_1(KECCAK_ARRAY_LENGTH - 1);
				input_governor <= input_governor + 1;
				read_ack_i <= '0';
			else
				-- input fresh data 
				SLV_to_message(message, m1, m2);
				keccak_data_1(0).state <= f_message_to_state(m1);
				keccak_data_1(0).nonce <= nonce_in;
				keccak_data_1(0).round <= 0;
				k_message_pipe(0) <= m2;
				if input_governor <  input_governor_count - 1 then  -- throttle input to the hash to minimize WIP on startup
					keccak_data_1(0).status <= JUNK;
					input_governor <= input_governor + 1;
					read_ack_i <= '0';
				else
					keccak_data_1(0).status <= KECCAK_IN_PROCESS;
					input_governor <= 0;
					read_ack_i <= '1';
					--advance message shift register when reading new input.  messages are needed between keccak blocks 1 and 2
					for ii in 1 to MESSAGE_ARRAY_LENGTH - 1 loop
						k_message_pipe(ii) <= k_message_pipe(ii-1);
					end loop;
					--if nonce_in = x"00000004ECF83A53" then
						--report "keccak register nonce in: " & to_hstring(nonce_in) & " m2: " & to_hstring(m2(0));
					--end if;
				end if;
			end if;
			--report "keccak nonce in: " & to_hstring(nonce_in) & " read_ack " & to_string(read_ack_i);
			--report "keccak block 1 input " & to_hstring(keccak_data_1(0).nonce) & " round flag: " & to_string(keccak_data_1(0).round) & " status: " & to_string(keccak_data_1(0).status);
			--report "keccak block 1 output " & to_hstring(keccak_data_1(KECCAK_ARRAY_LENGTH-1).nonce) & " round flag: " & to_string(keccak_data_1(KECCAK_ARRAY_LENGTH-1).round) & " status: " & to_string(keccak_data_1(KECCAK_ARRAY_LENGTH-1).status);
			
			-- for ii in 0 to KECCAK_ARRAY_LENGTH-1 loop
			-- if keccak_data_1(ii).nonce = x"00000004ECF83A53" then
				-- report "keccak block 1 state before round " & to_string(ii) & " : " & to_hstring(keccak_data_1(ii).state(0)(0)) & " round flag: " & to_string(keccak_data_1(ii).round) & " status: " & to_string(keccak_data_1(ii).status);
			-- end if;
			-- end loop;
			
			-- entry to keccak block 2
			-- if keccak_data_1(KECCAK_ARRAY_LENGTH - 1).nonce = x"00000004ECF83A53" then
				-- report "keccak super round 2 m2: " & to_hstring(k_message_pipe(MESSAGE_ARRAY_LENGTH - 1)(0)) & " state: " & to_hstring(keccak_data_1(KECCAK_ARRAY_LENGTH - 1).state(0)(0));
			-- end if;
			if keccak_data_1(KECCAK_ARRAY_LENGTH - 1).status = KECCAK_IN_PROCESS and keccak_data_1(KECCAK_ARRAY_LENGTH - 1).round /= 24 then
				--recylce the data back into keccak block 2
				keccak_data_2(0) <= keccak_data_2(KECCAK_ARRAY_LENGTH - 1);
			else
				-- transition from block 1 to block 2
				keccak_data_2(0) <= keccak_data_1(KECCAK_ARRAY_LENGTH - 1);
				temp_state := f_State_XOR(keccak_data_1(KECCAK_ARRAY_LENGTH - 1).state, k_message_pipe(MESSAGE_ARRAY_LENGTH - 1));
				keccak_data_2(0).state <= temp_state;
				keccak_data_2(0).round <= 0;
			end if;
			-- transition from keccak block 2 to 3
			if keccak_data_2(KECCAK_ARRAY_LENGTH - 1).status = KECCAK_IN_PROCESS and keccak_data_2(KECCAK_ARRAY_LENGTH - 1).round /= 24 then
				keccak_data_3(0) <= keccak_data_3(KECCAK_ARRAY_LENGTH - 1);
			else
				keccak_data_3(0) <= keccak_data_2(KECCAK_ARRAY_LENGTH - 1);
				keccak_data_3(0).round <= 0;
				--result_valid_2 <= '1';
				result_i <= f_state_to_output(keccak_data_3(KECCAK_ARRAY_LENGTH - 1).state);
				nonce_out_i <= keccak_data_3(KECCAK_ARRAY_LENGTH - 1).nonce;
			end if;

			--if nonce_out_i = x"00000004ECF83A53" then
			--report "keccak out nonce: " & to_hstring(nonce_out_i) & " hash result: " & to_hstring(result_i);
			--end if;
			
		end if;
	end process;
	
	result <= result_i;
	read_ack <= read_ack_i;
	nonce_out <= nonce_out_i;
	
	
end rtl;
		
