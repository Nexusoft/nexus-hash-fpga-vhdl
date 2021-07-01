-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Skein for Nexus
-- One fully unrolled skein case is repeated twice.  

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
entity skein_nxs is 
	
	port
	(
		clk			: in std_logic;
		--reset		: in std_logic;
		key2		: in key_type;
		message2	: in state_type;
		read_ack	: out std_logic; -- acknowledge read of input data
		result		: out std_logic_vector(1023 downto 0); -- skein hash result as a std_logic_vector
		nonce_out	: out unsigned(63 downto 0)
		--result_valid: out std_logic
	);
	end skein_nxs;

architecture rtl of skein_nxs is

	
	signal result_valid_i : std_logic := '0';
	signal read_ack_i : std_logic := '0';
	signal result_i : std_logic_vector(1023 downto 0) := (others => '0');
	
	signal latency : integer; --latency of the skein component for debug
	signal skein_in_i, skein_block_in, skein_block_out, skein_make_key_in : skein_pipe_type := skein_pipe_init;
	constant input_governor_count : integer := FOLD_RATIO;
	signal input_governor : integer range 0 to input_governor_count - 1 := 0;
	signal nonce_out_i : unsigned (63 downto 0) := (others => '0');
	
	
	constant SKEIN_ROUND_INSTANCES : integer := SKEIN_ROUNDS_PER_BLOCK / FOLD_RATIO;
	constant SKEIN_DATA_ARRAY_LENGTH : integer := SKEIN_ROUND_INSTANCES;
	type skein_array_type is array (0 to SKEIN_DATA_ARRAY_LENGTH-1) of skein_pipe_type;
	type skein_2_array_type is array (0 to SKEIN_DATA_ARRAY_LENGTH-1) of skein_2_pipe_type;

	signal skein_data_2_in, skein_data_2_out : skein_2_array_type := (others => skein_2_pipe_init);
	signal skein_data_3_in, skein_data_3_out : skein_array_type := (others => skein_pipe_init);
	signal skein_3_last_subkey_out: skein_pipe_type := skein_pipe_init;
	signal skein_2_last_subkey_out: skein_2_pipe_type := skein_2_pipe_init;
	signal skein_round_state : unsigned(FOLD_RATIO_NUM_BITS-1 downto 0) := (others => '0');
	signal skein_make_key_out : key_type := (others => (others => '0'));
	signal skein_make_key_nonce_out : unsigned (63 downto 0) := (others => '0');
	
	
	
begin
	
	
	generate_skein_2: for ii in 0 to SKEIN_ROUND_INSTANCES-1 generate
		skein_round_2 : entity work.skein_round_2
		port map
		(
			clk => clk,
			state_in => skein_data_2_in(ii).state,
			subkey_1_in => f_Get_Subkey(ii*4, T2, key2),
			subkey_2_in => f_Get_Subkey(ii*4+1, T2, key2),
			subkey_3_in => f_Get_Subkey(ii*4+2, T2, key2),
			subkey_4_in => f_Get_Subkey(ii*4+3, T2, key2),
			nonce_in => skein_data_2_in(ii).nonce,
			state_out => skein_data_2_out(ii).state,
			nonce_out => skein_data_2_out(ii).nonce
		);
	end generate generate_skein_2;
	
	skein_2_last_subkey : entity work.skein_2_last_subkey
	port map
	(
		clk => clk,
		subkey_in => f_Get_Subkey(20, T2, key2),
		skein_in => skein_data_2_out(SKEIN_DATA_ARRAY_LENGTH-1),
		skein_out => skein_2_last_subkey_out
	);
	
	
	skein_make_key: entity work.skein_make_key
	port map
	(
		clk => clk,
		state_in => skein_2_last_subkey_out.state,
		message_in => message2,
		nonce_in => skein_2_last_subkey_out.nonce,
		key_out => skein_make_key_out,
		nonce_out => skein_make_key_nonce_out
	);
	
	generate_skein_3: for ii in 0 to SKEIN_ROUND_INSTANCES-1 generate
		skein_round_3 : entity work.skein_round
		generic map (
			SUBKEY_BASE_ROUND => ii*2*FOLD_RATIO,
			TWEAK => T3
		)
		port map
		(
			clk => clk,
			skein_in => skein_data_3_in(ii),
			skein_out => skein_data_3_out(ii)
		);
	end generate generate_skein_3;
	
	skein_last_subkey_3 : entity work.skein_last_subkey
	generic map (
		TWEAK => T3
	)
	port map
	(
		clk => clk,
		skein_in => skein_data_3_out(SKEIN_DATA_ARRAY_LENGTH-1),
		skein_out => skein_3_last_subkey_out
	);

	
	process(clk)
		variable temp_state : state_type;
		variable temp_message : state_type;
		variable temp_key : key_type;
	begin
		if rising_edge(clk) then
			skein_round_state <= skein_round_state + 1;

			if skein_round_state = 0 then
				-- feed new data
				skein_data_2_in(0).state <= message2;
				skein_data_2_in(0).nonce <= message2(10);
				--skein_data_2_in(0).loop_count <= (others => '0');
				read_ack_i <= '1';
				skein_data_3_in(0).key <= f_Get_First_Subkey(T3, skein_make_key_out);
				skein_data_3_in(0).subkey_round <= 0;
				skein_data_3_in(0).state <= (others =>(others => '0'));  -- in round 3 the message is all zeros
				skein_data_3_in(0).nonce <= skein_make_key_nonce_out;
				--skein_data_3_in(0).loop_count <= (others => '0');
				result_i <= f_State_to_SLV(skein_3_last_subkey_out.state);  -- only register the output when it is valid
				nonce_out_i <= skein_3_last_subkey_out.nonce;
				
				-- if skein_data_2_in(0).nonce = x"00000004ECF83A53" then
				-- report "skein_data_2_in(0) state: " & to_hstring(skein_data_2_in(0).state(0));
				-- end if;
				-- if skein_data_2_out(0).nonce = x"00000004ECF83A53" then
					-- report "skein_data_2_out(0) state: " & to_hstring(skein_data_2_out(0).state(0));
				-- end if;
				if skein_2_last_subkey_out.nonce = x"00000004ECF83A53" then
					report "skein round 2 output: " & to_hstring(skein_2_last_subkey_out.state(0));
				end if;
				-- if skein_data_3_in(0).nonce = x"00000004ECF83A53" then
					-- report "skein_data_3_in(0) state: " & to_hstring(skein_data_3_in(0).state(0)) & " key " & to_hstring(skein_data_3_in(0).key(0));
				-- end if;
				if skein_3_last_subkey_out.nonce = x"00000004ECF83A53" then
					report "skein round 3 output: " & to_hstring(skein_3_last_subkey_out.state(0));
				end if;
				
			else
				--recycle
				skein_data_2_in(0) <= skein_data_2_out(0);
				skein_data_3_in(0) <= skein_data_3_out(0);
				read_ack_i <= '0';
			end if;
			
			
			--report to_string(to_integer(skein_round_state)) & " loop count " & to_string(to_integer(skein_make_key_out.loop_count));
			
			for ii in 1 to SKEIN_ROUND_INSTANCES-1 loop
				skein_data_2_in(ii) <= skein_data_2_out(ii-1) when skein_round_state = 0 else skein_data_2_out(ii);
				skein_data_3_in(ii) <= skein_data_3_out(ii-1) when skein_round_state = 0 else skein_data_3_out(ii);
			end loop;
			
		
		end if;
	end process;
	
	result <= result_i;
	read_ack <= read_ack_i;  
	nonce_out <= nonce_out_i;
	
end rtl;
		
