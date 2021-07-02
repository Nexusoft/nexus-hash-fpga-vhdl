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
		key2		: in key_type;
		message2	: in state_type;
		read_ack	: out std_logic; -- acknowledge read of input data
		result		: out std_logic_vector(1023 downto 0); -- skein hash result as a std_logic_vector
		nonce_out	: out unsigned(63 downto 0)
	);
	end skein_nxs;

architecture rtl of skein_nxs is

	
	signal read_ack_i : std_logic := '0';
	signal result_i : std_logic_vector(1023 downto 0) := (others => '0');
	signal nonce_out_i : unsigned (63 downto 0) := (others => '0');
	
	
	--constant SKEIN_ROUND_INSTANCES : integer := SKEIN_ROUNDS_PER_BLOCK / FOLD_RATIO;
	--constant SKEIN_DATA_ARRAY_LENGTH : integer := SKEIN_ROUND_INSTANCES;
	--type skein_array_type is array (0 to SKEIN_DATA_ARRAY_LENGTH-1) of skein_pipe_type;
	--type skein_2_array_type is array (0 to SKEIN_DATA_ARRAY_LENGTH-1) of skein_2_pipe_type;

	--signal skein_data_2_in, skein_data_2_out : skein_2_array_type := (others => skein_2_pipe_init);
	--signal skein_data_3_in, skein_data_3_out : skein_array_type := (others => skein_pipe_init);
	--signal skein_3_last_subkey_out: skein_pipe_type := skein_pipe_init;
	--signal skein_2_last_subkey_out: skein_2_pipe_type := skein_2_pipe_init;
	signal skein_round_state : unsigned(FOLD_RATIO_NUM_BITS-1 downto 0) := (others => '0');
	--signal skein_make_key_out : key_type := (others => (others => '0'));
	--signal skein_make_key_in : state_type := (others => (others => '0'));
	--signal skein_make_key_nonce_in, skein_make_key_nonce_out : unsigned (63 downto 0) := (others => '0');
	
	signal skein_data_in, skein_data_out : skein_pipe_type := skein_pipe_init;
	
begin
	
	
	-- generate_skein_2: for ii in 0 to SKEIN_ROUND_INSTANCES-1 generate
		-- skein_round_2 : entity work.skein_round_2
		-- port map
		-- (
			-- clk => clk,
			-- state_in => skein_data_2_in(ii).state,
			-- subkey_1_in => f_Get_Subkey(ii*4, T2, key2),
			-- subkey_2_in => f_Get_Subkey(ii*4+1, T2, key2),
			-- subkey_3_in => f_Get_Subkey(ii*4+2, T2, key2),
			-- subkey_4_in => f_Get_Subkey(ii*4+3, T2, key2),
			-- nonce_in => skein_data_2_in(ii).nonce,
			-- state_out => skein_data_2_out(ii).state,
			-- nonce_out => skein_data_2_out(ii).nonce
		-- );
	-- end generate generate_skein_2;
	
	-- skein_2_last_subkey : entity work.skein_2_last_subkey
	-- port map
	-- (
		-- clk => clk,
		-- subkey_in => f_Get_Subkey(20, T2, key2),
		-- skein_in => skein_data_2_out(SKEIN_DATA_ARRAY_LENGTH-1),
		-- skein_out => skein_2_last_subkey_out
	-- );
	
	
	-- skein_make_key: entity work.skein_make_key
	-- port map
	-- (
		-- clk => clk,
		-- state_in => skein_2_last_subkey_out.state,
		-- message_in => message2,
		-- nonce_in => skein_2_last_subkey_out.nonce,
		-- key_out => skein_make_key_out,
		-- nonce_out => skein_make_key_nonce_out
	-- );
	
	-- generate_skein_3: for ii in 0 to SKEIN_ROUND_INSTANCES-1 generate
		-- skein_round_3 : entity work.skein_round
		-- generic map (
			-- TWEAK => T3
		-- )
		-- port map
		-- (
			-- clk => clk,
			-- skein_in => skein_data_3_in(ii),
			-- skein_out => skein_data_3_out(ii)
		-- );
	-- end generate generate_skein_3;
	
	-- skein_last_subkey_3 : entity work.skein_last_subkey
	-- port map
	-- (
		-- clk => clk,
		-- skein_in => skein_data_3_out(SKEIN_DATA_ARRAY_LENGTH-1),
		-- skein_out => skein_3_last_subkey_out
	-- );
	
	
		skein_half_block : entity work.skein_block_half
		port map
		(
			clk => clk,
			skein_in => skein_data_in,
			skein_out => skein_data_out
		);
		
		-- skein_make_key: entity work.skein_make_key
		-- port map
		-- (
			-- clk => clk,
			-- state_in => skein_data_out.state,
			-- message_in => message2,
			-- nonce_in => skein_data_out.nonce,
			-- key_out => skein_make_key_out,
			-- nonce_out => skein_make_key_nonce_out
		-- );

	

	
	process(clk)
		variable temp_state : state_type;
		variable temp_message : state_type;
		variable temp_key : key_type;
	begin
		if rising_edge(clk) then
			skein_round_state <= skein_round_state + 1;

			case skein_round_state is
			when "00" =>
				-- feed new data to skein block 2
				skein_data_in.state <= message2;
				skein_data_in.key <= f_Get_First_Subkey(T2, key2);
				skein_data_in.nonce <= message2(10);
				skein_data_in.loop_count <= 0;
				read_ack_i <= '1';
				nonce_out_i <= skein_data_out.nonce;
				result_i <= f_State_to_SLV(skein_data_out.state);
				if skein_data_out.nonce = x"00000004ECF83A53" then
					report "skein round 3 output: " & to_hstring(skein_data_out.state(0));
				end if;
			when "01"|"11" => 
				-- recycle
				skein_data_in <= skein_data_out;
				read_ack_i <= '0';
				--if skein_data_out.nonce = x"00000004ECF83A53" then
					--report "half way state: " & to_hstring(skein_data_out.state(0));
				--end if;
			when "10" => 
				-- start skein block 3
				skein_data_in <= skein_data_out;
				temp_message := message2;
				temp_message(10) := skein_data_out.nonce;
				skein_data_in.state <= f_State_XOR(skein_data_out.state, temp_message);  -- used to generate the next key inside the skein block
				read_ack_i <= '0';
				if skein_data_out.nonce = x"00000004ECF83A53" then
					temp_key := f_make_key(f_State_XOR(skein_data_out.state, message2));
					report "skein round 2 output: " & to_hstring(skein_data_out.state(0)) & " next key " & to_hstring(temp_key(16));
				end if;
			when others =>
			end case;
			
		
		end if;
	end process;
	
	result <= result_i;
	read_ack <= read_ack_i;  
	nonce_out <= nonce_out_i;
	
end rtl;
		
