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
	signal skein_round_state : integer range 0 to FOLD_RATIO-1 := 0;
	signal skein_data_in, skein_data_out : skein_pipe_type := skein_pipe_init;
	
begin
	
		generate_quarter_size: if FOLD_RATIO = 4 generate
			skein_half_block : entity work.skein_block_half
			port map
			(
				clk => clk,
				skein_in => skein_data_in,
				skein_out => skein_data_out
			);
		end generate generate_quarter_size;
		
		generate_half_size: if FOLD_RATIO = 2 generate
			skein_half_block : entity work.skein_block
			port map
			(
				clk => clk,
				skein_in => skein_data_in,
				skein_out => skein_data_out
			);
		end generate generate_half_size;
		
	
	process(clk)
		variable temp_state : state_type;
		variable temp_message : state_type;
		variable temp_key : key_type;
	begin
		if rising_edge(clk) then
			if skein_round_state < FOLD_RATIO-1 then
				skein_round_state <= skein_round_state + 1;
			else
				skein_round_state <= 0;
			end if;
			
			if skein_round_state = 0 then
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
			elsif skein_round_state = FOLD_RATIO/2 then
				-- start skein block 3
				skein_data_in <= skein_data_out;
				temp_message := message2;
				temp_message(10) := skein_data_out.nonce;
				skein_data_in.state <= f_State_XOR(skein_data_out.state, temp_message);  -- used to generate the next key inside the skein block
				read_ack_i <= '0';
				if skein_data_out.nonce = x"00000004ECF83A53" then
					temp_key := f_make_key(f_State_XOR(skein_data_out.state, message2));
					report "skein round 2 output: " & to_hstring(skein_data_out.state(0)) & " next key " & to_hstring(temp_key(0));
				end if;
			else
				-- recycle
				skein_data_in <= skein_data_out;
				read_ack_i <= '0';
				--if skein_data_out.nonce = x"00000004ECF83A53" then
					--report "mid state: " & to_hstring(skein_data_out.state(0));
				--end if;
			end if;
			
		
		end if;
	end process;
	
	result <= result_i;
	read_ack <= read_ack_i;  
	nonce_out <= nonce_out_i;
	
end rtl;
		
