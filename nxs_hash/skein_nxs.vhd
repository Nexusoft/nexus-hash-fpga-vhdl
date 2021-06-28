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
	signal skein_in_i, skein_block_in, skein_block_out, skein_make_key_in, skein_make_key_out : skein_pipe_type := skein_pipe_init;
	constant input_governor_count : integer := 2;
	signal input_governor : integer range 0 to input_governor_count - 1 := 0;
	signal nonce_out_i : unsigned (63 downto 0) := (others => '0');
	
	
begin
	
	skein_block_1: entity work.skein_block
	port map
	(
		clk => clk,
		skein_in => skein_block_in,
		skein_out => skein_block_out,
		latency => latency
	);
	
	skein_make_key: entity work.skein_make_key
	port map
	(
		clk => clk,
		skein_in => skein_make_key_in,
		skein_out => skein_make_key_out,
		message_in => message2
	);

	
	process(clk)
		variable temp_state : state_type;
		variable temp_message : state_type;
		variable temp_key : key_type;
	begin
		if rising_edge(clk) then
			-- if reset = '1' then
				-- skein_in_i <= skein_pipe_init;
				-- skein_block_in <= skein_pipe_init;
				-- skein_make_key_in <= skein_pipe_init;
				-- result_valid_i <= '0';
				-- result_i <= (others => '0');
				-- input_governor <= 0;
				-- read_ack_i <= '0';
				-- nonce_out_i <= (others => '0');
			-- else
				skein_block_in <= skein_make_key_out;
				--if skein_block_in.nonce = x"00000004ECF83A53" then
				--	report "skein_block_in status: " & to_string(skein_block_in.status) & " state: " & to_hstring(skein_block_in.state(0)) & " key: " & to_hstring(skein_block_in.key(0));
				--end if;
				if skein_block_out.nonce = x"00000004ECF83A53" and skein_block_out.status /= JUNK then
					report "skein_block_out status: " & to_string(skein_block_out.status) & " state: " & to_hstring(skein_block_out.state(0));
				end if;
				case skein_block_out.status is
					when A_DONE => 
						skein_in_i <= skein_block_out;
						skein_make_key_in <= skein_in_i;
						input_governor <= 0;
						read_ack_i <= '0';
					when others =>
						skein_in_i.state <= message2;
						skein_in_i.key <= key2;
						skein_in_i.nonce <= message2(10);
						if input_governor <  input_governor_count - 1 then  -- limit input to skein to minimize WIP
							skein_in_i.status <= NOT_STARTED;
							input_governor <= input_governor + 1;
							read_ack_i <= '1';
						else
							skein_in_i.status <= JUNK;
							input_governor <= 0;
							read_ack_i <= '0';
						end if;
						skein_make_key_in <= skein_in_i;
				end case;
				--report "skein_in_i: " & to_string(skein_in_i.status) & " nonce: " & to_hstring(skein_in_i.nonce);
				--result_valid_i <= '1' when skein_block_out.status = B_DONE else '0';
				result_i <= f_State_to_SLV(skein_block_out.state);
				nonce_out_i <= skein_block_out.nonce;
			--end if;
		end if;
	end process;
	
	--result_valid <= result_valid_i;
	result <= result_i;
	read_ack <= read_ack_i;  -- '1' when (skein_block_out.status /= A_DONE) and (reset = '0') and (input_governor <  input_governor_count - 1) else '0';
	nonce_out <= nonce_out_i;
	
end rtl;
		
