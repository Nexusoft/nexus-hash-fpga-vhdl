-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- Hash channel miner for Nexus

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
entity nxs_hash is 
	
	port
	(
		clk			: in std_logic;
		reset		: in std_logic;
		key2		: in key_type;
		message2	: in state_type;
		nonce		: out unsigned(63 downto 0); -- current nonce or nonce that solves the hash when found = 1
		found		: out std_logic;
		found_counter	: out unsigned(31 downto 0)
	);
	end nxs_hash;

architecture rtl of nxs_hash is
	
	constant HASH_RESULT_BIT_WIDTH : integer := 32;
	signal hash_reset : std_logic := '1';
	signal nonce_found : std_logic := '0';
	signal nonce_ctr : unsigned(63 downto 0) := (others => '0');
	signal nonce_out : unsigned(63 downto 0) := (others => '0');  
	signal nonce_found_out : unsigned(63 downto 0) := (others => '0');  
	signal hash_upper : unsigned(HASH_RESULT_BIT_WIDTH-1 downto 0);
	signal sk_result_valid : std_logic;
	signal message_to_check : state_type := (others => (others => '0'));
	signal found_count : unsigned(31 downto 0) := (others => '0');
	signal hash_upper32 : unsigned (31 downto 0);
	type state_machine_type is (IDLE, PARSE, HASHING);
	signal state : state_machine_type := IDLE;
	signal skein_read_ack : std_logic;
	
begin
 

	sk1024: entity work.sk1024
	port map
	(
		clk => clk,
		reset => hash_reset,
		key2 => key2,
		message2 => message_to_check,
		read_ack => skein_read_ack,
		result => hash_upper,
		result_valid => sk_result_valid
	);
	
	nonce <= nonce_found_out;
	found <= nonce_found;
	found_counter <= found_count;
	hash_upper32 <= hash_upper(HASH_RESULT_BIT_WIDTH - 1 downto HASH_RESULT_BIT_WIDTH - 32);  -- gross difficulty check to see if the leading 32 bits are zeros.  Further filtering must be done by the software.

	process(clk)
		begin
			if rising_edge(clk) then
				case state is
					when IDLE => 
						found_count <= (others => '0');
						if reset = '0' then
							state <= PARSE;
						else
							nonce_found <= '0';
							hash_reset <= '1';
							nonce_out <= (others => '0');  
						end if;
					when PARSE =>
						if reset = '0' then
							-- get the starting nonce from the header
							nonce_ctr <= message2(10);  --initialize the input nonce with the value in the message.
							nonce_out <= message2(10) - 1;  -- initialize the output nonce counter
							message_to_check <= message2;
							state <= HASHING;
							hash_reset <= '0';  -- take the hash block out of reset
							-- report "starting nonce: " & to_hstring(message2(10));
						else
							state <= IDLE;
							hash_reset <= '1';
						end if;
					when HASHING =>
						if reset = '0' then
							-- if the hash is valid and is less than the difficulty then we found a solution.
							if sk_result_valid = '1' then
								if hash_upper32 = 0 then  -- check if the leading 32 bits of the hash are all zeros.
									nonce_found <= '1';
									nonce_found_out <= nonce_out + 1;
									found_count <= found_count + 1;
								end if;
								-- when a valid answer comes out the other end, increment the output nonce.  This is the nonce adjusted for the pipeline length
								nonce_out <= nonce_out + 1;
							end if;
							if skein_read_ack = '1' then			-- increment the nonce only if it has been consumed
								nonce_ctr <= nonce_ctr + 1;			-- increment the input nonce
								message_to_check(10) <= nonce_ctr + 1;  -- update the nonce portion of the message
							end if;
						else
							state <= IDLE;
							hash_reset <= '1';
						end if;
					when others =>
				end case;
			end if;
		end process;
	
end rtl;
		
