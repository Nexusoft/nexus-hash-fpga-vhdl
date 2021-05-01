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
		bits_target	: in unsigned (31 downto 0); -- The "bits" field determines the difficulty 
		nonce		: out unsigned(63 downto 0); -- current nonce or nonce that solves the hash when found = 1
		found		: out std_logic;
		found_counter	: out unsigned(31 downto 0);
		activity_counter		: out unsigned(31 downto 0) -- counter of hashes with 28 leading zeros for debug
	);
	end nxs_hash;

architecture rtl of nxs_hash is
	
	constant HASH_RESULT_BIT_WIDTH : integer := 448;
	signal hash_reset : std_logic := '1';
	signal nonce_found : std_logic := '0';
	signal nonce_ctr : unsigned(63 downto 0) := (others => '0');
	signal nonce_out : unsigned(63 downto 0) := (others => '0');  
	signal nonce_found_out : unsigned(63 downto 0) := (others => '0');  
	signal hash_upper : unsigned(HASH_RESULT_BIT_WIDTH-1 downto 0);
	signal sk_result_valid : std_logic;
	--signal invalid_count	: unsigned(15 downto 0) := (others => '0'); -- count the clocks to get to a valid result.
	signal message_to_check : state_type := (others => (others => '0'));
	signal difficulty_target_upper : unsigned (HASH_RESULT_BIT_WIDTH-1 downto 0) := (others => '0');
	signal diff28_count : unsigned (31 downto 0) :=  (others => '0');
	signal found_count : unsigned(31 downto 0) := (others => '0');
	signal hash_upper28 : unsigned (27 downto 0);
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
	activity_counter <= diff28_count;
	found_counter <= found_count;
	-- for debug get the upper 28 bits of the hash
	hash_upper28 <= hash_upper(HASH_RESULT_BIT_WIDTH - 1 downto HASH_RESULT_BIT_WIDTH - 28);

	
	process(clk)
		variable bits_1 : unsigned(15 downto 0);  -- exponent byte from the bits field
		variable bits_2 : unsigned(23 downto 0);  -- main part of the bits field
		variable temp_diff : unsigned(1023 downto 0);
		
		begin
			if rising_edge(clk) then
				case state is
					when IDLE => 
						--invalid_count <= (others => '0');
						diff28_count <= (others => '0');
						found_count <= (others => '0');
						if reset = '0' then
							state <= PARSE;
						else
							nonce_found <= '0';
							hash_reset <= '1';
							nonce_out <= (others => '0');  
							--nonce_found_out <= (others => '0');
						end if;
					when PARSE =>
						if reset = '0' then
							-- convert the bits field to difficulty
							-- bits_1 := 8*(message2(9)(63 downto 56) - 3); -- exponent
							-- bits_2 := message2(9)(55 downto 32);  -- mantissa
							bits_1 := 8*(bits_target(31 downto 24) - 3); -- exponent
							bits_2 := bits_target(23 downto 0);  -- mantissa

							temp_diff := (others => '0');
							temp_diff(23 downto 0) := bits_2;
							temp_diff := shift_left(temp_diff, to_integer(bits_1)); --TODO Check this!!
							difficulty_target_upper(HASH_RESULT_BIT_WIDTH-1 downto 0) <= temp_diff(1023 downto 1023-(HASH_RESULT_BIT_WIDTH-1));
							-- get the starting nonce from the header
							nonce_ctr <= message2(10);  --initialize the input nonce with the value in the message.
							nonce_out <= message2(10) - 1;  -- initialize the output nonce counter
							message_to_check <= message2;
							state <= HASHING;
							hash_reset <= '0';  -- take the hash block out of reset
						else
							state <= IDLE;
							hash_reset <= '1';
						end if;
					when HASHING =>
						if reset = '0' then
							--nonce_found <= '0';
							-- if the hash is valid and is less than the difficulty then we found a solution.
							if sk_result_valid = '1' then
								if hash_upper < difficulty_target_upper then
									nonce_found <= '1';
									nonce_found_out <= nonce_out + 1;
									found_count <= found_count + 1;
									--state <= IDLE;
									--hash_reset <= '1';
								end if;
								-- count hashes with 28 leading zeros or better.  This should increment about 1 in every 134 million hashes on average.
								if hash_upper28 = 0 then
									diff28_count <= diff28_count + 1;
								end if;
								-- when a valid answer comes out the other end, increment the output nonce.  This is the nonce adjusted for the pipeline length
								nonce_out <= nonce_out + 1;
								
							--else
								-- keep track of the number of invalid results to adjust for the pipeline length.
								--invalid_count <= invalid_count + 1;
							end if;
							--nonce_out <= nonce_ctr - invalid_count;  -- adjust the nonce for the pipeline length
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
		
