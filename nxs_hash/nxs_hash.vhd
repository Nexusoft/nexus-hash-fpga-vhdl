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
		found		: out std_logic
	);
	end nxs_hash;

architecture rtl of nxs_hash is
	
	constant HASH_RESULT_BIT_WIDTH : integer := 32;
	signal nonce_found, nonce_found_slow : std_logic := '0';
	signal hash_nonce_out : unsigned(63 downto 0) := (others => '0');  
	signal nonce_found_out, nonce_out_slow : unsigned(63 downto 0) := (others => '0');  
	signal hash_upper : unsigned(HASH_RESULT_BIT_WIDTH-1 downto 0);
	--signal sk_result_valid : std_logic;
	signal message_to_check : state_type := (others => (others => '0'));
	signal hash_upper32 : unsigned (31 downto 0);
	type state_machine_type is (IDLE, PARSE, HASHING);
	signal state : state_machine_type := IDLE;
	signal skein_read_ack : std_logic;
	
	constant fast_to_slow_clock_ratio : integer := 8;
	signal stretch_counter, post_reset_counter : integer range 0 to fast_to_slow_clock_ratio * 2 - 1 := 0;

	
begin
 

	sk1024: entity work.sk1024
	port map
	(
		clk => clk,
		key2 => key2,
		message2 => message_to_check,
		read_ack => skein_read_ack,
		result => hash_upper,
		--result_valid => sk_result_valid,
		nonce_out => hash_nonce_out
	);
	
	nonce <= nonce_out_slow;
	found <= nonce_found_slow;
	hash_upper32 <= hash_upper(HASH_RESULT_BIT_WIDTH - 1 downto HASH_RESULT_BIT_WIDTH - 32);  -- gross difficulty check to see if the leading 32 bits are zeros.  Further filtering must be done by the software.

	process(clk)
		begin
			if rising_edge(clk) then
				if reset = '1' then
					nonce_found <= '0';
					state <= IDLE;	
					post_reset_counter <= 0;				
				else
					case state is
						when IDLE => 
							-- give enough clocks for the message data to settle before registering it with the fast hash clock
							if post_reset_counter < fast_to_slow_clock_ratio - 1 then
								post_reset_counter <= post_reset_counter + 1;
							else
								state <= PARSE; 
							end if;
						when PARSE =>
							message_to_check <= message2;
							state <= HASHING;
							report "starting nonce " & to_hstring(message2(10));
						when HASHING =>
							-- if the hash is less than the difficulty then we found a solution.
							if hash_upper32 = 0 then  -- check if the leading 32 bits of the hash are all zeros.
								nonce_found <= '1';
								nonce_found_out <= hash_nonce_out;
								report "nxs_hash found a nonce " & to_hstring(hash_nonce_out);
							else
								nonce_found <= '0';
							end if;
							if skein_read_ack = '1' then			-- increment the nonce only if it has been consumed
								message_to_check(10) <= message_to_check(10) + 1;  -- increment the nonce portion of the message
							end if;
					end case;
				end if;
			end if;
		end process;
		
		
	-- fast to slow data transition.  We assume a found nonce happens infrequently and therefore the nonce signal can be stretched out to be read by the slow clock. 
	process(clk)	
	begin
		if rising_edge(clk) then
			if stretch_counter < fast_to_slow_clock_ratio * 2 - 1 then
				stretch_counter <= stretch_counter + 1;
			else
				if nonce_found = '1' then
					nonce_found_slow <= '1';
					nonce_out_slow <= nonce_found_out;
					stretch_counter <= 0;
				else
					nonce_found_slow <= '0';
				end if;
			end if;
		end if;
	end process;
	
end rtl;
		
