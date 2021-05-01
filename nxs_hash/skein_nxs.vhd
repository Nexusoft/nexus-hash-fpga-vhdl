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
		reset		: in std_logic;
		key2		: in key_type;
		message2	: in state_type;
		read_ack	: out std_logic; -- acknowledge read of input data
		result		: out std_logic_vector(1023 downto 0); -- skein hash result as a std_logic_vector
		result_valid: out std_logic
	);
	end skein_nxs;

architecture rtl of skein_nxs is

	
	constant TF_PIPELINE_STAGES : integer := 101;  -- 80 threefish rounds plus 21 subkey adds
	constant SKEIN_TOTAL_STAGES : integer := TF_PIPELINE_STAGES * 2;
	type state_array_type is array (0 to TF_PIPELINE_STAGES - 1) of state_type;  -- threefish state pipeline
	type key_array_type is array (0 to TF_PIPELINE_STAGES - 1) of key_type; -- key pipeline
	type valid_array_type is array(0 to TF_PIPELINE_STAGES - 1) of std_logic;  -- valid bit
	type word_array_type is array(0 to TF_PIPELINE_STAGES - 1) of unsigned(63 downto 0);  -- used to pipeline the nonce portion of the message
	type tweak_array_type is array(0 to TF_PIPELINE_STAGES - 1) of tweak_type;
	type skein_state_type is (ROUND2, ROUND3);
	
	-- the pipes are a superset of the required pieces for each round
	
	-- pipes
	signal state_pipe	: state_array_type := (others =>(others =>(others => '0')));
	signal nonce_pipe	: word_array_type := (others =>(others => '0'));
	signal key_pipe		: key_array_type := (others =>(others =>(others => '0')));
	signal tweak_pipe	: tweak_array_type := (others =>(others =>(others => '0')));
	
	--signal key : key_type := (others =>(others => '0'));
	--signal tweak : tweak_type := T2;
	signal result_valid_i : std_logic := '0';
	signal result_i : std_logic_vector(1023 downto 0) := (others => '0');
	signal valid_counter : integer range 0 to SKEIN_TOTAL_STAGES := 0;
	signal pipeline_counter : integer range 0 to TF_PIPELINE_STAGES := 0;
	signal skein_state : skein_state_type := ROUND2;
	
	function f_State_to_SLV ( state : in state_type) return std_logic_vector is
		-- convert an array of words to 1024 bit std_logic_vector
		variable state_SLV : std_logic_vector(1023 downto 0);
		begin
			for ii in 0 to 15 loop
				for jj in 0 to 7 loop
					-- reverse the word order and byte order
					state_SLV(64*ii+8*jj+7 downto 64*ii+8*jj) := std_logic_vector(state(15-ii)(8*((7-jj)+1)-1 downto 8*(7-jj)));
				end loop;
			end loop;
		return state_SLV;
		end function f_State_to_SLV;
		
	function f_SLV_to_State ( state_slv : in std_logic_vector(1023 downto 0)) return state_type is
		-- convert a 1024 bit std_logic_vector to an array of words
		variable state : state_type;
		begin
			for ii in 0 to 15 loop
				for jj in 0 to 7 loop
					-- reverse the word order and byte order
					state(15-ii)(8*((7-jj)+1)-1 downto 8*(7-jj)) := unsigned(state_SLV(64*ii+8*jj+7 downto 64*ii+8*jj));
				end loop;
			end loop;
		return state;
		end function f_SLV_to_State;
	
begin
	process(clk)
		variable temp_state : state_type;
		variable temp_message, last_message : state_type;
		variable temp_key : key_type;
		variable stage : integer range 0 to 100;
		variable round : integer range 0 to 80;
	begin
		if rising_edge(clk) then
			if reset = '1' then
				valid_counter <= 0;
				result_valid_i <= '0';
				skein_state <= ROUND2;
				pipeline_counter <= 0;
			else
				-- keep track of where we are in the pipeline
				if pipeline_counter < TF_PIPELINE_STAGES - 1 then
					pipeline_counter <= pipeline_counter + 1;
				else
					pipeline_counter <= 0;
					-- swap states every time we go through one full round.
					if skein_state = ROUND2 then
						skein_state <= ROUND3;
					else
						skein_state <= ROUND2;
					end if;
				end if;
				
				-- the first round output is invalid as we fill the pipeline
				if valid_counter < SKEIN_TOTAL_STAGES then
					valid_counter <= valid_counter + 1;
				else
					if skein_state = ROUND2 then
						-- output results are valid when we are reading in new data in round 2
						result_valid_i <= '1';
					else 
						result_valid_i <= '0';
					end if;
				end if;
			end if;
						
			case skein_state is
				when ROUND2 => 
					tweak_pipe(0) <= T2;
					key_pipe(0) <= key2;
					state_pipe(0) <= f_State_Add(message2, f_Get_Subkey(0, T2, key2));	
					
				when ROUND3 =>
					tweak_pipe(0) <= T3;
					-- xor the result of round 2 with the message
					temp_message := message2;
					temp_message(10) := nonce_pipe(100);
					temp_state := f_State_XOR(state_pipe(100), temp_message);
					temp_key := f_make_key(temp_state);
					key_pipe(0) <= temp_key;
					-- generate the first subkey.  The message is zero so the subkey is the state
					state_pipe(0) <= f_Get_Subkey(0, T3, temp_key);
					
				when others =>
			end case;
			-- for all cases
			nonce_pipe(0) <= message2(10);
			
			-- threefish rounds 0 through 3 (pipeline stages 1 through 4)
			for jj in 1 to 4 loop
				state_pipe(jj) <= f_threefish_1(state_pipe(jj-1), jj-1);
				key_pipe(jj) <= key_pipe(jj-1);
				nonce_pipe(jj) <= nonce_pipe(jj-1);
				tweak_pipe(jj) <= tweak_pipe(jj-1);
			end loop;

			-- threefish rounds 4 through 79 (pipeline stages 5 through 99)
			for ii in 1 to 19 loop
				stage := ii*5; -- stages step by 5
				round := ii*4; -- rounds step by 4
				-- add the next subkey
				state_pipe(stage) <= f_State_Add(state_pipe(stage-1), f_Get_Subkey(ii, tweak_pipe(stage-1), key_pipe(stage-1)));
				-- pipeline
				key_pipe(stage) <= key_pipe(stage-1);
				nonce_pipe(stage) <= nonce_pipe(stage-1);
				tweak_pipe(stage) <= tweak_pipe(stage-1);
				-- four rounds of threefish per pipeline stage
				for jj in 1 to 4 loop
					state_pipe(stage+jj) <= f_threefish_1(state_pipe(stage+jj-1), round + jj - 1);
					-- pipeline
					key_pipe(stage+jj) <= key_pipe(stage+jj-1);
					nonce_pipe(stage+jj) <= nonce_pipe(stage+jj-1);
					tweak_pipe(stage+jj) <= tweak_pipe(stage+jj-1);
				end loop;
			end loop;
			-- add the last subkey
			state_pipe(100) <= f_State_Add(state_pipe(99), f_Get_Subkey(20, tweak_pipe(99), key_pipe(99)));
			nonce_pipe(100) <= nonce_pipe(99);
			--key_pipe(100) <= key_pipe(99);
			result_i <= f_State_to_SLV(state_pipe(100));
		end if;
	end process;
	
	result_valid <= result_valid_i;
	result <= result_i;
	-- we consume new data during round 2 not during round 3
	read_ack <= '1' when skein_state = ROUND2 else '0';
	
end rtl;
		
