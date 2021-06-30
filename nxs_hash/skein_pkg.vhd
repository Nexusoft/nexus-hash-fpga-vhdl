-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- package file for skein specific types

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package skein_pkg is
	constant C240 : unsigned := x"5555555555555555";  -- special constant used in key generation
	
	type R_type is ARRAY(0 to 7, 0 to 7) of integer range 0 to 63;  -- rotation array type
	
	constant R : R_type := ((55, 43, 37, 40, 16, 22, 38, 12), (25, 25, 46, 13, 14, 13, 52, 57),
                  (33, 8, 18, 57, 21, 12, 32, 54), (34, 43, 25, 60, 44, 9, 59, 34),
                  (28, 7, 47, 48, 51, 9, 35, 41), (17, 6, 18, 25, 43, 42, 40, 15),
                  (58, 7, 32, 45, 19, 18, 2, 56), (47, 49, 27, 58, 37, 48, 53, 56));
				  
	type permute_indices_type is ARRAY(0 to 15) of integer range 0 to 15;  -- permute indices type
	--constant PERMUTE_INDICIES : permute_indices_type := (0, 9, 2, 13, 6, 11, 4, 15, 10, 7, 12, 3, 14, 5, 8, 1); -- permute map - position 15 maps to 1
	constant PERMUTE_INDICIES : permute_indices_type := (0, 15, 2, 11, 6, 13, 4, 9, 14, 1, 8, 5, 10, 3, 12, 7); -- reversed - position 1 receives result at index 15
	
	type tweak_type is ARRAY(0 to 2) of unsigned(63 downto 0);
	type state_type is array (0 to 15) of unsigned(63 downto 0);  -- 1024 bits represented as an array of 16 64 bit unsigned integers
	type key_type is array(0 to 16) of unsigned(63 downto 0);  -- the key has one extra word at the end
	
	-- Precalculated tweaks.  
	constant T2 : tweak_type := (x"00000000000000D8", x"B000000000000000", x"B0000000000000D8");
	constant T3 : tweak_type := (x"0000000000000008", x"FF00000000000000", x"FF00000000000008");
	
	type skein_pipe_status_type is (JUNK, NOT_STARTED, A_IN_PROCESS, A_DONE, B_IN_PROCESS, B_DONE);

	constant FOLD_RATIO : integer := 2;  -- number of times we repeat each skein round due to folding.  only 2 works. 
	constant FOLD_RATIO_NUM_BITS : integer := FOLD_RATIO/2; -- number of bits required to represent the fold count in unsigned number
	constant SKEIN_ROUNDS_PER_BLOCK : integer := 10;  -- 80 threefishes divided by 8 threefishes per round
	
	-- record (like a struct in c) used to combine data for easier pipelining
	type skein_pipe_type is record
		state  	: state_type;
		--tweak   : tweak_type;
		key  	: key_type;
		nonce	: unsigned(63 downto 0);
		--status	: skein_pipe_status_type;
		--loop_count : unsigned(FOLD_RATIO_NUM_BITS-1 downto 0);
	end record skein_pipe_type;
	
	constant skein_pipe_init : skein_pipe_type := (state => (others =>(others => '0')),
                                              --tweak => T2,
                                              key => (others =>(others => '0')),
											  nonce => (others=> '0')
											  --status => JUNK,
											  --loop_count => (others => '0')
											  );

	
	-- function prototypes
	function f_State_to_SLV ( state : in state_type) return std_logic_vector;
	function f_Get_Subkey (	subkey_round : in integer range 0 to 20; tweak : in tweak_type; key : in key_type)
							return state_type;
	function f_State_Add ( s1 : in state_type; s2 : in state_type) return state_type;
	function f_State_XOR ( s1 : in state_type; s2 : in state_type) return state_type;
	procedure mix (x0 : in unsigned(63 downto 0); 
				  x1 : in unsigned(63 downto 0); 
				  rotation : in integer range 0 to 63; 
				  y0 : out unsigned(63 downto 0);
				  y1 : out unsigned(63 downto 0));
	function f_threefish_1(state_in : in state_type; round : in integer range 0 to 79) return state_type;
	function f_make_key(key_in : in state_type ) return key_type;
	
end package skein_pkg;

package body skein_pkg is

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


	function f_Get_Subkey (	subkey_round : in integer range 0 to 20; tweak : in tweak_type; key : in key_type)
							return state_type is
		-- for a given subkey round, key, and tweak, return the 16 word subkey
		variable subkey : state_type;
		begin
			for ii in 0 to 15 loop
				subkey(ii) := key((subkey_round + ii) mod 17);
			end loop;  -- ii
			subkey(13) := subkey(13) + tweak(subkey_round mod 3);
			subkey(14) := subkey(14) + tweak((subkey_round + 1) mod 3);
			subkey(15) := subkey(15) + to_unsigned(subkey_round,64);
			return subkey;
		end function f_Get_Subkey;
		
	function f_State_Add ( s1 : in state_type; s2 : in state_type) return state_type is
		-- add two state arrays
		variable sum : state_type;
		begin
			for ii in 0 to 15 loop
				sum(ii) := s1(ii) + s2(ii);
			end loop;
		return sum;
		end function f_State_Add;
		
	function f_State_XOR ( s1 : in state_type; s2 : in state_type) return state_type is
		-- XOR two state arrays
		variable sum : state_type;
		begin
			for ii in 0 to 15 loop
				sum(ii) := s1(ii) XOR s2(ii);
			end loop;
		return sum;
		end function f_State_XOR;
		
	procedure mix (x0 : in unsigned(63 downto 0); 
				  x1 : in unsigned(63 downto 0); 
				  rotation : in integer range 0 to 63; 
				  y0 : out unsigned(63 downto 0);
				  y1 : out unsigned(63 downto 0)) is
		-- skein mix function
		variable sum : unsigned(63 downto 0);
		begin
			sum := x0 + x1;
			y0 := sum;
			y1 := rotate_left(x1,rotation) XOR sum;
		end procedure mix;
		
	function f_threefish_1(state_in : in state_type; round : in integer range 0 to 79) return state_type is
		-- one round of threefish
		variable result : state_type;
		variable iii : integer range 0 to 15;  -- alternate index allows us to increment by 2
		variable in1, in2, out1, out2 : unsigned(63 downto 0);
		begin
			for ii in 0 to 7 loop
				iii := ii*2;
				in1 := state_in(iii);
				in2 := state_in(iii+1);
				mix(in1, in2, R(round mod 8,ii), out1, out2);
				result(PERMUTE_INDICIES(iii)) := out1;
				result(PERMUTE_INDICIES(iii+1)) := out2;
			end loop;
			
			return result;
		end function f_threefish_1;
	
	function f_make_key(key_in : in state_type ) return key_type is
		-- generate the key including the special last key word given a 1024 bit starting key
		variable key_out : key_type;
		begin
			key_out(16) := C240;  
			for ii in 0 to 15 loop
				key_out(ii) := key_in(ii);
				key_out(16) := key_out(16) XOR key_in(ii);
			end loop;
			return key_out;
		end function f_make_key;


end package body skein_pkg;