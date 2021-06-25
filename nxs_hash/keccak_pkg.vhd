-------------------------------------------------------------------------------
-- Andrew Hatstat
-- Aperture Mining, LLC
-------------------------------------------------------------------------------
-- package file for keccak specific types

-------------------------------------------------------------------------------  
-- Libraries
-------------------------------------------------------------------------------  
Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package keccak_pkg is
	constant num_plane : integer := 5;
    constant num_sheet : integer := 5;
    constant logD : integer := 4;

	subtype k_lane 		is std_logic_vector(63 downto 0);
    type k_plane        is array ((num_sheet-1) downto 0)  of k_lane;    
    type k_state        is array ((num_plane-1) downto 0)  of k_plane;  
	
	type k_message		is array (0 to 8) of k_lane;
	
	-- Nexus specific suffix words
	constant NXS_SUFFIX_1 : k_lane := x"0000000000000005";
	constant NXS_SUFFIX_2 : k_lane := x"8000000000000000";
	
	type round_constant_type is ARRAY(0 to 23) of std_logic_vector(63 downto 0);
	constant ROUND_CONST :round_constant_type := 
							   (X"0000000000000001", X"0000000000008082", X"800000000000808A", X"8000000080008000",
								X"000000000000808B", X"0000000080000001", X"8000000080008081", X"8000000000008009",
								X"000000000000008A", X"0000000000000088", X"0000000080008009", X"000000008000000A",
								X"000000008000808B", X"800000000000008B", X"8000000000008089", X"8000000000008003",
								X"8000000000008002", X"8000000000000080", X"000000000000800A", X"800000008000000A",
								X"8000000080008081", X"8000000000008080", X"0000000080000001", X"8000000080008008" );
	
	type keccak_pipe_status_type is (JUNK, KECCAK_IN_PROCESS, KECCAK_DONE);
	subtype keccak_round_type is integer range 0 to 24;
	
	-- record (like a struct in c) used to combine data for easier pipelining
	type keccak_pipe_type is record
		state  	: k_state;
		nonce	: unsigned(63 downto 0);
		status	: keccak_pipe_status_type;
		round   : keccak_round_type;
	end record keccak_pipe_type;
	
	constant keccak_pipe_init : keccak_pipe_type := (state => (others =>(others =>(others => '0'))),
											  nonce => (others=> '0'),
											  status => JUNK,
											  round => 0
											  );
											 

	
	
	-- function prototypes
	
	procedure SLV_to_message ( k_state_slv : in std_logic_vector(1023 downto 0); k_message_1 : out k_message; k_message_2 : out k_message);
	function f_state_to_output (ks : in k_state) return unsigned;
	function f_message_to_state (msg : in k_message) return k_state;
	function f_State_XOR ( s : in k_state; m : in k_message) return k_state;
	function f_round_constant (round_num : in integer range 0 to 23) return std_logic_vector;
	function f_keccak_round(round_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;
	function f_keccak_theta(k_state_in : in k_state) return k_state;
	function f_keccak_rho(k_state_in : in k_state) return k_state;
	function f_keccak_pi(k_state_in : in k_state) return k_state;
	function f_keccak_chi(k_state_in : in k_state) return k_state;
	function f_keccak_iota(k_state_in : in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;
	function f_keccak_A(k_state_in: in k_state) return k_state;
	function f_keccak_B(k_state_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;

end package keccak_pkg;

package body keccak_pkg is

	procedure SLV_to_message ( k_state_slv : in std_logic_vector(1023 downto 0);
								 k_message_1 : out k_message;
								 k_message_2 : out k_message) is
		-- convert a 1024 bit std_logic_vector into two array of words for absorbtion
		variable m1, m2 : k_message;
		begin
			-- first set of words
			for ii in 0 to 8 loop -- words
				for jj in 0 to 7 loop  -- bytes
					m1(ii)(8*jj+7 downto 8*jj) := k_state_slv(1023 - (64*ii+8*jj) downto 1023 - (64*ii+8*jj) - 7);
				end loop;
			end loop;
			-- second set of words
			for ii in 0 to 6 loop
				for jj in 0 to 7 loop
					m2(ii)(8*jj+7 downto 8*jj) := k_state_slv(1023 - 576 - (64*ii+8*jj) downto 1023 - 576 - (64*ii+8*jj) - 7);
				end loop;
			end loop;
			m2(7)(63 downto 0) := NXS_SUFFIX_1;
			m2(8)(63 downto 0) := NXS_SUFFIX_2;
			k_message_1 := m1;
			k_message_2 := m2;
			
			-- the last two words are the constant suffix
	end procedure SLV_to_message;
	
	function f_state_to_output (ks : in k_state) return unsigned is
		variable output : unsigned (31 downto 0); --(447 downto 0);
		begin
			--for ii in 0 to 6 loop
				--output(64*ii + 63 downto 64*ii) := unsigned(ks(ii / 5)(ii mod 5));
			--end loop
			output := unsigned(ks(1)(1)(63 downto 32));
			
			return output;
	end function f_state_to_output;
	
		
	function f_message_to_state (msg : in k_message) return k_state is
		variable temp_state : k_state;
		begin
			temp_state := (others => (others => (others => '0')));
			for ii in 0 to 8 loop
				temp_state(ii / 5)(ii mod 5) := msg(ii);
			end loop;
			return temp_state;
			
	end function f_message_to_state;

	function f_State_XOR ( s : in k_state; m : in k_message) return k_state is
		-- XOR a message with a state array
		variable temp_state : k_state;
		begin
			temp_state := s;
			for ii in 0 to 8 loop
				temp_state(ii / 5)(ii mod 5) := s(ii / 5)(ii mod 5) XOR m(ii);
			end loop;
		return temp_state;
	end function f_State_XOR;


	function f_round_constant (round_num : in integer range 0 to 23) return std_logic_vector is
		variable temp : std_logic_vector(63 downto 0);
		begin
			temp := ROUND_CONST(round_num);
		return temp;
	end function f_round_constant;
		
	function f_keccak_round(round_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state is
		begin
			return f_keccak_iota(f_keccak_chi(f_keccak_pi(f_keccak_rho(f_keccak_theta(round_in)))),round_constant);
		end function f_keccak_round;
		
	-- split keccak into a few pieces for pipelining
	function f_keccak_A(k_state_in: in k_state) return k_state is
		begin
			return f_keccak_rho(f_keccak_theta(k_state_in));
		end function f_keccak_A;
		
	function f_keccak_B(k_state_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state is
		begin
			return f_keccak_iota(f_keccak_chi(f_keccak_pi(k_state_in)),round_constant);
		end function f_keccak_B;
	
	
	function f_keccak_theta(k_state_in : in k_state) return k_state is
		variable theta_in, theta_out : k_state;
		variable sum_sheet: k_plane;
		begin
			theta_in:=k_state_in;
			
			--compute sum of columns
			for x in 0 to 4 loop
				for i in 0 to 63 loop
					sum_sheet(x)(i):=theta_in(0)(x)(i) xor theta_in(1)(x)(i) xor theta_in(2)(x)(i) xor theta_in(3)(x)(i) xor theta_in(4)(x)(i);
				end loop;	
			end loop;

			for y in 0 to 4 loop
				for x in 1 to 3 loop
					theta_out(y)(x)(0):=theta_in(y)(x)(0) xor sum_sheet(x-1)(0) xor sum_sheet(x+1)(63);
					for i in 1 to 63 loop
						theta_out(y)(x)(i):=theta_in(y)(x)(i) xor sum_sheet(x-1)(i) xor sum_sheet(x+1)(i-1);
					end loop;	
				end loop;
			end loop;

			for y in 0 to 4 loop
				theta_out(y)(0)(0):=theta_in(y)(0)(0) xor sum_sheet(4)(0) xor sum_sheet(1)(63);
				for i in 1 to 63 loop
					theta_out(y)(0)(i):=theta_in(y)(0)(i) xor sum_sheet(4)(i) xor sum_sheet(1)(i-1);
				end loop;	
			end loop;

			for y in 0 to 4 loop
				theta_out(y)(4)(0):=theta_in(y)(4)(0) xor sum_sheet(3)(0) xor sum_sheet(0)(63);
				for i in 1 to 63 loop
					theta_out(y)(4)(i):=theta_in(y)(4)(i) xor sum_sheet(3)(i) xor sum_sheet(0)(i-1);
				end loop;	
			end loop;
				
			return theta_out;
		end function f_keccak_theta;
		
	function f_keccak_rho(k_state_in : in k_state) return k_state is
		variable rho_in, rho_out : k_state;
		begin
			rho_in:=k_state_in;	
			
			for i in 0 to 63 loop
				rho_out(0)(0)(i):=rho_in(0)(0)(i);
			end loop;	
			for i in 0 to 63 loop
				rho_out(0)(1)(i):=rho_in(0)(1)((i-1)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(0)(2)(i):=rho_in(0)(2)((i-62)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(0)(3)(i):=rho_in(0)(3)((i-28)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(0)(4)(i):=rho_in(0)(4)((i-27)mod 64);
			end loop;

			for i in 0 to 63 loop
				rho_out(1)(0)(i):=rho_in(1)(0)((i-36)mod 64);
			end loop;	
			for i in 0 to 63 loop
				rho_out(1)(1)(i):=rho_in(1)(1)((i-44)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(1)(2)(i):=rho_in(1)(2)((i-6)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(1)(3)(i):=rho_in(1)(3)((i-55)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(1)(4)(i):=rho_in(1)(4)((i-20)mod 64);
			end loop;

			for i in 0 to 63 loop
				rho_out(2)(0)(i):=rho_in(2)(0)((i-3)mod 64);
			end loop;	
			for i in 0 to 63 loop
				rho_out(2)(1)(i):=rho_in(2)(1)((i-10)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(2)(2)(i):=rho_in(2)(2)((i-43)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(2)(3)(i):=rho_in(2)(3)((i-25)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(2)(4)(i):=rho_in(2)(4)((i-39)mod 64);
			end loop;

			for i in 0 to 63 loop
				rho_out(3)(0)(i):=rho_in(3)(0)((i-41)mod 64);
			end loop;	
			for i in 0 to 63 loop
				rho_out(3)(1)(i):=rho_in(3)(1)((i-45)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(3)(2)(i):=rho_in(3)(2)((i-15)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(3)(3)(i):=rho_in(3)(3)((i-21)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(3)(4)(i):=rho_in(3)(4)((i-8)mod 64);
			end loop;

			for i in 0 to 63 loop
				rho_out(4)(0)(i):=rho_in(4)(0)((i-18)mod 64);
			end loop;	
			for i in 0 to 63 loop
				rho_out(4)(1)(i):=rho_in(4)(1)((i-2)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(4)(2)(i):=rho_in(4)(2)((i-61)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(4)(3)(i):=rho_in(4)(3)((i-56)mod 64);
			end loop;
			for i in 0 to 63 loop
				rho_out(4)(4)(i):=rho_in(4)(4)((i-14)mod 64);
			end loop;
			
			return rho_out;
		end function f_keccak_rho;
		
	function f_keccak_pi(k_state_in : in k_state) return k_state is
		variable pi_in, pi_out : k_state;
		begin
			pi_in:=k_state_in;		
			for y in 0 to 4 loop
				for x in 0 to 4 loop
					for i in 0 to 63 loop
						pi_out((2*x+3*y) mod 5)(0*x+1*y)(i):=pi_in(y) (x)(i);
					end loop;	
				end loop;
			end loop;
			return pi_out;
		end function f_keccak_pi;
	
	function f_keccak_chi(k_state_in : in k_state) return k_state is
		variable chi_in, chi_out : k_state;
		begin
			chi_in:=k_state_in;
			
			for y in 0 to 4 loop
				for x in 0 to 2 loop
					for i in 0 to 63 loop
						chi_out(y)(x)(i):=chi_in(y)(x)(i) xor  ( not(chi_in (y)(x+1)(i))and chi_in (y)(x+2)(i));
					end loop;	
				end loop;
			end loop;

			for y in 0 to 4 loop
				for i in 0 to 63 loop
					chi_out(y)(3)(i):=chi_in(y)(3)(i) xor  ( not(chi_in (y)(4)(i))and chi_in (y)(0)(i));            
				end loop;	
			end loop;
				
			for y in 0 to 4 loop
				for i in 0 to 63 loop
					chi_out(y)(4)(i):=chi_in(y)(4)(i) xor  ( not(chi_in (y)(0)(i))and chi_in (y)(1)(i));        
				end loop;	
			end loop;
			return chi_out;
		end function f_keccak_chi;
	
	function f_keccak_iota(k_state_in : in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state is
		variable iota_in, iota_out : k_state;
		begin
			iota_in:=k_state_in;
			for y in 1 to 4 loop
				for x in 0 to 4 loop
					for i in 0 to 63 loop
						iota_out(y)(x)(i):=iota_in(y)(x)(i);
					end loop;	
				end loop;
			end loop;
			
			for x in 1 to 4 loop
				for i in 0 to 63 loop
					iota_out(0)(x)(i):=iota_in(0)(x)(i);
				end loop;	
			end loop;

			for i in 0 to 63 loop
				iota_out(0)(0)(i):=iota_in(0)(0)(i) xor round_constant(i);
			end loop;	
			return iota_out;
		end function f_keccak_iota;
	
			
end package body keccak_pkg;
