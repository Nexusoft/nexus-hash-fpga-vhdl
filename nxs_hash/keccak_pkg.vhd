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
    constant logD : integer :=4;
    -- constant N : integer := 64;

    --type k_lane        is  array (63 downto 0)  of std_logic;  
	subtype k_lane 		is std_logic_vector(63 downto 0);
    type k_plane        is array ((num_sheet-1) downto 0)  of k_lane;    
    type k_state        is array ((num_plane-1) downto 0)  of k_plane;  
	
	type k_message		is array (0 to 8) of k_lane;
	
	-- Nexus specific suffix words
	constant NXS_SUFFIX_1 : k_lane := x"0000000000000005";
	constant NXS_SUFFIX_2 : k_lane := x"8000000000000000";
	
	-- function prototypes
	function f_round_constant (round_num : in integer range 0 to 23) return std_logic_vector;
	function f_keccak_round(round_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;
	function f_keccak_theta(k_state_in : in k_state) return k_state;
	function f_keccak_rho(k_state_in : in k_state) return k_state;
	function f_keccak_pi(k_state_in : in k_state) return k_state;
	function f_keccak_chi(k_state_in : in k_state) return k_state;
	function f_keccak_iota(k_state_in : in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;
	--function f_keccak_round_monolith(round_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;
	function f_keccak_A(k_state_in: in k_state) return k_state;
	function f_keccak_B(k_state_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state;


	

end package keccak_pkg;

package body keccak_pkg is

	function f_round_constant (round_num : in integer range 0 to 23) return std_logic_vector is
		variable temp : std_logic_vector(63 downto 0);
		begin
			case round_num is
				when 0 => temp := X"0000000000000001" ;
				when 1 => temp := X"0000000000008082" ;
				when 2 => temp := X"800000000000808A" ;
				when 3 => temp := X"8000000080008000" ;
				when 4 => temp := X"000000000000808B" ;
				when 5 => temp := X"0000000080000001" ;
				when 6 => temp := X"8000000080008081" ;
				when 7 => temp := X"8000000000008009" ;
				when 8 => temp := X"000000000000008A" ;
				when 9 => temp := X"0000000000000088" ;
				when 10 => temp := X"0000000080008009" ;
				when 11 => temp := X"000000008000000A" ;
				when 12 => temp := X"000000008000808B" ;
				when 13 => temp := X"800000000000008B" ;
				when 14 => temp := X"8000000000008089" ;
				when 15 => temp := X"8000000000008003" ;
				when 16 => temp := X"8000000000008002" ;
				when 17 => temp := X"8000000000000080" ;
				when 18 => temp := X"000000000000800A" ;
				when 19 => temp := X"800000008000000A" ;
				when 20 => temp := X"8000000080008081" ;
				when 21 => temp := X"8000000000008080" ;
				when 22 => temp := X"0000000080000001" ;
				when 23 => temp := X"8000000080008008" ;	    	    
				when others => temp := (others => '0');
			end case;
			
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
	
	-- function f_keccak_round_monolith(round_in: in k_state; round_constant : in std_logic_vector(63 downto 0)) return k_state is
		-- variable theta_in,theta_out,pi_in,pi_out,rho_in,rho_out,chi_in,chi_out,iota_in,iota_out, round_out : k_state;
		-- variable sum_sheet: k_plane;
		-- begin
			-- --order theta, rho, pi, chi, iota
			-- theta_in:=round_in;
			
			-- --theta
			-- --compute sum of columns
			-- for x in 0 to 4 loop
				-- for i in 0 to 63 loop
					-- sum_sheet(x)(i):=theta_in(0)(x)(i) xor theta_in(1)(x)(i) xor theta_in(2)(x)(i) xor theta_in(3)(x)(i) xor theta_in(4)(x)(i);
				-- end loop;	
			-- end loop;

			-- for y in 0 to 4 loop
				-- for x in 1 to 3 loop
					-- theta_out(y)(x)(0):=theta_in(y)(x)(0) xor sum_sheet(x-1)(0) xor sum_sheet(x+1)(63);
					-- for i in 1 to 63 loop
						-- theta_out(y)(x)(i):=theta_in(y)(x)(i) xor sum_sheet(x-1)(i) xor sum_sheet(x+1)(i-1);
					-- end loop;	
				-- end loop;
			-- end loop;

			-- for y in 0 to 4 loop
				-- theta_out(y)(0)(0):=theta_in(y)(0)(0) xor sum_sheet(4)(0) xor sum_sheet(1)(63);
				-- for i in 1 to 63 loop
					-- theta_out(y)(0)(i):=theta_in(y)(0)(i) xor sum_sheet(4)(i) xor sum_sheet(1)(i-1);
				-- end loop;	
			-- end loop;

			-- for y in 0 to 4 loop
				-- theta_out(y)(4)(0):=theta_in(y)(4)(0) xor sum_sheet(3)(0) xor sum_sheet(0)(63);
				-- for i in 1 to 63 loop
					-- theta_out(y)(4)(i):=theta_in(y)(4)(i) xor sum_sheet(3)(i) xor sum_sheet(0)(i-1);
				-- end loop;	
			-- end loop;
			
			-- rho_in:=theta_out;
			
			-- --rho
			-- for i in 0 to 63 loop
				-- rho_out(0)(0)(i):=rho_in(0)(0)(i);
			-- end loop;	
			-- for i in 0 to 63 loop
				-- rho_out(0)(1)(i):=rho_in(0)(1)((i-1)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(0)(2)(i):=rho_in(0)(2)((i-62)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(0)(3)(i):=rho_in(0)(3)((i-28)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(0)(4)(i):=rho_in(0)(4)((i-27)mod 64);
			-- end loop;

			-- for i in 0 to 63 loop
				-- rho_out(1)(0)(i):=rho_in(1)(0)((i-36)mod 64);
			-- end loop;	
			-- for i in 0 to 63 loop
				-- rho_out(1)(1)(i):=rho_in(1)(1)((i-44)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(1)(2)(i):=rho_in(1)(2)((i-6)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(1)(3)(i):=rho_in(1)(3)((i-55)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(1)(4)(i):=rho_in(1)(4)((i-20)mod 64);
			-- end loop;

			-- for i in 0 to 63 loop
				-- rho_out(2)(0)(i):=rho_in(2)(0)((i-3)mod 64);
			-- end loop;	
			-- for i in 0 to 63 loop
				-- rho_out(2)(1)(i):=rho_in(2)(1)((i-10)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(2)(2)(i):=rho_in(2)(2)((i-43)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(2)(3)(i):=rho_in(2)(3)((i-25)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(2)(4)(i):=rho_in(2)(4)((i-39)mod 64);
			-- end loop;

			-- for i in 0 to 63 loop
				-- rho_out(3)(0)(i):=rho_in(3)(0)((i-41)mod 64);
			-- end loop;	
			-- for i in 0 to 63 loop
				-- rho_out(3)(1)(i):=rho_in(3)(1)((i-45)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(3)(2)(i):=rho_in(3)(2)((i-15)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(3)(3)(i):=rho_in(3)(3)((i-21)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(3)(4)(i):=rho_in(3)(4)((i-8)mod 64);
			-- end loop;

			-- for i in 0 to 63 loop
				-- rho_out(4)(0)(i):=rho_in(4)(0)((i-18)mod 64);
			-- end loop;	
			-- for i in 0 to 63 loop
				-- rho_out(4)(1)(i):=rho_in(4)(1)((i-2)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(4)(2)(i):=rho_in(4)(2)((i-61)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(4)(3)(i):=rho_in(4)(3)((i-56)mod 64);
			-- end loop;
			-- for i in 0 to 63 loop
				-- rho_out(4)(4)(i):=rho_in(4)(4)((i-14)mod 64);
			-- end loop;
			
			-- pi_in:=rho_out;

			-- -- pi
			-- for y in 0 to 4 loop
				-- for x in 0 to 4 loop
					-- for i in 0 to 63 loop
						-- pi_out((2*x+3*y) mod 5)(0*x+1*y)(i):=pi_in(y) (x)(i);
					-- end loop;	
				-- end loop;
			-- end loop;
			
			-- chi_in:=pi_out;

			-- --chi
			-- for y in 0 to 4 loop
				-- for x in 0 to 2 loop
					-- for i in 0 to 63 loop
						-- chi_out(y)(x)(i):=chi_in(y)(x)(i) xor  ( not(chi_in (y)(x+1)(i))and chi_in (y)(x+2)(i));
					-- end loop;	
				-- end loop;
			-- end loop;

			-- for y in 0 to 4 loop
				-- for i in 0 to 63 loop
					-- chi_out(y)(3)(i):=chi_in(y)(3)(i) xor  ( not(chi_in (y)(4)(i))and chi_in (y)(0)(i));            
				-- end loop;	
			-- end loop;
				
			-- for y in 0 to 4 loop
				-- for i in 0 to 63 loop
					-- chi_out(y)(4)(i):=chi_in(y)(4)(i) xor  ( not(chi_in (y)(0)(i))and chi_in (y)(1)(i));        
				-- end loop;	
			-- end loop;
			
			-- iota_in:=chi_out;

			-- --iota
			-- for y in 1 to 4 loop
				-- for x in 0 to 4 loop
					-- for i in 0 to 63 loop
						-- iota_out(y)(x)(i):=iota_in(y)(x)(i);
					-- end loop;	
				-- end loop;
			-- end loop;


			-- for x in 1 to 4 loop
				-- for i in 0 to 63 loop
					-- iota_out(0)(x)(i):=iota_in(0)(x)(i);
				-- end loop;	
			-- end loop;

			-- for i in 0 to 63 loop
				-- iota_out(0)(0)(i):=iota_in(0)(0)(i) xor round_constant(i);
			-- end loop;	
				
			-- round_out:=iota_out;

		-- return round_out;
	-- end function f_keccak_round_monolith;
			
end package body keccak_pkg;
