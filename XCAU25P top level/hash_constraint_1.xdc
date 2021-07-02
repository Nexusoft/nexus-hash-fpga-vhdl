
set_multicycle_path -setup -start -from [get_clocks *hash_clk*] -to [get_clocks *sys_clk*] 8
set_multicycle_path -hold -from [get_clocks *hash_clk*] -to [get_clocks *sys_clk*] 7
set_multicycle_path -setup -from [get_clocks *sys_clk*] -to [get_clocks *hash_clk*] 8
set_multicycle_path -hold -end -from [get_clocks *sys_clk*] -to [get_clocks *hash_clk*] 7