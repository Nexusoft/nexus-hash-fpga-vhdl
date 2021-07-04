# build nxs_hash for kcu105
set_param general.maxThreads 8

set outputDir ../../K7
file mkdir $outputDir

set_part "xc7k325tfbg676-2"

# read all design files
read_vhdl ../uart/comp/uart_tx.vhd
read_vhdl ../uart/comp/uart_rx.vhd
read_vhdl ../uart/comp/uart_parity.vhd
read_vhdl ../uart/uart.vhd

read_vhdl -vhdl2008 ../nxs_hash/fifo.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_pkg.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_block_half.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_block.vhd

read_vhdl -vhdl2008 ../nxs_hash/skein_nxs.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_pkg.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_round.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_nxs.vhd
read_vhdl -vhdl2008 ../nxs_hash/sk1024.vhd
read_vhdl -vhdl2008 ../nxs_hash/nxs_hash.vhd
read_vhdl -vhdl2008 ../uart_interface/uart_nexus_pkg.vhd
read_vhdl -vhdl2008 ../uart_interface/uart_nexus_interface.vhd
read_vhdl -vhdl2008 K7_nxs_hash_top.vhd


read_ip ../../K7/clk_wiz_0/clk_wiz_0.xci
# read constraints
read_xdc K7_nexus_hash.xdc

# generate ip
generate_target all [get_ips]
# Synthesize Design
synth_design -top nxs_hash_top -directive AreaOptimized_high -part xc7k325tfbg676-2 -shreg_min_size 5
# -retiming


read_xdc hash_constraint_1.xdc
#optimize design
opt_design -directive ExploreArea

write_checkpoint -force $outputDir/post_synth.dcp
report_timing_summary -file $outputDir/post_synth_timing_summary.rpt
report_utilization -file $outputDir/post_synth_util.rpt
report_design_analysis -logic_level_distribution -file $outputDir/post_synth_logic_level_dist.rpt
# report_timing -of_objects [report_design_analysis -end_point_clock clk_out_hash_clk_wiz_0 -logic_levels 10 -timing -return_timing_paths]


