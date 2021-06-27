# build nxs_hash for kcu105
set_param general.maxThreads 8

set outputDir ../../kcu105
file mkdir $outputDir

set_part "xcku040-ffva1156-2-e"
set_property BOARD_PART xilinx.com:kcu105:1.7 [current_project]


# read all design files
read_vhdl ../uart/comp/uart_tx.vhd
read_vhdl ../uart/comp/uart_rx.vhd
read_vhdl ../uart/comp/uart_parity.vhd
read_vhdl ../uart/uart.vhd

read_vhdl -vhdl2008 ../nxs_hash/fifo.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_pkg.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_round.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_make_key.vhd
read_vhdl -vhdl2008 ../nxs_hash/skein_nxs.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_pkg.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_round.vhd
read_vhdl -vhdl2008 ../nxs_hash/keccak_nxs.vhd
read_vhdl -vhdl2008 ../nxs_hash/sk1024.vhd
read_vhdl -vhdl2008 ../nxs_hash/nxs_hash.vhd
read_vhdl -vhdl2008 ../uart_interface/uart_nexus_pkg.vhd
read_vhdl -vhdl2008 ../uart_interface/uart_nexus_interface.vhd
read_vhdl -vhdl2008 KCU105_nxs_hash_top.vhd


read_ip ../../kcu105_nxs_hash_half_size/kcu105_nxs_hash_half_size.srcs/sources_1/ip/clk_wiz_0/clk_wiz_0.xci
# read constraints
read_xdc KCU105_nexus_hash.xdc

# generate ip
generate_target all [get_ips]
# Synthesize Design
synth_design -top KCU105_nxs_hash_top -part xcku040-ffva1156-2-e -directive PerformanceOptimized -shreg_min_size 5 -retiming


read_xdc hash_constraint_1.xdc
#optimize design
opt_design

write_checkpoint -force $outputDir/post_synth.dcp
report_timing_summary -file $outputDir/post_synth_timing_summary.rpt
report_utilization -file $outputDir/post_synth_util.rpt

