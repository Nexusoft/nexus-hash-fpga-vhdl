# build nxs_hash for kcu105
set_param general.maxThreads 8

set outputDir ../../kcu105
file mkdir $outputDir

# set_part "xcku040-ffva1156-2-e"
set_property BOARD_PART xilinx.com:kcu105:1.7 [current_project]

# read constraints
read_xdc KCU105_nexus_hash.xdc
read_xdc hash_constraint_1.xdc

place_design -directive Auto -timing_summary
report_clock_utilization -file $outputDir/clock_util.rpt
#
# Optionally run optimization if there are timing violations after placement
#if {[get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]] < 0} {
# puts "Found setup timing violations => running physical optimization"
phys_opt_design
#}
write_checkpoint -force $outputDir/post_place.dcp
report_utilization -file $outputDir/post_place_util.rpt
report_timing_summary -file $outputDir/post_place_timing_summary.rpt

route_design -ultrathreads

# again
place_design -post_place_opt -timing_summary
phys_opt_design
route_design

write_checkpoint -force $outputDir/post_route.dcp
report_route_status -file $outputDir/post_route_status.rpt
report_timing_summary -file $outputDir/post_route_timing_summary.rpt
report_power -file $outputDir/post_route_power.rpt
report_drc -file $outputDir/post_imp_drc.rpt

#write_bitstream -force $outputDir/kcu105_nxs_half_size.bit

