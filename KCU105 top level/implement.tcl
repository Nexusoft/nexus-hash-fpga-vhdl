
# read implentation constraints
read_xdc hash_constraint_1.xdc

place_design -directive Auto -timing_summary
#AltSpreadLogic_medium
# report_clock_utilization -file $outputDir/clock_util.rpt
#
# Optionally run optimization if there are timing violations after placement
#if {[get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]] < 0} {
# puts "Found setup timing violations => running physical optimization"
phys_opt_design -directive AggressiveExplore
phys_opt_design -directive AggressiveFanoutOpt
phys_opt_design -directive AlternateReplication

#}
write_checkpoint -force $outputDir/post_place.dcp
report_utilization -file $outputDir/post_place_util.rpt
report_timing_summary -file $outputDir/post_place_timing_summary.rpt

route_design -directive Explore -ultrathreads
#phys_opt_design -directive AggressiveExplore

# again
#place_design -post_place_opt -timing_summary
#phys_opt_design
#route_design -ultrathreads
#phys_opt_design
#route_design -tns_cleanup -ultrathreads

write_checkpoint -force $outputDir/post_route.dcp
report_route_status -file $outputDir/post_route_status.rpt
report_timing_summary -file $outputDir/post_route_timing_summary.rpt
report_power -file $outputDir/post_route_power.rpt
report_drc -file $outputDir/post_imp_drc.rpt

#write_bitstream -force $outputDir/kcu105_nxs_half_size.bit

