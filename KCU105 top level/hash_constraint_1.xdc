# KCU105 constraints for hashing.

# IOs used in the design
# Bank  66 VCCO - VADJ_1V8_FPGA_10A - IO_L12P_T1U_N10_GC_66
set_property IOSTANDARD LVDS [get_ports CLK_125MHZ_P]
# Bank  66 VCCO - VADJ_1V8_FPGA_10A - IO_L12N_T1U_N11_GC_66
set_property PACKAGE_PIN G10 [get_ports CLK_125MHZ_P]
set_property PACKAGE_PIN F10 [get_ports CLK_125MHZ_N]
set_property IOSTANDARD LVDS [get_ports CLK_125MHZ_N]
# Bank  84 VCCO -          - IO_L22P_T3U_N6_DBC_AD0P_64
set_property PACKAGE_PIN AN8 [get_ports CPU_RESET]
set_property IOSTANDARD LVCMOS18 [get_ports CPU_RESET]
# Bank  95 VCCO -          - IO_L2P_T0L_N2_FOE_B_65
set_property PACKAGE_PIN G25 [get_ports USB_UART_TX]
set_property IOSTANDARD LVCMOS18 [get_ports USB_UART_TX]
# Bank  95 VCCO -          - IO_L3P_T0L_N4_AD15P_A26_65
set_property PACKAGE_PIN K26 [get_ports USB_UART_RX]
set_property IOSTANDARD LVCMOS18 [get_ports USB_UART_RX]
# Bank  84 VCCO -          - IO_L22N_T3U_N7_DBC_AD0N_64
set_property PACKAGE_PIN AP8 [get_ports GPIO_LED_0_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_0_LS]
# Bank  95 VCCO -          - IO_T0U_N12_A28_65
set_property PACKAGE_PIN H23 [get_ports GPIO_LED_1_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_1_LS]
# Bank  85 VCCO -          - IO_L20P_T3L_N2_AD1P_D08_65
set_property PACKAGE_PIN P20 [get_ports GPIO_LED_2_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_2_LS]
# Bank  85 VCCO -          - IO_L20N_T3L_N3_AD1N_D09_65
set_property PACKAGE_PIN P21 [get_ports GPIO_LED_3_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_3_LS]
# Bank  85 VCCO -          - IO_L19P_T3L_N0_DBC_AD9P_D10_65
set_property PACKAGE_PIN N22 [get_ports GPIO_LED_4_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_4_LS]
# Bank  85 VCCO -          - IO_L19N_T3L_N1_DBC_AD9N_D11_65
set_property PACKAGE_PIN M22 [get_ports GPIO_LED_5_LS]
set_property IOSTANDARD LVCMOS18 [get_ports GPIO_LED_5_LS]


set_multicycle_path -setup -start -from [get_clocks *hash_clk*] -to [get_clocks *sys_clk*] 8
set_multicycle_path -hold -from [get_clocks *hash_clk*] -to [get_clocks *sys_clk*] 7
set_multicycle_path -setup -from [get_clocks *sys_clk*] -to [get_clocks *hash_clk*] 8
set_multicycle_path -hold -end -from [get_clocks *sys_clk*] -to [get_clocks *hash_clk*] 7