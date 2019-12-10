
# temporary y0_base
y0_base = SID_mat
y0_base_HIV = y0_base[,"S", "lo"]
# y0_base_HIV["S_lo"] = 0
# y0_base_HIV["S_hi"] = 2000
# y0_base_HIV["S_pr"] = 500
# y0_base_HIV["I_lo_new"] = 7000
y0_base[,"S","lo"] = y0_base_HIV
