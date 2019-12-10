PLHIV_dat = data.frame(t = PLHIV0$Year, value = PLHIV0$`Total PLHIV`, type='pop', dt=1,
                       sti_pop='all', risk_pop='all', HIV_pop='PLHIV', source='data', scen='')
HIV_diag_dat = data.frame(t = diagnoses0$Year, value = diagnoses0$`New diagnoses`, type='trans', dt=1,
                          sti_pop='all', risk_pop='all', HIV_pop='HIV_diag', source='data', scen='')

all_dat = rbind.fill(PLHIV_dat, HIV_diag_dat)

output_keys = c('PLHIV', 'HIV_diag')

calibrationvars = c('f_infect_HIV', 'init_diag_prop')

optim_result = nmkb(c(1e-6, 0.1), distance_given_f_infect_HIV_optim,
                    lower=c(0, 0),
                    upper=c(1e-2, 1),
                    keys=calibrationvars)
optim_pars = optim_result$par

for(i in 1:length(optim_pars)){
  baselist[[calibrationvars[i]]] = optim_pars[i]
}

cal = run_model(y0_base, modelpars=baselist)

tvec_split = tvec_base[tvec_base >= split_year]
y0_split = cal$SID[as.character(split_year),,,]

p = plot_df(widen_sources(all_dat, extr(cal, output_keys)))

saveopen(p, 'calibration', 'plots')
