PLHIV_dat = data.frame(t = PLHIV0$Year, value = PLHIV0$`Total PLHIV`, type='pop', dt=1,
                       sti_pop='all', risk_pop='all', HIV_pop='PLHIV', source='data', scen='')
HIV_diag_dat = data.frame(t = diagnoses0$Year, value = diagnoses0$`New diagnoses`, type='trans', dt=1,
                          sti_pop='all', risk_pop='all', HIV_pop='HIV_diag', source='data', scen='')

all_dat = rbind.fill(PLHIV_dat, HIV_diag_dat)

output_keys = c('PLHIV', 'HIV_diag')

# calibrationvars = c('f_infect_HIV', 'init_PLHIV', 'init_diag_prop')
calibrationvars = c('f_infect_HIV', 'init_diag_prop')

distance_given_f_infect_HIV_optim = function(x, keys, norm=l2){
  callist = baselist
  for(i in 1:length(x)){
    callist[[keys[i]]] = x[i]
  }
  
  output = run_model(y0_base, tvec=seq(min(tvec_base), max(all_dat$t)+1, by=dt), modelpars=callist,
                     options=list('only_cal_outs' = TRUE))
  
  res = extr(output, output_keys)
  
  df_wide = widen_sources(all_dat, res)

  distance = norm(df_wide$data, df_wide$model)
  
  saveprob = 0
  if(runif(1) < saveprob){
    p = plot_calibration(output, all_dat)
    ggsave(paste0('calplots/', round(distance * 1e7), '.png'), p)
  }
  
  return(distance)
}

plot_calibration = function(result, dat){
  res = extr(result, output_keys)
  
  df = widen_sources(dat, res)
  df$HIV_group = paste(df$sti_pop, df$risk_pop, sep='_')
  df$t = as.numeric(as.character(df$t))
  
  p = plot_df(df)
  
  return(p)
}

optim_result = nmkb(c(1e-6, 0.1), distance_given_f_infect_HIV_optim,
                    lower=c(0, 0),
                    upper=c(1e-2, 1),
                    keys=calibrationvars)
optim_pars = optim_result$par

for(i in 1:length(optim_pars)){
  baselist[[calibrationvars[i]]] = optim_pars[i]
}

cal = run_model(y0_base, modelpars=baselist)

p = plot_calibration(cal, all_dat)
saveopen(p, 'calibration', 'plots')
