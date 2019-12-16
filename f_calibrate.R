distance_given_f_infect_HIV_optim = function(x, keys, norm=l2){
  callist = baselist
  for(i in 1:length(x)){
    callist[[keys[i]]] = x[i]
  }
  
  output = run_model(y0_base, tvec=seq(min(tvec_base), max(all_dat$t)+1, by=dt), modelpars=callist,
                     options=list('only_cal_outs' = TRUE))
  
  res = extr(output, cal_keys)
  
  dat = all_dat
  dat$scen = ""
  dat$scen_long = ""
  res$scen = ""
  res$scen_long = ""
  
  df_wide = widen_sources(dat, res)
  
  distance = norm(df_wide$data, df_wide$model)
  
  saveprob = 0
  if(runif(1) < saveprob){
    p = plot_df(df_wide)
    ggsave(paste0('calplots/', round(distance * 1e7), '.png'), p)
  }
  
  return(distance)
}

gen_calibration = function(calibrationvars = c('f_infect_HIV', 'init_diag_prop')){
  
  optim_result <<- nmkb(c(1e-6, 0.1), distance_given_f_infect_HIV_optim,
                        lower=c(0, 0),
                        upper=c(1e-4, 1),
                        keys=calibrationvars)
  
  for(i in 1:length(calibrationvars)){
    baselist[[calibrationvars[i]]] = optim_result$par[i]
  }
  
  baselist <<- baselist
  cal <<- run_model(y0_base, modelpars=baselist)
  
  tvec_split <<- tvec_base[tvec_base >= split_year]
  y0_split <<- cal$SID[as.character(split_year),,,]
  
}

l2 = function(x, y){
  v = (x - y) / pmax(1e-6, pmin(x, y))
  v = v^2
  return(sum(v, na.rm=T))
}