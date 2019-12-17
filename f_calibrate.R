l2 = function(x, y){
  v = (x - y) / pmax(1e-6, pmin(x, y))
  v = v^2
  return(sum(v, na.rm=T))
}

distance_given_cal_vec = function(x, keys, norm=l2){
  callist = baselist
  for(i in 1:length(x)){
    callist[[keys[i]]] = x[i]
  }
  
  output = run_model(tvec=seq(min(tvec_base), max(all_dat$t)+1, by=dt),
                     modelpars=callist, options=list('only_cal_outs' = TRUE))
  
  df = compare_model_to_data(output)
  
  distance = norm(df$data, df$model)
  
  saveprob = 0
  if(runif(1) < saveprob){
    p = plot_uncertainty(df)
    saveopen(p, paste0('calplots/', round(distance * 1e7)), open=FALSE)
  }
  
  return(distance)
}

compare_model_to_data = function(output){
  
  # res = extr(output, cal_keys)
  res = extr(output, plot_keys[1:5])
  
  dat = all_dat
  dat$scen = ""
  dat$scen_long = ""
  res$scen = ""
  res$scen_long = ""
  
  df_wide = widen_sources(dat, res)
  return(df_wide)
}

gen_calibration = function(cal_vars = c('f_infect_HIV', 'init_diag_prop'), control=list()){
  
  bes = c('f_infect_HIV' = 6e-6, 'init_diag_prop' = 0.7, 'init_prev_HIV' = 0.07)
  ubs = c('f_infect_HIV' = 1e-4, 'init_diag_prop' = 0.9, 'init_prev_HIV' = 0.3)
  ncal_vars = length(cal_vars)
  
  optim_result <<- nmkb(par=bes[cal_vars],
                        fn=distance_given_cal_vec,
                        lower=numeric(ncal_vars),
                        upper=ubs[cal_vars],
                        control=control,
                        keys=cal_vars)
  
  for(i in 1:length(cal_vars)){
    baselist[[cal_vars[i]]] = optim_result$par[i]
  }
  
  baselist <<- baselist
  cal <<- run_model(modelpars=baselist)
  
  tvec_split <<- tvec_base[tvec_base >= split_year]
  y0_split <<- cal$SID[as.character(split_year),,,]
  
}
