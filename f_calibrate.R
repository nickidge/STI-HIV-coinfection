l2 = function(x, y){
  v = (x - y) / pmin(x, y)
  v = fixnan(v)
  v = v^2
  return(sum(v, na.rm=T))
}

prob = function(x) {
  if(x <= 0){
    return(FALSE)
  } else if(x >= 1){
    return(TRUE)
  } else {
    return(runif(1) < x)
  }
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
  
  if(prob(0)){
    p = plot_uncertainty(df)
    saveopen(p, paste0('calplots/', round(distance * 1e7)), open=FALSE)
  }
  
  return(distance)
}

compare_model_to_data = function(output){
  
  thisdat = all_dat
  thisdat = subset(thisdat, !(med_pop == 'tot' & HIV_pop == 'PLHIV'))
  
  res = extr(output, unique(thisdat$plot))
  
  df_wide = widen_sources(thisdat %>% select(-contains('scen')), res %>% select(-contains('scen')))
  
  return(df_wide)
}

gen_calibration = function(cal_vars = c('f_infect_HIV', 'init_diag_prop'), control=list()){
  
  lbs = c('f_infect_HIV' = 0, 'int_factor' = 1, 'init_diag_prop' = 0, 'init_prev_HIV_aus' = 0, 'init_prev_HIV_int' = 0)
  bes = c('f_infect_HIV' = 6e-6, 'int_factor' = 1.2, 'init_diag_prop' = 0.6, 'init_prev_HIV_aus' = 0.07, 'init_prev_HIV_int' = 0.07)
  ubs = c('f_infect_HIV' = 1e-4, 'int_factor' = 10, 'init_diag_prop' = 0.7, 'init_prev_HIV_aus' = 0.18, 'init_prev_HIV_int' = 0.3)
  
  optim_result <<- nmkb(par=bes[cal_vars],
                        fn=distance_given_cal_vec,
                        lower=lbs[cal_vars],
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

plot_cals = function(df){
  p1 = plot_uncertainty(df, toplot=c('pop', 'PLHIV', 'HIV_prev', 'prop_prep', 'HIV_diag', 'HIV_inf', 'HIV_diag_new', 'HIV_diag_old'), colour_strat = 'med')
  p2 = plot_uncertainty(df, toplot=c('popsize_by_risk', 'num_cascade', 'HIV_prev_by_risk', 'care_cascade', 'HIV_diag_by_pop'))
  p = arrangeGrob(p1, p2, nrow=1)
  return(p)
}