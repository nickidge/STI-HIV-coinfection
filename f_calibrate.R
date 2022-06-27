l_ = function(x, y, weights=1, exponent=2){
  v = abs(x - y) / pmin(abs(x), abs(y))
  v = fixnan(v)
  v = v^exponent
  v = v*weights
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

year_weighting = data.frame(
  pid = "HIV_diag_tot",
  t = 2016:2019,
  year_weight = 6
)

distance_given_cal_vec = function(x, keys, norm=l_){
  callist = baselist
  for(i in 1:length(x)){
    callist[[keys[i]]] = x[i]
  }
  
  output = run_model(tvec=seq(min(tvec_base), max(all_dat$t)+1, by=dt),
                     modelpars=callist, options=list('only_cal_outs' = TRUE))
  
  df = compare_model_to_data(output) %>% 
    mutate(data_weight = cal_weights[as.character(plot)]) %>% 
    left_join(year_weighting, by=intersect(names(.), names(year_weighting))) %>%
    mutate(year_weight = replace_na(year_weight, 1),
           weights = data_weight * year_weight)
  
  distance = norm(df$data, df$model, weights=df$weights, exponent=1)
  
  if(prob(0)){
    p = plot_uncertainty(df)
    saveopen(p, paste0('calplots/', round(distance * 1e7)), open=FALSE)
  }

  print(distance)
  return(distance)
}

compare_model_to_data = function(output){
  
  thisdat = all_dat
  # thisdat = subset(thisdat, !(med_pop == 'tot' & HIV_pop == 'PLHIV'))
  # thisdat = subset(thisdat, !(med_pop == 'tot' & substr(HIV_pop, 1, 8) == 'HIV_diag'))
  # thisdat = subset(thisdat, HIV_pop != 'PLHIV')
  
  res = extr(output, unique(thisdat$plot))
  
  df_wide = widen_sources(thisdat %>% select(-contains('scen')), res %>% select(-contains('scen')))
  df_wide = df_wide[!is.na(df_wide$model) & !is.na(df_wide$data),]
  
  return(df_wide)
}

gen_calibration = function(cal_vars = c('f_infect_HIV', 'init_diag_prop'), control=list()){
  
  cal_raw <<- data.frame(read_excel("data_sti.xlsx", sheet="calibration"), row.names=1)
  this_cal_raw <<- cal_raw[cal_vars,]
  
  optim_result <<- nmkb(par=this_cal_raw$bes,
                        fn=distance_given_cal_vec,
                        lower=this_cal_raw$lbs,
                        upper=this_cal_raw$ubs,
                        control=control,
                        keys=cal_vars)
  
  calibrated_values = lapply(1:length(cal_vars), function(x) optim_result$par[x]) %>% 
    setNames(cal_vars)
  
  return(calibrated_values)
}

plot_cals = function(df){
  p1 = plot_uncertainty(df, toplot=c('pop', 'PLHIV_diag', 'HIV_prev', 'prop_prep', 'HIV_diag', 'HIV_inf', 'HIV_diag_new', 'HIV_diag_old'), colour_strat = 'med')
  p2 = plot_uncertainty(df, toplot=c('popsize_by_risk', 'num_cascade', 'HIV_prev_by_risk', 'care_cascade', 'HIV_diag_by_pop'))
  p = arrangeGrob(p1, p2, nrow=1)
  return(p)
}