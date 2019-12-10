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
    p = plot_df(df_wide)
    ggsave(paste0('calplots/', round(distance * 1e7), '.png'), p)
  }
  
  return(distance)
}


l2 = function(x, y){
  v = (x - y) / pmax(1e-6, pmin(x, y))
  v = v^2
  return(sum(v, na.rm=T))
}