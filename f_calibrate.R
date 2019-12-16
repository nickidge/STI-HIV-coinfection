distance_given_cal_vec = function(x, keys, norm=l2){
  callist = baselist
  for(i in 1:length(x)){
    callist[[keys[i]]] = x[i]
  }
  
  output = run_model(y0=y0_base, tvec=seq(min(tvec_base), max(all_dat$t)+1, by=dt),
                     modelpars=callist, options=list('only_cal_outs' = TRUE))
  
  df = compare_model_to_data(output)
  
  distance = norm(df$data, df$model)
  
  saveprob = 0.1
  if(runif(1) < saveprob){
    p = plot_calibration(df)
    saveopen(p, paste0('calplots/', round(distance * 1e7)), open=FALSE)
    # ggsave(paste0('calplots/', round(distance * 1e7), '.png'), p)
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

plot_calibration = function(df){
  
  colour_scale = data.frame(HIV_pop = c('prop_diag', 'prop_treat', 'prop_suppr'),
                            long = c('Proportion diagnosed', 'Proportion diagnosed on treatment', 'Proportion on treatment virally suppressed'),
                            col = c('orange', 'yellow', 'green'))
  
  max_df = df %>%
    group_by(plot) %>%
    filter(min(plot_years) <= t & t <= max(plot_years)) %>% 
    summarise(max = max(model, data, na.rm=T)) %>%
    # mutate(upperlim = ifelse(plot %in% c('HIV_prev', 'care_cascade'), 1, 1.1 * max)) %>% 
    mutate(upperlim = ifelse(plot %in% c('care_cascade'), 1, 1.1 * max)) %>% 
    as.data.frame()
  
  df$HIV_pop = factor(df$HIV_pop, levels = unique(df$HIV_pop)[order(sapply(unique(df$HIV_pop), function(x) max(c(0, match(x, colour_scale$HIV_pop)), na.rm=TRUE)), decreasing=TRUE)])
  
  p = ggplot(df, aes(x=t, group=HIV_pop, colour=HIV_pop, fill=HIV_pop))
  
  p = p + facet_wrap(.~plot, scales="free", ncol=2,
                     labeller = labeller(plot = setNames(plot_long, plot_keys)))
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3)
  p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3)
  
  p = p + geom_blank(data=max_df, aes(y=upperlim), inherit.aes = F)
  p = p + scale_x_continuous(breaks = label_years,
                             name = 'Year',
                             limits = c(min(df$t), max(df$t)),
                             expand = c(0, 0))
  p = p + scale_y_continuous(expand = c(0,0))
  p = p + expand_limits(y = 0)
  
  
  guide_list = list(
    linetype = 1,
    shape = NA,
    fill = NA,
    alpha = 1
  )
  
  p = p + guides(colour = guide_legend(override.aes = guide_list))
  
  p = p + scale_colour_manual(na.value = 'black',
                              limits = colour_scale$HIV_pop,
                              breaks = colour_scale$HIV_pop,
                              values = colour_scale$col,
                              labels = colour_scale$long,
                              aesthetics = c('colour', 'fill'),
                              name = 'Care cascade')
  
  p = p + coord_cartesian(xlim = plot_years)
  
  p = p + theme_all
  
  p = convert_axis(p)
  
  return(p)
}



gen_calibration = function(cal_vars = c('f_infect_HIV', 'init_diag_prop')){
  
  bes = c('f_infect_HIV' = 6e-6, 'init_diag_prop' = 0.7, 'init_prev_HIV' = 0.07)
  ubs = c('f_infect_HIV' = 1e-4, 'init_diag_prop' = 0.9, 'init_prev_HIV' = 0.3)
  ncal_vars = length(cal_vars)
  
  optim_result <<- nmkb(par=bes[cal_vars],
                        fn=distance_given_cal_vec,
                        lower=numeric(ncal_vars),
                        upper=ubs[cal_vars],
                        keys=cal_vars)
  
  for(i in 1:length(cal_vars)){
    baselist[[cal_vars[i]]] = optim_result$par[i]
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