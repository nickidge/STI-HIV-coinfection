gen_uncertainty = function(ntrials=10){
  trials_df = ci_df(ntrials=ntrials, basevar=0.2, options=list('keep_static'=TRUE))
  
  cal_wide = widen_sources(trials_df)
  cal_wide$scen = 'base'
  cal_wide$scen_long = 'Base'
  cal_wide = rbind.fill(cal_wide, data_wide)
  
  # p = plot_df(cal_wide)
  # p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=T)
  
  return(cal_wide)
}

plot_uncertainty = function(df){
  
  colour_scale = data.frame(HIV_pop = c('prop_diag', 'prop_treat', 'prop_suppr'),
                            long = c('Proportion diagnosed', 'Proportion diagnosed on treatment', 'Proportion on treatment virally suppressed'),
                            col = c('orange', 'yellow', 'green'))
  
  max_df = df %>%
    group_by(plot) %>%
    filter(min(plot_years) <= t & t <= max(plot_years)) %>% 
    summarise(max = max(model, data, upper_ci, na.rm=T)) %>%
    mutate(upperlim = ifelse(plot == 'care_cascade', 1, 1.1 * max)) %>% 
    as.data.frame()
  
  df$HIV_pop = factor(df$HIV_pop, levels = unique(df$HIV_pop)[order(sapply(unique(df$HIV_pop), function(x) max(c(0, match(x, colour_scale$HIV_pop)), na.rm=TRUE)), decreasing=TRUE)])
  
  p = ggplot(df, aes(x=t, group=HIV_pop, colour=HIV_pop, fill=HIV_pop))
  
  p = p + facet_wrap(.~plot, scales="free_y", ncol=1,
                     labeller = labeller(plot = setNames(plot_long, plot_keys)))
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3)
  p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3)
  p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=T)
  
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
  p = p + theme(legend.justification = c(0, 0.08))
  
  return(p)
}

randomise_within_bounds = function(v, lb, ub){
  if(lb < v & v < ub){
    lbp = v - lb
    ubp = 1 / (ub - v)
    if(runif(1) < lbp / (lbp + ubp)){
      y = runif(1, min=lb, max=v)
    } else {
      y = runif(1, min=v, max=ub)
    }
  } else {
    y = runif(1, min=lb, max=ub)
  }
  return(y)
}

randomise_keys = function(statkeys=static_pars, timepars=baselist, basekeyindex=c(), basevar=0.05, syear=0){
  
  for(key in names(statkeys)){
    # statkeys[[key]]$v = runif(1, min=statkeys[[key]]$lb, max=statkeys[[key]]$ub)
    statkeys[[key]]$v = randomise_within_bounds(statkeys[[key]]$v, statkeys[[key]]$lb, statkeys[[key]]$ub)
  }
  
  for(ind in names(basekeyindex)){
    var = runif(1, (1-basevar), 1/(1-basevar))
    columns = basekeyindex[[ind]]
    tv = rownames(timepars[[ind]])
    multyears = (as.numeric(tv) >= syear)
    if(length(timepars[[ind]]) == 1){
      
    } else if(is.na(columns)){
      timepars[[ind]][multyears,] = timepars[[ind]][multyears,] * var
      # timepars[[ind]] = timepars[[ind]] * var
    } else {
      timepars[[ind]][multyears,columns] = timepars[[ind]][multyears,columns] * var
      # timepars[[ind]][,columns] = timepars[[ind]][,columns] * var
    }
  }
  
  return(list(timepars, statkeys))
}

run_trial = function(timepars=baselist, basevar=0.05, y0=y0_base, tvec=tvec_base, options=options, syear=0){
  basekeyindex = c(t_testing = NA, condom_usage = NA, num_prep = NA, eff_condom = NA,
                   f_infect_HIV = NA, risk_mat = NA)
  
  this_stat_pars = randomise_keys(timepars=timepars, basekeyindex = basekeyindex, basevar=basevar, syear=syear)
  
  mpars = this_stat_pars[[1]]
  if(getdict(options, 'keep_static', FALSE)){
    spars = static_pars
  } else {
    spars = this_stat_pars[[2]]
  }
  
  trial = run_model(y0=y0, tvec=tvec, options=options, modelpars=mpars, spars=spars)
  trial_df = extr(trial, plot_keys)
  return(trial_df)
}

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

summarise_trials = function(df, value='value', lbfunc=lower_ci, ubfunc=upper_ci){
  df = df %>%
    group_by_at(vars(-!!sym(value), -trial)) %>%
    summarise(smean = mean(!!sym(value), na.rm = TRUE),
              ssd = sd(!!sym(value), na.rm = TRUE),
              count = n(),
              value = first(!!sym(value))) %>%
    mutate(se = ssd / sqrt(count),
           lower_ci = lbfunc(smean, se, count),
           upper_ci = ubfunc(smean, se, count)) %>%
    # mutate(lower_ci = max(!!sym(value)),
    #        upper_ci = min(!!sym(value))) %>% 
    ungroup() %>% 
    mutate(t = as.numeric(t)) %>% 
    as.data.frame()
}

ci_df = function(ntrials=5, timepars=baselist, basevar=0.05, y0=y0_base, tvec=tvec_base, options=list(), syear=0){
  trials_df = run_model(y0=y0, tvec=tvec, modelpars=timepars, options=options)
  trials_df = extr(trials_df, plot_keys)
  trials_df$trial=0
  if(ntrials < 1){
    trials_df = summarise_trials(trials_df)
    return(trials_df)
  }
  for(i in 1:ntrials){
    this_trial = run_trial(timepars, basevar=basevar, y0=y0, tvec=tvec, options=options, syear=syear)
    this_trial$trial = i
    trials_df = rbind.fill(trials_df, this_trial)
  }
  trials_df = summarise_trials(trials_df)
  return(trials_df)
}