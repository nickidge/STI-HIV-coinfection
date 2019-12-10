randomise_keys = function(statkeys=static_pars, timepars=baselist, basekeyindex=c(), basevar=0.05, syear=0){
  
  for(key in names(statkeys)){
    statkeys[[key]]$v = runif(1, min=statkeys[[key]]$lb, max=statkeys[[key]]$ub)
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
  trial_df = extr(trial, scen_keys)
  return(trial_df)
}

lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

summarise_trials = function(df, value='value'){
  df = df %>%
    # select(-trial) %>% 
    group_by_at(vars(-!!sym(value), -trial)) %>%
    summarise(smean = mean(!!sym(value), na.rm = TRUE),
              ssd = sd(!!sym(value), na.rm = TRUE),
              count = n(),
              value = first(!!sym(value))) %>%
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count)) %>%
    # mutate(lower_ci = max(!!sym(value)),
    #        upper_ci = min(!!sym(value))) %>% 
    # select(-trial) %>%
    as.data.frame() %>% 
    mutate(t = as.numeric(t))
}

ci_df = function(ntrials=5, timepars=baselist, basevar=0.05, y0=y0_base, tvec=tvec_base, options=list(), syear=0){
  trials_df = run_model(y0=y0, tvec=tvec, modelpars=timepars, options=options)
  trials_df = extr(trials_df, scen_keys)
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