# generate uncertainty range for scenario
gen_uncertainty = function(ntrials=0, variance=base_variance){
  trials_df = ci_df(ntrials=ntrials, basevar=variance, options=list('keep_static'=TRUE))
  
  cal_wide = widen_sources(trials_df)
  cal_wide$scen = 'base'
  cal_wide$scen_long = 'Base'
  
  this_data_wide = data_wide
  # this_data_wide = subset(this_data_wide, med_pop %in% unique(cal_wide$med_pop))
  if(!medicare_ineligible){this_data_wide = subset(this_data_wide, !(med_pop %in% med_labs))}
  
  cal_wide = rbind.fill(cal_wide, this_data_wide)
  
  cal_wide = filter_extr(cal_wide)
  
  return(cal_wide)
}

plot_uncertainty = function(thisdf, colour_strat='cascade', toplot=NULL){
  
  max_df = max_df_base[max_df_base$plot %in% thisdf$plot,]
  max_df$plot = factor(max_df$plot)
  if(!is.null(toplot)){
    thisdf = subset(thisdf, plot %in% toplot)
    max_df = subset(max_df, plot %in% toplot)
    thisdf$plot = factor(thisdf$plot, levels = toplot)
  }
  
  if(colour_strat == 'cascade'){
    legend_name = 'Care cascade'
    # define colour scale for care cascade
    colour_scale = data.frame(HIV_pop = c('num_und', 'num_diag', 'num_treat', 'num_suppr'),
                              long = c('Proportion undiagnosed', 'Proportion diagnosed', 'Proportion diagnosed on treatment', 'Proportion on treatment virally suppressed'),
                              risk_pop = 'all',
                              col = c('red', 'orange', 'yellow', 'green'))
    black_colour_scale = data.frame(HIV_pop = unique(thisdf$HIV_pop)[!(unique(thisdf$HIV_pop) %in% colour_scale$HIV_pop)],
                                    col = 'black',
                                    risk_pop = 'all',
                                    long = 'na')
    risk_colour_scale = data.frame(HIV_pop = 'all',
                                   long = c('Low risk', 'High risk not on PrEP', 'High risk on PrEP', 'High risk (all)'),
                                   risk_pop = c('lo', 'hi', 'pr', 'hi_all'),
                                   col = c('darkgreen', 'darkred', 'darkorange', 'purple'))
    final_cs = rbind(colour_scale, black_colour_scale, risk_colour_scale)
    final_cs$col_pop = paste0(final_cs$HIV_pop, '_', final_cs$risk_pop)
    thisdf$col_pop = factor(paste0(ifelse(thisdf$HIV_pop %in% c('HIV_diag_by_pop', 'HIV_prev'), 'all', thisdf$HIV_pop), '_', thisdf$risk_pop),
                        levels=rev(final_cs$col_pop))
  } else if(colour_strat == 'med'){
    legend_name = 'Medicare eligibility'
    final_cs = data.frame(HIV_pop = 'all',
                          long = c('Total', 'Medicare eligible', 'Medicare ineligible'),
                          med_pop = c('tot', med_labs),
                          col = c('black', 'red', 'blue'))
    thisdf$col_pop = thisdf$med_pop
    final_cs$col_pop = final_cs$med_pop
    
    final_cs = subset(final_cs, col_pop %in% unique(thisdf$col_pop))
  } else if(colour_strat == 'prev'){
    legend_name = 'Population group'
    # thisdf$col_pop = paste(thisdf$HIV_pop, thisdf$risk_pop, thisdf$med_pop, sep='_')
    thisdf$col_pop = paste(thisdf$HIV_pop, thisdf$risk_pop, thisdf$med_pop, sep='_')
    final_cs = data.frame(col_pop = unique(thisdf$col_pop),
                          long = unique(thisdf$col_pop))
    # final_cs$col = rainbow(nrow(final_cs))
    final_cs$col = rep(rainbow(4), 3)
  }
  
  thisdf = subset(thisdf, t >= plot_years[1] & t <= plot_years[2])

  # initialise plot
  p = ggplot(subset(thisdf, plot != 'num_cascade'), aes(x=t, group=col_pop, colour=col_pop, fill=col_pop))
  p = p + facet_wrap(.~plot, scales="free", ncol=2, labeller = labeller(plot = setNames(plot_long, plot_keys)))
  
  # plot information
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3) # data points
  
  if(colour_strat == 'prev'){
    p = p + geom_path(aes(y = model, linetype = med_pop), na.rm=T, lwd=1.3, alpha=0.5) # best estimate line
  } else {
    p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3) # best estimate line
  }
  
  # p = p + geom_line(aes(y = model), na.rm=T, lwd=1.3) # best estimate line
  if('lower_ci' %in% names(thisdf)){
    # p = p + geom_ribbon(data=subset(thisdf, med_pop %in% med_labs & is.finite(lower_ci)), aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=T) # 95% confidence interval
    p = p + geom_ribbon(data=subset(thisdf, is.finite(lower_ci) & plot != 'num_cascade'),
                        aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=T) # 95% confidence interval
  }
  if(colour_strat == 'cascade'){
    p = p + geom_area(data=subset(thisdf, plot=='num_cascade'), aes(y = model), alpha=0.8, na.rm=T) # stacked care cascade
  }
  
  # define axes scales
  # if(nrow(max_df) > 0){
  #   p = p + geom_blank(data=max_df, aes(y=upperlim), inherit.aes = F)
  # }
  p = p + scale_x_continuous(breaks = label_years,
                             name = 'Year',
                             limits = c(min(thisdf$t), max(thisdf$t)),
                             expand = c(0, 0))
  # p = p + scale_y_continuous(expand = c(0,0))
  p = p + scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  p = p + expand_limits(y = 0)
  
  # guides / legend
  p = p + guides(colour = guide_legend(override.aes = list(
    linetype = 1,
    shape = NA,
    # fill = NA,
    colour = NA,
    alpha = 1
  )))
  # p = p + scale_colour_manual(na.value = 'black',
  #                             limits = colour_scale$HIV_pop,
  #                             breaks = colour_scale$HIV_pop,
  #                             values = colour_scale$col,
  #                             labels = colour_scale$long,
  #                             aesthetics = c('colour', 'fill'),
  #                             name = 'Care cascade')
  p = p + scale_colour_manual(na.value = NA,
                              limits = final_cs$col_pop,
                              values = final_cs$col,
                              # breaks = colour_scale$HIV_pop,
                              # labels = colour_scale$long,
                              breaks = final_cs$col_pop[final_cs$long != 'na'],
                              labels = final_cs$long[final_cs$long != 'na'],
                              aesthetics = c('colour', 'fill'),
                              name = legend_name)
  
  # p = p + coord_cartesian(xlim = c(min(thisdf$t), max(thisdf$t)))
  p = p + coord_cartesian(xlim = plot_years)
  
  # themes
  p = p + theme_all
  # p = p + theme(legend.justification = c(0.5, 0.5),
  #               # legend.position = c(1/4, 3/8 - 0.04))
  #               legend.position = c(3/4, 1/6 - 0.02))
  # p = p + theme(legend.justification = c(0, 0.5),
  #               legend.position = c(1, 0.5))
  p = p + theme(legend.position = 'right')
  
  
  # add percentages
  if(colour_strat == 'cascade'){
    perc_axes = c('2-1', '2-2')
  } else if(colour_strat == 'med'){
    perc_axes = c('2-1', '2-2')
  } else {perc_axes = NULL}
  if(!is.null(perc_axes)){
    p = convert_axis(p, paste0('axis-l-', perc_axes))
  }
  
  return(p)
}

randomise_within_bounds = function(v, lb, ub){
  # if(lb < v & v < ub){
  #   lbp = v - lb
  #   ubp = 1 / (ub - v)
  #   if(runif(1) < lbp / (lbp + ubp)){
  #     y = runif(1, min=lb, max=v)
  #   } else {
  #     y = runif(1, min=v, max=ub)
  #   }
  # } else {
  #   y = runif(1, min=lb, max=ub)
  # }
  # return(y)
  if(lb > ub){
    return(v)
  } else if(lb > 0){
    return(exp(runif(1, log(lb), log(ub))))
  } else {
    return(runif(1, lb, ub))
  }
  
}

randomise_keys = function(statkeys=static_pars, timepars=baselist, basekeyindex=c(), basevar=0.05, syear=0){
  
  for(ind in names(basekeyindex)){
    columns = basekeyindex[[ind]]
    tv = rownames(timepars[[ind]])
    multyears = (as.numeric(tv) >= syear)
    
    n_var = length(columns)
    
    if(ind %in% prop_pars_index){
      ub = 1
    } else {
      ub = Inf
    }
    
    v = gen_var(var=basevar, n=n_var)
    timepars[[ind]][multyears,columns] = timepars[[ind]][multyears,columns] * v
    timepars[[ind]][timepars[[ind]] > ub] = ub
  }
  
  return(list(timepars, statkeys))
}

run_trial = function(timepars=baselist, basevar=0.05, y0=NULL, tvec=tvec_base, options=options, syear=0){
  
  basekeys = c(cal_pars, 't_testing')
  if(syear != 0){
    basekeys = basekeys[!grepl('init', basekeys)]
  }
  basekeyindex = setNames(rep(list(NA), length(basekeys)), basekeys)

  for(i in 1:length(basekeys)){
    ind = basekeys[i]
    if(!is.matrix(timepars[[ind]])){
      timepars[[ind]] = matrix(timepars[[ind]], ncol=1, nrow=length(tvec_base))
      rownames(timepars[[ind]]) = tvec_base
    }
    if(is.na(basekeyindex[[ind]])){
      basekeyindex[[ind]] = 1:ncol(timepars[[basekeys[[i]]]])
    }
  }

  this_stat_pars = randomise_keys(timepars=timepars, basekeyindex = basekeyindex, basevar=basevar, syear=syear)
  
  mpars = this_stat_pars[[1]]
  for(ind in basekeys){
    if(nrow(unique(mpars[[ind]])) == 1){
      mpars[[ind]] = mpars[[ind]][1,]
    }
  }
  
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

summarise_trials = function(df, value='value', lbfunc=lower_ci, ubfunc=upper_ci, conf_level=0.95){
  # med_count = 0
  # med_exclude = c()
  # for(i_med in 1:length(med_labs)){
  #   med_values = subset(df, med_pop == med_labs[i_med])$value
  #   if(sum(is.finite(med_values) & med_values != 0) > 0){
  #     med_count = med_count + 1
  #   } else {
  #     med_exclude = c(med_exclude, med_labs[i_med])
  #   }
  # }
  # 
  # if(med_count == 1){
  #   df = subset(df, !(med_pop %in% c(med_exclude, 'tot'))_)
  # }
  
  if(!medicare_ineligible){
    df = subset(df, !(med_pop %in% med_labs))
  }
  
  df = df %>%
    group_by_at(vars(-!!sym(value), -trial)) %>%
    summarise(smean = mean(!!sym(value), na.rm = TRUE),
              ssd = sd(!!sym(value), na.rm = TRUE),
              count = n(),
              lower_ci = quantile(!!sym(value), probs=(1-conf_level)/2, na.rm=TRUE),
              upper_ci = quantile(!!sym(value), probs=1 - (1-conf_level)/2, na.rm=TRUE),
              value = first(!!sym(value))) %>%
    ungroup() %>% 
    mutate(t = as.numeric(t)) %>% 
    as.data.frame()
  
  return(df)
}

# find confidence interval
ci_df = function(ntrials=5, timepars=baselist, basevar=0.05, y0=NULL, tvec=tvec_base, options=list(), syear=0){
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