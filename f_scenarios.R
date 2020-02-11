
gen_scenarios = function(scen_df=NULL, scenarios = list(), ntrials=3, basevar=0.2){
  
  dat = all_dat
  dat$scen = 'data'
  dat$scen_long = 'Data'
  dat = widen_sources(dat)
  
  
  if(is.null(scen_df)){
    if(base_uncertainty){
      scen_df = ci_df(ntrials, basevar=basevar, options=list('keep_static'=TRUE))
    } else {
      scen_df = run_model(y0=y0, tvec=tvec, modelpars=timepars, options=options)
      scen_df = extr(scen_df, plot_keys)
      # scen_df$trial=0
    }
      
    scen_df = widen_sources(scen_df)
    scen_df$scen = 'base'
    scen_df$scen_long = 'Base'
  }
  
  scen_df = rbind.fill(dat, scen_df)
  
  for(s in scenarios){
    thisscen = load_time_par_sheet(s$sheet, deflist = baselist, syear=split_year+1)
    
    this_scen_trials = ci_df(ntrials=ntrials, timepars=thisscen, basevar=basevar, options=list('keep_static'=TRUE), syear=split_year+1)
    # this_scen_trials = ci_df(ntrials=5, timepars=thisscen, basevar=0.1, tvec=tvec_split, y0=y0_split, options=list('split'=TRUE, 'keep_static'=TRUE), syear=split_year+1)
    
    thisdf = this_scen_trials
    thisdf = widen_sources(thisdf)
    thisdf$scen = s$short
    thisdf$scen_long = s$long
    
    scen_df = rbind.fill(scen_df, thisdf)
  }
  
  # diffscens = thisscen[laply(names(thisscen), function(x) !identical(thisscen[[x]], baselist[[x]]))]
  
  scen_df$scen = factor(scen_df$scen, levels = rev(unique(scen_df$scen)), labels = rev(unique(scen_df$scen_long)))
  
  return(scen_df)
  
}

plot_scens = function(df, base_uncertainty=F){
  
  scen_colours = c('black', 'red', 'green')
  
  scen_keys = c('PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'num_diag')
  if(!base_uncertainty){
    baserows = df$scen == 'Base'
    df[baserows, c('lower_ci', 'upper_ci')] = df$model[baserows]
  }
  df = subset(df, HIV_pop %in% scen_keys & plot != 'HIV_prev_by_risk')
  df = subset(df, plot != 'num_cascade')
  df = subset(df, med_pop %nin% med_labs)

  # df$plot[df$HIV_pop == 'prop_diag'] = 'prop_diag'
  
  # max_df = df %>%
  #   group_by(scen, plot) %>%
  #   filter(min(plot_years) <= t & t <= max(plot_years)) %>% 
  #   summarise(max = max(model, data, na.rm=T)) %>%
  #   # mutate(upperlim = ifelse(plot %in% c('HIV_prev', 'care_cascade'), 1, 1.1 * max)) %>% 
  #   mutate(upperlim = ifelse(plot %in% c('care_cascade'), 1, 1.1 * max)) %>% 
  #   as.data.frame()
  max_df = max_df_base[max_df_base$plot %in% df$plot,]
  max_df$plot = factor(max_df$plot)
  
  # initialise plot
  p = ggplot(df, aes(x=t, group=scen, colour=scen, fill=scen))
  p = p + facet_wrap(.~plot, scales="free", ncol=2, labeller = labeller(plot = setNames(plot_long, plot_keys)))
  
  # plot information
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3)
  p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3)
  p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA)
  
  # define axes scales
  p = p + geom_blank(data=max_df, aes(y=upperlim), inherit.aes = F)
  p = p + scale_x_continuous(breaks = label_years,
                             name = 'Year',
                             limits = c(min(df$t), max(df$t)),
                             expand = c(0, 0))
  p = p + scale_y_continuous(expand = c(0,0))
  p = p + expand_limits(y = 0)
  p = p + coord_cartesian(xlim = plot_years)
  
  scen_longs = unique(df$scen_long)
  
  # guides / legend
  p = p + guides(colour = guide_legend(override.aes = list(
    linetype = c(0, 1, 1, 1),
    shape = c(19, NA, NA, NA),
    fill = NA,
    alpha = 1,
    size = c(2, 1, 1, 1)
  )))
  p = p + scale_colour_manual(limits = scen_longs,
                              name='Scenarios',
                              values=c('black', scen_colours),
                              aesthetics = c("colour", "fill"))
  
  # themes
  p = p + theme_all
  p = p + theme(legend.justification = c(0.5, 0.5),
                legend.position = c(3/4, 1/6))
  
  # add percentages
  p = convert_axis(p, c('axis-l-3-1', 'axis-l-2-2'))
  
  return(p)
}

