
gen_scenarios = function(base_df=NULL, scenarios = scenarios, ntrials=0, variance=base_variance){
  
  dat = all_dat
  dat$scen = 'data'
  dat$scen_short = 'data'
  dat$scen_long = 'Data'
  dat = widen_sources(dat)
  
  scen_df = data.frame()
  if(is.null(base_df)){
    if(base_uncertainty){
      base_df = ci_df(ntrials, basevar=variance, options=list('keep_static'=TRUE))
    } else {
      base_df = run_model(y0=y0, tvec=tvec, modelpars=timepars, options=options)
      base_df = extr(base_df, plot_keys)
    }
      
    base_df = widen_sources(base_df)
    base_df$scen = 'base'
    base_df$scen_long = 'Base'
  }
  scen_df = rbind.fill(scen_df, base_df)
  scen_df$scen_short = 'base'
  
  # base_interval_df = subset(base_df, t >= split_year - 1 & t < split_year)
  base_interval_df = subset(base_df, t == split_year - 1 & scen == 'base')
  base_interval_df$lower_ci = base_interval_df$model
  base_interval_df$upper_ci = base_interval_df$model
  
  dat = subset(dat, med_pop %in% unique(scen_df$med_pop))
  
  scen_df = rbind.fill(dat, scen_df)
  
  for(s in scenarios){
    thisscen = load_time_par_sheet(s$sheet, deflist = baselist, syear=split_year)
    
    this_scen_trials = ci_df(ntrials=ntrials, timepars=thisscen, basevar=variance, y0=y0_split, tvec=tvec_split, options=list('keep_static'=TRUE), syear=split_year)
    
    thisdf = this_scen_trials
    thisdf = widen_sources(thisdf)
    thisdf = rbind.fill(base_interval_df, thisdf)
    thisdf$scen = s$short
    thisdf$scen_short = s$short
    thisdf$scen_long = s$long
    
    scen_df = rbind.fill(scen_df, thisdf)
  }
  
  
  # diffscens = thisscen[laply(names(thisscen), function(x) !identical(thisscen[[x]], baselist[[x]]))]
  
  scen_df$scen = factor(scen_df$scen, levels = rev(unique(scen_df$scen)), labels = rev(unique(scen_df$scen_long)))
  
  return(scen_df)
  
}

scen_update = function(this_df){

  this_df = subset(this_df, plot %in% scen_keys)
  this_df$plot = factor(this_df$plot, levels=scen_keys)
  this_df = subset(this_df, plot != 'care_cascade' | HIV_pop == 'num_diag')
  return(this_df)
}

plot_scens = function(this_df, base_uncertainty=F){
  
  scen_colours = c('black', 'red', 'green')
  
  this_df = scen_update(this_df)
  if(!medicare_ineligible){this_df = subset(this_df, !(med_pop %in% med_labs))}
  
  if(!base_uncertainty){
    baserows = this_df$scen == 'Base'
    this_df[baserows, c('lower_ci', 'upper_ci')] = this_df$model[baserows]
  }
  
  max_df = max_df_base[max_df_base$plot %in% this_df$plot,]
  max_df$plot = factor(max_df$plot)
  
  this_df = subset(this_df, t >= plot_years[1] & t <= plot_years[2])
  
  # initialise plot
  p = ggplot(this_df, aes(x=t, group=scen, colour=scen, fill=scen))
  p = p + facet_wrap(.~plot, scales="free", ncol=4, labeller = labeller(plot = setNames(plot_long, plot_keys)))
  
  # plot information
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3)
  p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3)
  p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=F)
  
  # define axes scales
  # p = p + geom_blank(data=max_df, aes(y=upperlim), inherit.aes = F)
  p = p + scale_x_continuous(breaks = label_years,
                             name = 'Year',
                             limits = c(min(this_df$t), max(this_df$t)),
                             expand = c(0, 0))
  # p = p + scale_y_continuous(expand = c(0,0))
  p = p + scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  p = p + expand_limits(y = 0)
  p = p + coord_cartesian(xlim = plot_years)
  
  scen_longs = unique(this_df$scen_long)
  
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
  p = p + theme(legend.position = 'right')
  
  # add percentages
  p = convert_axis(p, paste0('axis-l-', c('1-3', '1-4')))
  
  return(p)
}

