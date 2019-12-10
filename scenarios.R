scen_keys = c('PLHIV', 'HIV_diag', 'HIV_inf')

dat = all_dat
dat$scen = 'data'
dat = widen_sources(dat)

ntrials_scen = 10

scen_df = ci_df(ntrials_scen, basevar=0.2)
# scen_df = extr(cal, scen_keys)
scen_df = widen_sources(scen_df)
scen_df$scen = 'base'
scen_df = rbind.fill(scen_df, dat)

scen_sheet_names = c('timepars', 'scen_1', 'scen_2')
scen_short_names = c('base', 'no_prep', 'less_test')
scen_long_names = c('Base', 'Everyone stops using PrEP', 'Testing less often')

for(s in 2:3){
  thisscen = load_time_par_sheet(scen_sheet_names[s], deflist = baselist, syear=split_year+1)
  
  this_scen_trials = ci_df(ntrials=ntrials_scen, timepars=thisscen, basevar=0.1, options=list('keep_static'=TRUE), syear=split_year+1)
  # this_scen_trials = ci_df(ntrials=5, timepars=thisscen, basevar=0.1, tvec=tvec_split, y0=y0_split, options=list('split'=TRUE, 'keep_static'=TRUE), syear=split_year+1)
  
  # thismodel = run_model(y0_base, modelpars=thisscen)
  # thismodel = run_model(y0_split, tvec = tvec_split, options=list('split'=TRUE), modelpars=thisscen)
  
  thisdf = this_scen_trials
  thisdf = widen_sources(thisdf)
  thisdf$scen = scen_short_names[s]

  scen_df = rbind.fill(scen_df, thisdf)
}

# diffscens = thisscen[laply(names(thisscen), function(x) !identical(thisscen[[x]], baselist[[x]]))]

scen_df$t = as.numeric(as.character(scen_df$t))
scen_df$scen = factor(scen_df$scen, levels = c('data', rev(scen_short_names)), labels = c('Data', rev(scen_long_names)))

p = plot_df(scen_df)
p = p + scale_colour_manual(limits = c('Data', scen_long_names),
                            values=c('black', 'black', 'red', 'green'),
                            aesthetics = c("colour", "fill"))
p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA)

saveopen(p, 'scenarios', 'plots')
