scen_keys = c('PLHIV', 'HIV_diag', 'HIV_inf')

dat = all_dat
dat$scen = 'data'
dat = widen_sources(dat)

scen_df = extr(cal, scen_keys)
scen_df = widen_sources(scen_df)
scen_df$scen = 'base'
scen_df = rbind(scen_df, dat)

scen_sheet_names = c('timepars', 'scen_1', 'scen_2')
scen_short_names = c('base', 'no_prep', 'less_test')
scen_long_names = c('Base', 'Everyone stops using PrEP', 'Testing less often')

for(s in 2:3){
  thisscen = load_time_par_sheet(scen_sheet_names[s], deflist = baselist)
  thismodel = run_model(y0_base, modelpars=thisscen)
  thisdf = extr(thismodel, scen_keys)
  thisdf = widen_sources(thisdf)
  thisdf$scen = scen_short_names[s]
  thisdf$data = NA
  scen_df = rbind(scen_df, thisdf)
}

scen_df$t = as.numeric(as.character(scen_df$t))
scen_df$scen = factor(scen_df$scen, levels = c('data', rev(scen_short_names)), labels = c('Data', rev(scen_long_names)))

p = plot_df(scen_df)
p = p + scale_colour_manual(limits = c('Data', scen_long_names), values=c('black', 'black', 'red', 'green'))
saveopen(p, 'scenarios', 'plots')
