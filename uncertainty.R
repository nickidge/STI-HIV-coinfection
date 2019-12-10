
trials_df = ci_df(5, basevar=0.2)

cal_wide = widen_sources(all_dat, extr(cal, scen_keys))

p = plot_df(cal_wide)

p = p + geom_ribbon(data=trials_df, aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA)
saveopen(p, 'uncertainty', 'plots')


