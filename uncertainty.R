
trials_df = ci_df(50, basevar=0.2)

data_wide = widen_sources(all_dat)

cal_wide = widen_sources(trials_df)
cal_wide = rbind.fill(cal_wide, data_wide)

p = plot_df(cal_wide)

# p = p + geom_ribbon(data=trials_df, aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA)
p = p + geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x=t), alpha=0.2, colour=NA, na.rm=T)
saveopen(p, 'uncertainty', 'plots')
