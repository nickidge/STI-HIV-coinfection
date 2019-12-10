debug_check = 1
debug_val = 0
debug_let = 0
intervention=0
sensitivity=0


prepare_plots(run_model(y0,tvec))
debug_syph_df = syph_df
debug_syph_df$scenario = factor("base")


SID_debug = list()
for(i in 1:6){
  debug_let = letters[i]
  # prepare_plots(run_model(y0, tvec))
  prepare_plots(run_model(y0_split, tvec0[which(tvec0>=split_year)]))
  syph_df$scenario = factor(debug_let)
  debug_syph_df = rbind(debug_syph_df, syph_df)
}


debug_syph_df = subset(debug_syph_df, HIV_group=="pop_HIV" & syph_group=="incidence_syph")[,c("year", "scenario", "value")]


debug_plot = ggplot(data = debug_syph_df, aes(x=year, y=value, colour=scenario)) +
  geom_line() +
  labs(x=NULL, y="New syphilis infections among Victorian GBM in year",
       title="Sensitivity to PrEP coverage",
       linetype=NULL) +
  theme(legend.key.size=unit(1.5,"line"),
        legend.title=element_blank(),
        plot.title=element_text(face="bold"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025), ylim=c(0,5000))

pdf("debug_plot.pdf", onefile=TRUE, width=14, height=8)
invisible(print(debug_plot))
dev.off()
# browseURL("debug_plot.pdf")




debug_check = 0
debug_val = 0
