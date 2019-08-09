# HIV_selection = as.factor(c("pop_HIV"))
HIV_selection = as.factor(c("S1", "S2", "HIV_plus"))
# sti_selection = as.factor(c("S_sti", "E_sti", "Sy_sti", "ASy_sti", "T_sti", "incidence_sti"))
sti_selection = as.factor(c("incidence_sti"))

all_df = SID_base[[6]]
all_df_sub = all_df
all_df_sub = all_df_sub[all_df_sub$HIV_stat %in% HIV_selection,]
all_df_sub = all_df_sub[all_df_sub$sti_stat %in% sti_selection,]
# all_df_sub = subset(all_df_sub, testing_stat=="all")

all_df_sub$sti_stat = factor(all_df_sub$sti_stat, levels = levels(all_df$sti_stat))
all_df_sub$HIV_stat = factor(all_df_sub$HIV_stat, levels = levels(all_df$HIV_stat))



facet_plot = ggplot(all_df_sub, aes(x=year, y=value, colour=risk_stat)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,NA)) +
  theme(panel.spacing=unit(0.5,"mm"),
        panel.background=element_rect(fill="grey95", colour=NA),
        panel.grid=element_blank()) +
  facet_grid(HIV_stat~sti_stat,
             scales="free",
             switch="y",
             drop=TRUE)

ggsave("facet_plot.pdf", plot=facet_plot, width = 8, height=8)
browseURL("facet_plot.pdf")