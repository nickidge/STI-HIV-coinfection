## 6-facet PLOT ##
# reset parameters

iss2=TRUE

source("reset_pars.R", echo = F)
# gel_up_heat=0
# gel_down_heat=0
# eff_gel_heat = eff_gel
is_gel=T
gel_mat = gel_mat_base
# gel_mat = gel_mat_best_estimate
prop_condom_s2 = rbind(
  (1-condom_by_HIV0[1,2:4])*(1-gel_mat[1,]),
  (1-condom_by_HIV0[1,2:4])*gel_mat[1,] + condom_by_HIV0[1,2:4]*gel_mat[2,],
  condom_by_HIV0[1,2:4]*(1-gel_mat[2,])
) * 100
dimnames(prop_condom_s2) = list(c("No protection", "Gel", "Condom"),
                                c("HIV-\nno PrEP", "HIV-\nPrEP", "HIV+\n"))
prop_condom_s2_df = setNames(melt(as.matrix(prop_condom_s2)), c("Protection", "HIV_status", "Prop"))

theme_plot_s2 = theme_plot_gel +
  theme(
    legend.direction="vertical",
    legend.background=element_blank(),
    # panel.background=element_rect(colour="black"),
    strip.background=element_rect(colour="black", fill="grey88"),
    text=element_text(size=10.5))


s2_df_list = df_prep(SID_base[[6]])
s2_inc_df = s2_df_list[[2]]
s2_prev_df = s2_df_list[[3]]

s2_inc_df$stat = factor(paste0(s2_inc_df$HIV_stat, ".", s2_inc_df$risk_stat))
s2_prev_df$stat = factor(paste0(s2_prev_df$HIV_stat, ".", s2_prev_df$risk_stat))

s2_prep_df = subset(s2_inc_df, HIV_stat=="HIV- PrEP")
s2_prep_df$value = s2_prep_df$value / subset(s2_inc_df, HIV_stat=="All")$value

s2_prep_prop_df = data.frame(year=as.numeric(names(prop_prep_interp)), prop=prop_prep_interp)

s2_inc_df = s2_inc_df[s2_inc_df$HIV_stat %nin% "All",]
# s2_inc_df = s2_inc_df[s2_inc_df$risk_stat %nin% "all",]
s2_prev_df = s2_prev_df[s2_prev_df$HIV_stat %nin% "All",]
# s2_prev_df = s2_prev_df[s2_prev_df$risk_stat %in% "all",]

s2_inc_df = s2_inc_df[s2_inc_df$risk_stat %nin% "all",]
s2_prev_df = s2_prev_df[s2_prev_df$risk_stat %nin% "all",]

# s2_inc_df = subset(s2_inc_df, (HIV_stat=="HIV- PrEP" & risk_stat!="all") | (HIV_stat!="HIV- PrEP" & risk_stat=="all"))
# s2_prev_df = subset(s2_prev_df, (HIV_stat=="HIV- PrEP" & risk_stat!="all") | (HIV_stat!="HIV- PrEP" & risk_stat=="all"))
# s2_inc_df$stat = factor(paste0(s2_inc_df$HIV_stat, ".", s2_inc_df$risk_stat), labels=c("HIV_minus", "HIV_PrEP_high", "HIV_PrEP_low", "HIV_plus"))
# s2_prev_df$stat = factor(paste0(s2_prev_df$HIV_stat, ".", s2_prev_df$risk_stat), labels=c("HIV_minus", "HIV_PrEP_high", "HIV_PrEP_low", "HIV_plus"))

s2_ribbon_df = spread(s2_prev_df[,colnames(s2_prev_df) %nin% "stat"], key=risk_stat, value=value)

# s2_inc_df$stat = factor(s2_inc_df$stat, levels=c(levels(s2_inc_df$stat)[grepl(".all", levels(s2_inc_df$stat))],
#                                                  levels(s2_inc_df$stat)[grepl(".low_risk", levels(s2_inc_df$stat))],
#                                                  levels(s2_inc_df$stat)[grepl(".high_risk", levels(s2_inc_df$stat))]))


# fig_s2_left = ggplot(s2_inc_df, aes(x=year, y=value, group=stat, fill=stat, colour=stat)) +
fig_s2_left = ggplot(s2_inc_df, aes(x=year, y=value, group=stat, fill=HIV_stat, alpha=risk_stat)) +
  scale_alpha_manual(limits = c("low_risk", "high_risk"),
                     values = c(1, 0.7),
                     labels = c("Low risk", "High risk"),
                     name = "Risk status") +
  scale_linetype_manual(limits = c("low_risk", "high_risk"),
                        values = c("solid", "dashed"),
                        labels = c("Low risk", "High risk"),
                        name = "Risk status") +
  # geom_line(size=1.6) +
  geom_area(position="stack") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  # scale_fill_discrete(breaks=c("HIV_minus", "HIV_PrEP_low", "HIV_PrEP_high", "HIV_plus"),
  #                     labels = c("HIV- (no PrEP)", "HIV- (low-risk PrEP)", "HIV- (high-risk PrEP)", "HIV+")) +
  # scale_colour_discrete(breaks=c("HIV_minus", "HIV_PrEP_low", "HIV_PrEP_high", "HIV_plus"),
  #                     labels = c("HIV- (no PrEP)", "HIV- (low-risk PrEP)", "HIV- (high-risk PrEP)", "HIV+")) +
  coord_cartesian(xlim=c(2007,end_year),
                  ylim=c(0,1.05*max(s2_df_list[[2]]$value))) +
  # coord_cartesian(xlim=c(2007,2031)) +
  labs(subtitle="Annual Notifications",
       fill = "HIV status",
       # colour = "HIV status",
       x="Year",
       y="Notifications among GBM in year") +
  theme_plot_s2
  # facet_wrap(~HIV_stat, ncol=1, scales="free_y")

fig_s2_right = ggplot(s2_prev_df, aes(x=year, y=value, group=stat, colour=HIV_stat, linetype=risk_stat, size=risk_stat)) +
  scale_linetype_manual(limits = c("low_risk", "high_risk"),
                     values = c("solid", "dashed"),
                     labels = c("Low risk", "High risk")) +
  scale_size_manual(limits = c("low_risk", "high_risk"),
                        values = c(1.5, 0.8),
                        labels = c("Low risk", "High risk")) +  # fig_s2_right = ggplot(s2_prev_df, aes(x=year, y=value, group=stat, colour=stat)) +
  geom_line(alpha=0.8) +
  geom_ribbon(inherit.aes=F, data=s2_ribbon_df, aes(x=year, ymin=low_risk, ymax=high_risk, fill=HIV_stat), alpha=0.1) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  coord_cartesian(xlim=c(2007,end_year),
                  ylim=c(0,1)) +
  labs(subtitle="Prevalence",
       x="Year",
       y="Prevalence") +
  theme_plot_s2
  # facet_wrap(~HIV_stat, ncol=1)

fig_s2_bottom_left = ggplot(prop_condom_s2_df, aes(x=HIV_status, y=Protection, fill=Prop, z=Prop)) +
  geom_tile() +
  geom_text(aes(label=round(Prop))) +
  theme_minimal() +
  # theme_bw() +
  theme(legend.position = "bottom") +
  geom_vline(xintercept=seq(-0.5, 3.5, by=1)) +
  scale_x_discrete(expand=c(0,0),
                   position="top") +
  scale_y_discrete(expand=c(0,0)) +
  labs(x=NULL, y=NULL, title="Condom usage (best estimate)") +
  guides(fill = guide_colourbar(ticks.colour=NA,
                                direction = "horizontal",
                                title.position = "top")) +
  scale_fill_gradient(expand=c(0,0),
                      limits=c(0,100),
                      low="white",
                      high="grey50")


fig_s2_bottom_right = ggplot(s2_prep_df, aes(x=year, y=value, colour=risk_stat)) +
  geom_line(size=1.2,
            # colour=muted("green"),
            alpha=0.8) +
  geom_point(data=data.frame(risk_stat="all", year=2016, value=0.18)) +
  geom_line(data=s2_prep_prop_df, aes(x=year, y=prop), inherit.aes=F,
            size=1, colour="black") +
  scale_colour_discrete(name = "Risk status",
                        limits = c("low_risk", "high_risk", "all"),
                        labels = c("Low risk", "High risk", "All")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0),
                     breaks=seq(0, 1, by=0.2)) +
  coord_cartesian(xlim=c(2007,end_year),
                  ylim=c(0,1)) +
  labs(subtitle="Proportion of cases attributable\nto PrEP",
       x="Year",
       y="Proportion of cases") +
  theme_plot_s2
# facet_wrap(~HIV_stat, ncol=1)

# combine plots and output pdf and png
fig_s2_top = ggarrange(fig_s2_left, fig_s2_right, nrow=1, ncol=2, common.legend=T, legend="bottom")
fig_s2_bottom = ggarrange(fig_s2_bottom_left, fig_s2_bottom_right, nrow=1, ncol=2, widths=c(1,2))
fig_s2 = ggarrange(fig_s2_top, fig_s2_bottom, nrow=2, ncol=1, heights=c(3,2), common.legend=F)
fig_s2 = annotate_figure(fig_s2, top = text_grob("Figure S2", face = "bold", size = 15))
ggsave("fig_s2.pdf", plot=fig_s2, width=170, height=190, units="mm")
ggsave("fig_s2.png", plot=fig_s2, width=170, height=190, units="mm", dpi=500)
browseURL("fig_s2.pdf")
# browseURL("fig_s2.png")

iss2=FALSE
is_gel=F
