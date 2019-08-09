## FIGURE 2 ##
# reset parameters
source("reset_pars.R", echo = F)
intervention=0
sensitivity=0
heat_scen_q=0
plot_gel_q=0
gel_up_heat=0
gel_down_heat=0

# create a common theme for fig 2
theme_fig_2 =
  theme_all +
  theme(text = element_text(size=11))  +
  theme(plot.title=element_text(size=11, face="bold", margin=margin())) +
  theme(plot.subtitle=element_text(margin=margin(t=3, b=0.2))) +
  theme(legend.position = c(0.01,1),
        legend.justification = c(0,1),
        legend.text = element_text(margin = margin(l=0, r=12)),
        legend.title = element_text(margin=margin(), vjust=0)) +
  theme(axis.title.y=element_text(size=11))
  
# run base scenario
prepare_plots(run_model(y0,tvec))

# add real data
y_sti_df = rbind.fill(y_sti_df, sti_main_data)
sti_df = rbind.fill(sti_df, sti_main_data)

# rename HIV group names
y_sti_df$HIV_group = factor(y_sti_df$HIV_group)
levels(y_sti_df$HIV_group) = c("HIV-", "HIV+", "All")
sti_df$HIV_group = factor(sti_df$HIV_group)
levels(sti_df$HIV_group) = c("HIV-", "HIV+", "All", "Not on PrEP", "On PrEP")

maxval_fig_2_left = max(subset(sti_df, sti_group=="diagnoses_sti" & (sti_df$HIV_group %in% c("HIV-", "HIV+")) & year<=2025)$value, na.rm=T)

# diagnoses plot
fig_2_left =
  ggplot(data = subset(sti_df, sti_group=="diagnoses_sti" & (HIV_group=="HIV-" | HIV_group=="HIV+")), aes(x=year, group=HIV_group, colour=HIV_group)) +
  geom_line(data = subset(sti_df, N=="model" & sti_group=="diagnoses_sti" & (HIV_group=="HIV-" | HIV_group=="HIV+")), aes(y=value, group=HIV_group, colour=HIV_group),
            lwd=1.6, alpha=0.75) +
  geom_point(data = subset(sti_df, N=="data" & sti_group=="diagnoses_sti"), aes(y=value, group=HIV_group, colour=HIV_group, shape=HIV_group), na.rm=TRUE,
            size=2.2) +
  labs(x=NULL, y="Notifications among GBM in year",
       colour="HIV Group  ", shape="HIV Group  ",
       title="Syphilis notifications among Victorian GBM by HIV status", subtitle="Data versus model") +
  scale_x_continuous(breaks = seq(2005, 2030, by = 1), expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_cartesian(xlim=c(2009.5,2017.5), ylim=c(0,600)) + #maxval_fig_2_left*1.05)) +
  theme_fig_2 +
  theme(axis.title.y = element_text(margin = margin(r=8))) +
  # theme(legend.direction = "vertical") +
  # theme(legend.key = element_rect(size=0, fill=NA, colour=NA)) +
  # theme(legend.key.width = unit(8, "mm")) +
  theme(legend.spacing.x = unit(0.2, "mm")) +
  scale_colour_manual(values=c("blue", "red"))

# prevalence plot
fig_2_right =
  ggplot(data = subset(y_sti_df, HIV_group=="HIV-" | HIV_group=="HIV+"), aes(x=year, y=value, group=HIV_group, colour=HIV_group)) +
  geom_line(data = subset(y_sti_df, N=="model" & sti_group=="sti_plus" & (HIV_group=="HIV-" | HIV_group=="HIV+")), aes(x=year, y=value /
                                                                                                                                     subset(y_sti_df, N=="model" & sti_group=="pop_sti" & (HIV_group=="HIV-" | HIV_group=="HIV+"))$value,
                                                                                                                                   group=HIV_group, colour=HIV_group),
            lwd=1.6, alpha=0.75) +
  geom_point(data = subset(y_sti_df, N=="data" & sti_group=="prev_sti" & (HIV_group=="HIV-" | HIV_group=="HIV+")), aes(x=year, y=value, group=HIV_group, colour=HIV_group, shape=HIV_group), na.rm=TRUE,
             size=2.2) +
  labs(x=NULL, y="Prevalence",
       colour="HIV Group  ", shape="HIV Group  ",
       title="Syphilis prevalence among\nVictorian GBM by HIV status", subtitle="Data versus model") +
  scale_x_continuous(breaks = seq(2005, 2030, by = 5), expand=c(0,0)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  coord_cartesian(xlim=c(2009.5,2025.5), ylim=c(0,0.1)) +
  theme_fig_2 +
  scale_colour_manual(values=c("blue", "red"))

# combine plots and output pdf and png
fig_2 = ggarrange(fig_2_left, fig_2_right, nrow=1, ncol=2, common.legend=F)
fig_2 = fig_2_left
#fig_2 = annotate_figure(fig_2, top = text_grob("Figure 2", face = "bold", size = 24))
ggsave("fig_2.pdf", plot=fig_2, width=120, height=100, units="mm")
ggsave("fig_2.png", plot=fig_2, width=120, height=100, units="mm", dpi=500)
# browseURL("fig_2.pdf")
# browseURL("fig_2.png")
