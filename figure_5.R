## FIGURE 5 ##
# reset parameters
source("reset_pars.R", echo = F)
intervention=0
sensitivity=0

# run base scenario
prepare_plots(run_model(y0,tvec))
fig_5_sti_df = sti_df
fig_5_sti_df$scenario = factor("base")

# prepare scenario parameters
sensitivity=1
unc=0

# run h scenario (PrEP)
int="h"
prepare_plots(run_model(y0_split, tvec_split))
sti_df$scenario = factor("PrEP")
fig_5_sti_df = rbind(fig_5_sti_df, sti_df)

# create a common theme for fig 5
theme_fig_5 =
  theme_all +
  theme_font +
  theme(plot.title=element_text(size=11)) +
  theme(legend.text=element_text(size=10),
        legend.key.height = unit(4.5, "mm"),
        legend.title=element_blank()) +
  theme(axis.text = element_text(size=9.5),
        axis.ticks.length = unit(1, "mm"),
        axis.title.y = element_text(size=10))

# prepare a list of SID outputs for debugging
SID_debug = list()

# # reset variable
# int = 0

# run scenarios
for(i in c("1a", "1b", "2a", "2b", "3a", "3b", "4a")){
  source("reset_pars.R", echo = F)
  unc <- i
  SID_debug[[unc]] = run_model(y0_split, tvec0[which(tvec0>=split_year)])
  prepare_plots(SID_debug[[unc]])
  sti_df$scenario = factor(unc)
  fig_5_sti_df = rbind(fig_5_sti_df, sti_df)
}

# only keep relevant data
fig_5_sti_df = subset(fig_5_sti_df, HIV_group=="pop_HIV" & sti_group=="incidence_sti")[,c("year", "scenario", "value")]

# converge scenarios at split year (so that the lines all meet)
sti_temp_df = subset(fig_5_sti_df, year==split_year)
fig_5_sti_df[rownames(subset(sti_temp_df, scenario!="base")),"value"] = fig_5_sti_df[rownames(subset(sti_temp_df, scenario=="base")),"value"]

# calculate maximum values for y-axis scale
fig_5_maxval = max(subset(fig_5_sti_df, year <= 2024)$value)

p_fig_5_legend = ggplot(data=subset(fig_5_sti_df, (fig_5_sti_df$scenario %in% c("base", "PrEP"))), aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  geom_line(lwd=1.1) +
  theme(legend.text=element_text(size=10),
        legend.key.height = unit(4.5, "mm"),
        legend.key.width=unit(1.5,"line"),
        legend.key = element_blank(),
        legend.title=element_text(face="bold"),
        plot.title=element_text(face="bold"),
        legend.position = "left") +
  guides(linetype=guide_legend(override.aes=list(fill=NA))) +
  scale_linetype_manual(name="Common scenarios", values=c('solid', 'dashed'),
                        labels = c("Base (no PrEP)", "PrEP (scenario h)")) +
  scale_colour_manual(name="Common scenarios", values=c("black", "black"),
                      labels = c("Base (no PrEP)", "PrEP (scenario h)"))

l_fig_5 = gtable_filter(ggplot_gtable(ggplot_build(p_fig_5_legend)), "guide-box")

# create plots
fig_5_topleft = ggplot(data = subset(fig_5_sti_df, scenario=="base" | scenario=="PrEP" | scenario=="1a" | scenario=="1b"), aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  geom_line(lwd=1.1) +
  labs(x=NULL,
       # y=NULL,
       y="Syphilis infections among\nVictorian GBM in year",
       title="Sensitivity to PrEP coverage",
       linetype=NULL) +
  theme(legend.key.width=unit(1.5,"line"),
        plot.title=element_text(face="bold"),
        legend.justification = c(1,0),
        legend.position = c(1,0)) +
  theme_fig_5 +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025), ylim=c(0,1.1*fig_5_maxval)) +
  scale_linetype_manual(name="guide_topleft", values=c('solid', 'dashed', 'solid', 'dashed'),
                        breaks = c("1a", "1b"),
                        labels = c("Base (no PrEP)", "PrEP (scenario h)", "PrEP among 10%", "PrEP among 30%")[-(1:2)]) +
  scale_colour_manual(name="guide_topleft", values=c("black", "black", "blue", "blue"),
                      breaks = c("1a", "1b"),
                      labels = c("Base (no PrEP)", "PrEP (scenario h)", "PrEP among 10%", "PrEP among 30%")[-(1:2)])

fig_5_topright = ggplot(data = subset(fig_5_sti_df, scenario=="base" | scenario=="PrEP" | scenario=="2a" | scenario=="2b"), aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  geom_line(lwd=1.1) +
  labs(x=NULL,
       # y=NULL,
       y="Syphilis infections among\nVictorian GBM in year",
       title="Sensitivity to PrEP condom use",
       linetype=NULL) +
  theme(legend.key.width=unit(1.5,"line"),
        plot.title=element_text(face="bold"),
        legend.justification = c(1,0),
        legend.position = c(1,0)) +
  theme_fig_5 +
  theme(legend.key.height=unit(8.6, "mm")) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025), ylim=c(0,1.1*fig_5_maxval)) +
  scale_linetype_manual(name="guide_topright", values=c('solid', 'dashed', 'solid', 'dashed'),
                        breaks = c("2a", "2b"),
                        labels = c("Base (no PrEP)", "PrEP (scenario h)", "Condom use reduced by \n 25% when on PrEP", "Condom use reduced by \n 75% when on PrEP")[-(1:2)]) +
  scale_colour_manual(name="guide_topright", values=c("black", "black", "red", "red"),
                      breaks = c("2a", "2b"),
                      labels = c("Base (no PrEP)", "PrEP (scenario h)", "Condom use reduced by \n 25% when on PrEP", "Condom use reduced by \n 75% when on PrEP")[-(1:2)])

fig_5_bottomleft = ggplot(data = subset(fig_5_sti_df, scenario=="base" | scenario=="PrEP" | scenario=="3a" | scenario=="3b"), aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  geom_line(lwd=1.1) +
  labs(x=NULL,
       # y=NULL,
       y="Syphilis infections among\nVictorian GBM in year",
       title="Sensitivity to sero discordant mixing",
       linetype=NULL) +
  theme(legend.key.width=unit(1.5,"line"),
        plot.title=element_text(face="bold"),
        legend.justification = c(1,0),
        legend.position = c(1,0)) +
  theme_fig_5 +
  theme(legend.key.height=unit(8.6, "mm")) +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025.5), ylim=c(0,1.1*fig_5_maxval)) +
  scale_linetype_manual(name="guide_bottomleft", values=c('solid', 'dashed', 'solid', 'dashed'),
                        breaks = c("3a", "3b"),
                        labels = c("Base (no PrEP)", "PrEP (scenario h)", "No increase in HIV sero \n discordant mixing when on PrEP", "Increase to 15% sero discordant \n mixing when on PrEP")[-(1:2)]) +
  scale_colour_manual(name="guide_bottomleft", values=c("black", "black", "darkgreen", "darkgreen"),
                      breaks = c("3a", "3b"),
                      labels = c("Base (no PrEP)", "PrEP (scenario h)", "No increase in HIV sero \n discordant mixing when on PrEP", "Increase to 15% sero discordant \n mixing when on PrEP")[-(1:2)])

fig_5_bottomright = ggplot(data = subset(fig_5_sti_df, scenario=="base" | scenario=="PrEP" | scenario=="4a"), aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  geom_line(lwd=1.1) +
  labs(x=NULL,
       # y=NULL,
       y="Syphilis infections among\nVictorian GBM in year",
       title="Sensitivity to testing when on PrEP",
       linetype=NULL) +
  theme(legend.key.width=unit(1.5,"line"),
        plot.title=element_text(face="bold"),
        legend.justification = c(1,0),
        legend.position = c(1,0)) +
  theme_fig_5 +
  scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025.5), ylim=c(0,1.1*fig_5_maxval)) +
  scale_linetype_manual(name="guide_bottomright", values=c('solid', 'dashed', 'solid'),
                        breaks = c("4a"),
                        labels = c("Base (no PrEP)", "PrEP (scenario h)", "No increase in testing \n when on PrEP")[-(1:2)]) +
  scale_colour_manual(name="guide_bottomright", values=c("black", "black", "purple"),
                      breaks = c("4a"),
                      labels = c("Base (no PrEP)", "PrEP (scenario h)", "No increase in testing \n when on PrEP")[-(1:2)])

# ggplot(data = subset(fig_5_sti_df, scenario=="PrEP" | scenario=="2a" | scenario=="2b"), aes(x=year, y=value, linetype=scenario)) +
#   geom_line() +
#   labs(x=NULL, y="New STI infections among Victorian GBM in year",
#        title="Sensitivity to PrEP condom use",
#        linetype=NULL) +
#   theme(legend.key.size=unit(1.5,"line"),
#         plot.title=element_text(face="bold"),
#         legend.justification = c(0, 1),
#         legend.position = c(0, 1)) +
#   scale_x_continuous(breaks = seq(2014, 2024, by = 2), expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   coord_cartesian(xlim=c(2014,2025)) +
#   scale_linetype_discrete(labels = c("PrEP (scenario h)", "Condom use reduced by \n 25% when on PrEP", "Condom use reduced by \n 75% when on PrEP"))

# combine plots and output pdf and png
fig_5 = ggarrange(fig_5_topleft, fig_5_topright, fig_5_bottomleft, fig_5_bottomright, nrow=2, ncol=2, common.legend=FALSE)
#fig_5 = annotate_figure(fig_5, top = text_grob("Figure 5", face = "bold", size = 24))

fig_5_all = arrangeGrob(fig_5, l_fig_5,
                        heights = unit.c(unit(plot_height, "mm"), unit(18, "mm")), ncol = 1)

ggsave("fig_5.pdf", plot=fig_5_all, width=plot_width, height=plot_height+18, units="mm")
ggsave("fig_5.png", plot=fig_5_all, width=plot_width, height=plot_height+18, units="mm", dpi=500)
# browseURL("fig_5.pdf")
# browseURL("fig_5.png")
