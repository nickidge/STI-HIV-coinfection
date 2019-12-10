## FIGURE 4 ##
# reset parameters
source("reset_pars.R", echo = F)
intervention=0
sensitivity=0
# prop_prep_interp[]=0

# create a common theme for fig 4
theme_fig_4 =
  theme_all +
  theme_font +
  theme(text = element_text(size=11.5)) +
  theme(legend.text=element_text(size=13)) +
  theme(axis.text = element_text(size=10.3),
        axis.title.y = element_text(size=15))

# run base scenario
prepare_plots(run_model(y0,tvec))
fig_4_sti_df = sti_df
fig_4_sti_df$scenario = factor("base")
fig_4_prev_df = prev_df
fig_4_prev_df$scenario = factor("base")

# # run h scenario
# intervention=1
# int="h"

# run alt scenario
intervention = 1
int="alt"
prepare_plots(run_model(y0_split, tvec_split))

# sti_df = rbind(sti_df, subset(fig_4_sti_df[,1:5], year==split_year-1))
sti_df$scenario = factor("PrEP")
fig_4_sti_df = rbind(fig_4_sti_df, sti_df)
prev_df$scenario = factor("PrEP")
fig_4_prev_df = rbind(fig_4_prev_df, prev_df)

# rename groups & scenarios
# levels(fig_4_sti_df$HIV_group)[levels(fig_4_sti_df$HIV_group) == "HIV_minus"] = "HIV-"
# levels(fig_4_sti_df$HIV_group)[levels(fig_4_sti_df$HIV_group) == "HIV_plus"] = "HIV+"
levels(fig_4_prev_df$prev_group)[levels(fig_4_prev_df$prev_group) == "sti_in_HIV_minus"] = "HIV-"
levels(fig_4_prev_df$prev_group)[levels(fig_4_prev_df$prev_group) == "sti_in_HIV_plus"] = "HIV+"
# levels(fig_4_sti_df$scenario)[levels(fig_4_sti_df$scenario) == "base"] = "Base (no PrEP)"
# levels(fig_4_sti_df$scenario)[levels(fig_4_sti_df$scenario) == "h"] = "PrEP (scenario h)"
# levels(fig_4_prev_df$scenario)[levels(fig_4_prev_df$scenario) == "base"] = "Base (no PrEP)"
# levels(fig_4_prev_df$scenario)[levels(fig_4_prev_df$scenario) == "h"] = "PrEP (scenario h)"

# converge scenarios at split year (so that the lines all meet)
sti_temp_df = subset(fig_4_sti_df, sti_group=="incidence_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus") & year==split_year)
fig_4_sti_df[rownames(subset(sti_temp_df, scenario!="base")),"value"] = fig_4_sti_df[rownames(subset(sti_temp_df, scenario=="base")),"value"]

# calculate maximum values for y-axis scale
fig_4_maxval_left = max(subset(fig_4_sti_df, sti_group=="incidence_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus"))$value)
fig_4_maxval_right = max(subset(fig_4_prev_df, (prev_group=="HIV-" | prev_group=="HIV+"))$value)

# create plots
fig_4_left = ggplot(data = subset(fig_4_sti_df, sti_group=="incidence_sti" & (HIV_group=="HIV_minus" | HIV_group=="HIV_plus")), aes(x=year, y=value, colour=HIV_group, linetype=scenario)) +
  geom_line(lwd=1.3) +
  labs(x=NULL,y="Estimated new STI infections in year",
       title="Estimated STI incidence among Victorian GBM\nby HIV status", subtitle="PrEP scenario",
       colour=NULL, linetype=NULL) +
  theme(legend.box = 'horizontal',
        legend.text=element_text(size=13),
        legend.key.width=unit(1,"line"),
        legend.spacing.x=unit(1, "mm"),
        legend.justification = c(0, 1),
        legend.position = c(0.1, 1),
        axis.text=element_text(size=12),
        plot.title=element_text(face="bold")) +
  theme_fig_4 +
  scale_x_continuous(breaks = seq(2005, 2040, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2007,2027),
                  ylim=c(0,1.1*fig_4_maxval_left)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_colour_manual(values=c("blue", "red"),
                      labels=c("HIV-", "HIV+")) +
  scale_linetype_discrete(labels=c("Base (no PrEP)", "PrEP (scenario h)"))


fig_4_right = ggplot(data = subset(fig_4_prev_df, (prev_group=="HIV-" | prev_group=="HIV+")), aes(x=year, y=value, colour=prev_group, linetype=scenario)) +
  geom_line(lwd=1.3) +
  labs(x=NULL, y="Prevalence",
       title="Estimated STI prevalence among Victorian GBM\nby HIV status", subtitle="PrEP scenario",
       colour=NULL, linetype=NULL) +
  theme(legend.box = 'horizontal',
        legend.text=element_text(size=13),
        legend.key.width=unit(1,"line"),
        legend.spacing.x=unit(1, "mm"),
        legend.justification = c(0, 1),
        legend.position = c(0.1, 1),
        axis.text=element_text(size=12),
        plot.title=element_text(face="bold")) + 
  theme_fig_4 +
  scale_x_continuous(breaks = seq(2005, 2040, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2007,2027),
                  ylim=c(0,1.1*fig_4_maxval_right)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  scale_colour_manual(values=c("blue", "red")) +
  scale_linetype_discrete(labels=c("Base (no PrEP)", "PrEP (scenario h)"))

# combine plots and output pdf and png
fig_4 = ggarrange(fig_4_left, fig_4_right, nrow=1, ncol=2, common.legend=FALSE)
# fig_4 = annotate_figure(fig_4, top = text_grob("Figure 4", face = "bold", size = 24))
ggsave("fig_4.pdf", plot=fig_4, width = 14, height=8)
ggsave("fig_4.png", plot=fig_4, width = 14, height=8, dpi=500)
browseURL("fig_4.pdf")
# browseURL("fig_4.png")
