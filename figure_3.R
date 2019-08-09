## FIGURE 3 ##
# reset parameters
source("reset_pars.R", echo = F)
intervention=0
sensitivity=0

# create a common theme for fig 3
theme_fig_3 =
  theme_all +
  theme_font +
  theme(legend.text = element_text(size=10)) +
  theme(strip.text = element_text(size=11)) +
  theme(axis.title.y=element_text(size=11),
        axis.text = element_text(size=10)) +
  theme(plot.subtitle=element_text(size=15, face="bold", margin=margin(b=2)))

# run base scenario
prepare_plots(run_model(y0,tvec))
fig_3_sti_df = sti_df
fig_3_sti_df$scenario = factor("base")
fig_3_prev_df = prev_df
fig_3_prev_df$scenario = factor("base")

# run intervention scenarios and add to base output
intervention=1
for(i in 1:4){
  source("reset_pars.R", echo = F)
  int <<- letters[i]
  prepare_plots(run_model(y0_split, tvec_split))
  sti_df$scenario = factor(letters[i])
  fig_3_sti_df = rbind(fig_3_sti_df, sti_df)
  prev_df$scenario = factor(letters[i])
  fig_3_prev_df = rbind(fig_3_prev_df, prev_df)
}

# rename scenarios and groups
levels(fig_3_sti_df$scenario) = c("Base",
                                     "(a) Increased testing coverage \n for HIV-negative",
                                     "(b) Increased testing frequency \n for HIV-negative",
                                     "(c) Increased testing coverage \n and frequency for HIV-negative",
                                     "(d) Decreased testing for \nHIV-positive")
levels(fig_3_sti_df$HIV_group)[levels(fig_3_sti_df$HIV_group) == "HIV_minus"] = "HIV-negative GBM in Victoria"
levels(fig_3_sti_df$HIV_group)[levels(fig_3_sti_df$HIV_group) == "HIV_plus"] = "HIV-positive GBM in Victoria"
levels(fig_3_prev_df$prev_group)[levels(fig_3_prev_df$prev_group) == "sti_in_HIV_minus"] = "HIV-negative GBM in Victoria"
levels(fig_3_prev_df$prev_group)[levels(fig_3_prev_df$prev_group) == "sti_in_HIV_plus"] = "HIV-positive GBM in Victoria"

# converge scenarios
sti_temp_df = subset(fig_3_sti_df, sti_group=="incidence_sti" & year==split_year)
fig_3_sti_df[rownames(subset(sti_temp_df, scenario!="Base")),"value"] = fig_3_sti_df[rownames(subset(sti_temp_df, scenario=="Base")),"value"]
prev_temp_df = subset(fig_3_prev_df, year==split_year)
fig_3_prev_df[rownames(subset(prev_temp_df, scenario!="base")),"value"] = fig_3_prev_df[rownames(subset(prev_temp_df, scenario=="base")),"value"]

fig_3_sti_df$order = match(fig_3_sti_df$scenario, rev(levels(fig_3_sti_df$scenario)))

# find maximum values
fig_3_maxval_left = max(subset(fig_3_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti") & year<=2027)$value)
fig_3_maxval_right = max(subset(fig_3_prev_df, prev_group=="HIV-negative GBM in Victoria" | prev_group=="HIV-positive GBM in Victoria" & year<=2027)$value)


temptemp = subset(fig_3_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti"))

# create plots
fig_3_left = ggplot(data = subset(fig_3_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti")), aes(x=year, y=value, group=order, colour=scenario)) +
  geom_line(lwd=1.2) +
  labs(x=NULL, y="Estimated new syphilis infections in year", subtitle="Scenarios a-d", colour=NULL) +
  theme(legend.key.width = unit(1.5,'lines'),
        legend.key.height = unit(1.2,'lines'),
        legend.spacing.y = unit(0, "mm"),
        legend.justification = c(0,1),
        legend.position = c(0.535,1)) +
  theme_fig_3 +
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2009.5,2025.5),
                  ylim=c(0,2100))+#1.05*fig_3_maxval_left)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_rep_wrap(~ HIV_group, dir="h", repeat.tick.labels = TRUE) +
  scale_colour_manual(values=c("black", "blue", "darkgreen", "orange", "purple"))

fig_3_right = ggplot(data = subset(fig_3_prev_df, prev_group=="HIV-negative GBM in Victoria" | prev_group=="HIV-positive GBM in Victoria"), aes(x=year, y=value, group=scenario, colour=scenario)) +
  geom_line(lwd=1.2) +
  labs(x=NULL, y="Prevalence", subtitle="Scenarios a-d", colour=NULL) +
  guides(colour=FALSE) +
  theme_fig_3 +
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2009.5,2025.5),
                  ylim=c(0,2100))+#1.05*fig_3_maxval_right)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  facet_rep_wrap(~ prev_group,
                 dir="v", repeat.tick.labels = TRUE,
                 scale="free") +
  scale_colour_manual(values=c("black", "blue", "darkgreen", "orange", "purple"))

# combine plots and output pdf and png
fig_3 = ggarrange(fig_3_left, fig_3_right, nrow=1, ncol=2, common.legend=FALSE)
fig_3 = fig_3_left
#fig_3 = annotate_figure(fig_3, top = text_grob("Figure 3", face = "bold", size = 24))
ggsave("fig_3.pdf", plot=fig_3, width=plot_width, height=plot_height, units="mm")
ggsave("fig_3.png", plot=fig_3, width=plot_width, height=100, units="mm", dpi=500)
# browseURL("fig_3.pdf")
# browseURL("fig_3.png")
