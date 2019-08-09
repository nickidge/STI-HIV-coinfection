# reset parameters
source("reset_pars.R", echo = F)

intervention=0
sensitivity=1
heat_scen_q=0
plot_gel_q=1
gel_up_heat=0
gel_down_heat=0


sens_scenarios = rbind(
  c("base", "Base Scenario"),
  c("a", "30% PrEP by 2022"),
  c("b", "60% gel effectiveness, 50/50 changes"),
  c("c", "Testing every 60 days for GBM on PrEP"),
  c("d", "Risk factor of 15 for higher risk group"),
  c("e", "Asymptomatic cases test 10x less often"),
  c("f", "Mixing goes up to 50% in all groups")
)
colnames(sens_scenarios) = c("scenario_cat", "scenario_long")

theme_plot_sens = theme_plot_gel +
  theme(
    legend.direction="vertical",
    legend.position=c(1,0),
    legend.justification=c(1,0),
    legend.key.height=unit(4.5,"mm"),
    text=element_text(size=10.5))

sens_df_list = df_prep(SID_base[[6]])

sens_inc_df = sens_df_list[[2]]
sens_prev_df = sens_df_list[[3]]
sens_inc_df$scenario = factor("base")
sens_prev_df$scenario = factor("base")

for(i_scen in 2:nrow(sens_scenarios)){
  sens_scen = sens_scenarios[i_scen,1]
  sens_df_list_temp = df_prep(run_model(y0_split, tvec_split)[[6]])
  sens_inc_df_temp = sens_df_list_temp[[2]]
  sens_inc_df_temp = sens_inc_df_temp[sens_inc_df_temp$risk_stat %in% "all",]
  sens_prev_df_temp = sens_df_list_temp[[3]]
  sens_inc_df_temp = rbind(sens_inc_df_temp, subset(sens_inc_df, year==split_year-1))
  sens_inc_df_temp$scenario = factor(sens_scen)
  sens_prev_df_temp$scenario = factor(sens_scen)
  sens_inc_df = rbind(sens_inc_df, sens_inc_df_temp)
  sens_prev_df = rbind(sens_prev_df, sens_prev_df_temp)
}


sens_inc_df = sens_inc_df[sens_inc_df$risk_stat %in% "all",]
sens_prev_df = sens_prev_df[sens_prev_df$risk_stat %in% "all",]
sens_inc_df = sens_inc_df[sens_inc_df$HIV_stat %in% "All",]
sens_prev_df = sens_prev_df[sens_prev_df$HIV_stat %in% "All",]
sens_inc_df$order = match(sens_inc_df$scenario, rev(sens_scenarios[,1]))
sens_prev_df$order = match(sens_prev_df$scenario, rev(sens_scenarios[,1]))


fig_sens_left = ggplot(sens_inc_df, aes(x=year, y=value, group=order, colour=scenario)) +
  geom_line(size=1.6) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_colour_discrete(breaks=sens_scenarios[,1],
                        labels=as.character(c(sens_scenarios[1,2], paste0("(", sens_scenarios[-1,1], ") ", sens_scenarios[-1,2])))) +
  coord_cartesian(xlim=c(2007,2031),
                  ylim=c(0,1.05*max(sens_inc_df$value))) +
  labs(subtitle="Annual notifications",
       colour = "Scenario",
       x="Year",
       y="Notifications among GBM in year") +
  theme_plot_sens

fig_sens_right = ggplot(sens_prev_df, aes(x=year, y=value, group=order, colour=scenario)) +
  geom_line(size=1.6) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  scale_colour_discrete(breaks=sens_scenarios[,1],
                        labels=sens_scenarios[,2]) +
  coord_cartesian(xlim=c(2007,2031),
                  ylim=c(0,1)) +
  labs(subtitle="Prevalence",
       x="Year",
       y="Prevalence") +
  guides(colour=FALSE) +
  theme_plot_sens


# combine plots and output pdf and png
fig_sens = ggarrange(fig_sens_left, fig_sens_right, nrow=1, ncol=2, common.legend=F)
fig_sens = annotate_figure(fig_sens, top = text_grob("Sensitivity", face = "bold", size = 15))
ggsave("fig_sens.pdf", plot=fig_sens, width=170, height=130, units="mm")
ggsave("fig_sens.png", plot=fig_sens, width=170, height=130, units="mm", dpi=500)
browseURL("fig_sens.pdf")
# browseURL("fig_sens.png")


sens_mat = sens_scenarios
for(i in 1:1) sens_mat = cbind(sens_mat, 0)
colnames(sens_mat)[3] = "Cumulative Incidence"
sens_mat[,3] = aggregate(value~scenario, data=subset(sens_inc_df, year>=split_year & year<2030), FUN=sum)$value
sens_vec_temp = round(100*(as.numeric(sens_mat[-1,3])/as.numeric(sens_mat[1,3]) - 1))
sens_vec_temp = ifelse(sens_vec_temp>0, paste0("+", sens_vec_temp), sens_vec_temp)
sens_mat[-1,3] = paste0(round(as.numeric(sens_mat[-1,3])), " (", sens_vec_temp, "%)")
sens_mat[1,3] = round(as.numeric(sens_mat[1,3]))

# write.csv(sens_mat, "sensitivity_output.csv")

intervention=0
sensitivity=0
heat_scen_q=0
plot_gel_q=0
gel_up_heat=0
gel_down_heat=0