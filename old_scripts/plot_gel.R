
## GEL PLOT ##
# reset parameters
source("reset_pars.R", echo = F)
is_gel=F
# gel_up_heat=0
# gel_down_heat=0
gel_mat = gel_mat_base
eff_gel_heat = eff_gel

# create a common theme for plot_gel
theme_plot_gel =
  theme_all +
  theme(text = element_text(size=10)) +
  theme(strip.text = element_text(size=10.5)) +
  theme(axis.title.y=element_text(size=11)) +
  theme(plot.subtitle=element_text(size=11.5, face="bold", margin=margin(b=1)))

# run base scenario
prepare_plots(SID_base)
plot_gel_sti_df = sti_df
plot_gel_sti_df$scenario = factor("base")
plot_gel_prev_df = prev_df
plot_gel_prev_df$scenario = factor("base")

is_gel=T
plot_gel_scens = rbind(c(0.5, 0.2),
                       c(0.2, 0.5))
# run intervention scenarios and add to base output
for(i_gel in 1:2){
  source("reset_pars.R", echo = F)
  # gel_up_heat = plot_gel_scens[i_gel,1]
  # gel_down_heat = plot_gel_scens[i_gel,2]
  gel_mat = rbind(rep(plot_gel_scens[i_gel,1], 3),
                  rep(plot_gel_scens[i_gel,2], 3))
  prepare_plots(run_model(y0_split, tvec_split))
  sti_df$scenario = factor(letters[i_gel])
  plot_gel_sti_df = rbind(plot_gel_sti_df, sti_df)
  prev_df$scenario = factor(letters[i_gel])
  plot_gel_prev_df = rbind(plot_gel_prev_df, prev_df)
}

# rename scenarios and groups
# levels(plot_gel_sti_df$scenario) = c("Base",
#                                      paste0("(a) Good:\nNil to gel: ", plot_gel_scens[1,1], "\nCondom to gel: ", plot_gel_scens[1,2]),
#                                      paste0("(b) Bad:\nNil to gel: ", plot_gel_scens[2,1], "\nCondom to gel: ", plot_gel_scens[2,2]))
levels(plot_gel_sti_df$scenario) = c("Base",
                                     "a",
                                     "b")

levels(plot_gel_sti_df$HIV_group)[levels(plot_gel_sti_df$HIV_group) == "HIV_minus"] = "HIV-negative GBM in Victoria"
levels(plot_gel_sti_df$HIV_group)[levels(plot_gel_sti_df$HIV_group) == "HIV_plus"] = "HIV-positive GBM in Victoria"
levels(plot_gel_prev_df$prev_group)[levels(plot_gel_prev_df$prev_group) == "sti_in_HIV_minus"] = "HIV-negative GBM in Victoria"
levels(plot_gel_prev_df$prev_group)[levels(plot_gel_prev_df$prev_group) == "sti_in_HIV_plus"] = "HIV-positive GBM in Victoria"

# converge scenarios
sti_temp_df = subset(plot_gel_sti_df, sti_group=="incidence_sti" & year==split_year)
plot_gel_sti_df[rownames(subset(sti_temp_df, scenario!="Base")),"value"] = plot_gel_sti_df[rownames(subset(sti_temp_df, scenario=="Base")),"value"]
prev_temp_df = subset(plot_gel_prev_df, year==split_year)
plot_gel_prev_df[rownames(subset(prev_temp_df, scenario!="base")),"value"] = plot_gel_prev_df[rownames(subset(prev_temp_df, scenario=="base")),"value"]

split_val = subset(plot_gel_sti_df, year==floor(split_year) & sti_group=="incidence_sti" & scenario=="Base" & (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria"))

# find maximum values
plot_gel_maxval_left = max(subset(plot_gel_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti") & year<=2027)$value)
plot_gel_maxval_right = max(subset(plot_gel_prev_df, prev_group=="HIV-negative GBM in Victoria" | prev_group=="HIV-positive GBM in Victoria" & year<=2027)$value)


# temptemp = subset(plot_gel_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti"))

# create plots
plot_gel_left = ggplot(data = subset(plot_gel_sti_df, (HIV_group=="HIV-negative GBM in Victoria" | HIV_group=="HIV-positive GBM in Victoria") & (sti_group=="incidence_sti")), aes(x=year, y=value, group=scenario, colour=scenario)) +
  geom_line(lwd=1.6) +
  labs(x=NULL, y="Estimated new sti infections in year", subtitle=paste0("Scenarios (gel eff. for\nGonorrhoea = ", percent(eff_gel_heat[2]), ")"), colour=NULL) +
  theme(legend.key.width = unit(1.5,'lines'),
        # legend.key.height = unit(1.75,'lines'),
        legend.text = element_text(margin = margin(b=1, unit="mm")),
        legend.spacing.y = unit(0, "mm"),
        legend.justification = rev(c(0, 1)),
        legend.position = rev(c(0.6, 1))) +
  theme_plot_gel +
  # annotate("segment", x=split_year, xend=split_year,
  #   y=split_val$value[1]-(plot_gel_maxval_left/10),
  #   yend=split_val$value[1],
  #   color="blue",
  #   arrow=arrow(length=unit(2,"mm")
  #   )) +
  # geom_vline(xintercept=split_year, linetype="dotted", colour="grey70") +
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2007+2,end_year),
                  ylim=c(0,1.05*plot_gel_maxval_left)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_rep_wrap(~ HIV_group, dir="v", repeat.tick.labels = TRUE) +
  # scale_colour_manual(values=c("black", "green", "red", "orange", "purple"),
  #                     labels= c("Base",
  #                               paste0("(a) Good:\nNil to gel: ", plot_gel_scens[1,1], "\nCondom to gel: ", plot_gel_scens[1,2]),
  #                               paste0("(b) Bad:\nNil to gel: ", plot_gel_scens[2,1], "\nCondom to gel: ", plot_gel_scens[2,2])))
  scale_colour_manual(values=c("black", "green", "red", "orange", "purple"),
                      labels= c("Base",
                                paste0("(a) Good:\n", percent(plot_gel_scens[1,1]), " up,\n", percent(plot_gel_scens[1,2]), " down"),
                                paste0("(b) Bad:\n", percent(plot_gel_scens[2,1]), " up,\n", percent(plot_gel_scens[2,2]), " down")))

plot_gel_right = ggplot(data = subset(plot_gel_prev_df, prev_group=="HIV-negative GBM in Victoria" | prev_group=="HIV-positive GBM in Victoria"), aes(x=year, y=value, group=scenario, colour=scenario)) +
  geom_line(lwd=1.6) +
  labs(x=NULL, y="Prevalence",
       subtitle="",
       # subtitle="Scenarios",
       colour=NULL) +
  guides(colour=FALSE) +
  # geom_vline(xintercept=split_year, linetype="dotted", colour="grey70") +
  theme_plot_gel +
  scale_x_continuous(breaks = seq(2005, 2025, by = 5), expand=c(0,0)) +
  coord_cartesian(xlim=c(2007+2,end_year),
                  ylim=c(0,1.05*plot_gel_maxval_right/(1.05 * plot_gel_maxval_right))) +
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) +
  facet_rep_wrap(~ prev_group,
                 dir="v", repeat.tick.labels = TRUE,
                 scale="free") +
  scale_colour_manual(values=c("black", "green", "red", "orange", "purple"))

# combine plots and output pdf and png
plot_gel = ggarrange(plot_gel_left, plot_gel_right, nrow=1, ncol=2, common.legend=FALSE)
plot_gel = annotate_figure(plot_gel, top = text_grob("Gel Condom Plot", face = "bold", size = 15))
ggsave("plot_gel.pdf", plot=plot_gel, width=170, height=150, units="mm")
ggsave("plot_gel.png", plot=plot_gel, width=170, height=150, units="mm")
browseURL("plot_gel.pdf")
# browseURL("plot_gel.png")

is_gel=F
# gel_up_heat=0
# gel_down_heat=0
gel_mat = gel_mat_base
eff_gel_heat = eff_gel

