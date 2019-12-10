## FIGURE 5 ##
# reset parameters
source("reset_pars.R", echo = F)
intervention=0
sensitivity_supp=0
is_gel=F

gel_mat = gel_mat_base
eff_gel = eff_gel_base


# run base scenario
prepare_plots(run_model(y0,tvec))
fig_5_sti_df = sti_df
fig_5_sti_df$scenario = factor("base")

# prepare scenario parameters
sensitivity_supp=1
unc=0
is_gel=T

# run h scenario (PrEP)
int="h"
prepare_plots(run_model(y0_split, tvec_split))
sti_df$scenario = factor("PrEP")
fig_5_sti_df = rbind(fig_5_sti_df, sti_df)

# create a common theme for fig 5
theme_fig_5 =
  theme_all +
  theme_font +
  theme(text = element_text(size=11.5)) +
  theme(legend.text=element_text(size=10.5),
        legend.title=element_blank()) +
  theme(axis.text = element_text(size=10.3),
        axis.title.y = element_text(size=12))

# prepare a list of SID outputs for debugging
SID_debug = list()

# # reset variable
# int = 0

# run scenarios
for(i in c("1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b", "4c")){
  source("reset_pars.R", echo = F)
  unc <- i
  SID_debug[[unc]] = run_model(y0_split, tvec0[which(tvec0>=split_year)])
  prepare_plots(SID_debug[[unc]])
  sti_df$scenario = factor(unc)
  fig_5_sti_df = rbind(fig_5_sti_df, sti_df)
}

# only keep relevant data
fig_5_sti_df = subset(fig_5_sti_df, HIV_group=="pop_HIV" & sti_group=="diagnoses_sti")[,c("year", "scenario", "value")]

# converge scenarios at split year (so that the lines all meet)
sti_temp_df = subset(fig_5_sti_df, year==split_year)
fig_5_sti_df[rownames(subset(sti_temp_df, scenario!="base")),"value"] = fig_5_sti_df[rownames(subset(sti_temp_df, scenario=="base")),"value"]


# Scenarios
topleft_scenarios = c("base", "PrEP", "1a", "1b")
topleft_labels = c("No gel uptake (status quo)",
                   "base (50% upgrade and 50% downgrade) Condom effectiveness 80%",
                   "70% condom effectiveness",
                   "90% condom effectiveness")

topright_scenarios = c("base", "PrEP", "2a", "2b")
topright_labels = c("No gel uptake (status quo)",
                   "base (50% upgrade and 50% downgrade) PrEP coverage stays at 28%",
                    "PrEP coverage reaches 50% HIV-neg GBM by 2025",
                    "PrEP coverage reaches 80% HIV-neg GBM by 2025")

bottomleft_scenarios = c("base", "PrEP", "3a", "3b")
bottomleft_labels = c("No gel uptake (status quo)",
                      "base (50% upgrade and 50% downgrade) 45% symptomatic rate ",
                      "20% symptomatic rate",
                      "60% symptomatic rate")

bottomright_scenarios = c("base", "PrEP", "4a", "4b", "4c")
bottomright_labels = c("No gel uptake (status quo)",
                       "base (50% upgrade and 50% downgrade) increased risk factor 7.5",                       
                       "Increased risk factor 2",
                       "Increased risk factor 10",
                       "Increased risk factor 20")

# calculate maximum values for y-axis scale
fig_5_maxval = max(subset(fig_5_sti_df, year <= 2024)$value)

fig_5_template = ggplot(data = NULL, aes(x=year, y=value, linetype=scenario, colour=scenario)) +
  labs(x=NULL, y="Gonorrhoea notifications among\nVictorian GBM in year",
       title="Sensitivity to PrEP coverage",
       linetype=NULL) +
  theme(legend.key.width=unit(1.5,"line"),
        plot.title=element_text(face="bold"),
        legend.justification = c(0, 1),
        legend.position = c(0, 1)) +
  theme_fig_5 +
  scale_x_continuous(breaks = seq(2018, 2024, by = 2), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim=c(2014,2025), ylim=c(0,1.1*fig_5_maxval))

# Create plots
fig_5_topleft = fig_5_template +
  geom_line(data=subset(fig_5_sti_df, scenario %in% topleft_scenarios), lwd=1.1) +
  scale_linetype_manual(name="guide_topleft", values=c('solid', 'dashed', 'solid', 'dashed'), labels = topleft_labels) +
  scale_colour_manual(name="guide_topleft", values=c("black", "black", "blue", "blue"), labels = topleft_labels)

fig_5_topright = fig_5_template +
  geom_line(data = subset(fig_5_sti_df, scenario %in% topright_scenarios), lwd=1.1) +
  scale_linetype_manual(name="guide_topright", values=c('solid', 'dashed', 'solid', 'dashed'), labels = topright_labels) +
  scale_colour_manual(name="guide_topright", values=c("black", "black", "red", "red"), labels = topright_labels)

fig_5_bottomleft = fig_5_template +
  geom_line(data = subset(fig_5_sti_df, scenario %in% bottomleft_scenarios), lwd=1.1) +
  scale_linetype_manual(name="guide_bottomleft", values=c('solid', 'dashed', 'solid', 'dashed'), labels = bottomleft_labels) +
  scale_colour_manual(name="guide_bottomleft", values=c("black", "black", "darkgreen", "darkgreen"), labels = bottomleft_labels)

fig_5_bottomright = fig_5_template + 
  geom_line(data = subset(fig_5_sti_df, scenario %in% bottomright_scenarios), lwd=1.1) +
  scale_linetype_manual(name="guide_bottomright", values=c('solid', 'dashed', 'solid', 'dashed', 'dotted'), labels = bottomright_labels) +
  scale_colour_manual(name="guide_bottomright", values=c("black", "black", "purple", "purple", "purple"), labels = bottomright_labels)

# combine plots and output pdf and png
fig_5 = ggarrange(fig_5_topleft, fig_5_topright, fig_5_bottomleft, fig_5_bottomright, nrow=2, ncol=2, common.legend=FALSE)
fig_5 = annotate_figure(fig_5, top = text_grob("Figure 5", face = "bold", size = 24))
ggsave("fig_5.pdf", plot=fig_5, width = 14, height=8)
ggsave("fig_5.png", plot=fig_5, width = 14, height=8, dpi=500)
 browseURL("fig_5.pdf")
 # browseURL("fig_5.png")

 
 sensitivity_supp=0