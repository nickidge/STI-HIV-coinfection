tvec_base = seq(2007, 2025, by=1/12)
plot_years = c(2010, 2022)
split_year = 2014

source("init.R", echo = F)
source("loadpars.R", echo = F)
# source("loadbase.R", echo = F)
source("info.R", echo = F)
source("default_values.R", echo = F)
source("y0.R", echo = F)
source("themes.R", echo = F)
source("plotting.R", echo = F)
source("pars.R", echo = F)

# function scripts
source("f_uncertainty.R", echo = F)
source("f_calibrate.R", echo = F)


# run model and calibrate
source("model.R", echo = F)
source("processing.R", echo=F)

if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  source("calibrate.R", echo = T)
  source("pars.R", echo = F)
}

source("scenarios.R", echo = T)
source("uncertainty.R", echo = T)


#######


# # # create base scenario
# # source("reset_pars.R", echo = F)
# # SID_base = run_model(y0,tvec)
# 
# 
# # create plots
# source("plot_prepare.R", echo = F)
# source("plot_HIV.R", echo = F)
# source("plot_sti.R", echo = F)
# 
# # # print calibrated parameters
# # print(c(r_diag_HIV, f_infect_HIV, f_infect_sti))
# # print(probX_strat)
# 
# # split_year is the year at which the different scenarios take effect, hence where the scenario plots diverge (split)
# split_year = 2018
# tvec_split = tvec0[which(tvec0>=split_year)]
# 
# # save the population distribution at the split
# SID_split = SID_list[[7]][as.character(split_year),,,]
# y0_split = SID_list[[1]][as.character(split_year),,]
# 
# # # run debug
# # source("debug.R", echo = TRUE)
# 
# iss2=T
# # create base scenario
# source("reset_pars.R", echo = F)
# SID_base = run_model(y0,tvec)
# prepare_plots(SID_base)
# iss2=F
# 
# # define default font (size)
# theme_font = theme(text = element_text(size=13.5))  
# 
# 
# df_prep = function(df_in){
#   s2_all_df = df_in
#   s2_all_df$scenario = factor("base")
#   s2_all_df = s2_all_df[s2_all_df$HIV_stat %in% c("S1", "S2", "HIV_plus", "pop_HIV"),]
#   s2_all_df = drop.levels(s2_all_df)
#   s2_all_df$HIV_stat = factor(s2_all_df$HIV_stat, levels=c("S1", "S2", "HIV_plus", "pop_HIV"), labels=c("HIV- no PrEP", "HIV- PrEP", "HIV+", "All"))
#   
#   s2_inc_df = s2_all_df[s2_all_df$sti_stat %in% "diagnoses_sti",]
#   s2_inc_df = s2_all_df[s2_all_df$sti_stat %in% "incidence_sti",]
#   # s2_inc_df = s2_inc_df[(s2_inc_df$risk_stat %nin% "all"),]
#   s2_inc_df = aggregate(value~floor(year)+HIV_stat+sti_stat+risk_stat+scenario, data=s2_inc_df, FUN=sum, simplify=F)
#   colnames(s2_inc_df)[1] = "year"
#   s2_inc_df$value = as.numeric(s2_inc_df$value)
#   
#   s2_prev_df = s2_all_df[s2_all_df$sti_stat %in% "sti_plus",]
#   s2_prev_df$value = s2_prev_df$value / s2_all_df[s2_all_df$sti_stat %in% "pop_sti","value"]
#   # s2_prev_df = s2_prev_df[s2_prev_df$risk_stat %in% "all",]
#   return(list(s2_all_df, s2_inc_df, s2_prev_df))
# }
# 
# # # create outputs
# # source("outputs.R", echo = T)
# 
# source("fig_HIV.R", echo = F)
# source("figure_2.R")
#  source("plot_facet.R")
# source("plot_gel.R", echo=T)
# source("fig_S2.R")
# source("sensitivity.R", echo=T)
# source("figure_5.R",echo=T)
# source("heatmap.R", echo=T)
# 
# # print the total run time
# print(proc.time() - ptm)
