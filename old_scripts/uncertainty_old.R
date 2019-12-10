ntrial = 100

tvec_uncert = seq(2010, (2039+1), by=dt)
uncert_output = matrix(0, nrow = (2039-2010+1), ncol=ntrial)
row.names(uncert_output) = seq(tvec_uncert[1], by=1, length.out=nrow(uncert_output))
uncert_all_outputs = list()
uncert_all_pars = list()
condom_df = list()

uncert_func <- function(n_out) {
  uncert_pars = matrix(0, nrow=nrow(pars_raw), ncol=ntrial)
  new.names = NULL
  
  run_interp(tvec_uncert)
  
  trial_list = list()
  
  for (trial in 1:ntrial){
    
    # set_pars()
    
    
    # if (s == 1){
    #   for(i in 1:nrow(pars_raw)){
    #     assign(paste(pars_raw[i,2]), as.numeric(pars_raw[i,3]), envir = .GlobalEnv)
    #     if(is.na(pars_raw[i,4])==FALSE)
    #     {
    #       assign(paste(pars_raw[i,2], "_lb", sep=""), as.numeric(pars_raw[i,4]), envir = .GlobalEnv)
    #       assign(paste(pars_raw[i,2], "_ub", sep=""), as.numeric(pars_raw[i,5]), envir = .GlobalEnv)
    #     }
    #   }
    # } else {
    for(i in 1:nrow(pars_raw)){
      assign(paste(pars_raw[i,2]), as.numeric(pars_raw[i,3]), envir = .GlobalEnv)
      assign("temp", get(paste(pars_raw[i,2])))
      uncert_pars[i,trial] <- as.numeric(temp)
      # new.names[i] = paste(pars_raw[i,2])
      if(is.na(pars_raw[i,4])==FALSE)
      {
        assign(paste(pars_raw[i,2], "_lb", sep=""), as.numeric(pars_raw[i,4]), envir = .GlobalEnv)
        assign(paste(pars_raw[i,2], "_ub", sep=""), as.numeric(pars_raw[i,5]), envir = .GlobalEnv)
        assign(paste(pars_raw[i,2]), runif(1,as.numeric(pars_raw[i,4]),as.numeric(pars_raw[i,5])), envir = .GlobalEnv)
        
        assign("temp", get(paste(pars_raw[i,2])))
        uncert_pars[i,trial] <- as.numeric(temp)
      }
    }
    # }
    
    y_2010 = sety0(start_year)
    
    
    # source("calibrate.R",echo=TRUE)
    
    xp_out <- nmkb(par = xp[-3], fn=SSQ, lower=cal_lb[-3], upper=cal_ub[-3])$par
    f_infect <<- xp_out[1]
    r_diag <<- xp_out[2]
    
    uncert_pars[2,trial] = f_infect
    uncert_pars[8,trial] = r_diag
    
    # xp <<- nmkb(par = xp, fn=SSQ, lower=cal_lb, upper=cal_ub)$par
    # SSQ(xp)
    # remove(f_infect,r_diag)
    # f_infect <<- xp_out[1]
    # r_diag <<- xp_out[2]
    # start_year=tvec0[min(which(tvec0>=xp[3]))-1]
    
    y_2010 = sety0()
    
    run_interp(tvec_uncert)
    
    y_trial <<- run_model(y_2010, tvec_uncert)
    for(year in 1:(2039-2010+1)){
      uncert_output[year,trial] = sum(y_trial[min(which(tvec_uncert>=(2010+year-1))):(min(which(tvec_uncert>=(2010+year)))-1),9,(nrisk+1)]) # incidence
    }
    trial_list[[trial]] = y_trial
    
    print((n_out-1 + (trial/ntrial))/3)
  }
  
  rownames(uncert_pars) = as.matrix(pars_raw[,2])
  
  uncert_interval = 0.95
  uncert_median = apply(uncert_output, 1, median)
  uncert_bounds = t(apply(uncert_output, 1, quantile, probs=c((1-uncert_interval)/2, (1+uncert_interval)/2)))
  
  # uncert_median_df = as.data.frame(uncert_median)
  # uncert_median_df["year"] = rownames(uncert_median_df)
  # uncert_bounds_df = as.data.frame(uncert_bounds)
  # uncert_bounds_df["year"] = rownames(uncert_bounds_df)
  
  uncert_df = as.data.frame(cbind(uncert_median, uncert_bounds))
  uncert_df["year"] = rownames(uncert_df)
  uncert_df["category"] = n_out
  
  colnames(uncert_df) = c("estimate", "lb", "ub", "year", "category")
 
  
  prop_condom_left = prop_condom_interp[1,1,] * (1 - prop_condom_to_gel_interp)
  prop_nil_left = (1 - prop_condom_interp[1,1,]) * (1 - prop_nil_to_gel_interp)
  prop_gel_left = 1 - prop_condom_left - prop_nil_left
  
  df_left = data.frame(Year = tvec_uncert, Condom = prop_condom_left, Unprotected = prop_nil_left, Gel = prop_gel_left)
  df_left = melt(df_left, measure.vars = c('Condom', 'Gel', 'Unprotected'), variable.name = 'Category', value.name = 'Proportion')
  
  
  uncert_all_outputs[[n_out]] <<- uncert_df
  uncert_all_pars[[n_out]] <<- uncert_pars
  condom_df[[n_out]] <<- df_left
  
}

s=0
intervene=1
cascade_scenario = base_pars[[1]]
parameter_change = base_pars[[2]]
prop_condom_scenario = base_pars[[3]]
eff_prep_change = base_pars[[4]]


eff_gel[] = 0.3

prop_nil_to_gel = 0
prop_condom_to_gel = 0
uncert_func(1)

prop_nil_to_gel = 0.8
prop_condom_to_gel = 0
uncert_func(2)

prop_nil_to_gel = 0
prop_condom_to_gel = 0.8
uncert_func(3)




uncert_df = rbind(uncert_all_outputs[[1]], uncert_all_outputs[[2]], uncert_all_outputs[[3]])

uncert_df[uncert_df=="1"] <- "Estimate"
uncert_df[uncert_df=="2"] <- "Good"
uncert_df[uncert_df=="3"] <- "Bad"


uncert_plot = ggplot(data=uncert_df, aes(ymin=lb, ymax=ub, x=year, y=estimate)) +
  
  geom_line(aes(group=category, colour=factor(category)), size=1.5) +
  geom_ribbon(aes(group=category, fill=factor(category)), alpha=0.2) +
  
  # geom_line(aes(group=category, colour=factor(category)), size=1.5) +
  # geom_ribbon(aes(group=category, fill=factor(category)), alpha=0.2) +
  # 
  # geom_line(aes(group=category, colour=factor(category)), size=1.5) +
  # geom_ribbon(aes(group=category, fill=factor(category)), alpha=0.2) +
  
  scale_colour_manual(values=c(muted("red"), "black", muted("blue")), name="Best estimate") +
  scale_fill_manual(values=c("red", "black", "blue"), name="95% Confidence interval") +
  labs(x="Year", y="Incidence of HIV", title="Incidence uncertainty analysis for three scenarios") +
  theme(plot.title = element_text(size=18, face="bold"),
        axis.text.x=element_text(angle=90, size=15, colour="black", vjust=0.5),
        axis.text.y=element_text(angle=0, size=15, colour="black", vjust=0.5),
        axis.title.x = element_text(size=16, vjust=-0.35),
        axis.title.y = element_text(size=16, vjust=1.5),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "grey20", linetype="dashed"),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.6, 0.8),
        legend.background = element_rect(fill="white",size = 0.5),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16)) +
  scale_x_discrete(expand= c(0, 0)) +
  scale_y_continuous(limits = c(0, 2000), expand = c(0.0, 0.0))

ggsave("uncertainty_plot.pdf", plot=uncert_plot, width = 16, height=9)
browseURL("uncertainty_plot.pdf")

# 
# prop_condom_to_gel = 0
# prop_nil_to_gel = 0
# 
# run_interp(tvec)
# 
# prop_condom_left = prop_condom_interp[1,1,] * (1 - prop_condom_to_gel_interp)
# prop_nil_left = (1 - prop_condom_interp[1,1,]) * (1 - prop_nil_to_gel_interp)
# prop_gel_left = 1 - prop_condom_left - prop_nil_left
# 
# df_left = data.frame(Year = tvec, Condom = prop_condom_left, Unprotected = prop_nil_left, Gel = prop_gel_left)
# df_left = melt(df_left, measure.vars = c('Condom', 'Gel', 'Unprotected'), variable.name = 'Category', value.name = 'Proportion')

condom_p = list()

for (n_trial in 1:3){
  condom_p[[n_trial]] <- ggplot(condom_df[[n_trial]], aes(Year, Proportion)) +
  geom_area(aes(fill=Category), position='stack') +
    scale_fill_manual("",
                    breaks = c("Condom", "Gel", "Unprotected"),
                    values = c("darkseagreen3", "darkseagreen2", "darkseagreen1")) +
    # geom_vline(xintercept = c(2018, 2020), linetype = "dashed") +
    theme(plot.title=element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank()) +
    scale_x_continuous(expand= c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}

plot_top_condom = ggarrange(condom_p[[1]] + theme(legend.text=element_text(size=13)), element_blank(), legend="right", widths=c(1.57,1))
plot_bottom_condom = ggarrange(condom_p[[2]], condom_p[[3]], legend="none")
plot_all_condom = ggarrange(plot_top_condom, plot_bottom_condom, nrow=2)
plot_all_condom = annotate_figure(plot_all_condom, top = text_grob("Condom usage scenarios", face = "bold", size = 24))
ggsave("condom_plot.pdf", plot=plot_all_condom, width = 14, height=8)
browseURL("condom_plot.pdf")
