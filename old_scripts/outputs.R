## OUTPUTS ##
# create Figure S1
source("fig_HIV.R", echo = F)

# reset parameters
source("reset_pars.R", echo = F)

# recreate the plots in the paper, outputting a pdf
source("figure_2.R", echo = F)
source("figure_3.R", echo = F)
source("figure_4.R", echo = F)
source("figure_5.R", echo = F)
figs = list(fig_2, fig_3, fig_4, fig_5)
pdf("figs.pdf", onefile=TRUE, width=14, height=8)
invisible(lapply(figs, print))
dev.off()
browseURL("figs.pdf")

# reset parameters and prepare base plot
source("reset_pars.R", echo = F)
intervention = 0
sensitivity = 0
prepare_plots(SID_base)

# # plot entire SID data, facetted
# source("plot_facet.R", echo = TRUE)

# prepare Table 3 output
int_table = matrix(0, nrow=9, ncol=6,
                   dimnames = list(
                     c("base", letters[seq(1:8)]),
                     c("HIV- 1 year", "HIV- cumulative", "HIV+ 1 year", "HIV+ cumulative", "total 1 year", "total cumulative")))

# create function to fill table
fill_table_inc = function(SID_in){
  inc = SID_in[[3]][,,"incidence_sti"]
  inc_years = as.numeric(rownames(inc))
  inc_1_year = inc["2016",]
  inc_cumulative = colSums(inc[which(inc_years>=last_data_year & inc_years<2030),])
  return(c(inc_1_year["HIV_minus"],
           inc_cumulative["HIV_minus"],
           inc_1_year["HIV_plus"],
           inc_cumulative["HIV_plus"],
           inc_1_year["pop_HIV"],
           inc_cumulative["pop_HIV"]))
}

# base scenario
int_table[1,] = fill_table_inc(SID_base)

# # create log of pr_infect for debugging
# pr_infect_all = list(pr_infect_log)

# set parameters for scenarios
intervention=1
sensitivity=0

# run scenarios and export data into table
for(i_let in 1:8){
  source("reset_pars.R", echo = F)
  intervention=1
  int <<- letters[i_let]
  int_temp = run_model(y0_split, tvec_split)
  int_table[i_let+1,] = fill_table_inc(int_temp)
  
  # if(intervention==1){
  #   print(intervention)
  #   
  # }
  
  
  
  # if(int=="d"){
  #   int_temp_out_1 = int_temp
  #   check = -int_temp_out_1[[1]]["2016.5",,] + SID_base[[1]]["2016.5",,]
  #   heatmap.2(check, Rowv=F, Colv=F, dendrogram="none", trace="none", density.info="none")
  # }
  # 
  # if(int=="h"){
  #   int_temp_out_2 = int_temp
  #   # check = -int_temp_out[[1]]["2016.5",,] + SID_list[[1]]["2016.5",,]
  #   # check = -int_temp_out_1[[1]]["2018.5",,] + int_temp_out_2[[1]]["2018.5",,]
  #   # check = check[rev(rownames(check)),]
  #   heatmap.2(check, Rowv=F, Colv=F, dendrogram="none", trace="none", density.info="none")
  #   
  # }
  
  # pr_infect_all[[i_let+1]] = pr_infect_log
}

# calculate (integer) percentages for table
int_table_perc = (int_table %*% diag(1/int_table[1,]) * 100) - 100
dimnames(int_table_perc) = dimnames(int_table)

# finalise Table 3 output
int_table_out = matrix(paste0(round(int_table), " (", round(int_table_perc), "%)"),
                       nrow=9, ncol=6,
                       dimnames = list(
                         c("base", letters[seq(1:8)]),
                         c("HIV- 1 year", "HIV- cumulative", "HIV+ 1 year", "HIV+ cumulative", "total 1 year", "total cumulative")))

# prepare Table 4 output
sens_table = matrix(0, nrow=9, ncol=2,
                    dimnames=list(
                      c("Base (no PrEP)",
                        "PrEP (scenario h)",
                        "PrEP, among 10% of men only",
                        "PrEP, among 30% of men only",
                        "PrEP, with condom use reduced by 25%",
                        "PrEP, with condom use reduced by 75%",
                        "PrEP, with no increase in HIV serodiscordant mixing",
                        "PrEP, with 15% increase in HIV serodiscordant mixing",
                        "PrEP, with no increase in sti testing frequency"),
                      c(paste0("Estimated cumulative number of new sti infections ", last_data_year+1, "–2030"),
                        "Percentage change from scenario with no PrEP")))

# create function to fill table
fill_table_sens = function(SID_in){
  sens = SID_in[[3]][,"pop_HIV","incidence_sti"]
  sens_years = as.numeric(names(sens))
  sens_cumulative = sum(sens[which(sens_years>=last_data_year & sens_years<2030)])
  return(sens_cumulative)
}

# reset parameters for base scenario
source("reset_pars.R", echo = F)
intervention = 0
sensitivity = 0

# run base scenario
sens_temp = run_model(y0,tvec)
sens_table[1,1] = fill_table_sens(sens_temp)

# set parameters for scenarios
sensitivity=1
unc=0

# run h scenario (PrEP)
int="h"
sens_temp = run_model(y0_split, tvec_split)
sens_table[2,1] = fill_table_sens(sens_temp)

# run other scenarios
unc_list = c("1a", "1b", "2a", "2b", "3a", "3b", "4a")
for(i in 1:length(unc_list)){
  source("reset_pars.R", echo = F)
  unc <<- unc_list[i]
  sens_temp = run_model(y0_split, tvec_split)
  sens_table[i+2,1] = fill_table_sens(sens_temp)
}

# finalise Table 4 output
sens_table[,2] = sens_table[,1] / sens_table[1,1] * 100 - 100
sens_table[,1] = round(sens_table[,1], 0)
sens_table[,2] = round(sens_table[,2], 1)
sens_table[,2] = paste0(sens_table[,2], "%")

# output Table 3 and Table 4 to csv files
write.csv(int_table_out[,c(2,4,6)], "int_table.csv")
write.csv(sens_table, "sens_table.csv")
# browseURL("int_table.csv")
# browseURL("sens_table.csv")
