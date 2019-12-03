### Parameter Calibration ###
# this calibrates the parameters to fit the available data
# these parameters in order are:
# r_diag_HIV, f_infect_HIV, f_infect_sti

# this sets the bounds for the calibration parameters, and some parameters
cal_bounds = matrix(0.00000001, nrow=2, ncol=4)
cal_bounds[2,1] = 5
cal_bounds[2,2:4] = 0.99
f_infect_base_bounds = matrix(c(-0.001,0.8,-0.001,0.8), nrow=2, ncol=2)
cal_bounds = cbind(cal_bounds, f_infect_base_bounds)
cal_which=0

# choose first calibration year
# this is used to find the most appropriate start year for the model
cal_start_year = start_year

# set the starting conditions
y_cal_start = sety0()

# load calibration data
cal_data = as.matrix(data_raw[,c(2,3,19,20,8,9, 4,5,6)])
cal_data = as.matrix(
  cbind(
    data_raw[,grepl("New diagnoses", colnames(data_raw), fixed=T)],
    data_raw[,grepl("Total PLHIV", colnames(data_raw), fixed=T)],
    data_raw[,grepl("Total Gon cases HIV", colnames(data_raw), fixed=T)],
    data_raw[,grepl("Total Gon+ HIV", colnames(data_raw), fixed=T)],
    data_raw[,grepl("Prop HIV", colnames(data_raw), fixed=T)]))

dimnames(cal_data)=list(unlist(data_raw[,1]),
                        c("HIV diagnoses", "HIV prevalence",
                          "sti diagnoses in HIV_minus", "sti diagnoses in HIV_plus",
                          "sti prevalence in HIV_minus", "sti prevalence in HIV_plus",
                          "prop diagnosed HIV", "prop on treatment HIV", "prop virally suppressed HIV"))

# set sigma (the scaling variable for each ) to be the 
sigma = sweep(matrix(1,nrow=(last_data_year-start_year+1),ncol=9), MARGIN=2, apply(cal_data, 2, sd, na.rm=TRUE), `*`)

# adjust weighting of calibration data
sigma[,1] = sigma[,1]
sigma[,2] = sigma[,2] * 1.1
sigma[,3] = sigma[,3] / 5 
sigma[,4] = sigma[,4] / 3 
# sigma[,5] = sigma[,5] * 100000
# sigma[,6] = sigma[,6] * 100000
sigma[,7:9] = sigma[,7:9] * 20

# create sum of squares function
SSQ = function(cal_pars_first, cal_pars_second) {
  
  # load calibration parameters from input
  r_diag_HIV <<- cal_pars_first[1]
  f_infect_HIV <<- cal_pars_first[2]
  f_infect_sti <<- cal_pars_second[1:2]
  # f_infect_base <<- cal_pars_second[3:4]
  probX = 0
  
  # run model
  SID_list_cal = run_model(y_cal_start,tvec_cal)
  
  # extract the output data for calibration
  output_cal = cal_data
  output_cal[] = 0
  
  # choose which values to calculate; only calculating the necessary values means the model calibrates faster
  # if(cal_which=="HIV"){
    output_cal[,1] = SID_list_cal[[2]][rownames(output_cal), "diagnoses_HIV"]
    output_cal[,2] = SID_list_cal[[1]][rownames(output_cal), "HIV_plus", "pop_sti"]
    output_cal[,7] = SID_list_cal[[1]][rownames(output_cal), "D", "pop_sti"] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_plus", "pop_sti"]
  # }
  if(cal_which=="sti"){
    output_cal[,3] = SID_list_cal[[3]][rownames(output_cal),c("HIV_minus"),"diagnoses_sti"]
    output_cal[,4] = SID_list_cal[[3]][rownames(output_cal),c("HIV_plus"),"diagnoses_sti"]
    output_cal[,5] = SID_list_cal[[1]][rownames(output_cal), c("HIV_minus"), "sti_plus"] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_minus", "pop_sti"]
    output_cal[,6] = SID_list_cal[[1]][rownames(output_cal), c("HIV_plus"), "sti_plus"] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_plus", "pop_sti"]
  }
 
  
  # 
  # output_cal[,8] = rowSums(SID_list_cal[[1]][rownames(output_cal), c("D2", "D3"), "pop_sti"]) /
  #   SID_list_cal[[1]][rownames(output_cal), "D", "pop_sti"]
  # output_cal[,9] = SID_list_cal[[1]][rownames(output_cal), "D3", "pop_sti"] /
  #   rowSums(SID_list_cal[[1]][rownames(output_cal), c("D2", "D3"), "pop_sti"])
  
    # probX = 10000 * sum(!is.finite(output_cal))
    
  # set output_cal globally
  output_cal <<- output_cal
  
  # calculate sum of squares
  cal_diffs = ((output_cal - cal_data)/sigma)
  cal_diffs[!is.finite(cal_diffs)] = 0
  # cal_diffs[is.na(cal_diffs)] = 0
  cal_squares <- cal_diffs * cal_diffs
  probX_strat <<- colSums(cal_squares)
  
  # # alternate method; calculate maximum residual instead of sum of squares
  # probX_max <<- apply(cal_squares, 2, max, na.rm=TRUE)
  
  # sum the squares for relevant values
  if(cal_which=="HIV"){
    probX <- probX + sum(probX_strat[c(1:2, 7)])
  }
  if(cal_which=="sti"){
    probX <- probX + sum(probX_strat[3:6])
  }
  
  if(any(f_infect_sti<0)){
    probX = probX + 10000
  }
  
  # print(f_infect_sti)
  
  # fill the probX_all matrix
  probX_all[i,1] <<- probX
  probX_all[i,2] <<- r_diag_HIV
  probX_all[i,3] <<- f_infect_HIV
  probX_all[i,4:5] <<- f_infect_sti
  probX_all[i,6:7] <<- f_infect_base
  
  return(probX)
}


# run the calibration for each start year to be tested
for (i in 1:length(seq(cal_start_year, start_year, by=dt))){

  # reset probX; probX measures the sum of squares
  # probX_all is a matrix containing the calibrated parameters for each tested start year
  probX = 0
  probX_all = matrix(nrow=length(seq(cal_start_year, start_year, by=dt)), ncol=7)
  rownames(probX_all) = seq(cal_start_year, start_year, by=dt)
  colnames(probX_all) = c("probX", "r_diag_HIV", "f_infect_HIV", "f_infect_sti_for_HIV-", "f_infect_sti_for_HIV+", "f_infect_base_for_HIV-", "f_infect_base_for_HIV+")
  
  # set the initial values
  # r_diag_HIV = 0.34
  # f_infect_HIV = 0.07
  # f_infect_sti = c(0, 0)
  cal_pars_first = c(r_diag_HIV, f_infect_HIV)
  # cal_pars_second = c(0.00005,0.000005, 0.000001, 0.000001)
  cal_pars_second = c(0.1,0.1)
  
  # cal_pars_first_optim = cal_pars_first
  # cal_pars_second_optim = cal_pars_second
  
  # set the start year and tvec
  start_year_temp = cal_start_year + dt * (i-1)
  tvec_cal = seq(start_year_temp,
                 as.numeric(tail(rownames(cal_data), 1)) + 1,
                 by=dt)
  
  # optimise the variables by minimising the sum of squares, then set the variables
  
  # calibrate HIV first, with no sti infections
  cal_which = "HIV"
  # cal_pars_first_optim = nmkb(par=cal_pars_first, fn=SSQ, cal_pars_second=c(f_infect_sti, f_infect_base), lower=c(0.1,0.05), upper=c(0.9,0.9))$par
  if(exists("cal_pars_first_optim")){
    cal_pars_first = cal_pars_first_optim
  }
  cal_pars_first_optim = nmkb(par=cal_pars_first, fn=SSQ, cal_pars_second=c(0,0), lower=c(0.1,0.05), upper=c(0.9,0.9))$par
  r_diag_HIV = cal_pars_first_optim[1]
  f_infect_HIV = cal_pars_first_optim[2]

  # next, calibrate sti, with the HIV variables from earlier
  cal_which = "sti"
  if(exists("cal_pars_second_optim")){
    cal_pars_second = cal_pars_second_optim
  }
  # cal_pars_second_optim = nmkb(par=cal_pars_second, fn=SSQ, cal_pars_first=cal_pars_first_optim, lower=c(0,0,0,0), upper=c(2,2,1,1))$par
  cal_pars_second_optim = nmkb(par=cal_pars_second, fn=SSQ, cal_pars_first=cal_pars_first_optim, lower=c(0.01,0.01), upper=c(5,5))$par
  f_infect_sti = cal_pars_second_optim[1:2]
  # f_infect_base = cal_pars_second_optim[3:4]
  
  cal_which = 0
}

# choose the 'best' start year for the model
# that is, the start year that results in the lowest sum of squares
# start_year = as.numeric(rownames(probX_all)[probX_all[,"probX"] == min(probX_all[,"probX"])])
probX_ideal = probX_all[paste0(start_year),]
r_diag_HIV = as.numeric(probX_ideal[2])
f_infect_HIV = as.numeric(probX_ideal[3])
f_infect_sti = as.numeric(probX_ideal[4:5])
# f_infect_base = as.numeric(probX_ideal[6:7])

# print the output for the 'best' year
# print(probX_ideal)

# create a dataframe containing the calibration and output data, for plotting
cal_data_df = melt(cal_data)
colnames(cal_data_df) = c("year", "var", "value")
cal_data_df$N = factor("data")
output_cal_df = melt(output_cal)
colnames(output_cal_df) = c("year", "var", "value")
output_cal_df$N = factor("model")

# create the calibration plots
cal_all = rbind(cal_data_df, output_cal_df)
cal_plots = ggplot(data=cal_all, aes(x=year, y=value, colour=N)) + geom_point(na.rm=TRUE) + facet_wrap( ~ var, scales="free")