### Parameter Calibration ###
# this calibrates the parameters to fit the available data
# these parameters in order are:
# r_diag_HIV, f_infect_HIV, f_infect_syph

# this sets the bounds for the calibration parameters
cal_bounds = matrix(0.00000001, nrow=2, ncol=4)
cal_bounds[2,1] = 5
cal_bounds[2,2:4] = 0.99

# choose first calibration year
# this is used to find the most appropriate start year for the model
cal_start_year = 2007

# set the starting conditions
y_cal_start = sety0(start_year)

# load calibration data
cal_data = as.matrix(data_raw[,c(2,3,15,16,8,9, 4,5,6)])
dimnames(cal_data)=list((2007:2015),
                        c("HIV diagnoses", "HIV prevalence", "syph diagnoses in HIV_minus", "syph diagnoses in HIV_plus", "syph prevalence in HIV_minus", "syph prevalence in HIV_plus", "prop diagnosed HIV", "prop on treatment HIV", "prop virally suppressed HIV"))

# set sigma (the scaling variable for each ) to be the 
sigma = sweep(matrix(1,nrow=(2015-2007+1),ncol=9), MARGIN=2, apply(cal_data, 2, sd, na.rm=TRUE), `*`)

# increase weighting for syphilis diagnoses, decrease for HIV care cascade
sigma[,3:4] = sigma[,3:4] / 10
sigma[,7:9] = sigma[,7:9] * 10

# run the calibration for each start year to be tested
for (i in 1:length(seq(cal_start_year, 2007, by=dt))){

  # reset probX; probX measures the sum of squares
  # probX_all is a matrix containing the calibrated parameters for each tested start year
  probX = 0
  probX_all = matrix(nrow=length(seq(cal_start_year, 2007, by=dt)), ncol=5)
  rownames(probX_all) = seq(cal_start_year, 2007, by=dt)
  colnames(probX_all) = c("probX", "r_diag_HIV", "f_infect_HIV", "f_infect_syph_for_HIV-", "f_infect_syph_for_HIV+")
  
  # set the initial values
  r_diag_HIV = 0.34
  f_infect_HIV = 0.07
  f_infect_syph = c(0, 0)
  cal_pars_first = c(r_diag_HIV, f_infect_HIV)
  cal_pars_second = c(0.5,0.5)

  # set the start year and tvec
  start_year = cal_start_year + dt * (i-1)
  tvec_cal = seq(start_year, 2017, by=dt)
 
  
  # sum of squares function
  # calculate SSQ between known data and model output
  SSQ = function(cal_pars_first, cal_pars_second) {
    
    # load calibration parameters from input
    r_diag_HIV <<- cal_pars_first[1]
    f_infect_HIV <<- cal_pars_first[2]
    f_infect_syph <<- cal_pars_second
    
    # run model
    SID_list_cal = run_model(y_cal_start,tvec_cal)
    
    # extract the output data for calibration
    output_cal = cal_data
    output_cal[] = 0
    output_cal[,1] = SID_list_cal[[2]][rownames(output_cal), "diagnoses_HIV"]
    output_cal[,2] = SID_list_cal[[1]][rownames(output_cal), "HIV_plus", 10]
    output_cal[,3] = SID_list_cal[[3]][rownames(output_cal),c("HIV_minus"),"diagnoses_syph"]
    output_cal[,4] = SID_list_cal[[3]][rownames(output_cal),c("HIV_plus"),"diagnoses_syph"]
    output_cal[,5] = SID_list_cal[[1]][rownames(output_cal), c("HIV_minus"), "syph_plus"] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_minus", 10]
    output_cal[,6] = SID_list_cal[[1]][rownames(output_cal), c("HIV_plus"), "syph_plus"] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_plus", 10]
    output_cal[,7] = SID_list_cal[[1]][rownames(output_cal), "D", 10] /
      SID_list_cal[[1]][rownames(output_cal), "HIV_plus", 10]
    output_cal[,8] = rowSums(SID_list_cal[[1]][rownames(output_cal), c("D2", "D3"), 10]) /
      SID_list_cal[[1]][rownames(output_cal), "D", 10]
    output_cal[,9] = SID_list_cal[[1]][rownames(output_cal), "D3", 10] /
      rowSums(SID_list_cal[[1]][rownames(output_cal), c("D2", "D3"), 10])
    
    # set output_cal globally
    output_cal <<- output_cal
    
    # calculate sum of squares
    cal_diffs = (output_cal - cal_data)/sigma
    cal_diffs[!is.finite(cal_diffs)] = 0
    cal_squares <<- cal_diffs * cal_diffs
    probX_strat <<- colSums(cal_squares)
    probX_max <<- apply(cal_squares, 2, max, na.rm=TRUE) # alternate method; calculate maximum residual instead of sum of squares. This is currently not in use
    probX <- sum(probX_strat)
    
    # fill the probX_all matrix
    probX_all[i,1] <<- probX
    probX_all[i,2] <<- r_diag_HIV
    probX_all[i,3] <<- f_infect_HIV
    probX_all[i,4:5] <<- f_infect_syph
    
    return(probX)
  }
  
  # optimise the variables by minimising the sum of squares, then set the variables
  # calibrate HIV first, with no syphilis infections
  cal_pars_first_optim = nmkb(par=cal_pars_first, fn=SSQ, cal_pars_second=f_infect_syph, lower=c(0.1,0.05), upper=c(0.9,0.9))$par
  r_diag_HIV = cal_pars_first_optim[1]
  f_infect_HIV = cal_pars_first_optim[2]
  
  # next, calibrate syphilis, with the HIV variables from earlier
  cal_pars_second_optim = nmkb(par=cal_pars_second, fn=SSQ, cal_pars_first=cal_pars_first_optim, lower=c(0,0), upper=c(0.9,0.9))$par
  f_infect_syph = cal_pars_second_optim
}

# choose the best start year for the model
# that is, the start year that results in the lowest sum of squares
start_year = as.numeric(rownames(probX_all)[probX_all[,"probX"] == min(probX_all[,"probX"])])
probX_ideal = probX_all[paste0(start_year),]
r_diag_HIV = as.numeric(probX_ideal[2])
f_infect_HIV = as.numeric(probX_ideal[3])
f_infect_syph = as.numeric(probX_ideal[4:5])

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
