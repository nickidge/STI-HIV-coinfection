
defaultlist = list()

defaultlist[['t_testing']] = rep(1, 12) # testing for i_lo_new, i_lo_old, i_hi_new, i_hi_old
defaultlist[['condom_usage']] = c(1, 0, 0, 1, 0, 0) # condom use for low risk hiv-, high risk hiv-, hiv+
defaultlist[['gel_mat']] = c(c(0, 0, 0), c(0, 0, 0)) # gel up for low risk hiv-, high risk hiv-, hiv+, and gel down for same groups
defaultlist[['eff_condom']] = 0.7 # effectiveness of condoms for HIV
defaultlist[['num_prep']] = 0 # number of people on PrEP
defaultlist[['eff_prep']] = 0.97
defaultlist[['test_wait']] = c(1) # defines length that people stay in each undiagnosed compartment (provided they don't get diagnosed)
defaultlist[['treatment_eff']] = c(1, 0.5, 0.1) # force of infection multiplier when diagnosed, on treatment, virally suppressed
defaultlist[['care_cascade']] = c(0.8, 0.8)
defaultlist[['stay_time']] = 1/3
defaultlist[['stay_prop']] = 0.2
for(thisname in names(defaultlist)){
  if(length(DIM(defaultlist[[thisname]])) == 1){
    defaultlist[[thisname]] = matrix(defaultlist[[thisname]], nrow=1)
  }
}

defaultlist[['f_infect_HIV']] = 6.5e-6 # initial guess for HIV force of infection
defaultlist[['int_factor']] = 2.5
defaultlist[['high_risk_factor']] = 1
defaultlist[['init_diag_prop']] = 0.7 # initial guess for proportion of PLHIV who are diagnosed
defaultlist[['init_prev_HIV_aus']] = 0.11608 # initial guess for proportion of people who are living with HIV
defaultlist[['init_prev_HIV_int']] = 0.12 # initial guess for proportion of people who are living with HIV
defaultlist[['init_late_prop']] = 0.1

defaultlist[['init_pop_aus']] = 25000
defaultlist[['init_pop_int']] = 0
defaultlist[['pop_growth']] = c(1.05, 1.1)

defaultlist[['f_infect_STI']] = 0 # foi for STI
defaultlist[['init_prev_STI']] = 0 # initial prevalence of STI

# # calculate population size at each time step
# population_values = data.frame(data_raw[,c('Year','pop_aus', 'pop_int')])
# population_values_notna = population_values[!is.na(population_values[,2]) & !is.na(population_values[,3]),]
# population_values = rbind(head(population_values_notna, 1), tail(population_values_notna, 1))
# growth = population_values[2,2:3] / population_values[1,2:3]
# population_year = as.numeric(data_years[1,1])
# 
# popsize_t = seq(1990, 2050, by=1/12)
# popsize = makearray(list(popsize_t, colnames(population_values[,2:3])))
# for(j in 1:ncol(popsize)){
#   popsize[,j] = population_values[1,j+1] * (growth[1,j]) ^ (1 / (population_values[2,1] - population_values[1,1]) * (popsize_t - population_year))
# }
# 
# popsize[,"pop_int"] = 0

dt = static_pars$dt$v