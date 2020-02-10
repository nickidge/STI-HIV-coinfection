
defaultlist = list()

defaultlist[['t_testing']] = c(c(1, 3), c(0.5, 1.5), c(3/12, 3/12)) # testing for i_lo_new, i_lo_old, i_hi_new, i_hi_old
# defaultlist[['condom_usage']] = c(0.42, 0.3, 0.1) # condom use for low risk hiv-, high risk hiv-, hiv+
defaultlist[['condom_usage']] = c(1, 0, 0, 1, 0, 0) # condom use for low risk hiv-, high risk hiv-, hiv+
defaultlist[['gel_mat']] = c(c(0, 0, 0), c(0, 0, 0)) # gel up for low risk hiv-, high risk hiv-, hiv+, and gel down for same groups
defaultlist[['eff_condom']] = 0.7 # effectiveness of condoms for HIV
defaultlist[['num_prep']] = 0 # number of people on PrEP
# defaultlist[['risk_mat']] = c(c(1, 1, 0.03), c(1, 1, 0.03)) # relative force of infection for low risk, high risk not on PrEP, high risk on PrEP
defaultlist[['eff_prep']] = 0.97
defaultlist[['test_wait']] = c(1) # defines length that people stay in each undiagnosed compartment (provided they don't get diagnosed)
defaultlist[['treatment_eff']] = c(1, 0.5, 0.1) # force of infection multiplier when diagnosed, on treatment, virally suppressed
# defaultlist[['medimix']] = 1 # amount of mixing between medicare eligibility compartments
# defaultlist[['prop_medi']] = 1 # what proportion of people are medicare eligible
defaultlist[['care_cascade']] = c(0.8, 0.8)
for(thisname in names(defaultlist)){
  if(length(DIM(defaultlist[[thisname]])) == 1){
    defaultlist[[thisname]] = matrix(defaultlist[[thisname]], nrow=1)
  }
}

defaultlist[['f_infect_HIV']] = 6.5e-6 # initial guess for HIV force of infection
defaultlist[['int_factor']] = 2.5
defaultlist[['init_diag_prop']] = 0.7 # initial guess for proportion of PLHIV who are diagnosed
# defaultlist[['init_prev_HIV']] = 0.1 # initial guess for proportion of people who are living with HIV
defaultlist[['init_prev_HIV_aus']] = 0.1 # initial guess for proportion of people who are living with HIV
defaultlist[['init_prev_HIV_int']] = 0.12 # initial guess for proportion of people who are living with HIV

defaultlist[['f_infect_STI']] = 0 # foi for STI
defaultlist[['init_prev_STI']] = 0 # initial prevalence of STI

# calculate population size at each time step
population_values = data.frame(data_raw[1:2,c('pop_aus', 'pop_int')])
growth = population_values[2,] / population_values[1,]
population_year = as.numeric(data_years[1,1])

popsize_t = seq(1990, 2050, by=1/12)
popsize = makearray(list(popsize_t, colnames(population_values)))
for(j in 1:ncol(popsize)){
  popsize[,j] = population_values[1,j] * (growth[1,j]) ^ (1 * (popsize_t - population_year))
}

dt = static_pars$dt$v