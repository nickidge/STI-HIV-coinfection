
# if(!exists('baselist')){
#   baselist = list()
# }
defaultlist = list()

defaultlist[['t_testing']] = c(c(1, 3, 10), c(0.5, 1.5, 5), c(3/12, 3/12, 3/12)) # testing for i_lo_new, i_lo_mid, i_lo_old, i_hi_new, i_hi_mid, i_hi_old
defaultlist[['condom_usage']] = c(0.42, 0.3, 0.1) # condom use for low risk hiv-, high risk hiv-, hiv+
defaultlist[['gel_mat']] = c(c(0, 0, 0), c(0, 0, 0)) # gel up for low risk hiv-, high risk hiv-, hiv+, and gel down for same groups
defaultlist[['eff_condom']] = 0.7
defaultlist[['num_prep']] = 0
defaultlist[['risk_mat']] = c(1, 10, 0.05)
defaultlist[['test_wait']] = c(Inf, 2)
# defaultlist[['care_cascade_interp']] = cascade_interp
defaultlist[['treatment_eff']] = c(1, 0.5, 0.1)

defaultlist[['f_infect_HIV']] = 6.5e-6
defaultlist[['init_PLHIV']] = 3000
defaultlist[['init_diag_prop']] = 0.7

# popsize_t = seq(1990, 2050, by=1/12)
# popsize = setNames(unlist(lapply(popsize_t, function(x) population_value * (1 + growth)^( 12 * (x - population_year)))), popsize_t)

popsize_t = seq(1990, 2050, by=1/12)
popsize = setNames(unlist(lapply(popsize_t, function(x) static_pars$population_value$v * (1 + static_pars$growth$v)^( 12 * (x - static_pars$population_year$v)))), popsize_t)

dt = static_pars$dt$v