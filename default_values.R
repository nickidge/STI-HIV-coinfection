
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
defaultlist[['treatment_eff']] = c(1, 0.5, 0.1)

defaultlist[['f_infect_HIV']] = 6.5e-6
# defaultlist[['init_PLHIV']] = 3000
defaultlist[['init_diag_prop']] = 0.7
defaultlist[['init_prev_HIV']] = 0.1

population_value = static_pars$population_value$v
growth = static_pars$growth$v
population_year = static_pars$population_year$v

population_est = function(years) setNames(population_value * (1 + growth)^( 12 * (years - population_year)), years)

popsize_t = seq(1990, 2050, by=1/12)
popsize = population_est(popsize_t)

dt = static_pars$dt$v