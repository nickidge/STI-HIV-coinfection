# optvarkeys = c(
#   # 'prop_testing_interp',
#   't_testing_interp',
#   # 'prop_prep_interp',
#   'test_wait_interp',
#   'condom_usage_interp',
#   'gel_mat_interp',
#   'eff_condom',
#   # 'prop_treat',
#   'risk_mat',
#   'care_cascade_interp',
#   'popsize_interp',
#   'treat_eff_interp',
#   'f_infect_HIV',
#   'init_PLHIV',
#   'init_diag_prop',
#   NULL)

optvarkeys = c(
  't_testing',
  'test_wait',
  'condom_usage',
  'gel_mat',
  'eff_condom',
  'risk_mat',
  'care_cascade',
  # 'popsize',
  'treatment_eff',
  # 'f_infect_HIV',
  # 'init_PLHIV',
  # 'init_diag_prop',
  NULL)

make_interp = function(v, times=nt){
  mat_interp = t(replicate(nt, v))
  row.names(mat_interp) = as.character(tvec)
  return(mat_interp)
}

defaultlist = list()
baselist = list()

defaultlist[['t_testing_interp']] = make_interp(c(c(1, 3, 10), c(0.5, 1.5, 5), c(3/12, 3/12, 3/12))) # testing for i_lo_new, i_lo_mid, i_lo_old, i_hi_new, i_hi_mid, i_hi_old
defaultlist[['condom_usage_interp']] = make_interp(c(0.42, 0.3, 0.1)) # condom use for low risk hiv-, high risk hiv-, hiv+
defaultlist[['gel_mat_interp']] = make_interp(c(c(0, 0, 0), c(0, 0, 0))) # gel up for low risk hiv-, high risk hiv-, hiv+, and gel down for same groups
defaultlist[['eff_condom']] = 0.7
defaultlist[['num_prep']] = 0
defaultlist[['risk_mat']] = c(1, 10, 0.05)
defaultlist[['test_wait_interp']] = make_interp(c(Inf, 2))
defaultlist[['care_cascade_interp']] = cascade_interp
defaultlist[['treatment_eff_interp']] = make_interp(c(1, 0.5, 0.1))
# defaultlist[['popsize_interp']] = setNames(unlist(lapply(tvec_base, function(x) population_value * (1 + growth)^( 12 * (x - population_year)))), tvec_base)

# defaultlist[['f_infect_HIV']] = 6.5e-6
# defaultlist[['init_PLHIV']] = 3000
# defaultlist[['init_diag_prop']] = cascade0$`Prop HIV diagnosed`[1]

popsize_t = seq(1990, 2050, by=1/12)
popsize = setNames(unlist(lapply(popsize_t, function(x) population_value * (1 + growth)^( 12 * (x - population_year)))), popsize_t)
