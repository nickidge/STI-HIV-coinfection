optvarkeys = c(
  # 'prop_testing_interp',
  't_testing_interp',
  # 'prop_prep_interp',
  'test_wait_interp',
  'condom_usage_interp',
  'gel_mat_interp',
  'eff_condom',
  # 'prop_treat',
  'risk_mat',
  'care_cascade_interp',
  'popsize_interp',
  'treat_eff_interp',
  'f_infect_HIV',
  'init_PLHIV',
  'init_diag_prop',
  NULL)

make_interp = function(v, times=nt){
  mat_interp = t(replicate(nt, v))
  row.names(mat_interp) = as.character(tvec)
  return(mat_interp)
}

t_testing_interp_base = make_interp(c(c(1, 3, 10), c(0.5, 1.5, 5), c(3/12, 3/12, 3/12))) # testing for i_lo_new, i_lo_mid, i_lo_old, i_hi_new, i_hi_mid, i_hi_old
condom_usage_interp_base = make_interp(c(0.42, 0.3, 0.1)) # condom use for low risk hiv-, high risk hiv-, hiv+
gel_mat_interp_base = make_interp(c(c(0, 0, 0), c(0, 0, 0))) # gel up for low risk hiv-, high risk hiv-, hiv+, and gel down for same groups
eff_condom_base = 0.7
risk_mat_base = c(1, 10, 0.05)
test_wait_interp_base = make_interp(c(Inf, 2))
# care_cascade_interp_base = cascade_interp[,2:3]
care_cascade_interp_base = cascade_interp
treat_eff_interp_base = make_interp(c(1, 0.5, 0.1))
popsize_interp_base = setNames(unlist(lapply(tvec_base, function(x) population_value * (1 + growth)^( 12 * (x - population_year)))), tvec_base)
f_infect_HIV_base = 6.5e-6
init_PLHIV_base = 3000
init_diag_prop_base = cascade0$`Prop HIV diagnosed`[1]




# for(key in optvarkeys){
#   ttt = (get(paste0(key, '_base')))
# }
