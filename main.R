setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
medicare_ineligible = T
source("init.R", echo = F)

# calibrate model
if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  cal_weights = c('PLHIV' = 0, 'HIV_diag' = 3.5, 'care_cascade' = 0.2, 'HIV_prev' = 2) # these define the weighting of each data type in the calibration
  cal_pars = c('f_infect_HIV', 'init_prev_HIV_aus', 'init_diag_prop', 'init_late_prop', 'high_risk_factor', 'init_pop_aus') # parameters to change in the calibration
  if(medicare_ineligible){cal_pars = union(cal_pars, 'init_pop_int')} # if stratifying by medicare ineligibility, make init_pop_int variable
  gen_calibration(cal_pars, control = list(tol = 0.01)) # run calibration with desired tolerance
}

# run base scenario (with uncertainty)
if(!exists('base_df')){
  base_df = gen_uncertainty(10)
  saveopen(plot_cals(base_df), 'calibration', 'plots', width=2*1.1*page_width)
}

# run all scenarios (with uncertainty)
scen_df = gen_scenarios(base_df=base_df, scenarios=scenarios, ntrials=10)
saveopen(plot_scens(scen_df, base_uncertainty = T), 'scenarios', 'plots', width=2*1.1*page_width)

# # create table of scenario results (old)
# t_results = make_tab(scen_df, tab_row_funcs, allscens)

# output results tables
save_results_xlsx(create_results_df(scen_df))

