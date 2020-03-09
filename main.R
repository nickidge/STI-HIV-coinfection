setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("init.R", echo = T)

# calibrate model
if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  cal_keys = c('PLHIV', 'HIV_diag', 'care_cascade', 'HIV_prev')
  cal_pars = c('f_infect_HIV', 'init_prev_HIV_aus', 'init_diag_prop', 'init_late_prop', 'high_risk_factor', 'init_pop_aus')
  gen_calibration(cal_pars, control = list(tol = 1e-2))
}

# run base scenario (with uncertainty)
if(!exists('base_df')){
  base_df = gen_uncertainty(100)
  saveopen(plot_cals(base_df), 'calibration', 'plots', width=2*1.1*page_width)
}

# run all scenarios (with uncertainty)
input_scenarios = list(list(sheet = 'scen_1',
                            short = 'no_prep',
                            long = 'Everyone stops using PrEP'),
                       list(sheet = 'scen_2',
                            short = 'care_cascade_stops',
                            long = 'Care cascade stays constant after 2014'))
scen_df = gen_scenarios(scen_df=base_df, scenarios=input_scenarios, ntrials=100)
saveopen(plot_scens(scen_df, base_uncertainty = T), 'scenarios', 'plots', width=1.5*1.1*page_width)

# # create table of scenario results (old)
# t_results = make_tab(scen_df, tab_row_funcs, allscens)

# output results tables
save_results_xlsx(create_results_df(scen_df))

