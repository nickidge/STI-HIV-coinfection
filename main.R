setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
medicare_ineligible = T
set.seed(1)
source("init.R", echo = F)


# set calibration values (if desired)
# can comment this out, and the script will auto-calibrate the cal_values parameters
# you can update the manual values accordingly
# this set of values is for medicare split
# cal_values = list(f_infect_HIV = 5.6907e-6,
#                   init_prev_HIV_aus = 0.1188241,
#                   init_diag_prop = 0.87936,
#                   init_late_prop = 0.103167,
#                   high_risk_factor = 1.310809,
#                   init_pop_aus = 19367,
#                   init_pop_int = 3173,
#                   prop_test = 1)

# calibrate model
if(!exists('baselist')) baselist = load_time_par_sheet('timepars', deflist = defaultlist)
cal_weights = c('PLHIV' = 0, 'HIV_diag' = 3.5, 'care_cascade' = 0.2, 'HIV_prev' = 2) # these define the weighting of each data type in the calibration
cal_pars = c('f_infect_HIV', 'init_prev_HIV_aus', 'init_diag_prop', 'init_late_prop', 'high_risk_factor', 'init_pop_aus', 'prop_test') # parameters to change in the calibration
if(medicare_ineligible){cal_pars = union(cal_pars, 'init_pop_int')} # if stratifying by medicare ineligibility, make init_pop_int variable
if(!exists('cal_values')) cal_values = gen_calibration(cal_pars, control = list(tol = 0.01)) # run calibration with desired tolerance
baselist[names(cal_values)] = cal_values
cal = run_model(modelpars=baselist)
y0_split <<- adrop(cal$SID[as.character(split_year),,,,drop=FALSE], 1)

# run base scenario (with uncertainty)
if(!exists('base_df')) base_df = gen_uncertainty(10)
saveopen(plot_cals(base_df), 'calibration', 'plots', width=2*1.1*page_width)

# run all scenarios (with uncertainty)
scen_df = gen_scenarios(base_df=base_df, scenarios=scenarios, ntrials=10)
saveopen(plot_scens(scen_df, base_uncertainty = T), 'scenarios', 'plots', width=2*1.1*page_width)

# # create table of scenario results (old)
# t_results = make_tab(scen_df, tab_row_funcs, allscens)

# output results tables
save_results_xlsx(create_results_df(scen_df))

