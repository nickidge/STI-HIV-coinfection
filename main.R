setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("init.R", echo = T)

# calibrate model
if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  cal_keys = c('PLHIV', 'HIV_diag', 'care_cascade')
  gen_calibration(c('f_infect_HIV', 'init_prev_HIV_aus', 'init_diag_prop'))
}

# model_df = run_model(y0=NULL, tvec=tvec_base, modelpars=baselist, options=options)
# model_df = extr(model_df, 'popsize')

# run base scenario (with uncertainty)
base_df = gen_uncertainty(0)
# saveopen(plot_uncertainty(base_df, toplot=c('pop', 'PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'prop_prep', 'HIV_diag_new', 'HIV_diag_old'), colour_strat = 'med'), 'cal1')
# saveopen(plot_uncertainty(base_df, toplot=c('num_cascade', 'care_cascade', 'HIV_diag_by_pop', 'HIV_prev_by_risk')), 'cal2')
saveopen(plot_cals(base_df), 'calibration', 'plots', width=2*1.1*page_width)

# # run all scenarios (with uncertainty)
# scen_df = gen_scenarios(scen_df=base_df, scenarios=scenarios, ntrials=1)
# saveopen(plot_scens(scen_df), 'scenarios', 'plots')

# # create table of scenario results
# t_results = make_tab(tab_row_funcs, allscens)

