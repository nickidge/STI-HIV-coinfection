# base model information
tvec_base = seq(2007, 2030, by=1/12)
plot_years = c(2007, 2030)
split_year = 2014

# scenario details (names, tab in data_sti.xlsx)
scenarios = list(list(sheet = 'scen_1',
                      short = 'no_prep',
                      long = 'Everyone stops using PrEP'),
                 list(sheet = 'scen_2',
                      short = 'more_test',
                      long = 'Testing more often'))

# bulk of code
source("init.R", echo = F)
source("info.R", echo = F)
source("processing.R", echo=F)
source("plotting.R", echo = F)
source("themes.R", echo = F)
source("pars.R", echo = F)
source("loadpars.R", echo = F)
source("default_values.R", echo = F)
source("model.R", echo = F)

# function scripts
source("f_calibrate.R", echo = F)
source("f_uncertainty.R", echo = F)
source("f_scenarios.R", echo = T)

# calibrate model
if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  cal_keys = c('PLHIV', 'HIV_diag', 'care_cascade')
  gen_calibration(c('f_infect_HIV', 'init_diag_prop', 'init_prev_HIV'))
}

# run base scenario (with uncertainty)
base_df = gen_uncertainty(30)
saveopen(plot_uncertainty(base_df), 'calibration', 'plots')

# run all scenarios (with uncertainty)
scen_df = gen_scenarios(scen_df=base_df, scenarios=scenarios, ntrials=30)
saveopen(plot_scens(scen_df), 'scenarios', 'plots')

