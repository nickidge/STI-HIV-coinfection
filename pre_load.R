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
source("f_scenarios.R", echo = F)
source("f_table.R", echo = F)