tvec_base = seq(2007, 2025, by=1/12)
plot_years = c(2010, 2022)
split_year = 2014

plot_keys = c('PLHIV', 'HIV_diag', 'HIV_inf')
plot_long = c('Total PLHIV', 'Annual HIV diagnoses', 'Annual HIV incidence')

source("init.R", echo = F)
source("loadpars.R", echo = F)
source("info.R", echo = F)
source("default_values.R", echo = F)
source("y0.R", echo = F)
source("themes.R", echo = F)
source("plotting.R", echo = F)
source("pars.R", echo = F)

# function scripts
source("f_uncertainty.R", echo = F)
source("f_calibrate.R", echo = F)


# run model and calibrate
source("model.R", echo = F)
source("processing.R", echo=F)

if(!exists('cal')){
  baselist = load_time_par_sheet('timepars', deflist = defaultlist)
  source("calibrate.R", echo = T)
  source("pars.R", echo = F)
}

source("scenarios.R", echo = T)
source("uncertainty.R", echo = T)


#######

# # print the total run time
# print(proc.time() - ptm)
