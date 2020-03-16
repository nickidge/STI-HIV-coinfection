### Main line of code: run this to run the whole model ###

# # On first run, make sure to run the following line of code to install necessary packages
# install.packages(c("plyr", "readxl", "abind", "lambda.tools", "dfoptim", "gplots", "ggplot2", "reshape2",
#                    "directlabels", "ggpubr", "scales", "gridExtra", "lemon", "tables", "gdata", "tidyr"))

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages
library(plyr)
library(base)
library(readxl)
library(abind)
library(lambda.tools)
library(dfoptim)
library(gplots)
library(ggplot2)
library(reshape2)
library(directlabels)
library(ggpubr)
library(scales)
library(gridExtra)
library(lemon)
library(tables)
library(gdata)
library(tidyr)
library(dplyr)
library(minpack.lm)
library(grid)
library(stringi)
library(stringr)
library(openxlsx)

# base model information
tvec_base = seq(2007, 2030, by=1/12)
plot_years = c(2010, 2030)
split_year = 2013
base_variance = 0.1

# scenario details (names, tab in data_sti.xlsx)
scenarios = list(list(sheet = 'scen_1',
                      short = 'no_prep',
                      long = 'Everyone stops using PrEP'),
                 list(sheet = 'scen_2',
                      short = 'care_cascade_stops',
                      long = 'Care cascade stays constant after 2014'))
scen_keys = c('pop', 'PLHIV', 'HIV_prev', 'prop_prep', 'HIV_diag', 'HIV_inf', 'HIV_diag_new', 'HIV_diag_old', 'care_cascade',
              'diagnosed_treated', 'treated_virally_suppressed', 'prev_lo', 'prev_hi', 'prev_pr')

label_years = unique(c(plot_years, seq(2000, 2050, by=5)))

options(stringsAsFactors = FALSE)

# bulk of code
source("processing.R", echo=F)
source("info.R", echo = F)
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
source("f_results.R", echo = F)
