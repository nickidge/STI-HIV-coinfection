
library(readxl)
library(plyr)

scen_1 = read_excel("parameters.xlsx", sheet="scen_1")

prop_prep = scen_1[,c(1,2)]
prop_prep = prop_prep[!is.na(prop_prep[,2]),]
prop_prep0 = as.numeric(prop_prep[1,2])

r_diag_scenario = scen_1[,c(1,3)]
r_diag_scenario = r_diag_scenario[!is.na(r_diag_scenario[,2]),]