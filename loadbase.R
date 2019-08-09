### Base scenario ###
# This loads all the base parameters and their values over time, and creates matrices containing their interpolation

# import data from excel spreadsheet
base_raw = read_excel("data_sti.xlsx", sheet="base")

# interpolation function
strat_interp = function(strat0){
  nvars = ncol(strat0) - 1
  st_interp = array(0, dim = c(length(tvec0), nvars))
  for (i_base in 1:nvars){
    st_interp[,i_base] = approx(unlist(strat0[,1]), unlist(strat0[,(i_base+1)]), xout=tvec0, rule=2)$y
  }
  rownames(st_interp) = tvec0
  return(st_interp)
}

# HIV care cascade
cascade_goal = rbind(c(2030, 0.95, 0.95, 0.95), c(2035, 0.99, 0.99, 0.99))
cascade_scenario = rbind(as.matrix(cascade0), cascade_goal)
cascade_interp <<- array(0, dim = c(length(tvec0), 3))
for(i_base in 1:3){
  cascade_interp[,i_base] = approx(cascade_scenario[,1], cascade_scenario[,(i_base+1)], xout=tvec0, rule=2)$y
}
rownames(cascade_interp) = tvec0

# effectiveness of PrEP
eff_prep0 = cbind(c(2010,2020),c(0,eff_prep))
eff_prep_interp = approx(eff_prep0[,1], eff_prep0[,2], xout=tvec0, rule=2)$y
names(eff_prep_interp) = tvec0

# rate of diagnosis of STI, stratified by HIV status
r_diag_sti0 = base_raw[,c(1,4:6)]
r_diag_sti0_index = !is.na(r_diag_sti0[,2])
r_diag_sti0 = r_diag_sti0[r_diag_sti0_index,]
r_diag_sti_interp = array(0, dim = c(length(tvec0), 3))
for (i_base in 1:3){
  r_diag_sti_interp[,i_base] = approx(unlist(r_diag_sti0[,1]), unlist(r_diag_sti0[,(i_base+1)]), xout=tvec0, rule=2)$y
}
rownames(r_diag_sti_interp) = tvec0

# condom usage, stratified by HIV status
condom_by_HIV0 = base_raw[,c(1,7:9)]
condom_by_HIV0_index = !is.na(condom_by_HIV0[,2])
condom_by_HIV0 = condom_by_HIV0[condom_by_HIV0_index,]
condom_by_HIV_interp = array(0, dim = c(length(tvec0), 3))
for (i_base in 1:3){
  condom_by_HIV_interp[,i_base] = approx(unlist(condom_by_HIV0[,1]), unlist(condom_by_HIV0[,(i_base+1)]), xout=tvec0, rule=2)$y
}
rownames(condom_by_HIV_interp) = tvec0

# proportion of people on PrEP
prop_prep = base_raw[,c(1,2)]
prop_prep = prop_prep[!is.na(prop_prep[,2]),]
prop_prep_interp = approx(unlist(prop_prep[,1]), unlist(prop_prep[,2]), xout=tvec0, rule=2)$y
names(prop_prep_interp) = tvec0
prop_prep_interp_base = prop_prep_interp

# the relative rate of diagnosis of HIV, compared to the starting value
# this value is constant at 1 if the rate of diagnosis is constant throughout the model
r_diag_HIV_scenario = base_raw[,c(1,3)]
r_diag_HIV_scenario = r_diag_HIV_scenario[!is.na(r_diag_HIV_scenario[,2]),]
r_diag_HIV_interp = approx(unlist(r_diag_HIV_scenario[,1]), unlist(r_diag_HIV_scenario[,2]), xout=tvec0, rule=2)$y
names(r_diag_HIV_interp) = tvec0

# proportion of people who are testing for STI, stratified by HIV status
prop_testing0 = base_raw[,c(1,10:12)]
prop_testing0_index = !is.na(prop_testing0[,2])
prop_testing0 = prop_testing0[prop_testing0_index,]
prop_testing_interp = array(0, dim = c(length(tvec0), 3))
for (i_base in 1:3){
  prop_testing_interp[,i_base] = approx(unlist(prop_testing0[,1]), unlist(prop_testing0[,(i_base+1)]), xout=tvec0, rule=2)$y
}
rownames(prop_testing_interp) = tvec0

# time between testing, stratified by HIV status
t_testing0 = base_raw[,c(1,13:18)]
t_testing0_index = !is.na(t_testing0[,2])
t_testing0 = t_testing0[t_testing0_index,]
t_testing_interp = array(0, dim = c(length(tvec0), 3, 2))
for (j_base in 1:2){
  for (i_base in 1:3){
    t_testing_interp[,i_base,j_base] = approx(unlist(t_testing0[,1]), unlist(t_testing0[,(i_base+3*j_base-2)]), xout=tvec0, rule=2)$y
  }
}
rownames(t_testing_interp) = tvec0

# proportion of people who change to gel condoms from nothing
gel_up0 = base_raw[,c(1,19:21)]
gel_up0_index = !is.na(gel_up0[,2])
gel_up0 = gel_up0[gel_up0_index,]
gel_up_interp = strat_interp(gel_up0)

# proportion of people who change to gel condoms from condoms
gel_down0 = base_raw[,c(1,22:24)]
gel_down0_index = !is.na(gel_down0[,2])
gel_down0 = gel_down0[gel_down0_index,]
gel_down_interp = strat_interp(gel_down0)
