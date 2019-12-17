### Loading Parameters and Data ###

# import data

data_raw = read_excel("data_sti.xlsx", sheet="data")

# create matrices from data and omit absent values,
# but create an index for which values thes are.
# this is only necessary for PLHIV0 and prop_diag0 for now

data_years = data_raw[,1]
cascade0 = data_raw[,c(1,4,5,6)]
cascade0 = cascade0[!is.na(cascade0[,2]),]
diagnoses0 = data_raw[,c(1,2)]
diagnoses0 = diagnoses0[!is.na(diagnoses0[,2]),];
PLHIV0 =  data_raw[,c(1,3)]
PLHIV0_index = !is.na(PLHIV0[,2]) # [1:max(which(!is.na(PLHIV0[,2])==TRUE))]
PLHIV0 = PLHIV0[!is.na(PLHIV0[,2]),]
prop_diag0 =  data_raw[,c(1,4)]
prop_diag0_index = !is.na(prop_diag0[,2]) # [1:max(which(!is.na(prop_diag0[,2])==TRUE))]
prop_diag0 = prop_diag0[!is.na(prop_diag0[,2]),]
prop_treat0 =  data_raw[,c(1,5)]
prop_treat0 = prop_treat0[!is.na(prop_treat0[,2]),]
prop_suppressed0 =  data_raw[,c(1,6)]
prop_suppressed0 = prop_suppressed0[!is.na(prop_suppressed0[,2]),]
totalpop0 =  data_raw[,c(1,7)] 
totalpop0 = totalpop0[!is.na(totalpop0[,2]),]


# prev_sti0 = data_raw[,c(1,8,9)]
prev_sti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Gon prev", colnames(data_raw))]
prev_sti0_index = !is.na(prev_sti0[,2])
prev_sti0 = prev_sti0[prev_sti0_index[,1],]

# PLsti0 = data_raw[,c(1,12,13)]
PLsti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total Gon+ HIV", colnames(data_raw), fixed=T)]
PLsti0_index = !is.na(PLsti0[,2])
PLsti0 = PLsti0[PLsti0_index[,1],]

# diagnoses_sti0 = data_raw[,c(1,15,16)]
diagnoses_sti0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total Gon cases HIV", colnames(data_raw), fixed=T)]
diagnoses_sti0_index = !is.na(diagnoses_sti0[,2])
diagnoses_sti0 = diagnoses_sti0[diagnoses_sti0_index[,1],]


# HIV_main_data = as.data.frame(data_raw[,1:3])
HIV_main_data = as.data.frame(data_raw[,grepl("Year", colnames(data_raw)) | grepl("New diagnoses", colnames(data_raw), fixed=T) | grepl("Total PLHIV", colnames(data_raw), fixed=T)])
colnames(HIV_main_data) = c("year", "diagnoses_HIV", "value")
HIV_main_data$N = factor("data")
HIV_main_data$HIV_group = "HIV_plus"


# # sti_main_data_1 = as.data.frame(data_raw[,c(1,7)])
# sti_main_data_1 = as.data.frame(data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total MSM", colnames(data_raw), fixed=T)])
# sti_main_data_1[,2] = (sti_main_data_1[,2] - HIV_main_data[,3]) *  data_raw[,grepl("Gon prev: HIV-", colnames(data_raw), fixed=T)]
# colnames(sti_main_data_1) = c("year", "value")
# sti_main_data_1$sti_group = "sti_plus"
# sti_main_data_1$HIV_group = "HIV_minus"
# 
# # sti_main_data_2 = as.data.frame(data_raw[,c(1,3)])
# sti_main_data_2 = HIV_main_data[,c(1,3)]
# sti_main_data_2[,2] = sti_main_data_2[,2] * data_raw[,grepl("Gon prev: HIV+", colnames(data_raw), fixed=T)]
# colnames(sti_main_data_2) = c("year", "value")
# sti_main_data_2$sti_group = "sti_plus"
# sti_main_data_2$HIV_group = "HIV_plus"
# 
# 
# # sti_main_data_3 = as.data.frame(data_raw[,c(1,19)])
# sti_main_data_3 = diagnoses_sti0[,c(1,2)]
# colnames(sti_main_data_3) = c("year", "value")
# sti_main_data_3$sti_group = "diagnoses_sti"
# sti_main_data_3$HIV_group = "HIV_minus"
# 
# # sti_main_data_4 = as.data.frame(data_raw[,c(1,20)])
# sti_main_data_4 = diagnoses_sti0[,c(1,3)]
# colnames(sti_main_data_4) = c("year", "value")
# sti_main_data_4$sti_group = "diagnoses_sti"
# sti_main_data_4$HIV_group = "HIV_plus"
# 
# # sti_main_data_5 = as.data.frame(data_raw[,c(1,8)])
# sti_main_data_5 = prev_sti0[,c(1,2)]
# colnames(sti_main_data_5) = c("year", "value")
# sti_main_data_5$sti_group = "prev_sti"
# sti_main_data_5$HIV_group = "HIV_minus"
# 
# # sti_main_data_6 = as.data.frame(data_raw[,c(1,9)])
# sti_main_data_6 = prev_sti0[,c(1,3)]
# colnames(sti_main_data_6) = c("year", "value")
# sti_main_data_6$sti_group = "prev_sti"
# sti_main_data_6$HIV_group = "HIV_plus"
# 
# sti_main_data = rbind(sti_main_data_1, sti_main_data_2, sti_main_data_3, sti_main_data_4, sti_main_data_5, sti_main_data_6)
# sti_main_data$N = factor("data")


PLHIV_dat = data.frame(t = PLHIV0$Year, value = PLHIV0$`Total PLHIV`, type='pop', dt=1, pid = 'PLHIV_tot',
                       sti_pop='all', risk_pop='all', HIV_pop='PLHIV', source='data', scen='')
HIV_diag_dat = data.frame(t = diagnoses0$Year, value = diagnoses0$`New diagnoses`, type='trans', dt=1, pid='HIV_diag_tot',
                          sti_pop='all', risk_pop='all', HIV_pop='HIV_diag', source='data', scen='')
prop_diag_dat = data.frame(t = cascade0$Year, value = cascade0$`Prop HIV diagnosed`, type='pop', dt=1, pid='num_diag_prop',
                          sti_pop='all', risk_pop='all', HIV_pop='num_diag', source='data', scen='', plot='care_cascade')

all_dat = rbind.fill(PLHIV_dat, HIV_diag_dat, prop_diag_dat)
all_dat$plot[is.na(all_dat$plot)] = all_dat$HIV_pop[is.na(all_dat$plot)]
all_dat$scen = 'data'
all_dat$scen_long = 'Data'
data_wide = widen_sources(all_dat)



# compile data into list for convenience

data = list(diagnoses0, PLHIV0, prop_diag0, prop_treat0, prop_suppressed0, totalpop0, prev_sti0, PLsti0, diagnoses_sti0)
# start_year = as.numeric(data_raw[1,1])
# end_year = 2040

# cascade0 = data_raw[,c(1,4,5,6)]
cascade0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Prop HIV", colnames(data_raw), fixed=T)]
cascade0 = cascade0[!is.na(cascade0[,2]),]

# HIV care cascade
cascade_goal = rbind(c(2030, 0.95, 0.95, 0.95), c(2035, 0.99, 0.99, 0.99))
cascade_scenario = rbind(as.matrix(cascade0), cascade_goal)
cascade_interp <<- array(0, dim = c(length(tvec_base), 3))
for(i_base in 1:3){
  cascade_interp[,i_base] = approx(cascade_scenario[,1], cascade_scenario[,(i_base+1)], xout=tvec_base, rule=2)$y
}
rownames(cascade_interp) = tvec_base

# import parameters, including upper and lower bounds (if they exist)

pars_raw = read_excel("data_sti.xlsx", sheet="pars", col_names=TRUE)

static_pars = list()
for(i in 1:nrow(pars_raw)){
  thisname = paste0(pars_raw[i,2])
  thisval = as.numeric(pars_raw[i,3])
  
  if(!is.na(pars_raw[i,4]))
  {
    thislb = as.numeric(pars_raw[i,4])
    thisub = as.numeric(pars_raw[i,5])

  } else {
    thislb = thisval
    thisub = thisval
  }
  # assign(thisname, thisval, envir = .GlobalEnv)
  # assign(paste0(thisname, "_lb"), thislb, envir = .GlobalEnv)
  # assign(paste0(thisname, "_ub"), thisub, envir = .GlobalEnv)
  thispar = list('name' = thisname,
                 'v' = thisval,
                 'lb' = thislb,
                 'ub' = thisub)
  static_pars[[thisname]] = thispar
}


