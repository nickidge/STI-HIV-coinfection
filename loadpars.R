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


# sti_main_data_1 = as.data.frame(data_raw[,c(1,7)])
sti_main_data_1 = as.data.frame(data_raw[,grepl("Year", colnames(data_raw)) | grepl("Total MSM", colnames(data_raw), fixed=T)])
sti_main_data_1[,2] = (sti_main_data_1[,2] - HIV_main_data[,3]) *  data_raw[,grepl("Gon prev: HIV-", colnames(data_raw), fixed=T)]
colnames(sti_main_data_1) = c("year", "value")
sti_main_data_1$sti_group = "sti_plus"
sti_main_data_1$HIV_group = "HIV_minus"

# sti_main_data_2 = as.data.frame(data_raw[,c(1,3)])
sti_main_data_2 = HIV_main_data[,c(1,3)]
sti_main_data_2[,2] = sti_main_data_2[,2] * data_raw[,grepl("Gon prev: HIV+", colnames(data_raw), fixed=T)]
colnames(sti_main_data_2) = c("year", "value")
sti_main_data_2$sti_group = "sti_plus"
sti_main_data_2$HIV_group = "HIV_plus"


# sti_main_data_3 = as.data.frame(data_raw[,c(1,19)])
sti_main_data_3 = diagnoses_sti0[,c(1,2)]
colnames(sti_main_data_3) = c("year", "value")
sti_main_data_3$sti_group = "diagnoses_sti"
sti_main_data_3$HIV_group = "HIV_minus"

# sti_main_data_4 = as.data.frame(data_raw[,c(1,20)])
sti_main_data_4 = diagnoses_sti0[,c(1,3)]
colnames(sti_main_data_4) = c("year", "value")
sti_main_data_4$sti_group = "diagnoses_sti"
sti_main_data_4$HIV_group = "HIV_plus"

# sti_main_data_5 = as.data.frame(data_raw[,c(1,8)])
sti_main_data_5 = prev_sti0[,c(1,2)]
colnames(sti_main_data_5) = c("year", "value")
sti_main_data_5$sti_group = "prev_sti"
sti_main_data_5$HIV_group = "HIV_minus"

# sti_main_data_6 = as.data.frame(data_raw[,c(1,9)])
sti_main_data_6 = prev_sti0[,c(1,3)]
colnames(sti_main_data_6) = c("year", "value")
sti_main_data_6$sti_group = "prev_sti"
sti_main_data_6$HIV_group = "HIV_plus"

sti_main_data = rbind(sti_main_data_1, sti_main_data_2, sti_main_data_3, sti_main_data_4, sti_main_data_5, sti_main_data_6)
sti_main_data$N = factor("data")


# compile data into list for convenience

data = list(diagnoses0, PLHIV0, prop_diag0, prop_treat0, prop_suppressed0, totalpop0, prev_sti0, PLsti0, diagnoses_sti0)
# start_year = as.numeric(data_raw[1,1])
# end_year = 2040

# cascade0 = data_raw[,c(1,4,5,6)]
cascade0 = data_raw[,grepl("Year", colnames(data_raw)) | grepl("Prop HIV", colnames(data_raw), fixed=T)]
cascade0 = cascade0[!is.na(cascade0[,2]),]

# import parameters, including upper and lower bounds (if they exist)

pars_raw = read_excel("data_sti.xlsx", sheet="pars", col_names=TRUE)

# set_pars = function(){
  if (s == 1){
    for(i in 1:nrow(pars_raw)){
      assign(paste(pars_raw[i,2]), as.numeric(pars_raw[i,3]), envir = .GlobalEnv)
      if(!is.na(pars_raw[i,4]))
      {
        assign(paste(pars_raw[i,2], "_lb", sep=""), as.numeric(pars_raw[i,4]), envir = .GlobalEnv)
        assign(paste(pars_raw[i,2], "_ub", sep=""), as.numeric(pars_raw[i,5]), envir = .GlobalEnv)
      }
    }
  } else {
    for(i in 1:nrow(pars_raw)){
      assign(paste(pars_raw[i,2]), as.numeric(pars_raw[i,3]), envir = .GlobalEnv)
      assign("temp", get(paste(pars_raw[i,2])))
      uncert_pars[i,trial] <<- as.numeric(temp)
      # new.names[i] = paste(pars_raw[i,2])
      if(!is.na(pars_raw[i,4]))
      {
        assign(paste(pars_raw[i,2], "_lb", sep=""), as.numeric(pars_raw[i,4]), envir = .GlobalEnv)
        assign(paste(pars_raw[i,2], "_ub", sep=""), as.numeric(pars_raw[i,5]), envir = .GlobalEnv)
        assign(paste(pars_raw[i,2]), runif(1,as.numeric(pars_raw[i,4]),as.numeric(pars_raw[i,5])), envir = .GlobalEnv)
       
        assign("temp", get(paste(pars_raw[i,2])))
        uncert_pars[i,trial] <<- as.numeric(temp)

        # alpha3 <<- 0.5
      }
    }
  }
# }

# set_pars()

t_exp_sti_de = t_exp_sti/(365 * dt)
# t_infect_sti_de = t_infect_sti/(365 * dt)
t_treatment_sti_de = t_treatment_sti/(365 * dt)

gel_mat_base = matrix(0, nrow=2, ncol=3, dimnames=list(c("gel_up", "gel_down"),
                                                       c("HIV- no PrEP", "HIV- PrEP", "HIV+")))
gel_mat = gel_mat_base
eff_gel = c(eff_gel_HIV, eff_gel_gon)

eff_gel_base = eff_gel
# alpha3 = 0.5

gel_mat_best_estimate =
  rbind(c(0.1, 0.2, 0.3),
        c(0.3, 0.5, 0.05))
