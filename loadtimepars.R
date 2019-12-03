
timepars_raw = read_excel("data_sti.xlsx", sheet="timepars", col_names = F)
timepars_raw = as.data.frame(timepars_raw)

timepars_col = ncol(timepars_raw)
timepars_row = nrow(timepars_raw)

t_dat = timepars_raw[4:timepars_row,1]
t_dat = as.numeric(unlist(t_dat))
baselist = list()
for(col in 2:timepars_col){
  par = timepars_raw[1,col]
  pop = timepars_raw[2,col]
  subpop = timepars_raw[3,col]
  
  y = as.numeric(timepars_raw[4:timepars_row, col])
 
}

par = function(t=t_dat, y=NULL, par=NA, pop=NA, subpop=NA, maxcol=0, tvec=tvec_base){
  notna_index = !is.na(y)
  
  getbase = sum(notna_index) == 0
  
  if(!getbase){
    thist = t_dat[notna_index]
    thisy = y[notna_index]
    
    const = sum(notna_index) == 1
    
    if(const){
      v = thisy
      names(v) = thist
    } else {
      v = approx(x = thist, y = thisy, rule = 2, xout = tvec)
      names(v) = tvec
    }
  }

  refvar = NA
  
  if(par == 't_testing'){
    col = -3 + 3*match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep")) + match(subpop, c("new_infection", "mid_infection", "old_infection"))
    refvar = 't_testing'
  } else if(par == 'test_wait'){
    col = match(subpop, c("new_to_mid", "mid_to_old"))
    refvar = 'test_wait'
  } else if(par == 'condom_usage'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep"))
    refvar = 'condom_usage'
  } else if(par == 'gel_up'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep"))
    refvar = 'gel_mat'
  } else if(par == 'gel_down'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep")) + 3
    refvar = 'gel_mat'
  } else if(par == 'care_cascade'){
    col = match(subpop, c("diagnosed", "diagnosed_treated", "treated_virally_suppressed"))
    refvar = 'care_cascade'
  } else if(par == 'treatment_eff'){
    col = match(subpop, c("diagnosed", "diagnosed_treated", "treated_virally_suppressed"))
    refvar = 'treatment_eff'
  }
  
  outlist = list(
    v = v,
    refvar = refvar,
    col = col,
    maxcol = maxcol,
    const = const,
    getbase = getbase
  )
  
  class(outlist) = append(class(outlist), "par")
  return(outlist)
  
}