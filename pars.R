
modelpar = function(t=t_dat, y=NULL, parname=NA, pop=NA, subpop=NA, tvec=tvec_base){
  notna_index = !is.na(y)
  
  getbase = sum(notna_index) == 0
  
  if(!getbase){
    thist = t[notna_index]
    thisy = y[notna_index]
    
    const = sum(notna_index) == 1
    
    if(const){
      # v = thisy
      # names(v) = thist
      v = rep(NA, length(tvec))
      names(v) = tvec
      if(as.character(thist) %in% tvec){
        v[thist] = thisy
      } else {
        v[1] = thisy
      }
    } else {
      v = approx(x = thist, y = thisy, rule = 2, xout = tvec)
      v = setNames(v$y, v$x)
      # names(v) = tvec
    }
  } else {
    v = NULL
    const = NULL
  }
  
  refvar = NA
  
  if(parname == 't_testing'){
    col = -3 + 3*match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep")) + match(subpop, c("new_infection", "mid_infection", "old_infection"))
    refvar = 't_testing'
    maxcol = 9
  } else if(parname == 'test_wait'){
    col = match(subpop, c("new_to_mid", "mid_to_old"))
    refvar = 'test_wait'
    maxcol = 2
  } else if(parname == 'condom_usage'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep"))
    refvar = 'condom_usage'
    maxcol = 3
  } else if(parname == 'gel_up'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep"))
    refvar = 'gel_mat'
    maxcol = 6
  } else if(parname == 'gel_down'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep")) + 3
    refvar = 'gel_mat'
    maxcol = 6
  } else if(parname == 'care_cascade'){
    col = match(subpop, c("diagnosed", "diagnosed_treated", "treated_virally_suppressed"))
    refvar = 'care_cascade'
    maxcol = 3
  } else if(parname == 'treatment_eff'){
    col = match(subpop, c("diagnosed", "diagnosed_treated", "treated_virally_suppressed"))
    refvar = 'treatment_eff'
    maxcol = 3
  } else if(parname == 'eff_condom'){
    col = 1
    refvar = 'eff_condom'
    maxcol = 1
  } else if(parname == 'relative_foi'){
    col = match(pop, c("HIV_low_risk", "HIV_high_risk", "HIV_prep"))
    refvar = 'risk_mat'
    maxcol = 3
  } else if(parname == 'num_prep'){
    col = 1
    refvar = 'num_prep'
    maxcol = 1
  } else {
    col = 0
    refvar = 'cant_find'
    maxcol = 0
    print(parname)
  }
  
  outlist = list(
    v = v,
    refvar = refvar,
    col = col,
    maxcol = maxcol,
    const = const,
    getbase = getbase
  )
  
  class(outlist) = append(class(outlist), "modelpar")
  return(outlist)
  
}


fill_vec = function(x, y){
  if(length(x) != length(y)){
    print('Different length vectors for fill_vec!')
    return()
  }
  
  if(any(is.na(x))){
    print('NA value in x')
    return()
  }
  
  notna = !is.na(y)
  if(sum(notna) == 0){
    print('No present values!')
    return()
  } else if(sum(notna) == 1){
    v = rep(y[notna], length(x))
    names(v) = x
    return(v)
  } else {
    v = approx(x = x[notna], y = y[notna], xout = x, rule=2)
    v = setNames(v$y, v$x)
    return(v)
  }
}

fill_list = function(parlist, deflist=baselist){
  outlist = list()
  for(parname in names(parlist)){
    thispar = parlist[[parname]]
    thist = rownames(thispar)
    notnas = apply(thispar, 2, function(x) length(which(!is.na(x))))
    for(i in 1:ncol(thispar)){
      if(notnas[i] == 0){
        thispar[,i] = deflist[[parname]][,i]
      }
    }
    notnas = apply(thispar, 2, function(x) length(which(!is.na(x))))
    if(any(notnas > 1)){
      thismat = apply(thispar, 2, function(y) fill_vec(x=thist, y=y))
    } else {
      thismat = matrix(apply(thispar, 2, function(x) ifelse(length(which(!is.na(x))) == 0, NA, x[which(!is.na(x))])), nrow=1)
    }
    outlist[[parname]] = thismat
  }
  # for(parname in optvarkeys){
  for(parname in names(deflist)){
    if(!(parname %in% names(outlist))){
      # print(paste0(parname, ' not in timepars sheet'))
      if(parname %in% names(deflist)){
        outlist[[parname]] = deflist[[parname]]
      } else if(paste0(parname, '_interp') %in% names(deflist)){
        outlist[[parname]] = deflist[[paste0(parname, '_interp')]]
      } else {
        # print(paste0("Cannot find '", parname, "' parameter anywhere but its in optkeys"))
      }
    }
  }
  return(outlist)
}

add_data_col = function(thislist, t_dat, data_col, deflist = baselist){
  
  parname = data_col[1]
  pop = data_col[2]
  subpop = data_col[3]
  y = as.numeric(data_col[4:length(data_col)])
  
  thispar = modelpar(t=t_dat, y=y, parname=parname, pop=pop, subpop=subpop, tvec=tvec_base)
  thisname = thispar$refvar
  thiscol = thispar$col
  
  if(!(thisname %in% names(thislist))){
    thislist[[thisname]] = matrix(NA, nrow = length(tvec_base), ncol = thispar$maxcol,
                                  dimnames = list(tvec_base, 1:thispar$maxcol))
  }
  colnames(thislist[[thisname]])[thiscol] = paste(parname, pop, subpop, sep='__')
  
  if(thispar$getbase){
    if(thisname %in% names(deflist)){
      if(is.matrix(deflist[[thisname]])){
        if(nrow(deflist[[thisname]]) > 1){
          thislist[[thisname]][,thiscol] = deflist[[thisname]][,thiscol]
        } else {
          thislist[[thisname]][1,thiscol] = deflist[[thisname]][1,thiscol]
        }
      } else {
        thislist[[thisname]][1,thiscol] = deflist[[thisname]][thiscol]
      }
    } else if(paste0(thisname, '_interp') %in% names(deflist)){
      tempname = paste0(thisname, '_interp')
      thislist[[thisname]][,thiscol] = deflist[[tempname]][,thiscol]
    } else {
      print(paste0("Variable '", thisname, "' not in defaultlist at all"))
    }
  } else {
    thislist[[thisname]][,thiscol] = thispar$v
  }
  
  return(thislist)
}


load_time_par_sheet = function(sheetname, deflist = baselist){
  
  timepars_raw = read_excel("data_sti.xlsx", sheet=sheetname, col_names = F)
  timepars_raw = as.data.frame(timepars_raw)
  
  timepars_col = ncol(timepars_raw)
  timepars_row = nrow(timepars_raw)
  
  t_dat = timepars_raw[4:timepars_row,1]
  t_dat = as.numeric(unlist(t_dat))
  
  thislist = list()
  for(col in 2:timepars_col){
    thislist = add_data_col(thislist, t_dat, timepars_raw[,col], deflist = deflist)
  }
  
  thislist = fill_list(thislist, deflist = deflist)
  
}

