
modelpar = function(t=t_dat, y=NULL, parname=NA, pop=NA, subpop=NA, tvec=tvec_base){
  notna_index = !is.na(y)
  
  getbase = sum(notna_index) == 0
  
  if(!getbase){
    thist = t_dat[notna_index]
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

baselist = list()
fill_list = function(parlist, deflist=baselist){
  outlist = list()
  for(parname in names(parlist)){
    thispar = parlist[[parname]]
    thist = rownames(thispar)
    notnas = apply(thispar, 2, function(x) length(which(!is.na(x))))
    if(any(notnas > 1)){
      thismat = apply(thispar, 2, function(y) fill_vec(x=thist, y=y))
    } else {
      thismat = matrix(apply(thispar, 2, function(x) ifelse(length(which(!is.na(x))) == 0, NA, x[which(!is.na(x))])), nrow=1)
    }
    outlist[[parname]] = thismat
  }
  for(parname in optvarkeys){
    if(!(parname %in% names(outlist))){
      print(paste0(parname, ' not in timepars sheet'))
      if(parname %in% names(deflist)){
        outlist[[parname]] = deflist[[parname]]
      } else if(paste0(parname, '_interp') %in% names(deflist)){
        outlist[[parname]] = deflist[[paste0(parname, '_interp')]]
      } else {
        print(paste0("Cannot find '", parname, "' parameter anywhere but its in optkeys"))
      }
    }
  }
  return(outlist)
}
