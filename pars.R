
modelpar = function(t=t_dat, y=NULL, parname=NA, pop=NA, subpop=NA, tvec=tvec_base){
  notna_index = !is.na(y)
  
  getbase = sum(notna_index) == 0
  
  if(!getbase){
    thist = t[notna_index]
    thisy = y[notna_index]
    
    thisy[thisy %in% c('Inf', 'inf')] = Inf
    
    const = sum(notna_index) == 1
    
    if(const){
      # # v = thisy
      # # names(v) = thist
      # v = rep(NA, length(tvec))
      v = rep(thisy, length(tvec))  ### is this right or should it be the line above?
      names(v) = tvec
      if(as.character(thist) %in% tvec){
        v[as.character(thist)] = thisy
      } else {
        v[1] = thisy
      }
    } else {
      v = approx(x = thist, y = thisy, rule = 2, xout = tvec)
      v = setNames(v$y, v$x)
    }
  } else {
    v = NULL
    const = NULL
  }
  
  refvar = NA
  
  popindex = c("HIV_low_risk", "HIV_high_risk", "HIV_prep")
  careindex = c("diagnosed", "diagnosed_treated", "treated_virally_suppressed")
  if(parname == 't_testing'){
    col = -8 + 2*match(substr(pop, 1, 2), HIV_risk_labs) +
      1*match(substr(pop, 4, 6), timeindex) +
      6*match(subpop, med_labs)
    refvar = 't_testing'
    maxcol = 12
  } else if(parname == 'test_wait'){
    col = match(subpop, c("new_to_mid", "mid_to_old"))
    refvar = 'test_wait'
    maxcol = 1
  } else if(parname == 'condom_usage'){
    col = -3 + 1*match(pop, popindex) + 3*match(subpop, med_labs)
    refvar = 'condom_usage'
    maxcol = 6
  } else if(parname == 'gel_up'){
    col = match(pop, popindex)
    refvar = 'gel_mat'
    maxcol = 6
  } else if(parname == 'gel_down'){
    col = match(pop, popindex) + 3
    refvar = 'gel_mat'
    maxcol = 6
  } else if(parname == 'care_cascade'){
    col = match(subpop, careindex[2:3])
    refvar = 'care_cascade'
    maxcol = 2
  } else if(parname == 'treatment_eff'){
    col = match(subpop, careindex)
    refvar = 'treatment_eff'
    maxcol = 3
  } else if(parname == 'eff_condom'){
    col = 1
    refvar = 'eff_condom'
    maxcol = 1
  } else if(parname == 'relative_foi'){
    col = -3 + 1*match(pop, popindex) + 3*match(subpop, med_labs)
    refvar = 'risk_mat'
    maxcol = 6
  } else if(parname == 'num_prep'){
    col = match(subpop, med_labs)
    refvar = 'num_prep'
    maxcol = 2
  } else if(parname == 'eff_prep'){
    col = 1
    refvar = 'eff_prep'
    maxcol = 1
  } else if(parname == 'prop_high_risk'){
    col = 1
    refvar = 'prop_high_risk'
    maxcol = 1
  } else if(parname == 'prop_medi'){
    col = 1
    refvar = 'prop_medi'
    maxcol = 1
  } else if(parname == 'medimix'){
    col = 1
    refvar = 'medimix'
    maxcol = 1
  } else if(parname == 'ineligibility_duration'){
    col = 1
    refvar = 'stay_time'
    maxcol = 1
  } else if(parname == 'proportion_stay'){
    col = 1
    refvar = 'stay_prop'
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
        thispar[,i][] = deflist[[parname]][,i]
      }
    }
    notnas = apply(thispar, 2, function(x) length(which(!is.na(x))))
    
    thismat = apply(thispar, 2, function(y) fill_vec(x=thist, y=y))
    
    if(all(apply(thismat, 2, function(x) length(unique(x)) == 1) == TRUE)){
      thismat = thismat[1,,drop=FALSE]
      rownames(thismat) = NULL
    }
      
    outlist[[parname]] = thismat
  }
  for(parname in names(deflist)){
    if(!(parname %in% names(outlist))){
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

add_data_col = function(thislist, t_dat, data_col, deflist = baselist, syear = NA){
  
  parname = data_col[1]
  pop = data_col[2]
  subpop = data_col[3]
  y = as.numeric(data_col[4:length(data_col)])
  
  thispar = modelpar(t=t_dat, y=y, parname=parname, pop=pop, subpop=subpop, tvec=tvec_base)
  thisname = thispar$refvar
  thiscol = thispar$col
  
  tvec_m = tvec_base[(syear < tvec_base) & (tvec_base < (syear + 1))]
  
  if(!(thisname %in% names(thislist))){
    thisentry = matrix(NA, nrow = length(tvec_base), ncol = thispar$maxcol,
                       dimnames = list(tvec_base, 1:thispar$maxcol))
  } else {
    thisentry = thislist[[thisname]]
  }
  colnames(thisentry)[thiscol] = paste(parname, pop, subpop, sep='__')
  thisentrycol = thisentry[,thiscol]
  
  if(thispar$getbase | !is.na(syear)){
    if(thisname %in% names(deflist)){
      if(is.matrix(deflist[[thisname]])){
        baseentry = deflist[[thisname]][,thiscol]
        if(nrow(deflist[[thisname]]) == 1){
          thisentrycol[] = baseentry
          baseentry = thisentrycol
        }
      } else {
        baseentry = deflist[[thisname]][thiscol]
        thisentrycol[1] = baseentry
      }
    } else if(paste0(thisname, '_interp') %in% names(deflist)){
      baseentry = deflist[[paste0(thisname, '_interp')]][,thiscol]
    } else {
      print(paste0("Variable '", thisname, "' not in defaultlist at all"))
      return(thislist)
    }
    
    baseentrylen = length(baseentry)
    thisentrycollen = length(thisentrycol)
    
    if(baseentrylen == 1){
      thisentrycolindex = 1
      print(thisname)
      print('this probably shouldnt be happening anymore')
    } else if(baseentrylen == thisentrycollen){
      thisentrycolindex = 1:baseentrylen
    } else if(baseentrylen < thisentrycollen){
      print('baseentry is shorter than thisentrycol')
      thisentrycolindex = 1:baseentrylen
    } else if(baseentrylen > thisentrycollen){
      print('baseentry is longer than thisentrycol')
      thisentrycolindex = 1:thisentrycollen
    }
    
    if(thispar$getbase){
      thisentrycol[thisentrycolindex] = baseentry[thisentrycolindex]
    } else {
      vl = baseentry[tvec_base <= syear]
      vh = thispar$v[tvec_base >= (syear + 1)]
      
      vm = approx(x = c(syear, syear + 1), y=c(vl[as.character(syear)], vh[as.character(syear + 1)]), xout=tvec_m,
                  method = 'constant',
                  f=1)
      vm = setNames(vm$y, vm$x)
      
      v = c(vl, vm, vh)
      thisentrycol = v
    }
    
  } else {
    thisentrycol = thispar$v
  }
  
  thisentry[,thiscol] = thisentrycol
  thislist[[thisname]] = thisentry
  
  return(thislist)
}

constant_prep = function(num_prep){
  if(nrow(num_prep) > 1){
    final_prep = tail(unique(num_prep), 1)
    final_year = rownames(final_prep)
    which_years = rownames(num_prep)[as.numeric(rownames(num_prep)) > as.numeric(final_year)]
    num_prep[which_years,] = sweep(popsize[which_years,], 2, final_prep / popsize[final_year,], FUN="*")
    num_prep = fixnan(num_prep)
  }
  return(num_prep)
}

smooth_par = function(thispar, thiscol=1){
  which_unique = which(thispar[,thiscol] != lag(thispar[,thiscol], default=-1))
  unique_ends = c(head(thispar[which_unique,thiscol], 1), tail(thispar[which_unique,thiscol], 1))
  thispar[which_unique,thiscol] = approx(names(unique_ends), unique_ends, xout=names(which_unique))$y
  return(thispar)
}

load_time_par_sheet = function(sheetname, deflist = baselist, syear=NA){
  
  timepars_raw = read_excel("data_sti.xlsx", sheet=sheetname, col_names = F)
  timepars_raw = as.data.frame(timepars_raw)
  
  timepars_col = ncol(timepars_raw)
  timepars_row = nrow(timepars_raw)
  
  t_dat = timepars_raw[4:timepars_row,1]
  t_dat = as.numeric(unlist(t_dat))
  
  thislist = list()
  for(col in 2:timepars_col){
    thislist = add_data_col(thislist, t_dat, timepars_raw[,col], deflist = deflist, syear = syear)
  }
  
  thislist = fill_list(thislist, deflist = deflist)
  
  # thislist$num_prep = constant_prep(thislist$num_prep)
  to_smooth = list('prop_high_risk' = 1)
  for(i_smooth in names(to_smooth)){
    for(j_smooth in 1:length(to_smooth[i_smooth])){
      thislist[[i_smooth]] = smooth_par(thislist[[i_smooth]], j_smooth)
    }
  }

  
  return(thislist)
  
}


