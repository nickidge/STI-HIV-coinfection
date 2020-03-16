# ### Recursive Model ###
# 
# # model function
# # takes initial conditions and tvec as input, and outputs the model as a matrix
run_model = function(y0=NULL, tvec=tvec_base, modelpars=list(), options=list(), spars=static_pars){
  
  interpvars = c()
  for(key in names(modelpars)){
    thispar = modelpars[[key]]
    if(is.matrix(thispar)){
      if(nrow(thispar) == 1){
        assign(key, thispar[1,])
      } else {
        interpvars = c(interpvars, key)
      }
    } else {
      assign(key, thispar)
    }
  }
  
  for(spar in spars){
    assign(spar$name, spar$v)
  }
  
  if(is.null(y0)){
    
    init_popsize = c(init_pop_aus, init_pop_int)
    
    # S
    y0 = SID_mat
    # y0[1,1,] = popsize[as.character(tvec[1]),]
    y0[1,1,] = init_popsize
    
    # risk
    prop_high_risk = modelpars$prop_high_risk[1,1]
    y0[sHIV$S_hi,,2] = y0[sHIV$S_hi,,2] + y0[sHIV$S_lo,,2]
    y0[sHIV$S_lo,,2] = 0
    num_high_risk = y0[sHIV$S_lo,,1] * prop_high_risk
    y0[sHIV$S_lo,,1] = y0[sHIV$S_lo,,1] - num_high_risk
    y0[sHIV$S_hi,,1] = y0[sHIV$S_hi,,1] + num_high_risk
    
    # HIV
    # ninf_HIV = y0[sHIV$S,,] * init_prev_HIV
    ninf_HIV = sweep(y0[sHIV$S,,,drop = FALSE], 3, c(init_prev_HIV_aus, init_prev_HIV_int), FUN="*")
    y0[sHIV$S,,] = y0[sHIV$S,,,drop = FALSE] - ninf_HIV
    y0[sHIV$I_new,,] = y0[sHIV$I_new,,, drop = FALSE] + ninf_HIV
    
    nlate_HIV = y0[sHIV$I_new,,,drop = FALSE] * init_late_prop
    y0[sHIV$I_new,,] = y0[sHIV$I_new,,,drop = FALSE] - nlate_HIV
    y0[sHIV$I_old,,] = y0[sHIV$I_old,,,drop = FALSE] + nlate_HIV
    
    ndiag_HIV = y0[sHIV$I,,, drop = FALSE] * init_diag_prop
    y0[sHIV$I,,] = y0[sHIV$I,,, drop = FALSE] - ndiag_HIV
    dimnames(ndiag_HIV)[[1]] = paste0('D1_', substr(dimnames(ndiag_HIV)[[1]], 3, 4))
    ndiag_HIV = acast(melt(ndiag_HIV), Var1 ~ Var2 ~ Var3, fun.aggregate = sum)
    y0[sHIV$D1,,] = y0[sHIV$D1,,, drop = FALSE] + ndiag_HIV
    
    # # STI
    # ninf_STI = y0[,'S',] * init_prev_STI
    
  } else {
    init_popsize = colSums(y0, dims=2)
  }
  
  popsize_t = tvec_base[tvec_base >= min(tvec)]
  popsize = makearray(list(popsize_t, med_labs))
  popsize[1,] = init_popsize
  for(j in 1:ncol(popsize)){
    popsize[,j] = popsize[1,j] * (matrix(annual_pop_growth, ncol=ncol(popsize))[,j]) ^ ((popsize_t - popsize_t[1]))
  }
  
  num_prep = constant_prep(modelpars$num_prep, popsize)
  if(nrow(num_prep) > 1){
    modelpars$num_prep = num_prep
  } else {
    interpvars = setdiff(interpvars, 'num_prep')
  }
  
  # initialise model array
  SID = replicate(length(tvec), SID_mat)
  SID = aperm(SID, c(4,1,2,3))
  dimnames(SID)[[1]] = tvec
  
  HIV_trans = makearray(list(HIV_transitions[,'trans'], STI_labs, med_labs))
  HIV_p = setNames(numeric(nrow(HIV_transitions)), HIV_transitions[,'trans'])
  
  HIV_trans_log = makearray(list(tvec, HIV_transitions[,"trans"], STI_labs, med_labs))
  deaths_log = makearray(list(tvec, HIV_labs, STI_labs, med_labs))
  popgrowth_log = makearray(list(tvec, colnames(popsize)))
  
  t_total = 0
  t_start = Sys.time()
  
  for (t in 1:(length(tvec))){
    
    # TT is the time counter - the actual year
    TT = as.character(tvec[t])
    
    for(key in interpvars){
      assign(key, modelpars[[key]][TT,])
    }
    
    thispopsize = popsize[TT,]
    
    # initialise temp
    if(t==1){
      prevdt = y0
      prevdt[1, 1,] = thispopsize - rowSums(aperm(y0, c(3, 1, 2))) + prevdt[1, 1, ]
    } else {
      prevdt = adrop(SID[t-1,,,, drop = FALSE], 1)
    }
    
    condom_thru = 1 - condom_usage * eff_condom
    
    #########################
    
    # risk and prep
    
    # calculate risk transitions
    pop_lo = prevdt[sHIV[['lo']],,1]
    pop_hi = prevdt[sHIV[['hi']],,1] + prevdt[sHIV[['pr']],,1]
    
    pop_lo_sum = sum(pop_lo)
    pop_hi_sum = sum(pop_hi)
    
    if(pop_lo_sum > 0){
      become_high_risk = ((pop_lo_sum + pop_hi_sum) * prop_high_risk - pop_hi_sum) / pop_lo_sum * pop_lo
    } else if(pop_hi_sum > 0){
      become_high_risk = (1 - prop_high_risk) * pop_hi
    } else {
      become_high_risk = 0
    }
    if(any(become_high_risk < -100)){
      print('negative become_high_risk')
    }
    
    prevdt[sHIV[['lo']],,1] = prevdt[sHIV[['lo']],,1] - become_high_risk
    prevdt[sHIV[['hi']],,1] = prevdt[sHIV[['hi']],,1] + become_high_risk
    
    if(any(num_prep > 0)){
      for(i_med in 1:length(med_labs)){
        
        # calculate prep transitions
        pop_hi = prevdt[sHIV[['und_hi']],,i_med]
        pop_pr = prevdt[sHIV[['und_pr']],,i_med]
        
        if(sum(pop_pr) == num_prep[i_med]){
          num_to_prep = 0
        } else if(sum(pop_pr) > num_prep[i_med]){
          num_to_prep = (num_prep[i_med] - sum(pop_pr)) / sum(pop_pr) * pop_pr
        } else if((sum(pop_hi) + sum(pop_pr)) <= num_prep[i_med]){
          # num_to_prep = sum(pop_hi)
          num_to_prep = pop_hi
        } else {
          num_to_prep = (num_prep[i_med] - sum(pop_pr)) / sum(pop_hi) * pop_hi
        }
        
        prevdt[sHIV[['und_hi']],,i_med] = prevdt[sHIV[['und_hi']],,i_med] - num_to_prep
        prevdt[sHIV[['und_pr']],,i_med] = prevdt[sHIV[['und_pr']],,i_med] + num_to_prep
        
      }
    }
    

    #################
    
    
    # calculate force of infections
    totalppl = sum(prevdt)
    # rel_inf_STI = sum(prevdt[,sSTI$I,])
    rel_inf_STI = 0
    
    mix_pops = makearray(list(c('inf', 'pop'), medi_states))
    for(i_med in med_labs){
      for(i_risk in HIV_risk_labs){
        mix_to = paste0(i_risk, '_', i_med)
        mix_pops['inf', mix_to] = sum(prevdt[sHIV[[paste0('I_', i_risk)]],,i_med])
        mix_pops['pop', mix_to] = sum(prevdt[sHIV[[i_risk]],,i_med])
        # mix_pops['inf', mix_to] = mix_pops['inf', mix_to] + sum(treatment_eff[1]*prevdt[paste0('D1_', i_risk),,i_med] + treatment_eff[2]*prevdt[paste0('D2_', i_risk),,i_med] + treatment_eff[3]*prevdt[paste0('D3_', i_risk),,i_med])
        mix_pops['inf', mix_to] = mix_pops['inf', mix_to] + sum(treatment_eff*prevdt[paste0('D', 1:3, '_', i_risk),,i_med])
        mix_pops['pop', mix_to] = mix_pops['pop', mix_to] + sum(prevdt[sHIV[[paste0('D_', i_risk)]],,i_med])
      }
    }
    
    foi_mix = mix_pops['inf',] %*% mixing / mix_pops['pop',] %*% mixing
    foi_mix = fixnan(foi_mix)
    
    
    foi_HIV = condom_thru * f_infect_HIV * foi_mix
    foi_HIV[grepl('pr_', colnames(foi_HIV))] = foi_HIV[grepl('pr_', colnames(foi_HIV))] * (1 - eff_prep)
    foi_HIV[2:3] = foi_HIV[2:3] * high_risk_factor
    foi_HIV[4:6] = foi_HIV[4:6] * int_factor
    foi_HIV = fixnan(foi_HIV)
    
    foi_STI = f_infect_STI * rel_inf_STI / totalppl
    
    
    ###### TRANSITIONS ######
    
    # HIV #
    HIV_p[] = 0
    
    # infections
    HIV_p[tHIV$inf] = as.vector(apply(prevdt[sHIV$S,,,drop=FALSE], MARGIN=c(1,3), FUN=sum)) * foi_HIV
    
    # waiting undiagnosed
    HIV_p[tHIV$wait_1] = duration2rate(test_wait[1])
    
    for(i_med in 1:length(med_labs)){
      this_testing = t_testing[((6 * i_med) - 5) : ((6 * i_med))] / 2
      HIV_p[paste0(sHIV$I, '_d_', med_labs[i_med])] = duration2rate(this_testing)
    }
    
    # ensure all transitions are as expected
    if(any(is.nan(HIV_p))) print('nan values in transitions?!')
    if(any(HIV_p < -1e-6)){
      print('negative transitions?!')
    }
    if(any(HIV_p[tHIV$inf] > 1) & t > 2){
      print('everyone infected?!')
    }
    HIV_p[HIV_p > 1] = 1
    HIV_p[HIV_p < 0] = 0
    
    HIV_trans[] = prevdt[HIV_transitions[,'from'],,,drop=FALSE] * as.numeric(HIV_p)
    for(i in 1:length(med_labs)){
      HIV_trans[as.numeric(HIV_transitions[,"med"]) == i,,-i] = 0
    }

    trip = 0
    for(i_med in 1:length(med_labs)){
      for(j_risk in 1:length(HIV_risk_labs)){
        risk_lab = HIV_risk_labs[j_risk]
        
        # care cascade info
        d1 = sum(prevdt[sHIV[[paste0('D1_', risk_lab)]],,i_med]) + get_movement(paste0('D1_', risk_lab), HIV_trans, med=i_med, sum=TRUE)
        d2 = sum(prevdt[sHIV[[paste0('D2_', risk_lab)]],,i_med]) + get_movement(paste0('D2_', risk_lab), HIV_trans, med=i_med, sum=TRUE)
        d3 = sum(prevdt[sHIV[[paste0('D3_', risk_lab)]],,i_med]) + get_movement(paste0('D3_', risk_lab), HIV_trans, med=i_med, sum=TRUE)
        d1plus = d1 + d2 + d3
        d2plus = d2 + d3
        
        # calculate care cascade transitions
        
        if(d1plus > 0){
          prop1_to_2 = (care_cascade[1] * (d1plus) - d2plus) / d1
          eff_d2 = d2 + prop1_to_2 * d1
          if(eff_d2 == 0){
            prop2_to_3 = 0
          } else {
            prop2_to_3 = (care_cascade[2] * (d2 + d3) - d3) / eff_d2
          }
          if(prop1_to_2 < -1e-1 | prop1_to_2 > 1) {print('Prop 1 to 2 not between 0 and 1!!')}
          if(prop2_to_3 < -1e-1 | prop2_to_3 > 1) {
            trip = 1
            print('Prop 2 to 3 not between 0 and 1!!')
          }
          
          HIV_p[paste0('treat_', risk_lab)] = prop1_to_2
          HIV_p[paste0('viral_supp_', risk_lab)] = prop2_to_3
          
          num1_to_2 = prop1_to_2 * (rowSums(aperm(prevdt[sHIV[[paste0('D1_', risk_lab)]],,i_med, drop = FALSE], c(2,1,3))) + rowSums(aperm(HIV_trans[tHIV[[paste0('test_', risk_lab)]],,i_med,drop=FALSE], c(2,1,3))))
          num2_to_3 = prop2_to_3 * rowSums(aperm(prevdt[sHIV[[paste0('D2_', risk_lab)]],,i_med, drop = FALSE], c(2,1,3))) + care_cascade[2] * num1_to_2

          HIV_trans[paste0('treat_', risk_lab),,i_med] = num1_to_2
          HIV_trans[paste0('viral_supp_', risk_lab),,i_med] = num2_to_3
        }
      }
    }
    
    
    
    # if(trip == 1){
    #   print('')
    # }
    for(i_diag in 1:3){
      already_in = adrop(prevdt[paste0('D', i_diag, '_pr'),,,drop=FALSE], 1)
      moving_in = get_movement(paste0('D', i_diag, '_pr'), HIV_trans)
      # if(abs(already_in[1,1]) + abs(moving_in[1,1]) > 0){
      #   print(already_in[1,1])
      #   print(moving_in[1,1])
      # }
      HIV_trans[paste0('D', i_diag, '_off_pr'),,] = already_in + moving_in
    }
    
    
    for(i in 1:length(med_labs)){
      HIV_trans[as.numeric(HIV_transitions[,"med"]) == i,,-i] = 0
    }
    
    t0 = Sys.time()
    
    # calculate transitions
    HIV_to = HIV_trans
    rownames(HIV_to) = HIV_transitions[,"to"]
    HIV_to = sapply(med_labs, function(x) rowsum(adrop(HIV_to[,,x,drop=FALSE], 3), HIV_transitions[,"to"]), simplify='array')
    
    HIV_from = HIV_trans
    rownames(HIV_from) = HIV_transitions[,"from"]
    HIV_from = sapply(med_labs, function(x) rowsum(adrop(HIV_from[,,x,drop=FALSE], 3), HIV_transitions[,"from"]), simplify='array')
    
    HIV_delta = SID_mat
    HIV_delta[rownames(HIV_to),,] = HIV_delta[rownames(HIV_to),,,drop=FALSE] + HIV_to
    HIV_delta[rownames(HIV_from),,] = HIV_delta[rownames(HIV_from),,,drop=FALSE] - HIV_from

    # apply transitions
    prevdt = prevdt + HIV_delta

    t_total = t_total + Sys.time() - t0

    if(any(prevdt < 0)){
      if(all(prevdt > -1e-6)){
        prevdt[prevdt > -1e-6 & prevdt < 0] = 0
      } else {
        print(paste0('negative pop for ', HIV_transitions[i,1], ' transition'))
      }
    }
    
    # ensure no low risk medicare ineligible
    prevdt["S_hi",,2] = prevdt["S_hi",,2] + prevdt["S_lo",,2]
    prevdt["S_lo",,2] = 0
    
    ### STI ###
    
    
    ###########
    

    # deaths
    deaths = mu * prevdt
    prevdt = prevdt - deaths
    
    num_no_longer_ineligible = prevdt[,,2] * duration2rate(stay_time)
    num_stay = num_no_longer_ineligible * stay_prop

    prevdt[,,1] = prevdt[,,1] + num_stay
    prevdt[,,2] = prevdt[,,2] - num_no_longer_ineligible
    
    # pop growth
    popgrowth = thispopsize - apply(prevdt, 3, sum)
    popgrowth = thispopsize - rowSums(aperm(prevdt, c(3, 1, 2)))
    
    thisgrowth = makearray(list(c("S_lo", "S_hi"), med_labs))
    thisgrowth[,1] = popgrowth[1] * c((1-prop_high_risk), prop_high_risk)
    thisgrowth[2,2] = popgrowth[2]
    
    # apply pop growth
    if(any(thisgrowth < 0)){
      print('Negative population growth?!')
    } else {
      prevdt[c("S_lo", "S_hi"), 1,] = prevdt[c("S_lo", "S_hi"), 1,] + thisgrowth
    }
    
    if(any(prevdt < 0)){
      print('negative pop')
    }
    
    ###################
    
    
    ###############
    # Save info and go to next iteration
    
    
    SID[t,,,] = prevdt
    HIV_trans_log[t,,,] = HIV_trans
    
    
    ################
    
  }
  
  # print(paste0('time prop = ', round(as.numeric(t_total) / as.numeric(as.numeric(Sys.time()) - as.numeric(t_start)), 3)))
  # print(paste0('total time = ', round(1000 * as.numeric(as.numeric(Sys.time()) - as.numeric(t_start)), 0), ' ms'))
  
  if('only_cal_outs' %in% names(options)){
    if(options$only_cal_outs){
      return(list('SID' = SID,
                  'HIV_trans_log' = HIV_trans_log))
    }
  }
  
  return(list('SID' = SID,
              'HIV_trans_log' = HIV_trans_log,
              'deaths_log' = deaths_log,
              'popgrowth_log' = popgrowth_log))
}
