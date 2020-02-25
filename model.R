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
  
  # if(!getdict(options, 'split', FALSE)){
  if(is.null(y0)){
    
    # S
    y0 = SID_mat
    y0[1,1,] = popsize[as.character(tvec[1]),]
    
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
  
  
  for (t in 1:(length(tvec))){
    
    # TT is the time counter - the actual year
    TT = as.character(tvec[t])
    
    for(key in interpvars){
      assign(key, asub(modelpars[[key]], TT, 1))
    }
    
    thispopsize = popsize[TT,]
    
    # initialise temp
    if(t==1){
      prevdt = y0
      # prevdt[1, 1,] = thispopsize - apply(y0, 3, sum) + prevdt[1, 1, ]
      prevdt[1, 1,] = thispopsize - rowSums(aperm(y0, c(3, 1, 2))) + prevdt[1, 1, ]
    } else {
      prevdt = adrop(SID[t-1,,,, drop = FALSE], 1)
    }
    
    
    # prop_condom_strat = rbind(
    #   (1-condom_by_HIV_de)*(1-gel_mat_de[1,]),
    #   (1-condom_by_HIV_de)*gel_mat_de[1,] + condom_by_HIV_de*gel_mat_de[2,],
    #   condom_by_HIV_de*(1-gel_mat_de[2,])
    # )
    # 
    # dimnames(prop_condom_strat) = list(c("Nil", "Gel", "Condom"),
    #                                               c("HIV- no prep", "HIV- prep", "HIV+"))
    # 
    # if(any(abs(colSums(prop_condom_strat)-1) > 10^-10) | any(prop_condom_strat<0)){
    #   print("Condom proportions are not contained!")
    # }
    # # through_condom_by_type = c(1,(1-eff_gel_de[1]),(1-eff_condom))
    # through_condom_by_type = cbind(1, 1-eff_gel_de, 1-eff_condom)
    # dimnames(through_condom_by_type) = list(c("HIV", "Gonorrhoea"),
    #                                         c("Nil", "Gel", "Condom"))
    # # dot_condom = prop_condom_strat * through_condom_by_type
    # dot_condom = NULL
    # for(i in 1:nrow(through_condom_by_type)){
    #   dot_condom = abind(dot_condom, prop_condom_strat * through_condom_by_type[i,], along=3)
    # }
    # dimnames(dot_condom)[[3]] = c("HIV", "Gonorrhoea")
    # mult_condom = colSums(dot_condom)
    # mult_condom_means = colSums(prevdt[c(1,2,9),"pop_sti",3] * mult_condom) / prevdt[13,"pop_sti",3]
    # cond_HIV = mult_condom_means[1]
    # cond_sti = mult_condom[,2]
    
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
    
    # prevdt[sHIV[['eff_I_lo']],,1] = prevdt[sHIV[['eff_I_lo']],,1] - become_high_risk
    # prevdt[sHIV[['eff_I_hi']],,1] = prevdt[sHIV[['eff_I_hi']],,1] + become_high_risk
    
    
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
    for(i_mix in 1:nrow(mixing)){
      mix_to = medi_states[i_mix]
      mix_to_split = strsplit(mix_to, "_")[[1]]
      mix_pops['inf', mix_to] = sum(prevdt[sHIV[[paste0('I_', mix_to_split[1])]],,mix_to_split[2]])
      mix_pops['pop', mix_to] = sum(prevdt[sHIV[[mix_to_split[1]]],,mix_to_split[2]])
      mix_pops['inf', mix_to] = mix_pops['inf', mix_to] + sum(treatment_eff[1]*prevdt[paste0('D1_', mix_to_split[1]),,mix_to_split[2]] + treatment_eff[2]*prevdt[paste0('D2_', mix_to_split[1]),,mix_to_split[2]] + treatment_eff[3]*prevdt[paste0('D3_', mix_to_split[1]),,mix_to_split[2]])
      mix_pops['pop', mix_to] = mix_pops['pop', mix_to] + sum(prevdt[sHIV[[paste0('D_', mix_to_split[1])]],,mix_to_split[2]])
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
    # HIV_p[tHIV$wait_1] = 1/test_wait[1]
    HIV_p[tHIV$wait_1] = duration2rate(test_wait[1])
    
    for(i_med in 1:length(med_labs)){
      this_testing = t_testing[((6 * i_med) - 5) : ((6 * i_med))] / 2
      # HIV_p[paste0(sHIV$I, '_d_', med_labs[i_med])] = 1/this_testing
      HIV_p[paste0(sHIV$I, '_d_', med_labs[i_med])] = duration2rate(this_testing)
    }
    
    # # multiply by dt
    # HIV_p = HIV_p * dt
    
    # ensure all transitions are as expected
    if(any(is.nan(HIV_p))) print('nan values in transitions?!')
    if(any(HIV_p < -1e-6)){
      print('negative transitions?!')
    }
    if(any(HIV_p[tHIV$inf] > 1) & t > 2){
      print('everyone infected?!')
    }
    HIV_p = vapply(HIV_p, function(x) median(c(0, x, 1)), 1)
    
    # functions to calculate number of people moving for each transition
    # getppl_HIV = function(from, med, prop){
    #   this = adrop(prevdt[from,,,drop=FALSE], 1) * as.numeric(prop)
    #   if(as.numeric(med) != 0){
    #     this[,-as.numeric(med)] = 0
    #   }
    #   return(this)
    # }
    # trans_i_HIV = function(i) getppl_HIV(HIV_transitions[i,'from'], HIV_transitions[i, 'med'], HIV_p[i])
    
    # calculate transitions in absolute numbers
    HIV_trans[] = 0
    for(i in 1:length(HIV_p)){
      med = HIV_transitions[i, 'med']
      this = adrop(prevdt[HIV_transitions[i,'from'],,,drop=FALSE], 1) * as.numeric(HIV_p[i])
      if(as.numeric(med) != 0){
        this[,-as.numeric(med)] = 0
      }
      # HIV_trans[i,,] = trans_i_HIV(i)
      HIV_trans[i,,] = this
    }
    # HIV_trans = lapply(1:length(HIV_p), FUN=trans_i_HIV)
    # HIV_trans = abind(HIV_trans, along=0, new.names=names(HIV_p))
    
    # # calculate risk transitions
    # s_lo = prevdt['S_lo',,1] + get_movement('S_lo', HIV_trans)
    # s_hi = apply(prevdt[c('S_hi', 'S_pr'),,1], c(2), sum) + get_movement(c('S_hi', 'S_pr'), HIV_trans)
    # 
    # if(sum(s_lo) > 0){
    #   HIV_trans['become_high_risk',,] = (sum(s_lo + s_hi) * prop_high_risk - sum(s_hi)) / sum(s_lo) * s_lo
    # } else if(sum(s_hi) > 0){
    #   HIV_trans['become_high_risk',,] = (1 - prop_high_risk) * s_hi
    # }
    # 
    # # 
    # for(i_med in 1:length(med_labs)){
    #   # calculate prep transitions
    #   s_hi = prevdt['S_hi',,i_med] + get_movement('S_hi', HIV_trans, med=i_med)
    #   s_pr = prevdt['S_pr',,i_med] + get_movement('S_pr', HIV_trans, med=i_med)
    #   
    #   thisname = paste0('start_prep_', med_labs[i_med])
    #   
    #   if(sum(s_pr) == num_prep[i_med]){
    #     HIV_trans[thisname,,] = 0
    #   } else if(sum(s_pr) > num_prep[i_med]){
    #     HIV_trans[thisname,,] = (num_prep[i_med] - sum(s_pr)) / sum(s_pr) * s_pr
    #   } else if(sum(s_hi + s_pr) <= num_prep[i_med]){
    #     HIV_trans[thisname,,i_med] = sum(s_hi)
    #   } else {
    #     HIV_trans[thisname,,i_med] = (num_prep[i_med] - sum(s_pr)) / sum(s_hi) * s_hi
    #   }
    # }
    trip = 0
    for(i_med in 1:length(med_labs)){
      for(j_risk in 1:length(HIV_risk_labs)){
        risk_lab = HIV_risk_labs[j_risk]
        
        # care cascade info
        d1 = sum(prevdt[sHIV[[paste0('D1_', risk_lab)]],,i_med]) + sum(apply(HIV_trans[tHIV[[paste0('test_', risk_lab)]],,,drop=FALSE], c(2,3), sum)[,i_med])
        d2 = sum(prevdt[sHIV[[paste0('D2_', risk_lab)]],,i_med])
        d3 = sum(prevdt[sHIV[[paste0('D3_', risk_lab)]],,i_med])
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
          
          # num1_to_2 = prop1_to_2 * (apply(prevdt[sHIV[[paste0('D1_', risk_lab)]],,i_med, drop = FALSE], 2, sum) + apply(HIV_trans[tHIV[[paste0('test_', risk_lab)]],,i_med,drop=FALSE], 2, sum))
          # num2_to_3 = prop2_to_3 * apply(prevdt[sHIV[[paste0('D2_', risk_lab)]],,i_med, drop = FALSE], 2, sum) + care_cascade[2] * num1_to_2

          num1_to_2 = prop1_to_2 * (rowSums(aperm(prevdt[sHIV[[paste0('D1_', risk_lab)]],,i_med, drop = FALSE], c(2,1,3))) + apply(HIV_trans[tHIV[[paste0('test_', risk_lab)]],,i_med,drop=FALSE], 2, sum))
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
    

    # apply transitions
    for(i in 1:length(HIV_p)){
      # get info
      hfrom = HIV_transitions[i,"from"]
      hto = HIV_transitions[i,"to"]
      hpop = HIV_transitions[i,"med"]
      hp = HIV_trans[i,,, drop=FALSE]
      hp = adrop(hp, drop=1)
      
      hpop = list("0" = med_labs, "1" = 1, "2" = 2)[[hpop]]
      
      # make transitions
      prevdt[hfrom,,hpop] = prevdt[hfrom,,hpop] - hp[,hpop]
      prevdt[hto,,hpop] = prevdt[hto,,hpop] + hp[,hpop]
      
      if(any(prevdt < 0)){
        if(all(prevdt > -1e-6)){
          prevdt[prevdt > -1e-6 & prevdt < 0] = 0
        } else {
          print(paste0('negative pop for ', HIV_transitions[i,1], ' transition'))
        }
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
    
    # ineligibile become eligible or leave
    # num_no_longer_ineligible = prevdt[,,2] * dt / stay_time
    num_no_longer_ineligible = prevdt[,,2] * duration2rate(stay_time)
    num_stay = num_no_longer_ineligible * stay_prop
    num_leave = num_no_longer_ineligible * (1 - stay_prop)
    
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
    # deaths_log[t,,,] = deaths
    # popgrowth_log[t,] = popgrowth
    
    
    ################
    
    
    # #################################
    # 
    # # S1 and S2 for HIV
    # temp[1,1:5,1:2] = prevdt[1,1:5,1:2] + dt * (prevdt[1,1:5,1:2] * (- foi_HIV * cond_HIV  -
    #                                                      mu))
    # temp[2,1:5,1:2] = prevdt[2,1:5,1:2] + dt * (prevdt[2,1:5,1:2] * (- foi_HIV * cond_HIV * (1 - eff_prep_de) -
    #                                                      mu))
    # 
    # # # growth
    # # temp[1,1,1:2] = temp[1,1,1:2] + dt * growth * prevdt["pop_HIV","pop_sti",1:2] * (1-prop_prep_de)
    # # temp[2,1,1:2] = temp[2,1,1:2] + dt * growth * prevdt["pop_HIV","pop_sti",1:2] * prop_prep_de
    # 
    # # I and D for HIV
    # temp[3,1:5,1:2] = prevdt[3,1:5,1:2] + dt * (prevdt[1,1:5,1:2] * (foi_HIV * cond_HIV) +
    #                                      prevdt[2,1:5,1:2] * (foi_HIV * cond_HIV * (1 - eff_prep_de)) -
    #                                      prevdt[3,1:5,1:2] * (r_diag_HIV_de + mu + extra_death_I))
    # temp[4,1:5,1:2] = prevdt[4,1:5,1:2] + dt * (prevdt[3,1:5,1:2] * r_diag_HIV_de -
    #                                      prevdt[4,1:5,1:2] * (mu + extra_death_D))
    # 
    # 
    # # HIV status totals
    # temp[8,,1:2] = colSums(temp[1:2,,1:2])
    # temp[9,,1:2] = colSums(temp[3:4,,1:2])
    # 
    # 
    # # HIV care cascade
    # temp[5,,1:2] = temp[4,,1:2] * (1 - cascade_de[2])
    # temp[6,,1:2] = temp[4,,1:2] * cascade_de[2] * (1 - cascade_de[3])
    # temp[7,,1:2] = temp[4,,1:2] * cascade_de[2] * cascade_de[3]
    # 
    # # population totals (pop_HIV)
    # temp[13,,1:2] = colSums(temp[8:9,,1:2])
    # 
    # # calculate HIV totals for the next time period (incidence, diagnoses and deaths)
    # temp[10,1:5,1:2] = dt *  colSums(prevdt[1:2,1:5,1:2] * foi_HIV * cond_HIV) # incidence of HIV
    # temp[11,1:5,1:2] = dt *  prevdt[3,1:5,1:2] * r_diag_HIV_de # diagnoses of HIV
    # temp[12,1:5,1:2] = dt *  (prevdt[8,1:5,1:2] * mu + # S deaths
    #                         prevdt[3,1:5,1:2] * (mu + extra_death_I) + # I deaths
    #                         prevdt[4,1:5,1:2] * (mu + extra_death_D)) # D deaths ## total deaths
    # 
    # # sti status totals
    # temp[,6,1:2] = apply(temp[,c(1,5),1:2], c(1,3), sum)
    # temp[,7,1:2] = apply(temp[,2:4,1:2], c(1,3), sum)
    # 
    # 
    # # calculate and distribute diagnoses and incidence
    # temp[c(1,2,9),8,1:2] = dt * pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat
    # # temp[c(1,2,9),9,1:2] = dt * t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], c(1,3), sum)) * c(1,0))
    # # temp[c(1,2,9),9,1] = dt * r_testing_de * rowSums(t(t(temp[c(1,2,9),2:4,1]) * c(1,lambda,1)))
    # temp[c(1,2,9),9,1:2] = dt * r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3)))+
    #   dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat)) 
    # temp[c(1,2,9),10,1:2] = dt * 1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]
    # 
    # 
    # temp[8,8:10,1:2] = colSums(temp[1:2,8:10,1:2])
    # 
    # temp[3:4,8,1:2] = temp[3:4,1,1:2] * (temp[9,8,1:2]/temp[9,1,1:2])
    # 
    # diag_temp = sweep(temp[3:4,2:4,1:2], MARGIN=c(2,3), temp[9,2:4,1:2], `/`)
    # diag_temp[is.na(diag_temp)] = 0
    # temp[3:4,9,1:2] = apply(diag_temp, c(1,3), sum) * temp[9,9,1:2]
    # # temp[3:4,9,1] = rowSums(t(t(temp[3:4,2:4,1]) / temp[9,2:4,1])) * temp[9,9,1]
    # 
    # temp[3:4,10,1:2] = temp[3:4,5,1:2] * (temp[9,10,1:2]/temp[9,5,1:2])
    # 
    # 
    # temp[5,8:10,1:2] = temp[4,8:10,1:2] * (1 - cascade_de[2])
    # temp[6,8:10,1:2] = temp[4,8:10,1:2] * cascade_de[2] * (1 - cascade_de[3])
    # temp[7,8:10,1:2] = temp[4,8:10,1:2] * cascade_de[2] * cascade_de[3]
    # 
    # # population totals
    # temp[,"pop_sti",1:2] = apply(temp[,1:5,1:2], c(1,3), sum)
    # temp[13,,1:2] = colSums(temp[1:4,,1:2])
    # 
    # # incidence and diagnoses overlaps for both diseases (e.g. how many people were infected with both sti AND HIV in a given time period)
    # temp[10:12,8:10,1] = outer(temp[10:12,"pop_sti",1], temp[13,8:10,1]) / temp[13,"pop_sti",1]
    # temp[10:12,8:10,2] = outer(temp[10:12,"pop_sti",2], temp[13,8:10,2]) / temp[13,"pop_sti",2]
    # 
    # 
    # 
    # #############################################
    
    
    
    
    #   # # calculate sti compartments
    #   # # S
    #   # SID[t+1,c(1,2,9),1,1:2] = temp[c(1,2,9),1,1:2] + dt * (- pr_infect_sti * temp[c(1,2,9),1,1:2] +
    #   #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
    #   # # E
    #   # SID[t+1,c(1,2,9),2,1:2] = temp[c(1,2,9),2,1:2] + dt * (pr_infect_sti * temp[c(1,2,9),1,1:2] -
    #   #                                                          1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
    #   #                                                          t(t(r_testing_de * temp[c(1,2,9),2,1:2]) * c(1,0)))
    #   # 
    #   # # I
    #   # SID[t+1,c(1,2,9),3,1:2] = temp[c(1,2,9),3,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
    #   #                                                          1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
    #   #                                                          t(t(r_testing_de * temp[c(1,2,9),3,1:2]) * c(1,0)))
    #   # 
    #   # # L
    #   # SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
    #   #                                                          # t(t(prop_testing_de * r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
    #   #                                                          t(t(r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
    #   # 
    #   # # T
    #   # # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (t(t(prop_testing_de * r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0)) -
    #   # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0)) -
    #   #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
    #   
    #   # calculate sti compartments
    #   # S
    #   SID[t+1,c(1,2,9),1,1:2] = temp[c(1,2,9),1,1:2] + dt * (- pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat +
    #                                                            1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
    # 
    #   SID[t+1,1,1,1:2] = SID[t+1,1,1,1:2] + (pop_growth - sum(temp["pop_HIV","pop_sti",1:2]))*c((1-rho),rho)
    #   
    #   # E
    #   SID[t+1,c(1,2,9),2,1:2] = temp[c(1,2,9),2,1:2] + dt * (pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat -
    #                                                            1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
    #                                                            r_testing_de * temp[c(1,2,9),2,1:2])
    #   
    #   # # Sy
    #   # SID[t+1,c(1,2,9),3,1:2] = temp[c(1,2,9),3,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * symp -
    #   #                                                          # 1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
    #   #                                                          r_testing_de * temp[c(1,2,9),3,1:2] * lambda)
    #   
    #   # ASy
    #   SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat) -
    #                                                            # 1/t_infect_sti_de * temp[c(1,2,9),4,1:2] -
    #                                                            r_testing_de * temp[c(1,2,9),4,1:2])
    #   
    #   
    #   # SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
    #   #                                                          # t(t(prop_testing_de * r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
    #   #                                                          t(t(r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
    #   
    #   # # T
    #   # SID[t+1,c(1,2,9),5,1] = temp[c(1,2,9),5,1] + dt * (rowSums(t(t(r_testing_de * temp[c(1,2,9),2:4,1])  *  c(1,lambda,1))) -
    #   #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1])
    #   # 
    #   # SID[t+1,c(1,2,9),5,2] = temp[c(1,2,9),5,2] + dt * (-1/t_treatment_sti_de * temp[c(1,2,9),5,2])
    #   
    #   # T
    #   # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3))) -
    #   #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]) +
    #   SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),c(2,4),1:2], c(2), c(1,1), FUN="*"), c(2,1,3))) -
    #                                                            1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]) +
    #     + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * prop_treat)
    #   
    #   
    #   # sti status totals
    #   SID[t+1,1:9,6,1:2] = apply(SID[t+1,1:9,c(1,5),1:2], c(1,3), sum)
    #   SID[t+1,1:9,7,1:2] = apply(SID[t+1,1:9,2:4,1:2], c(1,3), sum)
    #   
    #   # apply(temp[c(1,2,9),2:4,1:2], c(1,3), sum)
    #   # apply(SID[t+1,1:9,2:4,1:2], c(1,3), sum)
    #   
    #   
    #   # redistribution within I and D of HIV
    #   # SID[t+1,3:4,1:7] = t(apply(temp[3:4,1:7,1:2], c(1,3), function(x) x * (SID[t+1,9,1:7,1:2] / temp[9,1:7,1:2]))) # potentially problematic but appears to work
    #  
    #   
    #   SID[t+1,3:4,1:7,1:2] = (temp[3:4,1:7,1:2]) * aperm(replicate(2, (SID[t+1,9,1:7,1:2] / temp[9,1:7,1:2])), c(3,1,2))
    #   SID[t+1,3:4,1:7,1:2][!is.finite(SID[t+1,3:4,1:7,1:2])] = 0
    #   
    #   # care cascade
    #   SID[t+1,5,1:7,1:2] = SID[t+1,4,1:7,1:2] * (1 - cascade_de[2])
    #   SID[t+1,6,1:7,1:2] = SID[t+1,4,1:7,1:2] * cascade_de[2] * (1 - cascade_de[3])
    #   SID[t+1,7,1:7,1:2] = SID[t+1,4,1:7,1:2] * cascade_de[2] * cascade_de[3]
    #   
    #   # calculate HIV- totals (sum of S1 and S2)
    #   SID[t+1,8,,1:2] = apply(SID[t+1,1:2,,1:2], c(2,3), sum)
    #   
    #   # population totals (pop_sti)
    #   SID[t+1,1:9,"pop_sti",1:2] = apply(SID[t+1,1:9,6:7,1:2], c(1,3), sum)
    #   
    #   
    #   # incidence and diagnoses of sti
    #   SID[t+1,c(1,2,9),8,1:2] = dt * pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat
    #   # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(prop_testing_de * r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0))
    #   # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0))
    #   # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(r_testing_de * temp[c(1,2,9),7,1:2]) * c(1,0))
    #   
    #   # SID[t+1,c(1,2,9),9,1] = dt * r_testing_de * rowSums(t(t(temp[c(1,2,9),2:4,1]) * c(1,lambda,1)))
    #   SID[t+1,c(1,2,9),9,1:2] = dt * r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3)))+
    #     dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat)) 
    #       
    #   SID[t+1,c(1,2,9),10,1:2] = dt * 1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]
    #   
    #   
    #   # recalculate HIV- totals (sum of S1 and S2), now with incidence and diagnoses
    #   SID[t+1,8,,1:2] = apply(SID[t+1,1:2,,1:2], c(2,3), sum)
    #   
    #   # redistribute HIV susceptible into PrEP proportions
    #   # SID[t+1,2,,1:2] = SID[t+1,8,,1:2] * prop_prep_de
    #   # SID[t+1,1,,1:2] = SID[t+1,8,,1:2] - SID[t+1,2,,1:2]
    #   
    #   # delta_prep_prop = (sum(SID[t+1,8,11,1:2])*prop_prep_de - sum(SID[t+1,2,11,1:2])) / sum(SID[t+1,1,11,1:2])
    #   delta_prep_prop = (sum(SID[t+1,8,11,1:2])*prop_prep_de - sum(SID[t+1,2,11,1:2])) / sum(SID[t+1,1,11,1:2])
    #   SID[t+1,2,c(1:5, 11),1:2] = SID[t+1,2,c(1:5, 11),1:2] + delta_prep_prop * SID[t+1,1,c(1:5, 11),1:2]
    #   SID[t+1,1,c(1:5, 11),1:2] = SID[t+1,1,c(1:5, 11),1:2] - delta_prep_prop * SID[t+1,1,c(1:5, 11),1:2]
    #   
    #   # if(iss2){
    #   #   delta_prep_prop_all[t] <<- delta_prep_prop
    #   #   
    #   # }
    #   # 
    #   SID[t+1,8,8:10,1:2] = apply(SID[t+1,1:2,8:10,1:2], c(2,3), sum)
    #   
    #   
    #   SID[t+1,3:4,8,1:2] = t(t(SID[t+1,3:4,1,1:2]) * (SID[t+1,9,8,1:2]/SID[t+1,9,1,1:2]))
    #   # SID[t+1,3:4,9,1:2] = t(t(SID[t+1,3:4,7,1:2]) * (SID[t+1,9,9,1:2]/SID[t+1,9,7,1:2]))
    #   # SID[t+1,3:4,9,1:2] = apply(SID[t+1,3:4,2:4,1:2] / aperm(replicate(2, SID[t+1,9,2:4,1:2]), c(3,1,2)) * aperm(replicate(2, replicate(3, SID[t+1,9,9,1:2])), c(3,2,1)), c(1,3), sum)
    #   # SID[t+1,3:4,9,1] = colSums(t(SID[t+1,3:4,2:4,1])*c(1,lambda,1) / sum(SID[t+1,9,2:4,1] * c(1,1,lambda)) * SID[t+1,9,9,1])
    #   SID[t+1,3:4,9,1:2] = t(t(colSums(aperm(sweep(SID[t+1,3:4,2:4,1:2], 2, c(1,lambda,1), FUN="*"), c(2,1,3)))) /
    #     colSums(SID[t+1,9,2:4,1:2] * c(1,lambda,1)) * SID[t+1,9,9,1:2])
    #   SID[t+1,3:4,10,1:2] = t(t(SID[t+1,3:4,5,1:2]) * (SID[t+1,9,10,1:2]/SID[t+1,9,5,1:2]))
    #   
    #   SID[t+1,5,8:10,1:2] = SID[t+1,4,8:10,1:2] * (1 - cascade_de[2])
    #   SID[t+1,6,8:10,1:2] = SID[t+1,4,8:10,1:2] * cascade_de[2] * (1 - cascade_de[3])
    #   SID[t+1,7,8:10,1:2] = SID[t+1,4,8:10,1:2] * cascade_de[2] * cascade_de[3]
    #   
    #   
    #   SID[t+1,10,,1:2] = apply(SID[t+1,1:2,,1:2] * foi_HIV * c(1,(1-eff_prep_de)), c(2,3), sum) * temp[10,,1:2] /
    #     apply(temp[1:2,,1:2] * foi_HIV * c(1,(1-eff_prep_de)), c(2,3), sum)
    #   SID[t+1,11,,1:2] = SID[t+1,3,,1:2] * temp[11,,1:2] / temp[3,,1:2]
    #   SID[t+1,12,,1:2] = apply(SID[t+1,c(8,3,4),,1:2] * c(mu, mu+extra_death_I, mu+extra_death_D), c(2,3), sum) * temp[12,,1:2] /
    #     apply(temp[c(8,3,4),,1:2] * c(mu, mu+extra_death_I, mu+extra_death_D), c(2,3), sum)
    #   
    #   SID[t+1,10:12,,1:2][!is.finite(SID[t+1,10:12,,1:2])] = 0
    #   
    #   
    #   SID[t+1,13,,1:2] = apply(SID[t+1,8:9,,1:2], c(2,3), sum)
    #   
    #   SID[t+1,,,3] = SID[t+1,,,1] + SID[t+1,,,2]
    #   
    #   # SID[t+1,,,3] = SID[t+1,,,3] * (1 - 1/50 * dt)
    #   
    #   # temp_out[t,,,3] <- temp[,,3]
    #   
    #   # print(SID[t+1,,] - prevdt[,])
    #   
    # 
    # }
    # 
    
  }
  
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
