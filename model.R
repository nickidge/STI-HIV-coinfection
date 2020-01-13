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
    y0[1,1,1] = popsize[as.character(tvec[1])]
    
    # HIV
    ninf_HIV = y0[sHIV$S,,] * init_prev_HIV
    y0[sHIV$S,,] = y0[sHIV$S,,] - ninf_HIV
    y0[sHIV$I_new,,] = y0[sHIV$I_new,,] + ninf_HIV
    
    ndiag_HIV = y0["I_lo_new",,] * init_diag_prop
    y0["I_lo_new",,] = y0["I_lo_new",,] - ndiag_HIV
    y0["D1",,] = y0["D1",,] + ndiag_HIV
    
    # STI
    ninf_STI = y0[,'S',] * init_prev_STI
    
  }
  
  # initialise model array
  SID = replicate(length(tvec), SID_mat)
  SID = aperm(SID, c(4,1,2,3))
  dimnames(SID)[[1]] = tvec
  
  HIV_trans_log = makearray(list(tvec, HIV_transitions[,"trans"], STI_labs, RISK_labs))
  deaths_log = makearray(list(tvec, HIV_labs, STI_labs, RISK_labs))
  popgrowth_log = setNames(numeric(length(tvec)), tvec)


    for (t in 1:(length(tvec))){
      
      # TT is the time counter - the actual year
      TT = as.character(tvec[t])
      
      # for(key in optvarkeys){
      #   if(grepl('_interp', key)){
      #     # assign(paste0(key, '_this'), asub(get(key), TT, 1))
      #     assign(gsub('.{7}$', '', key), asub(get(key), TT, 1))
      #   }
      # }
      
      for(key in interpvars){
        assign(key, asub(modelpars[[key]], TT, 1))
      }
      
      thispopsize = popsize[TT]
      
      # initialise temp
      if(t==1){
        prevdt = y0
        prevdt[1, 1, 1] = thispopsize - sum(y0)
      } else {
        prevdt = SID[t-1,,,]
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
      
      condom_thru = 1 - condom_usage
      
      #########################
      
      
      # calculate force of infections
      totalppl = sum(prevdt)
      # rel_inc_HIV = sum(prevdt[sHIV$I,,]) + sum(treatment_eff[1]*prevdt["D1",,]) + sum(treatment_eff[2]*prevdt["D2",,]) + sum(treatment_eff[3]*prevdt["D3",,])
      inf_HIV = apply(prevdt[sHIV$I,,], c(2,3), sum) + treatment_eff[1]*prevdt["D1",,] + treatment_eff[2]*prevdt["D2",,] + treatment_eff[3]*prevdt["D3",,]
      rel_inf_HIV = apply(inf_HIV, 2, sum)
      rel_inf_HIV = c(rel_inf_HIV[1] + medimix * rel_inf_HIV[2], rel_inf_HIV[2] + medimix * rel_inf_HIV[1])
      rel_inf_STI = sum(prevdt[,sSTI$I,])
      
      pop_by_med = apply(prevdt, 3, sum)
      rel_pop_HIV = c(pop_by_med[1] + medimix * pop_by_med[2], pop_by_med[2] + medimix * pop_by_med[1])
      
      foi_HIV = outer(risk_mat * condom_thru * f_infect_HIV, rel_inf_HIV / rel_pop_HIV)
      foi_HIV = fixnan(foi_HIV)
      
      foi_STI = f_infect_STI * rel_inf_STI / totalppl
      
      ###### TRANSITIONS ######
      
      # HIV #
      HIV_p = setNames(numeric(nrow(HIV_transitions)), HIV_transitions[,'trans'])
      
      # infections
      HIV_p[tHIV$inf] = as.vector(apply(prevdt[sHIV$S,,], MARGIN=c(1,3), FUN=sum) * foi_HIV)
      
      # waiting undiagnosed
      HIV_p[tHIV$wait_1] = 1/test_wait[1]
      HIV_p[tHIV$wait_2] = 1/test_wait[2]

      # diagnoses
      HIV_p["I_lo_new_d"] = 1/t_testing[1]
      HIV_p["I_lo_mid_d"] = 1/t_testing[2]
      HIV_p["I_lo_old_d"] = 1/t_testing[3]
      HIV_p["I_hi_new_d"] = 1/t_testing[4]
      HIV_p["I_hi_mid_d"] = 1/t_testing[5]
      HIV_p["I_hi_old_d"] = 1/t_testing[6]
      HIV_p["I_pr_new_d"] = 1/t_testing[7]
      HIV_p["I_pr_mid_d"] = 1/t_testing[8]
      HIV_p["I_pr_old_d"] = 1/t_testing[9]
      
      # multiply by dt
      HIV_p = HIV_p * dt
      
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
      getppl = function(from, med, prop){
        this = prevdt[from,,] * as.numeric(prop)
        if(as.numeric(med) != 0){
          this[,-as.numeric(med)] = 0
        }
        return(this)
      }
      trans_i = function(i) getppl(HIV_transitions[i,'from'], HIV_transitions[i, 'med'], HIV_p[i])
      
      # calculate transitions in absolute numbers
      HIV_trans = lapply(1:length(HIV_p), FUN=trans_i)
      HIV_trans = abind(HIV_trans, along=0, new.names=names(HIV_p))
      
      # if(any(!is.finite(HIV_trans)) | any(!is.finite(prevdt))){
      #   print('')
      # }
      # 
      # if(any(!is.finite(get_movement(c('S_hi', 'S_pr', "S_lo"), HIV_trans)))){
      #   print('')
      # }
      
      # calculate risk transitions
      s_lo = prevdt['S_lo',,] + get_movement('S_lo', HIV_trans)
      s_hi = apply(prevdt[c('S_hi', 'S_pr'),,], c(2,3), sum) + get_movement(c('S_hi', 'S_pr'), HIV_trans)
      
      if(sum(s_lo) > 0){
        HIV_trans['become_high_risk',,] = (sum(s_lo + s_hi) * prop_high_risk - sum(s_hi)) / sum(s_lo) * s_lo
      } else if(sum(s_hi) > 0){
        HIV_trans['become_high_risk',,] = (1 - prop_high_risk) * s_hi
      }

      # calculate prep transitions
      s_hi = prevdt['S_hi',,] + get_movement('S_hi', HIV_trans)
      s_pr = prevdt['S_pr',,] + get_movement('S_pr', HIV_trans)
      
      if(sum(s_pr) > num_prep){
        HIV_trans['start_prep',,] = (num_prep - sum(s_pr)) / sum(s_pr) * s_pr
      } else if(sum(s_hi + s_pr) <= num_prep){
        HIV_trans['start_prep',,] = s_hi
      } else {
        HIV_trans['start_prep',,] = (num_prep - sum(s_pr)) / sum(s_hi) * s_hi
      }

      # care cascade info
      d1 = sum(prevdt["D1",,] + apply(HIV_trans[tHIV$test,,], c(2,3), sum))
      d2 = sum(prevdt["D2",,])
      d3 = sum(prevdt["D3",,])
      d1plus = d1 + d2 + d3
      d2plus = d2 + d3
      
      # calculate care cascade transitions
      
      prop1_to_2 = (care_cascade[2] * (d1plus) - d2plus) / d1 
      if(d2 == 0){
        prop2_to_3 = 0
      } else {
        prop2_to_3 = (care_cascade[3] * (d2plus) - d3) / d2 
      }
      if(prop1_to_2 < -1e-6 | prop1_to_2 > 1) {print('Prop 1 to 2 not between 0 and 1!!')}
      if(prop2_to_3 < -1e-6 | prop2_to_3 > 1) {print('Prop 2 to 3 not between 0 and 1!!')}
      
      HIV_p['treat'] = prop1_to_2
      HIV_p['viral_supp'] = prop2_to_3
      
      num1_to_2 = prop1_to_2 * (prevdt["D1",,] + apply(HIV_trans[tHIV$test,,], c(2,3), sum))
      num2_to_3 = prop2_to_3 * prevdt["D2",,] + care_cascade[3] * num1_to_2
      
      i1 = grep("^treat$", names(HIV_p))      
      i2 = grep("^viral_supp$", names(HIV_p))
      
      HIV_trans[i1,,] = num1_to_2
      HIV_trans[i2,,] = num2_to_3
      
      
      # apply transitions
      for(i in 1:length(HIV_p)){
        # get info
        hfrom = HIV_transitions[i,"from"]
        hto = HIV_transitions[i,"to"]
        hp = HIV_trans[i,,]
        
        # make transitions
        prevdt[hfrom,,] = prevdt[hfrom,,] - hp
        prevdt[hto,,] = prevdt[hto,,] + hp
      }
      
      
      ### STI ###
      
      
      
      ###########
      
      
      
      # deaths
      deaths = mu * prevdt
      prevdt = prevdt - deaths
      
      # pop growth
      popgrowth = thispopsize - sum(prevdt)
      if(popgrowth < 0){
        print('Negative population growth?!')
      } else {
        prevdt[c("S_lo", "S_hi"), 1, 1] = prevdt[c("S_lo", "S_hi"), 1, 1] + popgrowth * c((1-prop_high_risk), prop_high_risk)
      }
      
      ###################
      
      
      # 
      # 
      # # care cascade info
      # d1 = sum(prevdt["D1",,])
      # d2 = sum(prevdt["D2",,])
      # d3 = sum(prevdt["D3",,])
      # d1plus = d1 + d2 + d3
      # d2plus = d2 + d3
      # 
      # # calculate care cascade transitions
      # if(d3 == 0){
      #   if(d2plus == 0){
      #     prop1_to_2 = care_cascade[1]
      #   } else {
      #     prop1_to_2 = (care_cascade[1] * (d1plus) - d2plus) / d1 
      #   }
      #   prop2_to_3 = care_cascade[2]
      # } else {
      #   prop1_to_2 = (care_cascade[1] * (d1plus) - d2plus) / d1 
      #   prop2_to_3 = (care_cascade[2] * (d2plus) - d3) / d2 
      # }
      # if(prop1_to_2 < -1e-6 | prop1_to_2 > 1) {print('Prop 1 to 2 not between 0 and 1!!')}
      # if(prop2_to_3 < -1e-6 | prop2_to_3 > 1) {print('Prop 2 to 3 not between 0 and 1!!')}
      # num1_to_2 = prop1_to_2 * prevdt["D1",,]
      # num2_to_3 = prop2_to_3 * prevdt["D2",,] + care_cascade[2] * num1_to_2
      # 
      # # apply care cascade transitions
      # prevdt["D1",,] = prevdt["D1",,] - num1_to_2
      # prevdt["D2",,] = prevdt["D2",,] + num1_to_2
      # prevdt["D2",,] = prevdt["D2",,] - num2_to_3
      # prevdt["D3",,] = prevdt["D3",,] + num2_to_3
      
      ###############
      # Save info and go to next iteration
      
      
      SID[t,,,] = prevdt
      HIV_trans_log[t,,,] = HIV_trans
      deaths_log[t,,,] = deaths
      popgrowth_log[t] = popgrowth
      
      
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
      