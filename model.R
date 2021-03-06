# ### Recursive Model ###
# 
# # model function
# # takes initial conditions and tvec as input, and outputs the model as a matrix
run_model = function(y_init, tvec_in){
  # set proportion on prep to 0%, to be changed if necessary
  # prop_prep_interp[] = 0
  # if(iss2){
  #   delta_prep_prop_all <<- numeric(length(tvec_in))
  #   names(delta_prep_prop_all) <<- tvec_in
  # }
  
  
  # apply intervention scenarios
  if (intervention==1){
    # print("--------------------------------")
    if(int=="a" | int=="c"){
      prop_testing_interp[,1:2] <- 0.75
    }
    
    if(int=="b" | int=="c"){
      t_testing_interp[,1:2] = 180
    }
    
    if(int=="d"){
      t_testing_interp[,3] = 180
    }
    
    if(int=="e" | int=="f" | int=="g" | int=="h"){
      mix2=0.1
      prop_prep_interp[] = prop_prep_base
    }
    
    if(int=="f" | int=="h"){
      condom_by_HIV_interp[,2] = 0.5 * condom_by_HIV_interp[,1]
    }
    
    if(int=="g" | int=="h"){
      t_testing_interp[,2] = 90
    }
    
    # if(int=="h"){
    #   prop_prep_interp[] = prop_prep_base
    # }
    
    if(int=="alt"){
      t_testing_interp = 0.5 * t_testing_interp
    }
    
  }
  
  # if(intervention==1 & int=="h"){
  #   prop_prep_interp[] = 0
  #   t_testing_interp[,c(1,3)] = 40
  #   mix2=0.5
  # }
  
 # apply sensitivity scenarios
  if (sensitivity_supp==1){
    gel_mat[1:2,1:3] = 0.5
    
    if(unc=="1a"){
      
      eff_condom = 0.7
    }

    if(unc=="1b"){
      eff_condom = 0.9
    }

    if(unc=="2a"){
      prop_prep=base_raw[,c(1,2)]
      prop_prep=prop_prep[c(1:8,9),]
      prop_prep[8,2]=0.5
      prop_prep_interp = approx(unlist(prop_prep[,1]), unlist(prop_prep[,2]), xout=tvec0, rule=2)$y
      names(prop_prep_interp) = tvec0
      
    }

    if(unc=="2b"){
      prop_prep=base_raw[,c(1,2)]
      prop_prep=prop_prep[c(1:8,9),]
      prop_prep[8,2]=0.80
      prop_prep_interp = approx(unlist(prop_prep[,1]), unlist(prop_prep[,2]), xout=tvec0, rule=2)$y
      names(prop_prep_interp) = tvec0
      
      
    }

    if(unc=="3a"){
      prop_treat=0.8
      
    }

    if(unc=="3b"){
      prop_treat=0.4
    }

    if(unc=="4a"){
      risk_mat[2,2]=2
    }
  
    if(unc=="4b"){
      risk_mat[2,2]=10
    }
  
    if(unc=="4c"){
      risk_mat[2,2]=20
    }
  }


  
  if(sensitivity==1){
    prop_prep_interp = approx(c(unlist(prop_prep[,1]), 2022), c(unlist(prop_prep[,2]), 0.3), xout=tvec0, rule=2)$y
    names(prop_prep_interp) = tvec0
    if(sens_scen=="a"){}
    if(sens_scen=="b"){
      gel_mat[1:2,1:3] = 0.5
    }
    if(sens_scen=="c"){
      gel_mat[2,] = 1
    }
    if(sens_scen=="d"){
      gel_mat[,2] = 1
    }
    if(sens_scen=="e"){
      gel_mat[,2:3]=0.5
    }
    if(sens_scen=="f"){
      gel_mat[,1]=0.5
    }
    if(sens_scen=="g"){
      gel_mat[1,2] = 1
    }
    if(sens_scen=="h"){
      gel_mat[2,2] = 1
    }
    if(sens_scen=="i"){
      gel_mat = gel_mat_best_estimate
    }
    if(sens_scen=="j"){
      gel_mat[2,2:3]=0.5
    }
    if(sens_scen=="k"){
      gel_mat[1,1]=0.5
    }
    if(sens_scen=="l"){
      gel_mat[1,1]=0.5
      gel_mat[2,2:3]=0.5
    }
    if(sens_scen=="m"){
      gel_mat[2,]=1
    }
    if(sens_scen=="n"){
      gel_mat[1,]=1
    }
    if(sens_scen=="o"){
      gel_mat[2,]=0.5
    }
    if(sens_scen=="p"){
      gel_mat[1,]=0.5
    }
    if(sens_scen=="q"){
      gel_mat[1:2,]=0.5
    }
  }
  # 
  # gel condom scenarios
  if (is_gel){
    gel_up_interp = strat_interp(rbind(c(split_year, 0, 0, 0),
                                       c(split_year+2, (gel_mat[1,]))))
    gel_down_interp = strat_interp(rbind(c(split_year, 0, 0, 0),
                                       c(split_year+2, (gel_mat[2,]))))
    gel_array = aperm(abind(gel_up_interp, gel_down_interp, along=3), c(1,3,2))
    # eff_gel = eff_gel_heat
  } else {
      # gel_up_interp[] = 0
      # gel_down_interp[] = 0
      gel_array = array(0, dim = c(length(tvec_in), 2, 3))
      dimnames(gel_array)[[1]] = tvec_in
  }
  
  # debugging
  if (debug_check==1){
    if (debug_let=="a"){
      prop_prep_interp[] = 0
    }
    
    if (debug_let=="b"){
      prop_prep_interp[] = prop_prep_base
      mix2=mix1
      condom_by_HIV_interp[,2] = condom_by_HIV_interp[,1]
      prop_testing_interp[,2] = prop_testing_interp[,1]
      t_testing_interp[,2] = t_testing_interp[,1]
    }
    
    if (debug_let=="c"){
      prop_prep_interp[] = prop_prep_base
      mix2=0.1
      condom_by_HIV_interp[,2] = condom_by_HIV_interp[,1]
      prop_testing_interp[,2] = prop_testing_interp[,1]
      t_testing_interp[,2] = t_testing_interp[,1]
    }
    
    if (debug_let=="d"){
      prop_prep_interp[] = prop_prep_base
      mix2=mix1
      condom_by_HIV_interp[,2] = 0.5 * condom_by_HIV_interp[,1]
      prop_testing_interp[,2] = prop_testing_interp[,1]
      t_testing_interp[,2] = t_testing_interp[,1]
    }
    
    if (debug_let=="e"){
      prop_prep_interp[] = prop_prep_base
      mix2=mix1
      condom_by_HIV_interp[,2] = condom_by_HIV_interp[,1]
      prop_testing_interp[,2] = prop_testing_interp[,1]
      t_testing_interp[,2] = 90
    }
    
    if (debug_let=="f"){
      prop_prep_interp[] = prop_prep_base
      mix2=0.1
      condom_by_HIV_interp[,2] = 0.5 * condom_by_HIV_interp[,1]
      prop_testing_interp[,2] = prop_testing_interp[,1]
      t_testing_interp[,2] = 90
    }
  }
  
  
  pr_infect_log <<- matrix(0, ncol=3, nrow=length(tvec_in)-2)
  
  
  # this loop runs the model multiple times (if chosen) to make the initial sti compartments more accurate
  # for (adj in c("run")){
    
    # if(adj!="run"){
    #   tvec_de = tvec_in[which(tvec_in<=2018)]
    # } else {
      tvec_de = tvec_in
    # }
    
    # initialise model array
    SID = array(0, dim = c(length(tvec_de)-1, 13, 11, 3),
                dimnames = list(head(tvec_de, -1),
                                c("S1", "S2", "I", "D", "D1", "D2", "D3", "HIV_minus", "HIV_plus", "incidence_HIV", "diagnoses_HIV", "deaths_HIV", "pop_HIV"),
                                c("S_sti", "E_sti", "Sy_sti", "ASy_sti", "T_sti", "sti_minus", "sti_plus", "incidence_sti", "diagnoses_sti", "recovered_sti", "pop_sti"),
                                c("low_risk", "high_risk", "all")))
    
    
    
    if(intervention==0 & sensitivity==0 & debug_check==0 & !is_gel){
      SID[1,,,3] = y_init
      # set initial compartments (at t=1)
      SID[1,1,,3] = SID[1,8,,3] * (1 - prop_prep_interp_base[paste0(tvec_de[1])])
      SID[1,2,,3] = SID[1,8,,3] * prop_prep_interp_base[paste0(tvec_de[1])]
      
      y_adj=SID[1,,,3]
      
      # make the adjusted compartments the initial conditions
      # if(adj!=1){
        sti_ratio = SID[1,1:9,"pop_sti",3]/y_adj[1:9,"pop_sti"]
        SID[1,1:9,1:5,3] = sweep(y_adj[1:9,1:5], MARGIN=1, sti_ratio, `*`)
        SID[1,1:9,1:5,3][is.na(SID[1,1:9,1:5,3])] = 0
      # }
      
      # SID
      SID[1,9,,3] = colSums(SID[1,3:4,,3])  # total HIV+
      SID[1,13,,3] = colSums(SID[1,8:9,,3]) # total population by sti
      SID[1,,6,3] = rowSums(SID[1,,c(1,5),3]) # total sti-
      SID[1,,7,3] = rowSums(SID[1,,2:4,3]) # total sti+
      SID[1,,"pop_sti",3] = rowSums(SID[1,,1:5,3]) # total population by HIV
      
      # prop_testing0[1,2] = 0.9
      
      # testing population
      # SID[,,,1] = SID[,,,3] * as.numeric(prop_testing0[1,2])
      # SID[1,2,,1] = SID[1,2,,3] * as.numeric(prop_testing0[1,3])
      # SID[1,3:7,,1] = SID[1,3:7,,3] * as.numeric(prop_testing0[1,4])
      
      # SID[,,,1] = SID[,,,3] * prop_testing_interp[paste0(tvec_in[1]),1]
      # SID[1,2,,1] = SID[1,2,,3] * prop_testing_interp[paste0(tvec_in[1]),2]
      # SID[1,3:7,,1] = SID[1,3:7,,3] * prop_testing_interp[paste0(tvec_in[1]),3]
      
      SID[,,,1] = SID[,,,3] * (1 - rho)
      SID[1,2,,1] = SID[1,2,,3] * (1 - rho)
      SID[1,3:7,,1] = SID[1,3:7,,3] * (1 - rho)
      
      
      # prop_testing_interp[paste0(tvec[1]),]
      
      
      SID[1,8,,1] = colSums(SID[1,1:2,,1])
      SID[1,9,,1] = colSums(SID[1,3:4,,1])
      SID[1,13,,1] = colSums(SID[1,8:9,,1]) # total population by sti
      
      # SID[1,,,1] / SID[1,,,3]
      
      # non-testing population
      SID[,,,2] = SID[,,,3] - SID[,,,1]
      
    } else {
      SID[1,,,] = SID_split
      
      # if(prop_prep_interp[paste0(split_year)]==0){
      if(prop_prep_interp_base[paste0(split_year)]==0){
        SID[1,1,,] = SID[1,8,,] * (1 - prop_prep_interp[1])
        SID[1,2,,] = SID[1,8,,] * prop_prep_interp[1]
      } else {
        SID[1,1,,] = SID[1,1,,] * (1 - prop_prep_interp[paste0(split_year)]) / (1 - prop_prep_interp_base[paste0(split_year)])
        SID[1,2,,] = SID[1,2,,] * prop_prep_interp[paste0(split_year)] / prop_prep_interp_base[paste0(split_year)]
      }
      
      
      # SID[1,,,1] = SID[1,,,1] * as.numeric(prop_testing_interp[paste0(tvec_in[1]),1] / prop_testing0[1,2])
      # SID[1,2,,1] = SID[1,2,,1] * as.numeric(prop_testing_interp[paste0(tvec_in[1]),2] / prop_testing0[1,2])
      # SID[1,3:7,,1] = SID[1,3:7,,1] * as.numeric(prop_testing_interp[paste0(tvec_in[1]),3] / prop_testing0[1,4])
      # 
      # SID[1,,,2] = SID[1,,,2] * as.numeric((1 - prop_testing_interp[paste0(tvec_in[1]),1]) / (1 - prop_testing0[1,2]))
      # SID[1,2,,2] = SID[1,2,,2] * as.numeric((1 - prop_testing_interp[paste0(tvec_in[1]),2]) / (1 - prop_testing0[1,2]))
      # SID[1,3:7,,2] = SID[1,3:7,,2] * as.numeric((1 - prop_testing_interp[paste0(tvec_in[1]),3]) / (1 - prop_testing0[1,4]))
      
      # SID[1,,,1] = SID[1,,,1] * (1 - rho)
      # SID[1,2,,1] = SID[1,2,,1] * (1 - rho)
      # SID[1,3:7,,1] = SID[1,3:7,,1] * (1 - rho)
      # 
      # SID[1,,,2] = SID[1,,,2] * rho
      # SID[1,2,,2] = SID[1,2,,2] * rho
      # SID[1,3:7,,2] = SID[1,3:7,,2] * rho
      
      SID[1,8,,1] = colSums(SID[1,1:2,,1])
      SID[1,9,,1] = colSums(SID[1,3:4,,1])
      SID[1,13,,1] = colSums(SID[1,8:9,,1]) # total population by sti
      SID[1,8,,2] = colSums(SID[1,1:2,,2])
      SID[1,9,,2] = colSums(SID[1,3:4,,2])
      SID[1,13,,2] = colSums(SID[1,8:9,,2]) # total population by sti
      
      SID[1,,,3] = SID[1,,,1] + SID[1,,,2]
      SID[1,,,] = SID[1,,,] * SID_split["pop_HIV", "pop_sti", 3] / SID[1,"pop_HIV", "pop_sti", 3]
      
      # if(prop_testing_interp[1,2]!=prop_testing0[1,2]){
      #   # SID_split[1,,] = SID_split[8,,] * (1 - prop_prep_interp[1])
      #   # SID_split[2,,] = SID_split[8,,] * prop_prep_interp[1]
      #   # 
      #   # SID[1,,,1] = SID[1,,,1] / SID_split[,,1] * SID[1,,,3] * prop_testing_interp[paste0(tvec_in[1]),1]
      #   # SID[1,2,,1] = SID[1,2,,1] / SID_split[2,,1] * SID[1,2,,3] * prop_testing_interp[paste0(tvec_in[1]),2]
      #   # SID[1,3:7,,1] = SID[1,3:7,,1] / SID_split[3:7,,1] * SID[1,3:7,,3] * prop_testing_interp[paste0(tvec_in[1]),3]
      #   # 
      #   # SID[1,,,2] = SID[1,,,2] / SID_split[,,2] * SID[1,,,3] * (1 - prop_testing_interp[paste0(tvec_in[1]),1])
      #   # SID[1,2,,2] = SID[1,2,,2] / SID_split[2,,2] * SID[1,2,,3] * (1 - prop_testing_interp[paste0(tvec_in[1]),2])
      #   # SID[1,3:7,,2] = SID[1,3:7,,2] / SID_split[3:7,,2] * SID[1,3:7,,3] * (1 - prop_testing_interp[paste0(tvec_in[1]),3])
      #   
      #   # SID[which(!is.finite(SID))] = 0
      #   # SID[which(SID[]>9999999)] = 0
      #   
      #   
      #   SID[1,8,,1] = colSums(SID[1,1:2,,1])
      #   SID[1,9,,1] = colSums(SID[1,3:4,,1])
      #   SID[1,13,,1] = colSums(SID[1,8:9,,1]) # total population by sti
      #   SID[1,8,,2] = colSums(SID[1,1:2,,2])
      #   SID[1,9,,2] = colSums(SID[1,3:4,,2])
      #   SID[1,13,,2] = colSums(SID[1,8:9,,2]) # total population by sti
      #   
      #   SID[1,,,3] = SID[1,,,1] + SID[1,,,2]
      #   
      #   # SID[1,,,1] / SID[1,,,3]
      #   
      #   # non-testing population
      #   # SID[,,,2] = SID[,,,3] - SID[,,,1]
      #   
      #   # SID[which(SID[]<0)] = 0.000001
      # }
      #  
    }
    
    # if(intervention==1 & sensitivity==0 & (int=="a" | int=="c")){
    #   SID[1,,,3] = SID_split[,,3]
    #   SID[1,,,1] = SID[1,,,3] * prop_testing_interp[paste0(tvec[1]),1]
    #   SID[1,2,,1] = SID[1,2,,3] * prop_testing_interp[paste0(tvec[1]),2]
    #   SID[1,3:7,,1] = SID[1,3:7,,3] * prop_testing_interp[paste0(tvec[1]),3]
    #   
    #   SID[1,8,,1] = colSums(SID[1,1:2,,1])
    #   SID[1,9,,1] = colSums(SID[1,3:4,,1])
    #   SID[1,13,,1] = colSums(SID[1,8:9,,1]) # total population by sti
    #   
    #   # SID[1,,,1] / SID[1,,,3]
    #   
    #   # non-testing population
    #   SID[,,,2] = SID[,,,3] - SID[,,,1]
    # }
    
    
    
    # initialise temp_out
    temp_out <<- SID[,,,3]
    
    for (t in 1:(length(tvec_de)-2)){
      
      # initialise temp
      temp = array(0, dim = c(13,11,3),
                   dimnames = list(c("S1", "S2", "I", "D", "D1", "D2", "D3", "HIV_minus", "HIV_plus", "incidence_HIV", "diagnoses_HIV", "deaths_HIV", "pop_HIV"),
                                   c("S_sti", "E_sti", "Sy_sti", "ASy_sti", "T_sti", "sti_minus", "sti_plus", "incidence_sti", "diagnoses_sti", "recovered_sti", "pop_sti"),
                                   c("low_risk", "high_risk", "all")))
      

      # TT is the time counter - the actual year
      TT = tvec_de[t]
      
      # get parameter values for this time period
      r_diag_HIV_de = r_diag_HIV_interp[t] * r_diag_HIV
      prop_prep_de = prop_prep_interp[as.character(TT)]
      eff_prep_de = eff_prep_interp[as.character(TT)]
      r_diag_sti_de = r_diag_sti_interp[as.character(TT),]
      condom_by_HIV_de = condom_by_HIV_interp[as.character(TT),]
      cascade_de = cascade_interp[as.character(TT),]
      prop_testing_de = prop_testing_interp[as.character(TT),]
      # prop_testing_de = rep(0.052, 3)
      # t_exp_sti_de = t_exp_sti/(365 * dt)
      # t_infect_sti_de = t_infect_sti/(365 * dt)
      # t_treatment_sti_de = t_treatment_sti/(365 * dt)
      t_testing_de = t_testing_interp[as.character(TT),,]
      r_testing_de = 4*(365 * dt)/t_testing_de
      eff_gel_de = eff_gel
      gel_mat_de = gel_array[as.character(TT),,]
      # gel_up_de = gel_up_interp[as.character(TT),]
      # gel_down_de = gel_down_interp[as.character(TT),]
      
      # prop_condom_strat = rbind(
      #   (1-condom_by_HIV_de)*(1-gel_up_de),
      #   (1-condom_by_HIV_de)*gel_up_de + condom_by_HIV_de*gel_down_de,
      #   condom_by_HIV_de*(1-gel_down_de)
      # )
      prop_condom_strat = rbind(
        (1-condom_by_HIV_de)*(1-gel_mat_de[1,]),
        (1-condom_by_HIV_de)*gel_mat_de[1,] + condom_by_HIV_de*gel_mat_de[2,],
        condom_by_HIV_de*(1-gel_mat_de[2,])
      )
      
      if(sensitivity==1){
        if(sens_scen=="a") {
          prop_condom_strat = rbind(
            c(0,0,0),
            rep(lube_threshold, 3),
            condom_by_HIV_de
          )
          prop_condom_strat[1,] = 1 - colSums(prop_condom_strat[2:3,])
          
        }
       }
      dimnames(prop_condom_strat) = list(c("Nil", "Gel", "Condom"),
                                                    c("HIV- no prep", "HIV- prep", "HIV+"))
      if(any(abs(colSums(prop_condom_strat)-1) > 10^-10) | any(prop_condom_strat<0)){
        print("Condom proportions are not contained!")
      }
      # through_condom_by_type = c(1,(1-eff_gel_de[1]),(1-eff_condom))
      through_condom_by_type = cbind(1, 1-eff_gel_de, 1-eff_condom)
      dimnames(through_condom_by_type) = list(c("HIV", "Gonorrhoea"),
                                              c("Nil", "Gel", "Condom"))
      # dot_condom = prop_condom_strat * through_condom_by_type
      dot_condom = NULL
      for(i in 1:nrow(through_condom_by_type)){
        dot_condom = abind(dot_condom, prop_condom_strat * through_condom_by_type[i,], along=3)
      }
      dimnames(dot_condom)[[3]] = c("HIV", "Gonorrhoea")
      mult_condom = colSums(dot_condom)
      mult_condom_means = colSums(SID[t,c(1,2,9),"pop_sti",3] * mult_condom) / SID[t,13,"pop_sti",3]
      cond_HIV = mult_condom_means[1]
      cond_sti = mult_condom[,2]
      
      # if(is_gel & t==20){print(mult_condom_means)}
      
      sti_testing_by_HIV = c(1,0)

      # calculate force of infections
      rel_incidence = 0
      rel_incidence = sum(SID[t,3,"pop_sti",3] + alpha1*SID[t,5,"pop_sti",3] + alpha2*SID[t,6,"pop_sti",3] + alpha3*SID[t,7,"pop_sti",3])
      pr_infect_HIV = f_infect_HIV * rel_incidence / SID[t,13,"pop_sti",3]
      pr_infect_sti = numeric(3)
      
      pr_infect_sti[1] =  f_infect_sti[1] * ((1-mix1) * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
                                                 mix1 * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      pr_infect_sti[2] =  f_infect_sti[1] * ((1-mix2) * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
                                                 mix2 * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      pr_infect_sti[3] =  f_infect_sti[2] * (mix3 * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
                                                 (1-mix3) * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      
      # pr_infect_sti[1] =  f_infect_sti[1] * ((1) * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
      #                                            mix1 * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      # pr_infect_sti[2] =  f_infect_sti[1] * ((1) * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
      #                                            mix2 * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      # pr_infect_sti[3] =  f_infect_sti[2] * (mix3 * sum(SID[t,8,3:4,3]) / SID[t,8,"pop_sti",3] + # HIV-
      #                                            (1) * sum(SID[t,9,3:4,3]) / SID[t,9,"pop_sti",3]) # HIV+
      
      # pr_infect_sti = c(f_infect_sti[1], f_infect_sti[1], f_infect_sti[2])
      
      pr_infect_sti = pr_infect_sti * cond_sti # + c(f_infect_base[1], f_infect_base[1], f_infect_base[2])
      
      pr_infect_log[t,] = pr_infect_sti
      
      # globalise force of infections
      pr_infect_HIV <<- pr_infect_HIV
      pr_infect_sti <<- pr_infect_sti
      
      
      # temp = SID[t,,,]
      
      pop_growth = SID[1,"pop_HIV", "pop_sti",3] * (1+growth)^(t)
      
      #################################
      
      # S1 and S2 for HIV
      temp[1,1:5,1:2] = SID[t,1,1:5,1:2] + dt * (SID[t,1,1:5,1:2] * (- pr_infect_HIV * cond_HIV  -
                                                           mu))
      temp[2,1:5,1:2] = SID[t,2,1:5,1:2] + dt * (SID[t,2,1:5,1:2] * (- pr_infect_HIV * cond_HIV * (1 - eff_prep_de) -
                                                           mu))

      # # growth
      # temp[1,1,1:2] = temp[1,1,1:2] + dt * growth * SID[t,"pop_HIV","pop_sti",1:2] * (1-prop_prep_de)
      # temp[2,1,1:2] = temp[2,1,1:2] + dt * growth * SID[t,"pop_HIV","pop_sti",1:2] * prop_prep_de

      # I and D for HIV
      temp[3,1:5,1:2] = SID[t,3,1:5,1:2] + dt * (SID[t,1,1:5,1:2] * (pr_infect_HIV * cond_HIV) +
                                           SID[t,2,1:5,1:2] * (pr_infect_HIV * cond_HIV * (1 - eff_prep_de)) -
                                           SID[t,3,1:5,1:2] * (r_diag_HIV_de + mu + extra_death_I))
      temp[4,1:5,1:2] = SID[t,4,1:5,1:2] + dt * (SID[t,3,1:5,1:2] * r_diag_HIV_de -
                                           SID[t,4,1:5,1:2] * (mu + extra_death_D))


      # HIV status totals
      temp[8,,1:2] = colSums(temp[1:2,,1:2])
      temp[9,,1:2] = colSums(temp[3:4,,1:2])
      

      # HIV care cascade
      temp[5,,1:2] = temp[4,,1:2] * (1 - cascade_de[2])
      temp[6,,1:2] = temp[4,,1:2] * cascade_de[2] * (1 - cascade_de[3])
      temp[7,,1:2] = temp[4,,1:2] * cascade_de[2] * cascade_de[3]

      # population totals (pop_HIV)
      temp[13,,1:2] = colSums(temp[8:9,,1:2])

      # calculate HIV totals for the next time period (incidence, diagnoses and deaths)
      temp[10,1:5,1:2] = dt *  colSums(SID[t,1:2,1:5,1:2] * pr_infect_HIV * cond_HIV) # incidence of HIV
      temp[11,1:5,1:2] = dt *  SID[t,3,1:5,1:2] * r_diag_HIV_de # diagnoses of HIV
      temp[12,1:5,1:2] = dt *  (SID[t,8,1:5,1:2] * mu + # S deaths
                              SID[t,3,1:5,1:2] * (mu + extra_death_I) + # I deaths
                              SID[t,4,1:5,1:2] * (mu + extra_death_D)) # D deaths ## total deaths

      # sti status totals
      temp[,6,1:2] = apply(temp[,c(1,5),1:2], c(1,3), sum)
      temp[,7,1:2] = apply(temp[,2:4,1:2], c(1,3), sum)


      # calculate and distribute diagnoses and incidence
      temp[c(1,2,9),8,1:2] = dt * pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat
      # temp[c(1,2,9),9,1:2] = dt * t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], c(1,3), sum)) * c(1,0))
      # temp[c(1,2,9),9,1] = dt * r_testing_de * rowSums(t(t(temp[c(1,2,9),2:4,1]) * c(1,lambda,1)))
      temp[c(1,2,9),9,1:2] = dt * r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3)))+
        dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat)) 
      temp[c(1,2,9),10,1:2] = dt * 1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]

      
      temp[8,8:10,1:2] = colSums(temp[1:2,8:10,1:2])

      temp[3:4,8,1:2] = temp[3:4,1,1:2] * (temp[9,8,1:2]/temp[9,1,1:2])
      
      diag_temp = sweep(temp[3:4,2:4,1:2], MARGIN=c(2,3), temp[9,2:4,1:2], `/`)
      diag_temp[is.na(diag_temp)] = 0
      temp[3:4,9,1:2] = apply(diag_temp, c(1,3), sum) * temp[9,9,1:2]
      # temp[3:4,9,1] = rowSums(t(t(temp[3:4,2:4,1]) / temp[9,2:4,1])) * temp[9,9,1]
      
      temp[3:4,10,1:2] = temp[3:4,5,1:2] * (temp[9,10,1:2]/temp[9,5,1:2])


      temp[5,8:10,1:2] = temp[4,8:10,1:2] * (1 - cascade_de[2])
      temp[6,8:10,1:2] = temp[4,8:10,1:2] * cascade_de[2] * (1 - cascade_de[3])
      temp[7,8:10,1:2] = temp[4,8:10,1:2] * cascade_de[2] * cascade_de[3]

      # population totals
      temp[,"pop_sti",1:2] = apply(temp[,1:5,1:2], c(1,3), sum)
      temp[13,,1:2] = colSums(temp[1:4,,1:2])

      # incidence and diagnoses overlaps for both diseases (e.g. how many people were infected with both sti AND HIV in a given time period)
      temp[10:12,8:10,1] = outer(temp[10:12,"pop_sti",1], temp[13,8:10,1]) / temp[13,"pop_sti",1]
      temp[10:12,8:10,2] = outer(temp[10:12,"pop_sti",2], temp[13,8:10,2]) / temp[13,"pop_sti",2]
      
      
      
      #############################################
      
      
      
      
      # # calculate sti compartments
      # # S
      # SID[t+1,c(1,2,9),1,1:2] = temp[c(1,2,9),1,1:2] + dt * (- pr_infect_sti * temp[c(1,2,9),1,1:2] +
      #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
      # # E
      # SID[t+1,c(1,2,9),2,1:2] = temp[c(1,2,9),2,1:2] + dt * (pr_infect_sti * temp[c(1,2,9),1,1:2] -
      #                                                          1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
      #                                                          t(t(r_testing_de * temp[c(1,2,9),2,1:2]) * c(1,0)))
      # 
      # # I
      # SID[t+1,c(1,2,9),3,1:2] = temp[c(1,2,9),3,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
      #                                                          1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
      #                                                          t(t(r_testing_de * temp[c(1,2,9),3,1:2]) * c(1,0)))
      # 
      # # L
      # SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
      #                                                          # t(t(prop_testing_de * r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
      #                                                          t(t(r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
      # 
      # # T
      # # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (t(t(prop_testing_de * r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0)) -
      # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0)) -
      #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
      
      # calculate sti compartments
      # S
      SID[t+1,c(1,2,9),1,1:2] = temp[c(1,2,9),1,1:2] + dt * (- pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat +
                                                               1/t_treatment_sti_de * temp[c(1,2,9),5,1:2])
    
      SID[t+1,1,1,1:2] = SID[t+1,1,1,1:2] + (pop_growth - sum(temp["pop_HIV","pop_sti",1:2]))*c((1-rho),rho)
      
      # E
      SID[t+1,c(1,2,9),2,1:2] = temp[c(1,2,9),2,1:2] + dt * (pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat -
                                                               1/t_exp_sti_de * temp[c(1,2,9),2,1:2] -
                                                               r_testing_de * temp[c(1,2,9),2,1:2])
      
      # # Sy
      # SID[t+1,c(1,2,9),3,1:2] = temp[c(1,2,9),3,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * symp -
      #                                                          # 1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
      #                                                          r_testing_de * temp[c(1,2,9),3,1:2] * lambda)
      
      # ASy
      SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat) -
                                                               # 1/t_infect_sti_de * temp[c(1,2,9),4,1:2] -
                                                               r_testing_de * temp[c(1,2,9),4,1:2])
      
      
      # SID[t+1,c(1,2,9),4,1:2] = temp[c(1,2,9),4,1:2] + dt * (1/t_infect_sti_de * temp[c(1,2,9),3,1:2] -
      #                                                          # t(t(prop_testing_de * r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
      #                                                          t(t(r_testing_de * temp[c(1,2,9),4,1:2]) * c(1,0)))
      
      # # T
      # SID[t+1,c(1,2,9),5,1] = temp[c(1,2,9),5,1] + dt * (rowSums(t(t(r_testing_de * temp[c(1,2,9),2:4,1])  *  c(1,lambda,1))) -
      #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1])
      # 
      # SID[t+1,c(1,2,9),5,2] = temp[c(1,2,9),5,2] + dt * (-1/t_treatment_sti_de * temp[c(1,2,9),5,2])
      
      # T
      # SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3))) -
      #                                                          1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]) +
      SID[t+1,c(1,2,9),5,1:2] = temp[c(1,2,9),5,1:2] + dt * (r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),c(2,4),1:2], c(2), c(1,1), FUN="*"), c(2,1,3))) -
                                                               1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]) +
        + dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * prop_treat)
      
      
      # sti status totals
      SID[t+1,1:9,6,1:2] = apply(SID[t+1,1:9,c(1,5),1:2], c(1,3), sum)
      SID[t+1,1:9,7,1:2] = apply(SID[t+1,1:9,2:4,1:2], c(1,3), sum)
      
      # apply(temp[c(1,2,9),2:4,1:2], c(1,3), sum)
      # apply(SID[t+1,1:9,2:4,1:2], c(1,3), sum)
      
      
      # redistribution within I and D of HIV
      # SID[t+1,3:4,1:7] = t(apply(temp[3:4,1:7,1:2], c(1,3), function(x) x * (SID[t+1,9,1:7,1:2] / temp[9,1:7,1:2]))) # potentially problematic but appears to work
     
      
      SID[t+1,3:4,1:7,1:2] = (temp[3:4,1:7,1:2]) * aperm(replicate(2, (SID[t+1,9,1:7,1:2] / temp[9,1:7,1:2])), c(3,1,2))
      SID[t+1,3:4,1:7,1:2][!is.finite(SID[t+1,3:4,1:7,1:2])] = 0
      
      # care cascade
      SID[t+1,5,1:7,1:2] = SID[t+1,4,1:7,1:2] * (1 - cascade_de[2])
      SID[t+1,6,1:7,1:2] = SID[t+1,4,1:7,1:2] * cascade_de[2] * (1 - cascade_de[3])
      SID[t+1,7,1:7,1:2] = SID[t+1,4,1:7,1:2] * cascade_de[2] * cascade_de[3]
      
      # calculate HIV- totals (sum of S1 and S2)
      SID[t+1,8,,1:2] = apply(SID[t+1,1:2,,1:2], c(2,3), sum)
      
      # population totals (pop_sti)
      SID[t+1,1:9,"pop_sti",1:2] = apply(SID[t+1,1:9,6:7,1:2], c(1,3), sum)
      
      
      # incidence and diagnoses of sti
      SID[t+1,c(1,2,9),8,1:2] = dt * pr_infect_sti * temp[c(1,2,9),1,1:2] * risk_mat
      # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(prop_testing_de * r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0))
      # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(r_testing_de * apply(temp[c(1,2,9),2:4,1:2], MARGIN=c(1,3), sum)) * c(1,0))
      # SID[t+1,c(1,2,9),9,1:2] = dt * t(t(r_testing_de * temp[c(1,2,9),7,1:2]) * c(1,0))
      
      # SID[t+1,c(1,2,9),9,1] = dt * r_testing_de * rowSums(t(t(temp[c(1,2,9),2:4,1]) * c(1,lambda,1)))
      SID[t+1,c(1,2,9),9,1:2] = dt * r_testing_de * colSums(aperm(sweep(temp[c(1,2,9),2:4,1:2], c(2), c(1,lambda,1), FUN="*"), c(2,1,3)))+
        dt * (1/t_exp_sti_de * temp[c(1,2,9),2,1:2] * (1-prop_treat)) 
          
      SID[t+1,c(1,2,9),10,1:2] = dt * 1/t_treatment_sti_de * temp[c(1,2,9),5,1:2]
      
      
      # recalculate HIV- totals (sum of S1 and S2), now with incidence and diagnoses
      SID[t+1,8,,1:2] = apply(SID[t+1,1:2,,1:2], c(2,3), sum)
      
      # redistribute HIV susceptible into PrEP proportions
      # SID[t+1,2,,1:2] = SID[t+1,8,,1:2] * prop_prep_de
      # SID[t+1,1,,1:2] = SID[t+1,8,,1:2] - SID[t+1,2,,1:2]
      
      # delta_prep_prop = (sum(SID[t+1,8,11,1:2])*prop_prep_de - sum(SID[t+1,2,11,1:2])) / sum(SID[t+1,1,11,1:2])
      delta_prep_prop = (sum(SID[t+1,8,11,1:2])*prop_prep_de - sum(SID[t+1,2,11,1:2])) / sum(SID[t+1,1,11,1:2])
      SID[t+1,2,c(1:5, 11),1:2] = SID[t+1,2,c(1:5, 11),1:2] + delta_prep_prop * SID[t+1,1,c(1:5, 11),1:2]
      SID[t+1,1,c(1:5, 11),1:2] = SID[t+1,1,c(1:5, 11),1:2] - delta_prep_prop * SID[t+1,1,c(1:5, 11),1:2]
      
      # if(iss2){
      #   delta_prep_prop_all[t] <<- delta_prep_prop
      #   
      # }
      # 
      SID[t+1,8,8:10,1:2] = apply(SID[t+1,1:2,8:10,1:2], c(2,3), sum)
      
      
      SID[t+1,3:4,8,1:2] = t(t(SID[t+1,3:4,1,1:2]) * (SID[t+1,9,8,1:2]/SID[t+1,9,1,1:2]))
      # SID[t+1,3:4,9,1:2] = t(t(SID[t+1,3:4,7,1:2]) * (SID[t+1,9,9,1:2]/SID[t+1,9,7,1:2]))
      # SID[t+1,3:4,9,1:2] = apply(SID[t+1,3:4,2:4,1:2] / aperm(replicate(2, SID[t+1,9,2:4,1:2]), c(3,1,2)) * aperm(replicate(2, replicate(3, SID[t+1,9,9,1:2])), c(3,2,1)), c(1,3), sum)
      # SID[t+1,3:4,9,1] = colSums(t(SID[t+1,3:4,2:4,1])*c(1,lambda,1) / sum(SID[t+1,9,2:4,1] * c(1,1,lambda)) * SID[t+1,9,9,1])
      SID[t+1,3:4,9,1:2] = t(t(colSums(aperm(sweep(SID[t+1,3:4,2:4,1:2], 2, c(1,lambda,1), FUN="*"), c(2,1,3)))) /
        colSums(SID[t+1,9,2:4,1:2] * c(1,lambda,1)) * SID[t+1,9,9,1:2])
      SID[t+1,3:4,10,1:2] = t(t(SID[t+1,3:4,5,1:2]) * (SID[t+1,9,10,1:2]/SID[t+1,9,5,1:2]))
      
      SID[t+1,5,8:10,1:2] = SID[t+1,4,8:10,1:2] * (1 - cascade_de[2])
      SID[t+1,6,8:10,1:2] = SID[t+1,4,8:10,1:2] * cascade_de[2] * (1 - cascade_de[3])
      SID[t+1,7,8:10,1:2] = SID[t+1,4,8:10,1:2] * cascade_de[2] * cascade_de[3]
      
      
      SID[t+1,10,,1:2] = apply(SID[t+1,1:2,,1:2] * pr_infect_HIV * c(1,(1-eff_prep_de)), c(2,3), sum) * temp[10,,1:2] /
        apply(temp[1:2,,1:2] * pr_infect_HIV * c(1,(1-eff_prep_de)), c(2,3), sum)
      SID[t+1,11,,1:2] = SID[t+1,3,,1:2] * temp[11,,1:2] / temp[3,,1:2]
      SID[t+1,12,,1:2] = apply(SID[t+1,c(8,3,4),,1:2] * c(mu, mu+extra_death_I, mu+extra_death_D), c(2,3), sum) * temp[12,,1:2] /
        apply(temp[c(8,3,4),,1:2] * c(mu, mu+extra_death_I, mu+extra_death_D), c(2,3), sum)
      
      SID[t+1,10:12,,1:2][!is.finite(SID[t+1,10:12,,1:2])] = 0
      
      
      SID[t+1,13,,1:2] = apply(SID[t+1,8:9,,1:2], c(2,3), sum)
      
      SID[t+1,,,3] = SID[t+1,,,1] + SID[t+1,,,2]
      
      # SID[t+1,,,3] = SID[t+1,,,3] * (1 - 1/50 * dt)
      
      # temp_out[t,,,3] <- temp[,,3]
      
      # print(SID[t+1,,] - SID[t,,])
      
    
    }
    
    y_adj <<- SID[dim(SID)[1],,,3]
  
  # for (t in 1:(length(tvec_de)-1)){
  
  # }
  
  pr_infect_log <<- pr_infect_log
  
  SID[1,10:12,,] =  SID[2,10:12,,]
  SID[1,,8:10,] =  SID[2,,8:10,]
  
  
  tvec_year = unique(floor(as.numeric(rownames(SID))))
  
  if(TRUE){
    HIV_out = array(0, dim = c(length(tvec_year), 3),
                    dimnames = list(tvec_year,
                                    c("incidence_HIV", "diagnoses_HIV", "deaths_HIV")))
    sti_out = array(0, dim = c(length(tvec_year), 5, 3),
                     dimnames = list(tvec_year,
                                     c("S1","S2", "HIV_minus", "HIV_plus","pop_HIV"),
                                     c("incidence_sti","diagnoses_sti","recovered_sti")))
    
    
    HIV_SID = SID[,10:12,"pop_sti",3]
    sti_SID = SID[,c(1,2,8,9,13),c(8,9,10),3]
    
    # for (i in 1:3){
    #   HIV_out[,i] = unname(tapply(HIV_SID[,i], (seq_along(HIV_SID[,i])-1) %/% (1/dt), sum))
    # }
    # 
    # for (i in 1:5){
    #   for (j in 1:2){
    #     # sti_out[,i,j] = unname(tapply(sti_SID[,i,j], (seq_along(sti_SID[,i,j])-1) %/% (1/dt), sum))
    #   }
    # }
    
    for (t in 1:length(tvec_year)){
      TT = tvec_year[t]
      for (i in 1:5){
        for (j in 1:3){
          sti_out[t,i,j] = sum(sti_SID[which(floor(as.numeric(dimnames(sti_SID)[[1]]))==TT),i,j])
        }
      }
      
      for (i in 1:3){
        HIV_out[t,i] = sum(HIV_SID[which(floor(as.numeric(dimnames(HIV_SID)[[1]]))==TT),i])
      }
    }
    
    prev_out = array(0, dim = c(length(tvec_in)-1, 4),
                     dimnames = list(tvec_in[1:(length(tvec_in)-1)],
                                     c("HIV", "sti_in_HIV_minus", "sti_in_HIV_plus", "sti")))
    
    prev_out[,1] = SID[,9,"pop_sti",3] / SID[,13,"pop_sti",3]
    prev_out[,2] = SID[,8,7,3] / SID[,8,"pop_sti",3]
    prev_out[,3] = SID[,9,7,3] / SID[,9,"pop_sti",3]
    prev_out[,4] = SID[,13,7,3] / SID[,13,"pop_sti",3]
    
    sti_by_testing = SID[,"pop_HIV","sti_plus",1:2] / SID[,"pop_HIV","pop_sti",1:2]
    sti_by_testing = as.data.frame.table(sti_by_testing)
    sti_by_testing[,1] = as.numeric(levels(sti_by_testing[,1]))[sti_by_testing[,1]]
    colnames(sti_by_testing) = c("year", "testing_status", "prop")
    # sti_by_testing = cbind(rownames(sti_by_testing), sti_by_testing)
    # colnames(sti_by_testing) = c("year", "testing", "not_testing")
    
    all_df = as.data.frame.table(SID)
    all_df[,1] = as.numeric(levels(all_df[,1]))[all_df[,1]]
    colnames(all_df) = c("year", "HIV_stat", "sti_stat", "risk_stat", "value")
    
    
    return(list(SID[,,,3], HIV_out, sti_out, prev_out, sti_by_testing, all_df, SID))
  } else {
    return(list(SID[,,,3]))
  }
  
 
  
}

# SID_list = run_model(y0,tvec0)