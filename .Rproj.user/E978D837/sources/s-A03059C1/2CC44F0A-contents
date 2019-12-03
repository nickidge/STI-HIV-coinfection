### Starting Conditions ###

#####
# old version
# sety0 = function(start_year){
#   
#   y0 = array(0, dim = c(13,11),
#              dimnames = list(c("S1", "S2", "I", "D", "D1", "D2", "D3", "HIV_minus", "HIV_plus", "incidence_HIV", "diagnoses_HIV", "deaths_HIV", "pop_HIV"),
#                              c("S_sti", "E_sti", "I_sti", "L_sti", "T_sti", "sti_minus", "sti_plus", "incidence_sti", "diagnoses_sti", "recovered_sti", "pop_sti")))
# 
#   # y0[1,1] = as.numeric(data[[6]][1,2] * (1-prop_prep_base)) # S1 (no prep) compartment
#   # y0[2,1] = as.numeric(data[[6]][1,2] * prop_prep_base) # S2 (prep) compartment
#   
#   y0[3,1] = as.numeric(data[[2]][1,2] * (1-data[[3]][1,2])) # I compartment
#   y0[4,1] = as.numeric(data[[2]][1,2] * data[[3]][1,2]) # D compartment
#   y0[5,1] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * (1 - data[[4]][1,2])) # D1 compartment
#   y0[6,1] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * data[[4]][1,2] * (1-data[[5]][1,2])) # D2 compartment
#   y0[7,1] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * data[[4]][1,2] * data[[5]][1,2]) # D3 compartment
#   y0[8,1] = as.numeric(data[[6]][1,2]) # HIV- compartment
#   
# 
#   
#   
#   # y0[1:2,7] = y0[1:2,1] * unlist(data[[7]][1,2]) # sti for HIV-
#   y0[8,7] = y0[8,1] * unlist(data[[7]][1,2]) # sti for HIV-
#   y0[3:7,7] = y0[3:7,1] * unlist(data[[7]][1,3]) # sti for HIV+
#   
#   
#   # 
#   #    y0 = array(0.1, dim = c(13,11),
#   #            dimnames = list(c("S1", "S2", "I", "D", "D1", "D2", "D3", "HIV_minus", "HIV_plus", "incidence_HIV", "diagnoses_HIV", "deaths_HIV", "pop_HIV"),
#   #                            c("S_sti", "E_sti", "I_sti", "L_sti", "T_sti", "sti_minus", "sti_plus", "incidence_sti", "diagnoses_sti", "recovered_sti", "pop_sti")))
#   # 
#   # 
#   # 
#   # y0["pop_HIV", "S_sti"]
#   # 
#   
#   y0[,2:4] = outer(y0[,7], c(11466, 10768, 18256) / 40925)
#   y0[,5] = y0[,7] * 0.01
#   
#   # y0[,1] = y0[,1] - y0[,3] * 1.03    # redistribution
#   
#   
#   # y0[8,] = colSums(y0[1:2,])  # total HIV-
#   y0[9,] = colSums(y0[3:4,])  # total HIV+
#   y0[13,] = colSums(y0[8:9,]) # total population by sti
#   
#   y0[,6] = rowSums(y0[,c(1,5)])           # total sti-
#   # y0[,7] = rowSums(y0[,2:5])  # total sti+
#   y0[,"pop_sti"] = rowSums(y0[,1:5]) # total popyulation by HIV
#   
#   
#   
# 
#   
#   
#   return(y0)
# }
#####

# new / current version
sety0 = function(){
  
  # create array for y0
  y0 = array(0, dim = c(13,11),
             dimnames = list(c("S1", "S2", "I", "D", "D1", "D2", "D3", "HIV_minus", "HIV_plus", "incidence_HIV", "diagnoses_HIV", "deaths_HIV", "pop_HIV"),
                             c("S_sti", "E_sti", "Sy_sti", "ASy_sti", "T_sti", "sti_minus", "sti_plus", "incidence_sti", "diagnoses_sti", "recovered_sti", "pop_sti")))

  # use data to create y0
  y0[3,11] = as.numeric(data[[2]][1,2] * (1-data[[3]][1,2])) # I compartment
  y0[4,11] = as.numeric(data[[2]][1,2] * data[[3]][1,2]) # D compartment
  y0[5,11] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * (1 - data[[4]][1,2])) # D1 compartment
  y0[6,11] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * data[[4]][1,2] * (1-data[[5]][1,2])) # D2 compartment
  y0[7,11] = as.numeric(data[[2]][1,2] * data[[3]][1,2] * data[[4]][1,2] * data[[5]][1,2]) # D3 compartment
  y0[8,11] = as.numeric(data[[6]][1,2] - data[[2]][1,2]) # HIV- compartment
  
  # totals
  y0[9,11] = sum(y0[3:7,11])
  
  # sti by HIV status
  # y0[1:2,7] = y0[1:2,11] * unlist(data[[7]][1,2]) # sti for HIV-
  y0[8,7] = y0[8,11] * unlist(data[[7]][1,2]) # sti for HIV-
  y0[c(3:7, 9),7] = y0[c(3:7, 9),11] * unlist(data[[7]][1,3]) # sti for HIV+
  

  y0[,6] = y0[,11] - y0[,7]
  
  y0[13,11] = sum(y0[8:9, 6:7])
  
  y0[,2:4] = outer(y0[,7], c(0.1, 0.9*(2/(2.8*symp))*symp, 0.9*(1-(2/(2.8*symp))*symp)))
  y0[,c(1,5)] = outer(y0[,6], c(0.995,0.005))

  

  
  # y0[,1] = y0[,1] - y0[,3] * 1.03    # redistribution
  
  
  # y0[8,] = colSums(y0[1:2,])  # total HIV-
  y0[9,] = colSums(y0[3:4,])  # total HIV+
  y0[13,] = colSums(y0[8:9,]) # total population by sti
  
  y0[,6] = rowSums(y0[,c(1,5)])           # total sti-
  # y0[,7] = rowSums(y0[,2:5])  # total sti+
  y0[,"pop_sti"] = rowSums(y0[,6:7]) # total population by HIV
  
  
  

  
  
  return(y0)
}