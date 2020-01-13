
allscens = c("Base", sapply(scenarios, function(x) x$long))

tab_inc = function(df) subset(df, HIV_pop == 'HIV_inf' & type == 'trans' & t >= 2017 & t < 2025)
tab_prev = function(df) subset(df, HIV_pop == 'HIV_prev' & plot == 'HIV_prev' & t == 2025)
tab_PLHIV = function(df) subset(df, HIV_pop == 'PLHIV' & plot == 'PLHIV' & t == 2025)
tab_propdiag = function(df) subset(df,  pid == 'num_diag_prop' & t == 2025)

tab_row_funcs = list('Incidence between 2017 and 2025' = list(tab_inc, function(x) round(x)),
                     'HIV prevalence in 2025' = list(tab_prev, function(x) paste0(100 * round(x, 3), '%')),
                     'PLHIV in 2025' = list(tab_PLHIV, function(x) round(x)),
                     'Proportion of PLHIV diagnosed at 2025' = list(tab_propdiag, function(x) paste0(100 * round(x, 3), '%')))

tab_get_value = function(df, fmat=function(x) x){
  values = c(sum(df$model), sum(df$lower_ci), sum(df$upper_ci))
  # values = fmat(values)
  # return(paste0(values[1], ' (', values[2], ' - ', values[3], ')'))
  # return(paste0(values[1], ' [', values[2], ', ', values[3], ']'))
  return(values[1])
}

make_tab = function(tab_row_funcs, allscens){
  tab_out = makearray(list(names(tab_row_funcs), allscens))
  tab_out[,] = do.call(rbind, lapply(1:length(tab_row_funcs), function(i)  sapply(allscens, function(x) tab_get_value(tab_row_funcs[[i]][[1]](subset(scen_df, scen==x)), tab_row_funcs[[i]][[2]]))))
  
  for(i in 2:length(allscens)){
    r_vals = unlist(lapply(1:length(tab_row_funcs), function(j) tab_row_funcs[[j]][[2]](as.numeric(tab_out[j,i]))))
    r_percs = round(100 * (as.numeric(tab_out[,i]) - as.numeric(tab_out[,1]))/as.numeric(tab_out[,1]))
    tab_out[,i] = paste0(r_vals, ' (', r_percs, '%)')
  }
  tab_out[,1] = unlist(lapply(1:length(tab_row_funcs), function(j) tab_row_funcs[[j]][[2]](as.numeric(tab_out[j,1]))))
  
  return(tab_out)
}
