getdict = function(list, key, default){
  if(key %in% names(list)){
    return(list[[key]])
  } else {
    return(default)
  }
}

widen_sources = function(...){
  df = rbind.fill(...)
  df$source = factor(df$source, levels=c('model', 'data'))
  df_wide = spread(df, source, value)
  for(key in c('model', 'data')){if(!(key %in% colnames(df_wide))){df_wide[key]=NA}}
  df_wide$t = as.numeric(as.character(df_wide$t))
  # df_wide$plot = factor(plot_keys[match(df_wide$HIV_pop, plot_keys)], levels=(plot_keys))
  
  # df_wide$plot = NULL
  # df_wide = merge(df_wide, plot_index, all.x=T, all.y=F, sort=F)
  df_wide = suppressMessages(join(df_wide, plot_index))
  df_wide$plot = factor(df_wide$plot, levels=plot_keys)
  
  return(df_wide)
}

make_interp = function(v, tvec=tvec_base){
  mat_interp = t(replicate(length(tvec), v))
  row.names(mat_interp) = as.character(tvec)
  return(mat_interp)
}

extr = function(output, keys, tvec=tvec_base){
  keys = unique(keys)
  SID = output$SID
  HIV_trans_log = output$HIV_trans_log
  tvec = as.character(tvec)
  tvec = intersect(tvec, dimnames(SID)[[1]])
  if(any(is.na(SID))){
    print('')
  }
  df = data.frame()
  for(i in 1:length(keys)){
    key = keys[i]
    if(key == 'PLHIV'){
      this = SID[tvec,sHIV$PLHIV,,]
      this = apply(this, 1, sum)
      thisdf = data.frame(t = names(this), value = this, type = 'pop', dt = 1, pid='PLHIV_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'PLHIV')
    } else if(key == 'HIV_diag'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,]
      this = apply(this, c(1,2), sum)
      this = rowSums(this)
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_diag_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_diag')
      thisdf = thisdf %>% 
        mutate(floort = floor(as.numeric(t))) %>% 
        group_by(floort) %>%
        mutate(value = mean(value) * 12) %>% 
        ungroup() %>% 
        filter(as.numeric(t) == floort) %>% 
        mutate(dt = 1) %>% 
        select(-floort) %>% 
        as.data.frame()
    } else if(key == 'HIV_inf'){
      this = HIV_trans_log
      this = this[tvec,tHIV$inf,,]
      this = apply(this, c(1,2), sum)
      this = rowSums(this)
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_inf_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_inf')
      thisdf = thisdf %>% 
        mutate(floort = floor(as.numeric(t))) %>% 
        group_by(floort) %>%
        mutate(value = mean(value) * 12) %>% 
        ungroup() %>% 
        filter(as.numeric(t) == floort) %>% 
        mutate(dt = 1) %>% 
        select(-floort) %>% 
        as.data.frame()
    } else if(key == 'care_cascade'){
      thisPLHIV = SID[tvec,sHIV$PLHIV,,]
      thisPLHIV = apply(thisPLHIV, 1, sum)
      thisD1plus = SID[tvec,sHIV$D1plus,,]
      thisD1plus = apply(thisD1plus, 1, sum)
      thisD2plus = SID[tvec,sHIV$D2plus,,]
      thisD2plus = apply(thisD2plus, 1, sum)
      thisD3plus = SID[tvec,sHIV$D3plus,,]
      thisD3plus = apply(thisD3plus, 1, sum)
      
      thisD1plus_prop = thisD1plus / thisPLHIV
      thisD2plus_prop = thisD2plus / thisD1plus
      thisD3plus_prop = thisD3plus / thisD2plus
      
      tvec = names(thisPLHIV)
      
      thisD1plus_df = data.frame(t = tvec, value = thisD1plus_prop, type = 'pop', dt = 1, pid='num_diag_prop',
                                 sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_diag', plot='care_cascade')
      thisD2plus_df = data.frame(t = tvec, value = thisD2plus_prop, type = 'pop', dt = 1, pid='num_treat_prop',
                                 sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_treat', plot='care_cascade')
      thisD3plus_df = data.frame(t = tvec, value = thisD3plus_prop, type = 'pop', dt = 1, pid='num_suppr_prop',
                                 sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_suppr', plot='care_cascade')
      thisdf = rbind.fill(thisD1plus_df, thisD2plus_df, thisD3plus_df)
      
    } else if(key == 'HIV_prev'){
      thisPLHIV = SID[tvec,sHIV$PLHIV,,]
      thisPLHIV = apply(thisPLHIV, 1, sum)
      thistotpop = SID[tvec,,,]
      thistotpop = apply(thistotpop, 1, sum)
      
      thisprev = thisPLHIV / thistotpop
      
      tvec = names(thistotpop)
      
      thisdf = data.frame(t = tvec, value = thisprev, type = 'pop', dt = 1, pid = 'HIV_prev_prop',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_prev', plot='HIV_prev')
      
    } else if(key == 'num_cascade'){
      thisund = SID[tvec,sHIV$I,,]
      thisund = apply(thisund, 1, sum)
      thisD1 = SID[tvec,'D1',,]
      thisD1 = apply(thisD1, 1, sum)
      thisD2 = SID[tvec,'D2',,]
      thisD2 = apply(thisD2, 1, sum)
      thisD3 = SID[tvec,'D3',,]
      thisD3 = apply(thisD3, 1, sum)
      
      tvec = names(thisund)
      
      thisund_df = data.frame(t = tvec, value = thisund, type = 'pop', dt = 1/12, pid='num_und_tot',
                              sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_und', plot='num_cascade')
      thisd1_df = data.frame(t = tvec, value = thisD1, type = 'pop', dt = 1/12, pid='num_diag_tot',
                             sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_diag', plot='num_cascade')
      thisd2_df = data.frame(t = tvec, value = thisD2, type = 'pop', dt = 1/12, pid='num_treat_tot',
                             sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_treat', plot='num_cascade')
      thisd3_df = data.frame(t = tvec, value = thisD3, type = 'pop', dt = 1/12, pid='num_suppr_tot',
                             sti_pop = 'all', risk_pop = 'all', HIV_pop = 'num_suppr', plot='num_cascade')

      thisdf = rbind.fill(thisund_df, thisd1_df, thisd2_df, thisd3_df)
      
    }
    df = rbind.fill(df, thisdf)
  }
  if('plot' %in% names(df)){
    df$plot[is.na(df$plot)] = df$HIV_pop[is.na(df$plot)]
  } else {
    df$plot = df$HIV_pop
  }
  df$source = 'model'
  df$scen = ''
  return(df)
}

get_movement = function(HIV_compartments, HIV_trans){
  l = lapply(1:nrow(HIV_transitions), function(x) HIV_trans[x,,] * ((HIV_transitions[x, "to"] %in% HIV_compartments) - (HIV_transitions[x, "from"] %in% HIV_compartments)))
  d = Reduce('+', l)
  # if(any(is.na(d))){
  #   print('')
  # }
  return(d)
}
