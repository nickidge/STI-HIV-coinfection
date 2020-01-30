getdict = function(list, key, default){
  if(key %in% names(list)){
    return(list[[key]])
  } else {
    return(default)
  }
}

fixnan = function(x){
  x[is.nan(x)] = 0
  return(x)
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

make_annual = function(df){
  df = df %>% 
    mutate(floort = floor(as.numeric(t))) %>% 
    group_by(floort, risk_pop) %>%
    mutate(value = mean(value) * 12) %>% 
    ungroup() %>% 
    filter(as.numeric(t) == floort) %>% 
    mutate(dt = 1) %>% 
    select(-floort) %>% 
    as.data.frame()
  return(df)
}

# extract data frame of values from model output
extr = function(output, keys, tvec=tvec_base){
  
  SID = output$SID
  HIV_trans_log = output$HIV_trans_log
  tvec = intersect(as.character(tvec), dimnames(SID)[[1]])
  
  df = data.frame()
  for(key in unique(keys)){
    
    if(key == 'PLHIV'){
      this = SID[tvec,sHIV$PLHIV,,]
      this = apply(this, 1, sum)
      thisdf = data.frame(t = names(this), value = this, type = 'pop', dt = 1, pid='PLHIV_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'PLHIV')
      
    } else if(key == 'HIV_diag'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,]
      this = apply(this, 1, sum)
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_diag_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_diag')
      thisdf = make_annual(thisdf)
      
    } else if(key == 'HIV_diag_by_pop'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,]
      # this = apply(this, 1, sum)
      this = apply(this, c(1,2), sum)
      this = melt(this, varnames = c("t", "I_group"), variable.name="value")
      this$risk_pop = substr(this$I_group, 3, 8)
      this$risk_pop = substr(this$risk_pop, 1, 2)
      thisdf = data.frame(this %>% select(-I_group), type = 'trans', dt = 1/12, pid='HIV_diag_by_pop',
                          sti_pop = 'all', HIV_pop = 'HIV_diag_by_pop')
      thisdf = make_annual(thisdf)
      
    } else if(key == 'HIV_inf'){
      this = HIV_trans_log
      this = this[tvec,tHIV$inf,,]
      this = apply(this, 1, sum)
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_inf_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_inf')
      thisdf = make_annual(thisdf)
      
    } else if(key == 'care_cascade'){
      
      get_num_ppl = function(comp_key) apply(SID[tvec,sHIV[[comp_key]],,], 1, sum)
      
      thisPLHIV = get_num_ppl('PLHIV')
      thisD1plus = get_num_ppl('D1plus')
      thisD2plus = get_num_ppl('D2plus')
      thisD3plus = get_num_ppl('D3plus')
      
      this_df_template = data.frame(t = names(thisPLHIV), type = 'pop', dt = 1,
                                     sti_pop = 'all', risk_pop = 'all', plot='care_cascade')
      
      thisD1plus_df = data.frame(this_df_template, value = thisD1plus / thisPLHIV, pid='num_diag_prop', HIV_pop = 'num_diag')
      thisD2plus_df = data.frame(this_df_template, value = thisD2plus / thisD1plus, pid='num_treat_prop', HIV_pop = 'num_treat')
      thisD3plus_df = data.frame(this_df_template, value = thisD3plus / thisD2plus, pid='num_suppr_prop', HIV_pop = 'num_suppr')
      
      thisdf = rbind.fill(thisD1plus_df, thisD2plus_df, thisD3plus_df)
      
    } else if(key == 'HIV_prev'){
      
      this = SID[tvec,,,]
      thisPLHIV = apply(this[,sHIV$PLHIV,,], 1, sum)
      thistotpop = apply(this, 1, sum)
      
      thisprev = thisPLHIV / thistotpop
      
      tvec = names(thistotpop)
      
      thisdf = data.frame(t = tvec, value = thisprev, type = 'pop', dt = 1, pid = 'HIV_prev_prop',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_prev', plot='HIV_prev')
      
    } else if(key == 'num_cascade'){
      
      get_num_ppl = function(HIV_ppl) apply(SID[tvec,HIV_ppl,,], 1, sum)
      
      thisund = get_num_ppl(sHIV$I)
      thisD1 = get_num_ppl('D1')
      thisD2 = get_num_ppl('D2')
      thisD3 = get_num_ppl('D3')
      
      this_df_template = data.frame(t = names(thisund), type='pop', dt = 1/12, sti_pop = 'all', risk_pop = 'all', plot='num_cascade')
      
      thisund_df = data.frame(this_df_template, value = thisund, pid='num_und_tot', HIV_pop = 'num_und')
      thisd1_df = data.frame(this_df_template, value = thisD1, pid='num_diag_tot', HIV_pop = 'num_diag')
      thisd2_df = data.frame(this_df_template, value = thisD2, pid='num_treat_tot', HIV_pop = 'num_treat')
      thisd3_df = data.frame(this_df_template, value = thisD3, pid='num_suppr_tot', HIV_pop = 'num_suppr')

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
