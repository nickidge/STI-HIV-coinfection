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
  df_wide$facet_long = plot_long[match(df_wide$HIV_pop, plot_keys)]
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
  dat = list()
  df = data.frame()
  for(i in 1:length(keys)){
    key = keys[i]
    if(key == 'PLHIV'){
      this = SID[tvec,sHIV$PLHIV,,]
      this = apply(this, 1, sum)
      thisdf = data.frame(t = names(this), value = this, type = 'pop', dt = 1,
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'PLHIV')
    } else if(key == 'HIV_diag'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,]
      this = apply(this, c(1,2), sum)
      this = rowSums(this)
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12,
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
      thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12,
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
    }
    dat = c(dat, setNames(list(this), key))
    df = rbind.fill(df, thisdf)
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

