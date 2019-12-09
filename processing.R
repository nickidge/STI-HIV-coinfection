widen_sources = function(...){
  df = rbind.fill(...)
  df$source = factor(df$source, levels=c('model', 'data'))
  df_wide = spread(df, source, value)
  for(key in c('model', 'data')){if(!(key %in% colnames(df_wide))){df_wide[key]=NA}}
  return(df_wide)
}

extr = function(output, keys, tvec=tvec_base){
  keys = unique(keys)
  SID = output$SID
  HIV_trans_log = output$HIV_trans_log
  tvec = as.character(tvec)
  tvec = intersect(tvec, dimnames(SID)[[1]])
  dat = list()
  df = data.frame()
  # if(!is.null(intersect(c('PLHIV'), keys))){
  #   SID = uncal$SID
  # }
  for(i in 1:length(keys)){
    key = keys[i]
    if(key == 'PLHIV'){
      this = SID[tvec,sHIV$PLHIV,,]
      this = apply(this, 1, sum)
      # this = this[as.numeric(names(this)) == floor(as.numeric(names(this)))]
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

l2 = function(x, y){
  v = (x - y) / pmax(1e-6, pmin(x, y))
  v = v^2
  return(sum(v, na.rm=T))
}

get_movement = function(HIV_compartment, HIV_trans){
  l = lapply(1:nrow(HIV_transitions), function(x) HIV_trans[x,,] * ((HIV_transitions[x, "to"] == HIV_compartment) - (HIV_transitions[x, "from"] == HIV_compartment)))
  d = Reduce('+', l)
  return(d)
}

