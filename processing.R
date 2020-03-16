getdict = function(list, key, default){
  if(key %in% names(list)){
    return(list[[key]])
  } else {
    return(default)
  }
}

fixnan = function(x){
  x[!is.finite(x)] = 0
  return(x)
}

makearray = function(dimnames){
  return(array(0, dim=lengths(dimnames), dimnames=dimnames))
}

gen_var = function(var, n=1, ub=Inf){
  y = (1 - var) ^ runif(n=n, min=-1, max=1)
  y[y > ub] = ub
  return(y)
}

DIM <- function( ... ){
  args <- list(...)
  lapply( args , function(x) { if( is.null( dim(x) ) )
    return( length(x) )
    dim(x) } )
}

duration2rate = function(x, dt=1/12) 1 - exp(-dt / x)

sum_dim = function(thisarr, thisdim){
  thisarr = abind(thisarr, apply(thisarr, setdiff(1:length(dim(thisarr)), thisdim), sum), along=thisdim)
  last(dimnames(thisarr)[[thisdim]]) = 'tot'
  return(thisarr)
}

widen_sources = function(...){
  df = rbind.fill(...)
  df$source = factor(df$source, levels=c('model', 'data'))
  df_wide = spread(df, source, value)
  for(key in c('model', 'data')){if(!(key %in% colnames(df_wide))){df_wide[key]=NA}}
  df_wide$t = as.numeric(as.character(df_wide$t))
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
  allcols = c('risk_pop', 'med_pop', 'diag_time')
  whichcols = c('floort', intersect(allcols, colnames(df)))
  df = df %>% 
    mutate(floort = floor(as.numeric(t))) %>% 
    # group_by(floort, risk_pop, med_pop) %>%
    group_by_at(vars(whichcols)) %>%
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
  if(any(grepl('pop', keys))){
    pop = SID
    for(i in 2:length(dim(pop))){
      pop = sum_dim(pop, i)
    }
  }
  if(any(grepl('PLHIV', keys))){
    PLHIV_risk = SID[,sHIV[['PLHIV']],,,drop=FALSE]
    dimnames(PLHIV_risk)[[2]] = HIV_risk_index[dimnames(PLHIV_risk)[[2]]]
    PLHIV_risk = acast(melt(PLHIV_risk), Var1 ~ Var2 ~ Var3 ~ Var4, fun.aggregate = sum)
    for(i in 2:length(dim(PLHIV_risk))){
      PLHIV_risk = sum_dim(PLHIV_risk, i)
    }
  }
  
  
  df = data.frame()
  for(key in unique(keys)){
    
    if(key == 'PLHIV'){
      this = SID[tvec,sHIV$PLHIV,,,drop=FALSE]
      this = apply(this, c(1,4), sum)
      this = as.data.frame(this)
      this$tot = rowSums(this)
      this = melt(as.matrix(this))
      colnames(this) = c('t', 'med_pop', 'value')
      thisdf = data.frame(this, type = 'pop', dt = 1, pid='PLHIV_tot',
                          sti_pop = 'all', HIV_pop = 'PLHIV', risk_pop = 'all')
      # thisdf = data.frame(t = names(this), value = this, type = 'pop', dt = 1, pid='PLHIV_tot',
      #                     sti_pop = 'all', risk_pop = 'all', HIV_pop = 'PLHIV')
      
    } else if(key == 'pop'){
      this = SID[tvec,,,,drop=FALSE]
      this = apply(this, c(1,4), sum)
      this = as.data.frame(this)
      this$tot = rowSums(this)
      this = melt(as.matrix(this))
      colnames(this) = c('t', 'med_pop', 'value')
      thisdf = data.frame(this, type = 'pop', dt = 1, pid='pop_tot', plot='pop',
                          sti_pop = 'all', HIV_pop = 'all', risk_pop = 'all')
    } else if(key == 'popsize') {
      thisdf = melt(SID, varnames = c('t', 'HIV_pop', 'sti_pop', 'med_pop'))
      thisdf = data.frame(this, type = 'pop', dt = 1, pid='pop', plot='pop')
    } else if(key == 'popsize_by_risk') {
      thispop = pop
      dimnames(thispop)[[2]] = HIV_risk_index[dimnames(thispop)[[2]]]
      this = melt(thispop, varnames = c('t', 'risk_pop', 'sti_pop', 'med_pop'))
      this = aggregate(value~., data = this, FUN=sum)
      this = subset(this, sti_pop == 'tot' & med_pop == 'tot')
      this$med_pop = 'all'
      
      thisdf = data.frame(this, type = 'pop', dt = 1, pid='pop_by_risk', plot='popsize_by_risk',
                          HIV_pop = 'all')
    } else if(key == 'HIV_diag'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,,drop=FALSE]
      this = apply(this, c(1,4), sum)
      this = as.data.frame(this)
      this$tot = rowSums(this) 
      this = melt(as.matrix(this))
      colnames(this) = c('t', 'med_pop', 'value')
      thisdf = data.frame(this, type = 'trans', dt = 1/12, pid='HIV_diag_tot', plot='HIV_diag',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_diag')
      # thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_diag_tot',
      #                     sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_diag')
      thisdf = make_annual(thisdf)
      
    } else if(key == 'HIV_diag_by_pop'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,,drop=FALSE]
      # this = apply(this, 1, sum)
      this = apply(this, c(1,2), sum)
      this = melt(this, varnames = c("t", "I_group"), variable.name="value")
      this$risk_pop = substr(this$I_group, 3, 8)
      this$risk_pop = substr(this$risk_pop, 1, 2)
      thisdf = data.frame(this %>% select(-I_group), type = 'trans', dt = 1/12, pid='HIV_diag_by_pop',
                          sti_pop = 'all', HIV_pop = 'HIV_diag_by_pop')
      thisdf = make_annual(thisdf)

    } else if(key == 'HIV_diag_new'){
      this = HIV_trans_log
      this = this[tvec,tHIV$test,,,drop=FALSE]
      # this = apply(this, 1, sum)
      this = apply(this, c(1,2,4), sum)
      
      dimnames(this)[[2]] = substr(dimnames(this)[[2]], 6, 8)
      # this = melt(acast(melt(this), Var1 ~ Var2 ~ Var3, fun.aggregate = sum))
      this = aggregate(value~., data=melt(this), sum)
      colnames(this) = c('t', 'diag_time', 'med_pop', 'value')
      
      this = rbind.fill(this, data.frame(aggregate(value~t+diag_time, FUN=sum, data=this), med_pop='tot'))
      
      thisdf = data.frame(this, type = 'trans', dt = 1/12,
                          sti_pop = 'all')
      thisdf[,c("pid", "HIV_pop", "plot")] = paste0('HIV_diag_', thisdf$diag_time)
      thisdf = make_annual(thisdf)
      
    } else if(key == 'HIV_inf'){
      this = HIV_trans_log
      this = this[tvec,tHIV$inf,,,drop=FALSE]
      this = apply(this, c(1,4), sum)
      this = as.data.frame(this)
      this$tot = rowSums(this) 
      this = melt(as.matrix(this))
      colnames(this) = c('t', 'med_pop', 'value')
      thisdf = data.frame(this, type = 'trans', dt = 1/12, pid='HIV_inf_tot',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_inf')
      # thisdf = data.frame(t = names(this), value = this, type = 'trans', dt = 1/12, pid='HIV_inf_tot',
      #                     sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_inf')
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
      
      this = SID[tvec,,,,drop=FALSE]
      thisPLHIV = apply(this[,sHIV$PLHIV,,,drop=FALSE], 1, sum)
      thistotpop = apply(this, 1, sum)
      thisprev = thisPLHIV / thistotpop

      thisPLHIVall = apply(this[,sHIV$PLHIV,,,drop=FALSE], c(1,4), sum)
      thistotpopall = apply(this, c(1,4), sum)
      thisprevall = thisPLHIVall / thistotpopall
      thisprevall = melt(as.matrix(thisprevall))
      colnames(thisprevall) = c('t', 'med_pop', 'value')

      tvec = names(thistotpop)

      thisdf = data.frame(t = tvec, value = thisprev, type = 'pop', dt = 1, pid = 'HIV_prev_prop',
                          sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_prev', plot='HIV_prev', med_pop='tot')
      thisdfall = data.frame(thisprevall, type = 'pop', dt = 1, pid = 'HIV_prev_prop',
                             sti_pop = 'all', risk_pop = 'all', HIV_pop = 'HIV_prev', plot='HIV_prev')
      thisdf = rbind.fill(thisdf, thisdfall)
      thisdf = thisdf[is.finite(thisdf$value),]

    }  else if(key == 'HIV_prev_by_risk'){
      
      this = apply(SID[tvec,,,,drop=FALSE], c(1,2,4), sum)
      thisPLHIV = this[,sHIV$PLHIV,,drop=FALSE]
      dimnames(thisPLHIV)[[2]] = HIV_risk_index[dimnames(thisPLHIV)[[2]]]
      # thisPLHIV = apply(this[,sHIV$PLHIV,,], 1, sum)
      thistotpop = this
      dimnames(thistotpop)[[2]] = HIV_risk_index[dimnames(thistotpop)[[2]]]
      
      
      thisPLHIV = acast(melt(thisPLHIV), Var1 ~ Var2 ~ Var3, fun.aggregate = sum)
      thistotpop = acast(melt(thistotpop), Var1 ~ Var2 ~ Var3, fun.aggregate = sum)
      
      # sum along aus/int dimension
      for(thisdim in 2:3){
        thisPLHIV = sum_dim(thisPLHIV, thisdim)
        # thisPLHIV = abind(thisPLHIV, apply(thisPLHIV, setdiff(c(1,2,3), thisdim), sum), along=thisdim)
        # last(dimnames(thisPLHIV)[[thisdim]]) = 'tot'
        
        thistotpop = sum_dim(thistotpop, thisdim)
        # thistotpop = abind(thistotpop, apply(thistotpop, setdiff(c(1,2,3), thisdim), sum), along=thisdim)
        # last(dimnames(thistotpop)[[thisdim]]) = 'tot'
      }
      
      thisPLHIV_hi_all = thisPLHIV[,'hi',] + thisPLHIV[,'pr',]
      thisPLHIV = abind(thisPLHIV, thisPLHIV_hi_all, along=2, new.names = )
      dimnames(thisPLHIV)[[2]][length(dimnames(thisPLHIV)[[2]])] = 'hi_all'
      
      thistotpop_hi_all = thistotpop[,'hi',] + thistotpop[,'pr',]
      thistotpop = abind(thistotpop, thistotpop_hi_all, along=2, new.names = )
      dimnames(thistotpop)[[2]][length(dimnames(thistotpop)[[2]])] = 'hi_all'
      
      thisprev = thisPLHIV / thistotpop
      
      thisprevall = melt(thisprev)
      colnames(thisprevall) = c('t', 'risk_pop', 'med_pop', 'value')
      
      thisprevall = subset(thisprevall, med_pop == 'tot')
      thisprevall$med_pop = 'all'
      
      thisdf = data.frame(thisprevall, type = 'pop', dt = 1, pid = 'HIV_prev_by_risk_prop',
                          sti_pop = 'all', HIV_pop = 'HIV_prev', plot='HIV_prev_by_risk')
      # thisdf = thisdf[is.finite(thisdf$value),]
      
    }  else if(key == 'HIV_prev_by_risk_all'){
      
      pop_risk = pop
      dimnames(pop_risk)[[2]] = HIV_risk_index[dimnames(pop_risk)[[2]]]
      pop_risk = acast(melt(pop_risk), Var1 ~ Var2 ~ Var3 ~ Var4, fun.aggregate = sum)
      thisprev = PLHIV_risk / pop_risk
      
      thisprevall = melt(thisprev)
      colnames(thisprevall) = c('t', 'risk_pop', 'sti_pop', 'med_pop', 'value')
      
      thisprevall = subset(thisprevall, sti_pop == 'tot')
      
      thisdf = data.frame(thisprevall, type = 'pop', dt = 1, pid = 'HIV_prev_by_risk_all',
                          HIV_pop = 'HIV_prev', plot='HIV_prev_by_risk_all')
      # thisdf = thisdf[is.finite(thisdf$value),]

    } else if(key == 'prop_prep'){
      
      this = SID[tvec,,,,drop=FALSE]
      thisprep = apply(this[,sHIV$S_pr,,,drop=FALSE], 1, sum)
      thistotpop = apply(this[,sHIV$S,,,drop=FALSE], 1, sum)
      thisprev = thisprep / thistotpop
      
      thisprepall = apply(this[,sHIV$S_pr,,,drop=FALSE], c(1,4), sum)
      thistotpopall = apply(this[,sHIV$S,,,drop=FALSE], c(1,4), sum)
      thisprevall = thisprepall / thistotpopall
      thisprevall = melt(as.matrix(thisprevall))
      colnames(thisprevall) = c('t', 'med_pop', 'value')
      
      tvec = names(thistotpop)
      
      thisdf = data.frame(t = tvec, value = thisprev, type = 'pop', dt = 1, pid = 'num_prep_prop',
                          sti_pop = 'all', risk_pop = 'pr', HIV_pop = 'S', plot='prop_prep', med_pop='tot')
      thisdfall = data.frame(thisprevall, type = 'pop', dt = 1, pid = 'num_prep_prop',
                             sti_pop = 'all', risk_pop = 'pr', HIV_pop = 'S', plot='prop_prep')
      thisdf = rbind.fill(thisdf, thisdfall)
      thisdf = thisdf[is.finite(thisdf$value),]
      
    } else if(key == 'num_cascade'){
      
      get_num_ppl = function(HIV_ppl) apply(SID[tvec,HIV_ppl,,,drop=FALSE], 1, sum)
      
      thisund = get_num_ppl(sHIV$I)
      thisD1 = get_num_ppl(sHIV$D1)
      thisD2 = get_num_ppl(sHIV$D2)
      thisD3 = get_num_ppl(sHIV$D3)
      
      this_df_template = data.frame(t = names(thisund), type='pop', dt = 1/12, sti_pop = 'all', risk_pop = 'all', plot='num_cascade')
      
      thisund_df = data.frame(this_df_template, value = thisund, pid='num_und_tot', HIV_pop = 'num_und')
      thisd1_df = data.frame(this_df_template, value = thisD1, pid='num_diag_tot', HIV_pop = 'num_diag')
      thisd2_df = data.frame(this_df_template, value = thisD2, pid='num_treat_tot', HIV_pop = 'num_treat')
      thisd3_df = data.frame(this_df_template, value = thisD3, pid='num_suppr_tot', HIV_pop = 'num_suppr')

      thisdf = rbind.fill(thisund_df, thisd1_df, thisd2_df, thisd3_df)
      
    } else {
      thisdf = data.frame()
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

get_movement = function(compartment, HIV_trans, med=1:length(med_labs), sum=FALSE){
  this = HIV_trans[,,med,drop=FALSE]
  if(sum){
    going_in = sum(this[HIV_transitions[,'to'] == compartment,,])
    going_out = sum(this[HIV_transitions[,'from'] == compartment,,])
    return(going_in - going_out)
  } else {
    going_in = colSums(this[HIV_transitions[,'to'] == compartment,,,drop=FALSE])
    going_out = colSums(this[HIV_transitions[,'from'] == compartment,,,drop=FALSE])
    d = going_in - going_out
    return(d)
  }
}
