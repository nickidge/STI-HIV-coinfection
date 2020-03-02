
create_results_df = function(scen_df){
  dft = scen_df %>% 
    scen_update() %>% 
    select(plot, scen_short, t, data, model, lower_ci, upper_ci) %>%
    filter(t == floor(t)) %>% 
    filter(t >= 2010) %>% 
    mutate(t = factor(t, levels=unique(t))) %>% 
    melt(id.vars = c('plot', 't', 'scen_short'), variable.name = 'source') %>% 
    filter(!is.na(value)) %>% 
    # unique() %>% 
    group_by(plot) %>% 
    mutate(value = case_when(
      max(value) <= 1 ~ round(value, 3),
      TRUE ~ round(value)
    )) %>% 
    spread(t, value, fill=NA, drop=F)
  return(dft)
}

save_results_xlsx = function(dft, filename='plots/results.xlsx'){
  wb = createWorkbook()
  for(thisscen in setdiff(unique(dft$scen_short), 'data')){
    for(thissource in unique(dft$source)){
      thissheet = paste(thisscen, thissource, sep='_')
      outt = dft %>% 
        filter(scen_short == thisscen) %>% 
        filter(source == thissource) %>% 
        select(-source, -scen_short)
      if(sum(!is.na(outt[,-1]))){
        addWorksheet(wb, thissheet)
        writeData(wb, sheet=thissheet, x=outt)
      }
    }
  }
  saveWorkbook(wb, filename, overwrite = T)
}
