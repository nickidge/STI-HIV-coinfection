
plot_index = rbind(
  c('pop_tot', 'pop'),
  c('pop_by_risk', 'popsize_by_risk'),
  c('PLHIV_tot', 'PLHIV'),
  c('num_prep_prop', 'prop_prep'),
  c('HIV_diag_tot', 'HIV_diag'),
  c('HIV_inf_tot', 'HIV_inf'),
  c('HIV_prev_prop', 'HIV_prev'),
  c('HIV_prev_by_risk_prop', 'HIV_prev_by_risk'),
  c('HIV_prev_by_risk_all', 'HIV_prev_by_risk_all'),
  c('num_diag_prop', 'care_cascade'),
  c('num_treat_prop', 'care_cascade'),
  c('num_suppr_prop', 'care_cascade'),
  c('num_und_tot', 'num_cascade'),
  c('num_diag_tot', 'num_cascade'),
  c('num_treat_tot', 'num_cascade'),
  c('num_suppr_tot', 'num_cascade'),
  c('HIV_diag_new', 'HIV_diag_new'),
  c('HIV_diag_old', 'HIV_diag_old')
)
colnames(plot_index) = c('pid', 'plot')
plot_index = data.frame(plot_index)

plot_key_index = rbind(
  c('pop', 'Population'),
  c('popsize_by_risk', 'Population by risk status'),
  c('PLHIV','Total PLHIV'),
  c('prop_prep', 'Proportion HIV- on PrEP'),
  c('HIV_diag', 'Annual HIV diagnoses'),
  c('HIV_inf', 'Annual HIV incidence'),
  c('HIV_prev', 'Prevalence of HIV'),
  c('HIV_prev_by_risk', 'HIV prevalence by risk population'),
  c('HIV_prev_by_risk_all', 'All HIV prevalence by risk population'),
  c('care_cascade', 'Care cascade'),
  c('num_diag', 'Proportion diagnosed'),
  c('num_cascade', 'PLHIV by care cascade'),
  c('HIV_diag_by_pop', 'Annual HIV diagnoses by pop'),
  c('HIV_diag_new', 'Annual early HIV diagnoses'),
  c('HIV_diag_old', 'Annual late HIV diagnoses')
)

plot_keys = plot_key_index[,1]
plot_long = plot_key_index[,2]

max_df_base = data.frame(
  plot = c('pop', 'PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'care_cascade', 'num_diag', 'num_cascade'),
  lowerlim = 0,
  # upperlim = c(20000, 1500, 1500, 0.2, 1, 1, 20000)
  upperlim = c(100000, 25000, 2500, 3000, 0.25, 1, 1, 25000)
)

saveopen = function(p, fname='untitled', fdir='/', ext='png', open=T, width=0.9*page_width, height=0.9*page_height, ...){
  pdfname = paste0(fname, '.', ext)
  fpath = file.path(getwd(), fdir)
  dir.create(fpath, showWarnings = FALSE)
  ggsave(pdfname, plot=p, path=fpath, units = "mm", width = width, height = height, device=ext, ...)
  if(open){browseURL(file.path(fpath, pdfname))}
}

convert_axis = function(out_gg, labels=c()){
  gp <- ggplotGrob(out_gg)
  
  gp <- grid.force(gp)
  
  for(l in labels){
    path.to.label <- gPath(l, "axis", "axis", "GRID.text")
    
    old.label <- getGrob(gTree = gp,
                         gPath = path.to.label,
                         grep = TRUE)[["label"]]
    
    new.label <- percent(as.numeric(old.label))
    
    gp = editGrob(grob = gp,
                  gPath = path.to.label,
                  label = new.label,
                  grep = TRUE)
  }
  
  return(gp)
}