
plot_index = rbind(
  c('PLHIV_tot', 'PLHIV'),
  c('HIV_diag_tot', 'HIV_diag'),
  c('HIV_inf_tot', 'HIV_inf'),
  c('HIV_prev_prop', 'HIV_prev'),
  c('num_diag_prop', 'care_cascade'),
  c('num_treat_prop', 'care_cascade'),
  c('num_suppr_prop', 'care_cascade'),
  c('num_und_tot', 'num_cascade'),
  c('num_diag_tot', 'num_cascade'),
  c('num_treat_tot', 'num_cascade'),
  c('num_suppr_tot', 'num_cascade')
)
colnames(plot_index) = c('pid', 'plot')
plot_index = data.frame(plot_index)

plot_keys = c('PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'care_cascade', 'num_diag', 'num_cascade')
plot_long = c('Total PLHIV', 'Annual HIV diagnoses', 'Annual HIV incidence', 'Prevalence of HIV', 'Care cascade', 'Proportion diagnosed', 'PLHIV by care cascade')

max_df_base = data.frame(
  plot = c('PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'care_cascade', 'num_diag', 'num_cascade'),
  lowerlim = 0,
  # upperlim = c(20000, 1500, 1500, 0.2, 1, 1, 20000)
  upperlim = c(25000, 2500, 3000, 0.25, 1, 1, 25000)
)

saveopen = function(p, fname='untitled', fdir='/', ext='png', open=T, ...){
  pdfname = paste0(fname, '.', ext)
  fpath = file.path(getwd(), fdir)
  dir.create(fpath, showWarnings = FALSE)
  ggsave(pdfname, plot=p, path=fpath, units = "mm", width = 0.9*page_width, height = 0.9*page_height, device=ext, ...)
  if(open){browseURL(file.path(fpath, pdfname))}
}

convert_axis = function(out_gg, labels=c('axis-l-3-1', 'axis-l-2-2')){
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