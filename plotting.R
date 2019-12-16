
plot_index = c(PLHIV = 'PLHIV',
               HIV_diag = 'HIV_diag',
               HIV_inf = 'HIV_inf',
               HIV_prev = 'HIV_prev',
               prop_diag = 'care_cascade',
               prop_treat = 'care_cascade',
               prop_suppr = 'care_cascade')

plot_keys = c('PLHIV', 'HIV_diag', 'HIV_inf', 'HIV_prev', 'care_cascade', 'prop_diag')
plot_long = c('Total PLHIV', 'Annual HIV diagnoses', 'Annual HIV incidence', 'Prevalence of HIV', 'Care cascade', 'Proportion diagnosed')


saveopen = function(p, fname='untitled', fdir='/', ext='png', open=T, ...){
  pdfname = paste0(fname, '.', ext)
  fpath = file.path(getwd(), fdir)
  dir.create(fpath, showWarnings = FALSE)
  ggsave(pdfname, plot=p, path=fpath, units = "mm", width = 0.9*page_width, height = 0.9*page_height, device=ext, ...)
  if(open){browseURL(file.path(fpath, pdfname))}
}

convert_axis = function(out_gg, labels=c('axis-l-3-1', 'axis-l-2-2')){
  gp <- ggplotGrob(out_gg)
  
  # gp[["layout"]]
  # gtable::gtable_show_layout(gp)
  
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