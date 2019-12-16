
plot_index = c(PLHIV = 'PLHIV',
               HIV_diag = 'HIV_diag',
               HIV_inf = 'HIV_inf',
               prop_diag = 'care_cascade',
               prop_treat = 'care_cascade',
               prop_suppr = 'care_cascade')

plot_keys = c('PLHIV', 'HIV_diag', 'HIV_inf', 'care_cascade')
plot_long = c('Total PLHIV', 'Annual HIV diagnoses', 'Annual HIV incidence', 'Care cascade')


saveopen = function(p, fname='untitled', fdir='/', ext='png', open=T, ...){
  pdfname = paste0(fname, '.', ext)
  fpath = file.path(getwd(), fdir)
  dir.create(fpath, showWarnings = FALSE)
  ggsave(pdfname, plot=p, path=fpath, units = "mm", width = 0.9*page_width, height = 0.9*page_height, device=ext, ...)
  if(open){browseURL(file.path(fpath, pdfname))}
}
