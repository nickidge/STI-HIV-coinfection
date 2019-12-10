
plot_df = function(df){
  p = ggplot(df, aes(x=t, group=scen, colour=scen, fill=scen))
  if('order' %in% colnames(df)){p = p + aes(order=order)}
  p = p + facet_wrap(.~HIV_pop+type, scales="free_y", ncol=1)
  p = p + geom_point(aes(y = data), na.rm=T, size=1.3)
  p = p + geom_path(aes(y = model), na.rm=T, lwd=1.3)
  
  p = p + expand_limits(y = 0)
  p = p + scale_x_continuous(breaks = floor(min(df$t)):ceiling(max(df$t)),
                             limits = c(min(df$t), max(df$t)),
                             expand = c(0, 0))
  
  p = p + coord_cartesian(xlim = plot_years)
  
  p = p + scale_y_continuous(expand = expand_scale(mult = c(0, .1)))
  p = p + theme_all
  
  return(p)
}


saveopen = function(p, fname='untitled', fdir='/', ext='png', ...){
  pdfname = paste0(fname, '.', ext)
  fpath = file.path(getwd(), fdir)
  dir.create(fpath, showWarnings = FALSE)
  ggsave(pdfname, plot=p, path=fpath, units = "mm", width = 0.9*page_width, device=ext, ...)
  browseURL(file.path(fpath, pdfname))
}
