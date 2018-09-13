g_caption<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  cap_index <- grep('plot.caption', sapply(tmp$grobs, function(x) x$name))
  caption <- tmp$grobs[[cap_index]]
  return(caption)}