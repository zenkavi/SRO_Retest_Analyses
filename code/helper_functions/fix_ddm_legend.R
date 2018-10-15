fix_ddm_legend = function(ddm.plot){
  mylegend<-g_legend(ddm_boot_plot)
  
  grob_name <- names(mylegend$grobs)[1]
  
  #manually fix the legend
  #move non-decision down
  #key
  mylegend$grobs[grob_name][[1]]$layout[11,c(1:4)] <- c(4,8,4,8)
  mylegend$grobs[grob_name][[1]]$layout[12,c(1:4)] <- c(4,8,4,8)
  #text
  mylegend$grobs[grob_name][[1]]$layout[17,c(1:4)] <- c(4,10,4,10)
  #move threshold down
  #key
  mylegend$grobs[grob_name][[1]]$layout[9,c(1:4)] <- c(3,8,3,8)
  mylegend$grobs[grob_name][[1]]$layout[10,c(1:4)] <- c(3,8,3,8)
  #text
  mylegend$grobs[grob_name][[1]]$layout[16,c(1:4)] <- c(3,10,3,10)
  #move drift rate right and up
  #key
  mylegend$grobs[grob_name][[1]]$layout[7,c(1:4)] <- c(2,8,2,8)
  mylegend$grobs[grob_name][[1]]$layout[8,c(1:4)] <- c(2,8,2,8)
  #text
  mylegend$grobs[grob_name][[1]]$layout[15,c(1:4)] <- c(2,10,2,10)
  
  grid.arrange(ddm_boot_plot +theme(legend.position="none"),
               mylegend, nrow=2, heights=c(10, 1))
}

