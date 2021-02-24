CleanData <- function(phy, data) {geiger::treedata(phy,data)
  #treedata() in Geiger is probably my favorite function in R.
}

VisualizeData <- function(phy, data) {
  plot(phy, cex = 0.3)
  #Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  # Now write the code to use VisualizeData() to actually look at your data
  
}

plot_tree <- function(phy, file, pars, ml) {
  pdf(file=file, width = 20, height = 20)
  plotAnc(phy, pars, cex = .9, cex.pie = .4)
  plotAnc(phy, ml, cex = .9, cex.pie = .4)
  
  dev.off()
}