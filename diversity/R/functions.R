VisualizeData <- function(phy, data) {
  plot(phy, cex = 0.3)
  #Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  # Now write the code to use VisualizeData() to actually look at your data
  
}

plot_diversification <- function(lambda1, mu1, lambda2, mu2) {
  trees.1 <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=lambda1, mu=mu1, complete=FALSE)
  trees.2 <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=lambda2, mu=mu2, complete=FALSE)
  depth.range <- range(unlist(lapply(trees.1,ape::branching.times)), unlist(lapply(trees.2,ape::branching.times)))
  max.depth <- sum(abs(depth.range)) #ape rescales depths
  plot(x=c(0, -1*max.depth), y=c(1, ape::Ntip(trees.1[[1]])), log="y", type="n", bty="n", xlab="Time", ylab="N")
  colors=c(rgb(1,0,0,0.5), rgb(0, 0, 0, 0.5))
  list.of.both <- list(trees.2, trees.1)
  for (i in sequence(2)) {
    tree.list <- list.of.both[[i]]
    for (j in sequence(length(tree.list))) {
      ape::ltt.lines(tree.list[[j]], col=colors[[i]])
    }
  }
}

plot_diversification_zoom <- function(lambda1, mu1, lambda2, mu2) {
  trees.1 <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=lambda1, mu=mu1, complete=FALSE)
  trees.2 <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=lambda2, mu=mu2, complete=FALSE)
  depth.range <- range(unlist(lapply(trees.1,ape::branching.times)), unlist(lapply(trees.2,ape::branching.times)))
  max.depth <- sum(abs(depth.range)) #ape rescales depths
  plot(x=c(0, -5), y=c(200, ape::Ntip(trees.1[[1]])), log="y", type="n", bty="n", xlab="Time", ylab="N")
  colors=c(rgb(1,0,0,0.5), rgb(0, 0, 0, 0.5))
  list.of.both <- list(trees.2, trees.1)
  for (i in sequence(2)) {
    tree.list <- list.of.both[[i]]
    for (j in sequence(length(tree.list))) {
      ape::ltt.lines(tree.list[[j]], col=colors[[i]])
    }
  }
}