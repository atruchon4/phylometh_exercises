CleanData <- function(phy, data) {geiger::treedata(phy,data)
  #treedata() in Geiger is probably my favorite function in R.
}

VisualizeData <- function(phy, data) {
  plot(phy, cex = 0.3)
  #Important here is to LOOK at your data before running it. Any weird values? Does it all make sense? What about your tree? Polytomies?
  
  # Now write the code to use VisualizeData() to actually look at your data
  
}

GetLatidunalRangesForAllSpecies <- function(tree) {
  result <- data.frame(species=tree$tip.label, minlat=NA, maxlat=NA, range=NA)
  for (tip_index in seq_along(tree$tip.label)) {
    ranges <- GetLatitudinalRangeForSpecies(tree$tip.label[tip_index])
    result$minlat[tip_index] <- ranges[1]
    result$maxlat[tip_index] <- ranges[2]
    result$range[tip_index] <- ranges[2]-ranges[1]
  }
  return(result)
}

GetLatitudinalRangeForSpecies <- function(species) {
  print(species)
  all_points <- NULL
  try(all_points <- rgbif::occ_search(scientificName = species, limit = 50, fields="minimal"))
  return_object <- c(NA, NA)
  if(!is.null(all_points)) {
    if(!is.null(all_points$data)) {
      return_object <- range(all_points$data$decimalLatitude)
    }
  }
  return(return_object)
}

GetTreeWithNameProcessing <- function(treefile) {
  raw <- (treefile)
  rawsplit <- strsplit(raw, '_')
  raw2 <- paste(asplit[[1]][1], asplit[[1]][2], sep = " ")
  phy <- ape::read.tree(text=raw2)
}

fourstate <- function(tree, trait) {
  for(i in sequence(Ntip(tree))) {
    if(trait[i,2]==0 && trait[i,3]==0) {
      fourstate.trait[i]<-0
    }
    if(trait[i,2]==0 && trait[i,3]==1) {
      fourstate.trait[i]<-1
    }
    if(trait[i,2]==1 && trait[i,3]==0) {
      fourstate.trait[i]<-2
    }
    if(trait[i,2]==1 && trait[i,3]==1) {
      fourstate.trait[i]<-3
    }
  }
}
