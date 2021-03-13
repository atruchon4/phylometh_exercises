plan <- drake_plan(
  tree = read.tree("dinoflag.edited.tree"),
  data = GetLatidunalRangesForAllSpecies(tree),
  data.no.na = na.omit(data),
  data.range = cbind(range = data.no.na[,4]),
  row.names(data.range) = data.no.na[,1],
  cleaned.cont = CleanData(tree, data.range),
  BM1 = geiger::fitContinuous(cleaned.cont$phy, cleaned.cont$data, model="BM"),
  OU1 = fitContinuous(cleaned.cont$phy, cleaned.cont$data, model="OU"),
  par(mfcol = (c(1,2))),
  plot(cleaned.cont$phy, cex = 0.5),
  ou.tree <- rescale(cleaned.cont$phy, model="OU", 5),
  plot(ou.tree),
  AIC.BM1 = BM1$opt$aic,
  AIC.OU1 = OU1$opt$aic
)