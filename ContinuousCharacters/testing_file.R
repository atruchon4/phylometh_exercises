tree = read.tree("dinoflag.edited.tree")
data = GetLatidunalRangesForAllSpecies(tree)
data.no.na = na.omit(data)
data.range = cbind(range <- data.no.na[,4])
data.range = cbind(range = data.no.na[,4])
row.names(data.range) <- data.no.na[,1]
cleaned.cont = CleanData(tree, data.range)
BM1 = geiger::fitContinuous(cleaned.cont$phy, cleaned.cont$data, model="BM")
OU1 = fitContinuous(cleaned.cont$phy, cleaned.cont$data, model="OU")
par(mfcol = (c(1,2)))
plot(cleaned.cont$phy, cex = 0.5, show.tip.label = FALSE)
ou.tree = rescale(cleaned.cont$phy, model="OU", 5)
ou.tree <- rescale(cleaned.cont$phy, model="OU", .8)
plot(ou.tree, cex = 0.5)
AIC.BM1 = BM1$opt$aic
AIC.OU1 = OU1$opt$aic
delta.AIC.BM1 = AIC.BM1 - AIC.OU1
delta.AIC.OU1 = AIC.OU1 - AIC.BM1
print(paste("BM1 has an AIC of", AIC.BM1, ", OU1 has an AIC of", AIC.OU1, "making the delta AIC between the two models", delta.AIC.OU1))
discrete = read.table(file= "photosynthetic_2.txt", stringsAsFactors = FALSE, row.names = 1, header = TRUE)
cleaned.discrete = CleanData(cleaned.cont$phy, discrete)
reconstruction.info = ace(cleaned.discrete$data, cleaned.discrete$phy, type="discrete", method="ML", CI=TRUE)
best.states = colnames(reconstruction.info$lik.anc)[apply(reconstruction.info$lik.anc, 1, which.max)]
species = row.names(cleaned.cont$data)
df = data.frame(species, cleaned.discrete$data[ ,1],cleaned.cont$data[ ,1])
cleaned.cont$phy$node.label <- best.states


nodeBased.OUMV = OUwie(cleaned.cont$phy, df ,model="OUMV", simmap.tree=FALSE, diagn=FALSE, ub = 1000000000)
print(nodeBased.OUMV)
models <- c("BM1","BMS","OU1","OUM","OUMV","OUMA","OUMVA")
results = lapply(models, RunSingleOUwieModel, phy=cleaned.cont$phy, data=df)
AICc.values =sapply(results, "[[", "AICc")
names(AICc.values) = models
AICc.values = AICc.values-min(AICc.values)
best = results[[which.min(AICc.values)]]


alpha.values <- seq(from=0.1, to = 0.8, length.out = 50)
likelihood.values <- rep(NA, length(alpha.values))
plot(x = alpha.values, y = likelihood.values, type = "l", bty = "n")
for (iteration in sequence(length(alpha.values))) {
     likelihood.values[iteration] <- OUwie.fixed(cleaned.cont$phy, df, model="BMS", alpha=rep(alpha.values[iteration],2), root.age = max(phytools::nodeHeights(cleaned.cont$phy)), sigma.sq=best$solution[2,], theta=best$theta[,1])$loglik }

for (iteration in sequence(length(alpha.values))) {
  likelihood.values[iteration] <- OUwie.fixed(cleaned.cont$phy, df, model="OU1", alpha=rep(alpha.values[iteration],2), root.age = max(phytools::nodeHeights(cleaned.cont$phy)), sigma.sq=best$solution[2,], theta=best$theta[,1])$loglik}

for (iteration in sequence(length(alpha.values))) {
  +     likelihood.values[iteration] <- OUwie.fixed(cleaned.cont$phy, df, model="OUMV", alpha=rep(alpha.values[iteration],2), root.age = max(phytools::nodeHeights(cleaned.cont$phy)), sigma.sq=best$solution[2,], theta=best$theta[,1])$loglik
  + }