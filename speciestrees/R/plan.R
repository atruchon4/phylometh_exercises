plan <- drake_plan(
  phy = get_study_tree("ot_485", "tree1"),
  #plot(phy, cex=0.3),
  phy1 = drop.random(phy, Ntip(phy) - 10),
  plot(phy1),
  axisPhylo(),
  gene.tree = sim.coaltree.phylo(phy1, pop.size=1e-12),
  plot(gene.tree),
 plot(cophylo(phy1, gene.tree, cbind(sort(phy1$tip.label), sort(gene.tree$tip.label)))),
 species.tree = rcoal(7),
 species.tree$edge.length <- species.tree$edge.length / (10*max(branching.times(species.tree))),
 gene.tree.1 = sim.coaltree.phylo(species.tree),
 plot(cophylo(species.tree, gene.tree.1, cbind(sort(species.tree$tip.label), sort(gene.tree.1$tip.label)))),
 tip.rows = which(species.tree$edge[,2]<=Ntip(species.tree)),
 species.tree2 = species.tree,
 species.tree2$edge.length[tip.rows] <- 100 + species.tree2$edge.length[tip.rows],
 gene.tree2 = sim.coaltree.phylo(species.tree2),
 plot(cophylo(species.tree2, gene.tree2, cbind(sort(species.tree2$tip.label), sort(gene.tree2$tip.label)))),
 species.tree2.clado = compute.brlen(species.tree2),
 gene.tree2.clado = compute.brlen(gene.tree2),
 plot(cophylo(species.tree2.clado, gene.tree2.clado, cbind(sort(species.tree2.clado$tip.label),
                                                           sort(gene.tree2.clado$tip.label))))
)

