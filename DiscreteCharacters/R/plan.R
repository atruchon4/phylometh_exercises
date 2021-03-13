plan <- drake_plan(
  tree = read.tree("dinoflag.edited.tree"),
  data = read.table(file= "photosynthetic_1.txt", stringsAsFactors = FALSE, row.names = 1, header = TRUE),
  cleaned.discrete = CleanData(tree, data),
  cleaned.discrete.phyDat = phangorn::phyDat(cleaned.discrete$data, type = "USER", levels = c("Yes", "No")),
  anc.p = phangorn::ancestral.pars(cleaned.discrete$phy, cleaned.discrete.phyDat),
  anc.ml = ancestral.pml(pml(cleaned.discrete$phy, cleaned.discrete.phyDat), type = "ml"),
  tree_print = plot_tree(cleaned.discrete$phy, file=file_out("results/trees.pdf"), anc.p, anc.ml),
  cleaned.data = cleaned.discrete$data,
  names = rownames(cleaned.data),
  rownames(cleaned.data) <- NULL,
  cleaned.data.1 = cbind(names, cleaned.data),
  p = corHMM(phy = cleaned.discrete$phy, data = cleaned.data.1, rate.cat = 1)
)