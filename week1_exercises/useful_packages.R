#Title: Rscript_test
#Author: Alex Truchon
#Date: 1/22/21
#Description: This file contains basic packages for R usage.

install.packages("ape")
install.packages("ctv")
library("ctv")

install.views(c("Phylogenetics", "WebTechnologies"))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")
BiocManager::install("Biostrings", ask=FALSE)