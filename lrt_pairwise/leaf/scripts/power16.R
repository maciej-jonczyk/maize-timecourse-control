# Compute adjacencies and topological overlap

# load required packages
rm(list = ls())
library(WGCNA)

# The following setting is important, do not omit.
options(stringsAsFactors = FALSE)
# Allow multi-threading within WGCNA.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
enableWGCNAThreads()

# Load data
load('leaf-before-adjacencies.RDa')

# calculate adjacencies for power 16
softPower = 16
# Initialize an appropriate array to hold the adjacencies
adjacencies = array(0, dim = c(nSets, nGenes, nGenes))
# Calculate adjacencies in each individual data set
for (set in 1:nSets)
  adjacencies[set, , ] = abs(cor(multiExpr[[set]]$data, use = "p"))^softPower

# Calculation of topological overlap

# Initialize an appropriate array to hold the TOMs
TOM = array(0, dim = c(nSets, nGenes, nGenes))
# Calculate TOMs in each individual data set
for (set in 1:nSets)
  TOM[set, , ] = TOMsimilarity(adjacencies[set, , ])

save(TOM, adjacencies, file = 'adj_TOM16.RDa')