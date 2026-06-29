# Compute adjacencies and topological overlap

# run with
# OMP_NUM_THREADS=23 MKL_NUM_THREADS=23 OPENBLAS_NUM_THREADS=23 R CMD BATCH TOMs_powers.R TOMs_powers.Rout

# load required packages

library(WGCNA)

# The following setting is important, do not omit.
options(stringsAsFactors = FALSE)
# Allow multi-threading within WGCNA.
# Caution: skip this line if you run RStudio or other third-party R environments.
# See note above.
nThreads <- enableWGCNAThreads()
message("WGCNA threads:", nThreads)
# opcjonalnie wymuś liczby wątków BLAS/OpenMP:
if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
  RhpcBLASctl::blas_set_num_threads(23)
  RhpcBLASctl::omp_set_num_threads(23)
}
message("OMP_NUM_THREADS=", Sys.getenv("OMP_NUM_THREADS"))

# calculate adjacencies for selected powers

for (softPower in 12:13){
  
  # clean environment and load data
  load('../rdata-saved/wgcna/leaf-before-adjacencies.RDa')
  
  # out file name
  out_file_name = paste0('../rdata-saved/wgcna/power', softPower, '.RDa')
  
  # Network construction starts by calculating the adjacencies in the individual sets, using the choosen soft thresholding power:
  
  # Initialize an appropriate array to hold the adjacencies
  adjacencies = array(0, dim = c(nSets, nGenes, nGenes))
  # Calculate adjacencies in each individual data set
  for (set in 1:nSets){
    adjacencies[set, , ] = adjacency(multiExpr[[set]]$data, 
                                     type = 'signed hybrid', 
                                     power = softPower, 
                                     corFnc = 'bicor',
                                     corOptions = list(maxPOutliers = 0.05))
                                     }
  
  # We now turn the adjacencies into Topological Overlap Matrix (TOM) 
  # [@doi:10.1126/science.1073374, @zhangGeneralFrameworkWeighted2005]
  # Calculation of topological overlap
  
  # Initialize an appropriate array to hold the TOMs
  TOM = array(0, dim = c(nSets, nGenes, nGenes))
  # Calculate TOMs in each individual data set
  for (set in 1:nSets){
    TOM[set, , ] = TOMsimilarity(adjacencies[set, , ])
    }
  
  save(TOM, adjacencies, file = out_file_name)
  
  rm(TOM, adjacencies)
  gc()
}
