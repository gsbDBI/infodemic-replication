

library(reticulate)
# install_miniconda() may need to install miniconda, or otherwise set python version
options(reticulate.conda_binary = miniconda_path())

np <- import('numpy')
probs_array <- np$load('../../data/contextual_probabilities.npy')
saveRDS(probs_array, '../objects/contextual_probabilities.RDS')
