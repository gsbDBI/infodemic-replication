
library(reticulate)
use_python('/opt/anaconda3/bin') # set appropriate python

np <- import("numpy")
probs_array <- np$load("../../data/contextual_probabilities.npy")
saveRDS(probs_array, '../objects/contextual_probabilities.RDS')