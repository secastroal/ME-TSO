# This script runs the analysis of the ME-TSO model in a cluster.

# CONTENTS
# 0.0 Prepare environment
# 1.0 Run models

# 0.0 Prepare environment ----
# Load required packages
rm(list=ls())
library(parallel)
library(MplusAutomation)
library(MASS)

# 1.0 Run Models ----
cl <- makeCluster(2)

input_files <- list.files(paste0(getwd(), "/Mplus"), pattern = ".inp", full.names = TRUE)
mclapply(input_files, runModels, mc.cores = 2)

stopCluster(cl)

rm(list = ls())
