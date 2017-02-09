rm(list=ls())
source("code/func.R")
if (!file.exists("data/data-clean.RData")){
  source("code/01-clean.R")
}
source("code/02-desc_stats.R")
source("code/03-ar.R")