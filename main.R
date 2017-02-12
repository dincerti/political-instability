rm(list=ls())
source("code/func.R")
if (!file.exists("data/data-clean.RData")){
  source("code/01-clean.R")
}
source("code/02-desc-stats.R")
source("code/03-ar.R")
source("code/04-volatility.R")
source("code/05-sensitivity.R")
source("code/06-protests.R")