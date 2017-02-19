rm(list=ls())
source("code/func.R")
if (!file.exists("data/data-clean.RData")){
  source("code/01-clean.R")
}
source("code/02-desc-stats.R")
source("code/03-event-studies.R")
source("code/04-ar.R")
source("code/05-volatility.R")
source("code/06-sensitivity.R")
source("code/07-protests.R")