 packages <- c("data.table", "ggplot2", "plm", "rugarch", "sandwich", "lmtest", 
              "Synth", "foreign", "reshape2", "chron", "xtable")
lapply(packages, library, character.only = TRUE)
theme_set(theme_bw())

rm(list=ls())
source("code/func.R")
source("code/01-clean.R")
source("code/02-desc_stats.R")
source("code/do.R")