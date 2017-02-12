rm(list = ls())
load("data/data-clean.RData")
source("code/func.R")
library("plm")

# EFFECT OF PUBLIC PROTESTS ON STOCK DATA --------------------------------------
# data 
index.rev <- vector(mode = "list", length(rev$ticker))
for (i in 1:length(rev$ticker)){
  index.rev[[i]] <- index[ticker == rev$ticker[i] & date >= (rev$start_date[i] - 250)
                          & date <= (rev$end_date[i] + 250)]
  index.rev[[i]][, rev_no := i]
  index.rev[[i]][, event := ifelse(date >= rev$start_date[i] & 
                                     date <= rev$end_date[i], 1, 0)]
}
index.rev <- rbindlist(index.rev)
index.rev <- pdata.frame(index.rev, index = c("rev_no", "date"))

# regressions
revfit1 <- plm(dr ~ event, data = index.rev, model = "within")
revfit2 <- plm(abs_dr ~ event, data = index.rev, model = "within")
revfit3 <- plm(abs_dr ~ event + emerg_abs_dr, data = index.rev, model = "within")

# table of results
coef <- 2
#coeftest(model.plm, vcov=vcovHC(model.plm,type="HC0",cluster="group"))
