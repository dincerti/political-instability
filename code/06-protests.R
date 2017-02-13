rm(list = ls())
load("data/data-clean.RData")
source("code/func.R")
library("plm")
library("lmtest")
library("broom")

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
revfit <- list(revfit1, revfit2, revfit3)
nfits <- length(revfit)

# table of results
vars <- c("event", "emerg_abs_dr")
varsl <- c("Protest", "Emerging market index")
lookup <- data.frame(term = vars, varname = varsl, id = seq(1, length(vars)))
revfit.table <- reg_table(models = revfit, lookup = lookup)
n <- nrow(revfit.table)
fe.lab <- rep(paste0("\\multicolumn{1}{c}{", "Yes}"), nfits)
fe.lab <- c("Event fixed effect?", fe.lab)
revfit.table <- rbind(revfit.table[1:(n-1), ], fe.lab, revfit.table[n, ])
events <- rep(NA, nfits)
for (i in 1:length(events)){
  events[i] <- pdim(revfit[[i]]$model)$nT$n
}
events <- paste0("\\multicolumn{1}{c}{", events, "}")
revfit.table <- rbind(revfit.table, c("Events", events))
myprint.xtable(revfit.table, file = "tables/protest-regtable.txt")

