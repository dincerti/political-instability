rm(list = ls())
library("xtable")
library("data.table")
library("ggplot2")
load("data/data-clean.RData")
theme_set(theme_bw())
source("code/func.R")

# EVENT STUDY ------------------------------------------------------------------
# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
sigma <- rep(NA, nrow(regime.change))
ar <- matrix(NA, nrow = 2 * event.window, ncol = nrow(regime.change))
car <- car.se <-  ar
dtr <- rep(NA, nrow(regime.change))
for (i in 1:ncol(ar)){
  tmp <-  event_study(stockdata = index[ticker == regime.change$ticker[i], 
                                       .(date, dr)],
                     event_window = event.window, estimation_window = est.window,
                     event_date = regime.change$stock_date[i], model = "constant")   
  sigma[i] <- tmp$sigma
  ar[, i] <- tmp$ar
  car[, i] <- tmp$car
  car.se[, i] <- tmp$car.se
  dtr[i] <- days_to_rebound(stockdata = index[ticker == regime.change$ticker[i], 
                                            .(date, p)],
                          event_date = regime.change$stock_date[i])
}
td.ew <- seq(-event.window, event.window -1)
regime.change.es <- list(td = td.ew, sigma = sigma, car = car, car.se = car.se) 
save(regime.change.es, file = "output/regime-change-event-study.RData")

# ABNORMAL RETURNS TABLES ------------------------------------------------------
# coups
coup.index <- which(regime.change$type == "Coup")
artable.coups <- ar_table(td = td.ew, ar = ar[, coup.index],
                          sigma = sigma[coup.index], dtr = dtr[coup.index],
                          country = regime.change[coup.index, country],
                          date = regime.change[coup.index, stock_date], 
                          coup = TRUE)
myprint.xtable(artable.coups$car, file = "tables/artable-coups-car.txt")
myprint.xtable(artable.coups$car.mean, file = "tables/artable-coups-car-mean.txt")

# assassinations
ass.index <- which(regime.change$type == "Assassination")
artable.ass <- ar_table(td = td.ew, ar = ar[, ass.index],
                        sigma = sigma[ass.index], dtr = dtr[ass.index],
                        country = regime.change[ass.index, country],
                        date = regime.change[ass.index, stock_date])
myprint.xtable(artable.ass$car, file = "tables/artable-ass-car.txt")
myprint.xtable(artable.ass$car.mean, file = "tables/artable-ass-car-mean.txt")

# resignations
res.index <- which(regime.change$type == "Resignation")
artable.res <- ar_table(td = td.ew, ar = ar[, res.index],
                        sigma = sigma[res.index], dtr = dtr[res.index],
                        country = regime.change[res.index, country],
                        date = regime.change[res.index, stock_date])
myprint.xtable(artable.res$car, file = "tables/artable-res-car.txt")
myprint.xtable(artable.res$car.mean, file = "tables/artable-res-car-mean.txt")

# VENEZUELA PARTIAL COUP -------------------------------------------------------
ven <- event[ticker == "_IBCD" & stock_date == "2002-04-12"]
ven.es <- event_study(stockdata = index[ticker == ven$ticker, .(date, dr)],
                     event_window = event.window, estimation_window = est.window,
                     event_date = ven$stock_date, model = "constant")  
ven.es <- data.table(td = ven.es$td,  ar = ven.es$ar, ar.se = ven.es$sigma)
ven.es[, lar := ar - qnorm(.975) * ar.se]
ven.es[, uar := ar + qnorm(.975) * ar.se]
p <- ggplot(ven.es[abs(td) <= 10], aes(x = td, y = ar)) + 
  geom_pointrange(aes(ymin = lar, ymax = uar), size = .2) +
  xlab("Days") + ylab("AR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("figs/venezuela_coup_attempt_2002.pdf", p, height = 5, width = 7)