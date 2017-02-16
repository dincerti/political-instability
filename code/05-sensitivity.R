rm(list = ls())
load("output/regime-change-event-study.RData")
load("data/data-clean.RData")
library("data.table")
library("ggplot2")
source("code/func.R")
theme_set(theme_bw())

# NON-PARAMETRIC TESTS ---------------------------------------------------------
ed <- which(regime.change.es$td == 0)

# coups 
coup.index <- which(regime.change$type == "Coup")
ar.mean.coup <- mean(regime.change.es$ar[ed, coup.index])
ranktest.coup <- rank_test(regime.change.es$ar[, coup.index], 
                              regime.change.es$td)

# assassinations
ass.index <- which(regime.change$type == "Assassination")
ar.mean.ass <- mean(regime.change.es$ar[ed, ass.index])
ranktest.ass <- rank_test(regime.change.es$ar[, ass.index], 
                           regime.change.es$td)

# resignations
res.index <- which(regime.change$type == "Resignation")
ar.mean.res <- mean(regime.change.es$ar[ed, res.index])
ranktest.res <- rank_test(regime.change.es$ar[, res.index], 
                          regime.change.es$td)

# CAR SURROUNDING REGIME CHANGES -----------------------------------------------
td0.indx <- which(regime.change.es$td == 0)
pos.events <- which(regime.change.es$ar[td0.indx, ] >= 0)
neg.events <- which(regime.change.es$ar[td0.indx, ] < 0)

# positive events
car.mean.pos <- mean_car_prepost(ar = regime.change.es$ar[, pos.events],
                         sigma = regime.change.es$sigma[pos.events], 
                         td = regime.change.es$td)
car.mean.pos[, lcar_mean := car_mean - qnorm(.975) * car_mean_se]
car.mean.pos[, ucar_mean := car_mean + qnorm(.975) * car_mean_se]
car.mean.pos[, lab := "Positive event"]

# negative events
car.mean.neg <- mean_car_prepost(ar = regime.change.es$ar[, neg.events],
                                 sigma = regime.change.es$sigma[neg.events], 
                                 td = regime.change.es$td)
car.mean.neg[, lcar_mean := car_mean - qnorm(.975) * car_mean_se]
car.mean.neg[, ucar_mean := car_mean + qnorm(.975) * car_mean_se]
car.mean.neg[, lab := "Negative event"]

# plot
car.mean <- rbind(car.mean.neg, car.mean.pos)
p.meancar <- ggplot(car.mean, aes(x = td, y = car_mean)) + geom_line() +
  geom_point(size = .8) + facet_wrap(~lab) +
  geom_ribbon(aes(ymin = lcar_mean, ymax = ucar_mean), alpha = 0.2) +
  xlab("Trading days") + ylab("Mean CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("figs/mean-car-pos-neg.pdf", p.meancar, height = 5, width = 7)
