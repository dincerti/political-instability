rm(list = ls())
load("output/regime-change-event-study.RData")
load("data/data-clean.RData")
library("data.table")
library("ggplot2")
source("code/func.R")
theme_set(theme_minimal())

# NON-PARAMETRIC TESTS ---------------------------------------------------------
ed <- which(rc.es$td == 0)
tab.np <- matrix(NA, nrow = 4, ncol = 7)
colnames(tab.np) <- c("mean_ar_treat", "rank_pval_treat", "sign_pval_treat",
                      "mean_ar_control", "rank_pval_control", "sign_pval_control",
                      "wilcoxon_rank_pval")
indx <- list(coup.index = which(regime.change$type == "Coup"),
             ass.index = which(regime.change$type == "Assassination"),
             res.index = which(regime.change$type == "Resignation"))
sign.dir <- c("negative", "negative", "positive")

## Treatment
# mean abnormal return
for (i in 1:3){
  tab.np[i, 1] <- mean(rc.es$ar.treat[ed, indx[[i]]])
}
tab.np[4, 1] <-  mean(rc.abs.es$ar.treat[ed, ])

# rank test p-value
for (i in 1:3){
  tab.np[i, 2] <- rank_test(rc.es$ar.treat[, indx[[i]]], rc.es$td)
}
tab.np[4, 2] <-  rank_test(rc.abs.es$ar.treat[, indx[[i]]], rc.abs.es$td)

# sign test p-value
for (i in 1:3){
  tab.np[i, 3] <- sign_test(rc.es$ar.treat[ed, indx[[i]]], sign.dir[i])
}
tab.np[4, 3] <-  sign_test(rc.abs.es$ar.treat[ed, indx[[i]]], sign.dir[i])

## Control
# mean abnormal return
for (i in 1:3){
  tab.np[i, 4] <- mean(rc.es$ar.control[ed, indx[[i]]], na.rm = TRUE)
}
tab.np[4, 4] <-  mean(rc.abs.es$ar.control[ed, ], na.rm = TRUE)

# rank test p-value
for (i in 1:3){
  tab.np[i, 5] <- rank_test(rc.es$ar.control[, indx[[i]]] , rc.es$td)
}
tab.np[4, 5] <-  rank_test(rc.abs.es$ar.control[, indx[[i]]], rc.abs.es$td)

# sign test p-value
for (i in 1:3){
  tab.np[i, 6] <- sign_test(rc.es$ar.control[ed, indx[[i]]], sign.dir[i])
}
tab.np[4, 6] <-  sign_test(rc.abs.es$ar.control[ed, indx[[i]]], sign.dir[i])

## Wilcoxon rank test
for (i in 1:3){
  wilcox.ar <- wilcox.test(x = rc.es$ar.treat[ed, indx[[i]]],
                             y = rc.es$ar.control[ed, indx[[i]]],
                            paired = TRUE)
  tab.np[i, 7] <- wilcox.ar$p.value
}
wilcox.ar <- wilcox.test(x = rc.abs.es$ar.treat[ed, ],
                         y = rc.abs.es$ar.control[ed, ],
                         paired = TRUE)
tab.np[4, 7] <- wilcox.ar$p.value
tab.np <- formatC(tab.np, format = "f", digits = 3)
event.type <- c("Coups", "Assassinations", "Resignations", "All (Absolute Value)")
tab.np <- cbind(event.type, tab.np)
myprint.xtable(tab.np, file = "tables/non-parametric-ar0.txt")


# CAR SURROUNDING REGIME CHANGES -----------------------------------------------
pos.events <- which(rc.es$ar.treat[ed, ] >= 0)
neg.events <- which(rc.es$ar.treat[ed, ] < 0)

# positive events
car.mean.pos <- mean_car_prepost(ar = rc.es$ar.treat[, pos.events],
                         sigma = rc.es$sigma.treat[pos.events], 
                         td = rc.es$td)
car.mean.pos[, lcar_mean := car_mean - qnorm(.975) * car_mean_se]
car.mean.pos[, ucar_mean := car_mean + qnorm(.975) * car_mean_se]
car.mean.pos[, lab := "Positive event"]

# negative events
car.mean.neg <- mean_car_prepost(ar = rc.es$ar.treat[, neg.events],
                                 sigma = rc.es$sigma.treat[neg.events], 
                                 td = rc.es$td)
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

# DAILY RETURN PLOTS: CONTROL VS TREATMENT -------------------------------------
# daily return by event
pdf("figs/dr-ts-by-event.pdf", onefile = TRUE)
for (i in 1:nrow(regime.change)){
  name <- paste0(regime.change$country[i], " (", 
                 regime.change$stock_date[i], ")")
  p <- ggplot(data = rc.es$dr[event_date == regime.change$stock_date[i]], 
              aes(x = td, y = dr, col = lab)) +
    geom_line() + geom_point(size = .5) + geom_vline(xintercept = 0, linetype = 2) +
    xlab("Trading day") + ylab("Daily return") + labs(title = name) +
    scale_colour_discrete("") + theme(plot.title = element_text(hjust = 0.5),
                                      legend.position = "bottom")
  print(p)
  print(i)
}
dev.off()

# car by event
pdf("figs/car-ts-by-event.pdf", onefile = TRUE)
for (i in 1:nrow(regime.change)){
  name <- paste0(regime.change$country[i], " (", 
                 regime.change$stock_date[i], ")")
  tmp <- rbind(data.table(car = rc.es$car.treat[, i], td = rc.es$td, lab = "Treatment"),
               data.table(car = rc.es$car.control[, i], td = rc.es$td, lab = "Control"))
  p <- ggplot(data = tmp, 
              aes(x = td, y = car, col = lab)) +
    geom_line() + geom_point(size = .5) + geom_vline(xintercept = 0, linetype = 2) +
    xlab("Trading day") + ylab("CAR") + labs(title = name) +
    scale_colour_discrete("") + theme(plot.title = element_text(hjust = 0.5),
                                      legend.position = "bottom")
  print(p)
  print(i)
}
dev.off()



