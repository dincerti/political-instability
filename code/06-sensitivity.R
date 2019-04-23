rm(list = ls())
load("output/regime-change-event-study.RData")
load("data/data-clean.RData")
library("data.table")
library("ggplot2")
library("xtable")
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
sign.dir <- c("negative", "negative", "positive", "positive")

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
tab.np[4, 2] <-  rank_test(rc.abs.es$ar.treat[, ], rc.abs.es$td)

# sign test p-value
for (i in 1:3){
  tab.np[i, 3] <- sign_test(rc.es$ar.treat[ed, indx[[i]]], sign.dir[i])
}
tab.np[4, 3] <-  sign_test(rc.abs.es$ar.treat[ed, ], sign.dir[4],
                           alternative = "greater")

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
tab.np[4, 5] <-  rank_test(rc.abs.es$ar.control[, ], rc.abs.es$td)

# sign test p-value
for (i in 1:3){
  tab.np[i, 6] <- sign_test(rc.es$ar.control[ed, indx[[i]]], sign.dir[i])
}
tab.np[4, 6] <- sign_test(rc.abs.es$ar.control[ed, ], sign.dir[4],
                           alternative = "greater")

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

# CAR SURROUNDING REGIME CHANGES: POSITIVE VS. NEGATIVE EVENTS -----------------
car_mean_plot_data <- function(rc_es, indices, label = NULL){
  car.mean <- mean_car_prepost(ar = rc_es$ar.treat[, indices],
                               sigma = rc.es$sigma.treat[indices], 
                               td = rc.es$td)
  car.mean[, lcar_mean := car_mean - qnorm(.975) * car_mean_se]
  car.mean[, ucar_mean := car_mean + qnorm(.975) * car_mean_se] 
  if (!is.null(label)){
    car.mean[, lab :=  label]    
  }
  return(car.mean[, ])
}

# Positive vs negative events

pos.events <- which(rc.es$ar.treat[ed, ] >= 0)
neg.events <- which(rc.es$ar.treat[ed, ] < 0)

# Create plot data
car.mean.pos <- car_mean_plot_data(rc.es, pos.events, "Positive event")
car.mean.neg <- car_mean_plot_data(rc.es, neg.events, "Negative event")
car.mean <- rbind(car.mean.neg, car.mean.pos)

# Plot
p.meancar <- ggplot(car.mean, aes(x = td, y = car_mean)) + geom_line() +
  geom_point(size = .8) + facet_wrap(~lab) +
  geom_ribbon(aes(ymin = lcar_mean, ymax = ucar_mean), alpha = 0.2) +
  xlab("Trading days") + ylab("Mean CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("figs/mean-car-pos-neg.pdf", p.meancar, height = 5, width = 7)

# CAR SURROUNDING REGIME CHANGES: BY EVENT TYPE --------------------------------
# Indices
coup.events <- which(regime.change$type == "Coup")
ass.events <- which(regime.change$type == "Assassination")
res.events <- which(regime.change$type == "Resignation")

# Create plot data
car.mean.coup <- car_mean_plot_data(rc.es, coup.events, "Coup")
car.mean.ass <- car_mean_plot_data(rc.es, ass.events, "Assassination")
car.mean.res <- car_mean_plot_data(rc.es, res.events, "Resignation")
car.mean <- rbind(car.mean.coup, car.mean.ass, car.mean.res)

# Plot
p.meancar <- ggplot(car.mean, aes(x = td, y = car_mean)) + geom_line() +
  geom_point(size = .8) + facet_wrap(~lab) +
  geom_ribbon(aes(ymin = lcar_mean, ymax = ucar_mean), alpha = 0.2) +
  xlab("Trading days") + ylab("Mean CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("figs/mean-car-by-regime-change-type.pdf", p.meancar, height = 5, width = 7)

# CAR SURROUNDING REGIME CHANGES: PLACEBO CHECKS --------------------------------
# Run placebo event studies
run_placebo_tests <- function(event_date_move){
  # Setup
  event.window <- 20
  est.window <- 200
  n.es <- nrow(regime.change)
  es.placebo <- vector(mode = "list", n.es)
  dtr.treat <- rep(NA, n.es)  
  
  # Run event studies
  for (i in 1:n.es){
    es.placebo[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                                   event_ticker = regime.change$ticker[i],
                                   event_window = event.window, estimation_window = est.window,
                                   event_date = regime.change$stock_date[i] + event_date_move, 
                                   model = "constant",
                                   control = FALSE, custom_v = NULL)  
  }
  rc.es.placebo <- combine_event_studies(es.placebo, event_country = regime.change$country,
                                         event_type = regime.change$type, 
                                         event_date = regime.change$stock_date + event_date_move)
  
  # Create plot data
  car.mean.coup.placebo <- car_mean_plot_data(rc.es.placebo, coup.events, "Coup")
  car.mean.ass.placebo <- car_mean_plot_data(rc.es.placebo, ass.events, "Assassination")
  car.mean.res.placebo <- car_mean_plot_data(rc.es.placebo, res.events, "Resignation")
  car.mean.placebo <- rbind(car.mean.coup.placebo, car.mean.ass.placebo, car.mean.res.placebo)
  
  # Return
  return(car.mean.placebo)
}

event.date.move <- c(seq(-20, 20, by = 5), seq(25 + 85, 365, 85))
car.mean.placebo <- vector(mode = "list", length = length(event.date.move))
names(car.mean.placebo) <- event.date.move
for (i in 1:length(event.date.move)){
  car.mean.placebo[[i]] <- run_placebo_tests(event.date.move[i])
  print(paste0("Completed time-shifted placebo with date shifted by ", 
               event.date.move[i],
                " days."))
}
tmp <- run_placebo_tests(365)


# Plot of CARs with event day shifted forward by 365 days
p.meancar.placebo <- ggplot(car.mean.placebo[["365"]], 
                            aes(x = td, y = car_mean)) + geom_line() +
  geom_point(size = .8) + facet_wrap(~lab) +
  geom_ribbon(aes(ymin = lcar_mean, ymax = ucar_mean), alpha = 0.2) +
  xlab("Trading days") + ylab("Mean CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2)
ggsave("figs/mean-car-by-regime-change-type-placebo.pdf", p.meancar.placebo, 
       height = 5, width = 7)

# Plot of event day ARs with event day shifted forward 
ar.mean.placebo <- rbindlist(lapply(car.mean.placebo, function(x) x[td == 0]))
ar.mean.placebo[, shift := rep(event.date.move, each = 3)]
p.meanar.placebo <- ggplot(ar.mean.placebo, 
                           aes(x = factor(shift), y = car_mean)) +
  geom_point(size = .8) + 
  geom_pointrange(aes(ymin = lcar_mean, ymax = ucar_mean), size = .3) +
  facet_wrap(~lab) +
  xlab("Number of days shifted from event date") + 
  ylab("Mean event day AR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2, color = "grey") +
  geom_vline(aes(xintercept = which(event.date.move == 0)), 
             linetype = 2, color = "grey") +
  theme_bw()
ggsave("figs/mean-ar-by-regime-change-type-placebo.pdf", p.meanar.placebo, 
       height = 5, width = 7)

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



