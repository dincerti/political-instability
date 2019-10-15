rm(list = ls())
library("xtable")
library("data.table")
library("ggplot2")
library("scales")
load("data/data-clean.RData")
load("output/regime-change-event-study.RData")
theme_set(theme_bw())
source("code/func.R")

# ABNORMAL RETURNS TABLES ------------------------------------------------------
# coups
coup.index <- which(regime.change$type == "Coup")
artable.coups <- ar_table(td = rc.es$td, ar = rc.es$ar.treat[, coup.index],
                          sigma = rc.es$sigma.treat[coup.index], 
                          dtr = rc.es$dtr.treat[coup.index],
                          country = regime.change[coup.index, country],
                          date = regime.change[coup.index, stock_date], 
                          coup = FALSE)
myprint.xtable(artable.coups$car, file = "tables/artable-coups-car.txt")
myprint.xtable(artable.coups$car.mean, file = "tables/artable-coups-car-mean.txt")

# assassinations
ass.index <- which(regime.change$type == "Assassination")
artable.ass <- ar_table(td = rc.es$td, ar = rc.es$ar.treat[, ass.index],
                        sigma = rc.es$sigma.treat[ass.index], 
                        dtr = rc.es$dtr.treat[ass.index],
                        country = regime.change[ass.index, country],
                        date = regime.change[ass.index, stock_date])
myprint.xtable(artable.ass$car, file = "tables/artable-ass-car.txt")
myprint.xtable(artable.ass$car.mean, file = "tables/artable-ass-car-mean.txt")

# resignations
res.index <- which(regime.change$type == "Resignation")
artable.res <- ar_table(td = rc.es$td, ar = rc.es$ar.treat[, res.index],
                        sigma = rc.es$sigma.treat[res.index], 
                        dtr = rc.es$dtr.treat[res.index],
                        country = regime.change[res.index, country],
                        date = regime.change[res.index, stock_date])
myprint.xtable(artable.res$car, file = "tables/artable-res-car.txt")
myprint.xtable(artable.res$car.mean, file = "tables/artable-res-car-mean.txt")

# VENEZUELA PARTIAL COUP -------------------------------------------------------
ven <- event[ticker == "_IBCD" & stock_date == "2002-04-12"]
ven.es <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                      event_ticker = ven$ticker,
                     event_window = 20, estimation_window = 200,
                     event_date = ven$stock_date, model = "constant", control = FALSE)  

ven.es$ar.treat[, lar := ar - qnorm(.975) * ven.es$sigma.treat]
ven.es$ar.treat[, uar := ar + qnorm(.975) * ven.es$sigma.treat]

p <- ggplot(ven.es$ar.treat[abs(td) <= 10], aes(x = td, y = ar)) + 
  geom_hline(aes(yintercept = 0), linetype = 2, color = "grey") +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "grey") +
  geom_pointrange(aes(ymin = lar, ymax = uar), size = .3) +
  xlab("Trading days") + 
  ylab("Abnormal Returns (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_classic()
print(p)

ggsave("figs/venezuela_coup_attempt_2002.pdf", p, height = 5, width = 7)

# VENEZUELA 1992 COUP ATTEMPT --------------------------------------------------
ven92 <- event[ticker == "_VE1" & stock_date == "1992-11-27"]
ven92.es <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                      event_ticker = ven92$ticker,
                      event_window = 20, estimation_window = 200,
                      event_date = ven92$stock_date, model = "constant", control = FALSE)  

ven92.es$ar.treat[, lar := ar - qnorm(.975) * ven92.es$sigma.treat]
ven92.es$ar.treat[, uar := ar + qnorm(.975) * ven92.es$sigma.treat]

p <- ggplot(ven92.es$ar.treat[abs(td) <= 10], aes(x = td, y = ar)) + 
  geom_hline(aes(yintercept = 0), linetype = 2, color = "grey") +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "grey") +
  geom_pointrange(aes(ymin = lar, ymax = uar), size = .3) +
  xlab("Trading days") + 
  ylab("Abnormal Returns (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(-12, 12),
                     breaks = scales::pretty_breaks(n = 10)) +
  theme_classic()
print(p)

ggsave("figs/venezuela_coup_attempt_1992.pdf", p, height = 5, width = 7)


# TURKEY 2016 COUP ATTEMPT -----------------------------------------------------

# Add 2016 Turkey failed coup to data.table
event = rbind(event,list(85, "Turkey", "_XU100D", "07/15/2016", 
                 as.Date("07/18/2016", "%m/%d/%Y"),
                 "Recep Tayyip Erdoğan", "Failed Coup", 
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))


turk16 <- event[ticker == "_XU100D" & stock_date == "2016-07-18"]
turk16.es <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                      event_ticker = turk16$ticker,
                      event_window = 20, estimation_window = 200,
                      event_date = turk16$stock_date, model = "constant", control = FALSE)  

turk16.es$ar.treat[, lar := ar - qnorm(.975) * turk16.es$sigma.treat]
turk16.es$ar.treat[, uar := ar + qnorm(.975) * turk16.es$sigma.treat]

p <- ggplot(turk16.es$ar.treat[abs(td) <= 10], aes(x = td, y = ar)) + 
  geom_hline(aes(yintercept = 0), linetype = 2, color = "grey") +
  geom_vline(aes(xintercept = 0), linetype = 2, color = "grey") +
  geom_pointrange(aes(ymin = lar, ymax = uar), size = .3) +
  xlab("Trading days") + 
  ylab("Abnormal Returns (%)") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(limits = c(-12, 12),
                     breaks = scales::pretty_breaks(n = 10)) +
  theme_classic()
print(p)

ggsave("figs/turkey_coup_attempt_2016.pdf", p, height = 5, width = 7)

