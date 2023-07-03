rm(list = ls())
load("data/data-clean.RData")
source("code/func.R")
library("data.table")
library("plm")
library("lmtest")
library("broom")
library(ggplot2)
library(rugarch)
theme_set(theme_minimal())

# EGYPTIAN REVOLUTION ----------------------------------------------------------
# Model estimation
egypt.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = "_EFGID",
                    event_window = 20, estimation_window = 200,
                    event_date = rev[name == "Egyptian Revolution", start_date],
                    model = "constant", control = FALSE)   
egypt.es <- c(egypt.es, car_prepost(egypt.es$ar.treat$td, egypt.es$ar.treat$ar,
                                    egypt.es$sigma.treat))
egypt.es <- data.table(date = egypt.es$ar.treat$date, td = egypt.es$ar.treat$td,  
                       car = egypt.es$car, car.se = egypt.es$car.se)
egypt.es[, lcar := car - qnorm(.975) * car.se]
egypt.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(egypt.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("02/10/2011", "%m/%d/%Y"), 
           y = 5, label = "Jan 25: uprising begins", size = 3) +
  geom_segment(aes(x = as.Date("02/07/2011", "%m/%d/%Y"), y = 4, 
                   xend = as.Date("01/25/2011", "%m/%d/%Y") , 
                   yend = 0),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("02/25/2011", "%m/%d/%Y"), 
           y = -14, label = "Feb 11: Mubarak resigns", size = 3) +
  geom_segment(aes(x = as.Date("02/25/2011", "%m/%d/%Y"), y = -15, 
                   xend = as.Date("02/11/2011", "%m/%d/%Y") , 
                   yend = -20),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("03/15/2011", "%m/%d/%Y"), 
           y = -36, label = "Mar 23: stock market reopens", size = 3) +
  geom_segment(aes(x = as.Date("03/15/2011", "%m/%d/%Y"), y = -35, 
                   xend = as.Date("2011-03-23"), 
                   yend = 1.01 * egypt.es[date == as.Date("2011-03-23"), car]),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/egypt-revolution-2011.pdf", p, height = 5, width = 7)

# INDONESIAN UPRISING ----------------------------------------------------------
# Model estimation
indonesia.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = "_JKSED",
                         event_window = 20, estimation_window = 200,
                         event_date = rev[name == "Indonesian Revolution", start_date],
                         model = "constant", control = FALSE)   
indonesia.es <- c(indonesia.es, car_prepost(indonesia.es$ar.treat$td, indonesia.es$ar.treat$ar,
                                            indonesia.es$sigma.treat))
indonesia.es <- data.table(date = indonesia.es$ar.treat$date, td = indonesia.es$ar.treat$td,  
                       car = indonesia.es$car, car.se = indonesia.es$car.se)
indonesia.es[, lcar := car - qnorm(.975) * car.se]
indonesia.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(indonesia.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("05/18/1998", "%m/%d/%Y"), 
           y = 10, label = "May 12: uprising begins", size = 3) +
  geom_segment(aes(x = as.Date("05/12/1998", "%m/%d/%Y"), y = 8, 
                   xend = as.Date("05/09/1998", "%m/%d/%Y") , 
                   yend = 0.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("05/25/1998", "%m/%d/%Y"), 
           y = -20, label = "May 20: Suharto resigns", size = 3) +
  geom_segment(aes(x = as.Date("05/25/1998", "%m/%d/%Y"), y = -18, 
                   xend = as.Date("05/21/1998", "%m/%d/%Y") , 
                   yend = -1),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/indonesia-1998.pdf", p, height = 5, width = 7)

# NEPAL UPRISING ----------------------------------------------------------
# Model estimation
nepal.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                             event_ticker = "_NEPSED",
                             event_window = 20, estimation_window = 200,
                             event_date = rev[name == "2006 Democracy Movement", start_date],
                             model = "constant", control = FALSE)   
nepal.es <- c(nepal.es, car_prepost(nepal.es$ar.treat$td, nepal.es$ar.treat$ar,
                                        nepal.es$sigma.treat))
nepal.es <- data.table(date = nepal.es$ar.treat$date, td = nepal.es$ar.treat$td,  
                           car = nepal.es$car, car.se = nepal.es$car.se)
nepal.es[, lcar := car - qnorm(.975) * car.se]
nepal.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(nepal.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("03/25/2006", "%m/%d/%Y"), 
           y = 6, label = "April 4: Democracy movement begins", size = 3) +
  geom_segment(aes(x = as.Date("03/25/2006", "%m/%d/%Y"), y = 5, 
                   xend = as.Date("04/04/2006", "%m/%d/%Y") , 
                   yend = 0.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("04/25/2006", "%m/%d/%Y"), 
           y = -7, label = "April 25: Reinstatement of Parliament", size = 3) +
  geom_segment(aes(x = as.Date("04/30/2006", "%m/%d/%Y"), y = -6, 
                   xend = as.Date("04/25/2006", "%m/%d/%Y") , 
                   yend = -1.5),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/nepal-2006.pdf", p, height = 5, width = 7)

# BANGLADESH UPRISING ----------------------------------------------------------
# Model estimation
bangla.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = "BDDSEXD",
                         event_window = 20, estimation_window = 200,
                         event_date = rev[name == "Bangladeshi Spring 1990", start_date],
                         model = "constant", control = FALSE)   
bangla.es <- c(bangla.es, car_prepost(bangla.es$ar.treat$td, bangla.es$ar.treat$ar,
                                      bangla.es$sigma.treat))
bangla.es <- data.table(date = bangla.es$ar.treat$date, td = bangla.es$ar.treat$td,  
                       car = bangla.es$car, car.se = bangla.es$car.se)
bangla.es[, lcar := car - qnorm(.975) * car.se]
bangla.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(bangla.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("11/23/1990", "%m/%d/%Y"), 
           y = -5.5, label = "November 27: State of emergency begins", size = 3) +
  geom_segment(aes(x = as.Date("11/23/1990", "%m/%d/%Y"), y = -5, 
                   xend = as.Date("11/27/1990", "%m/%d/%Y") , 
                   yend = -1.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("12/20/1990", "%m/%d/%Y"), 
           y = 6, label = "December 7: Ershad resigns", size = 3) +
  geom_segment(aes(x = as.Date("12/20/1990", "%m/%d/%Y"), y = 5, 
                   xend = as.Date("12/07/1990", "%m/%d/%Y") , 
                   yend = 1),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/bangla-1990.pdf", p, height = 5, width = 7)

# ECUADOR 2005 ----------------------------------------------------------
# Model estimation
ecuador.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                          event_ticker = "_BVGD",
                          event_window = 20, estimation_window = 200,
                          event_date = rev[name == "Ecuadorian Protests", start_date],
                          model = "constant", control = FALSE)   
ecuador.es <- c(ecuador.es, car_prepost(ecuador.es$ar.treat$td, ecuador.es$ar.treat$ar,
                                        ecuador.es$sigma.treat))
ecuador.es <- data.table(date = ecuador.es$ar.treat$date, td = ecuador.es$ar.treat$td,  
                        car = ecuador.es$car, car.se = ecuador.es$car.se)
ecuador.es[, lcar := car - qnorm(.975) * car.se]
ecuador.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(ecuador.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("04/13/2005", "%m/%d/%Y"), 
           y = 5, label = "April 13: Protests begin", size = 3) +
  geom_segment(aes(x = as.Date("04/13/2005", "%m/%d/%Y"), y = 4.5, 
                   xend = as.Date("04/13/2005", "%m/%d/%Y") , 
                   yend = 0.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("04/20/2005", "%m/%d/%Y"), 
           y = -8, label = "April 20: Gutiérrez resigns", size = 3) +
  geom_segment(aes(x = as.Date("04/20/2005", "%m/%d/%Y"), y = -7, 
                   xend = as.Date("04/20/2005", "%m/%d/%Y") , 
                   yend = -2.5),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/ecuador-2005.pdf", p, height = 5, width = 7)

# PHILIPPINES EDSA 2 -----------------------------------------------------------
# Model estimation
phil2.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                           event_ticker = "_PSID",
                           event_window = 20, estimation_window = 200,
                           event_date = rev[name == "EDSA II", start_date],
                           model = "constant", control = FALSE)   
phil2.es <- c(phil2.es, car_prepost(phil2.es$ar.treat$td, phil2.es$ar.treat$ar,
                                    phil2.es$sigma.treat))
phil2.es <- data.table(date = phil2.es$ar.treat$date, td = phil2.es$ar.treat$td,  
                         car = phil2.es$car, car.se = phil2.es$car.se)
phil2.es[, lcar := car - qnorm(.975) * car.se]
phil2.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(phil2.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("01/17/2001", "%m/%d/%Y"), 
           y = -10, label = "Jan 17: Protests begin", size = 3) +
  geom_segment(aes(x = as.Date("01/17/2001", "%m/%d/%Y"), y = -9, 
                   xend = as.Date("01/17/2001", "%m/%d/%Y") , 
                   yend = -6.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("01/27/2001", "%m/%d/%Y"), 
           y = -8.5, label = "January 19: Estrada resigns", size = 3) +
  geom_segment(aes(x = as.Date("01/19/2001", "%m/%d/%Y"), y = -8, 
                   xend = as.Date("01/19/2001", "%m/%d/%Y") , 
                   yend = -6),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("01/20/2001", "%m/%d/%Y"), 
           y = 15, label = "January 20: Gloria Macapagal Arroyo becomes president", size = 3) +
  geom_segment(aes(x = as.Date("01/20/2001", "%m/%d/%Y"), y = 14.5, 
                   xend = as.Date("01/20/2001", "%m/%d/%Y") , 
                   yend = 5),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/phil-2001.pdf", p, height = 5, width = 7)

# ARGENTINA 2001 -----------------------------------------------------------
# Model estimation
arg.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = "_IBGD",
                         event_window = 20, estimation_window = 200,
                         event_date = rev[name == "Argentina Riots", start_date],
                         model = "constant", control = FALSE)   
arg.es <- c(arg.es, car_prepost(arg.es$ar.treat$td, arg.es$ar.treat$ar,
                                  arg.es$sigma.treat))
arg.es <- data.table(date = arg.es$ar.treat$date, td = arg.es$ar.treat$td,  
                       car = arg.es$car, car.se = arg.es$car.se)
arg.es[, lcar := car - qnorm(.975) * car.se]
arg.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(arg.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("12/10/2001", "%m/%d/%Y"), 
           y = 30, label = "Dec 16: Riots begin", size = 3) +
  geom_segment(aes(x = as.Date("12/16/2001", "%m/%d/%Y"), y = 28, 
                   xend = as.Date("12/16/2001", "%m/%d/%Y") , 
                   yend = 5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("12/20/2001", "%m/%d/%Y"), 
           y = 40, label = "Dec 20: De la Rúa resigns", size = 3) +
  geom_segment(aes(x = as.Date("12/20/2001", "%m/%d/%Y"), y = 38, 
                   xend = as.Date("12/20/2001", "%m/%d/%Y") , 
                   yend = 23),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/argentina-2001.pdf", p, height = 5, width = 7)

# UKRAINE 2004 -----------------------------------------------------------
# Model estimation
ukraine.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                       event_ticker = "_PFTSID",
                       event_window = 20, estimation_window = 200,
                       event_date = rev[name == "Orange Revolution", start_date],
                       model = "constant", control = FALSE)   
ukraine.es <- c(ukraine.es, car_prepost(ukraine.es$ar.treat$td, ukraine.es$ar.treat$ar,
                                    ukraine.es$sigma.treat))
ukraine.es <- data.table(date = ukraine.es$ar.treat$date, td = ukraine.es$ar.treat$td,  
                     car = ukraine.es$car, car.se = ukraine.es$car.se)
ukraine.es[, lcar := car - qnorm(.975) * car.se]
ukraine.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(ukraine.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("11/20/2004", "%m/%d/%Y"), 
           y = 10, label = "November 22: Protests begin", size = 3) +
  geom_segment(aes(x = as.Date("11/22/2004", "%m/%d/%Y"), y = 9, 
                   xend = as.Date("11/22/2004", "%m/%d/%Y") , 
                   yend = 0.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("12/10/2004", "%m/%d/%Y"), 
           y = 40, label = "Dec 28: Yanukovych announces resignation", size = 3) +
  geom_segment(aes(x = as.Date("12/28/2004", "%m/%d/%Y"), y = 38, 
                   xend = as.Date("12/28/2004", "%m/%d/%Y") , 
                   yend = 28),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/ukraine-2004.pdf", p, height = 5, width = 7)

# TUNISIA 2011 -----------------------------------------------------------
# Model estimation
tunisia.es <-  event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                           event_ticker = "_TUNINDD",
                           event_window = 25, estimation_window = 200,
                           event_date = rev[name == "Tunisian Revolution", start_date],
                           model = "constant", control = FALSE)   
tunisia.es <- c(tunisia.es, car_prepost(tunisia.es$ar.treat$td, tunisia.es$ar.treat$ar,
                                        tunisia.es$sigma.treat))
tunisia.es <- data.table(date = tunisia.es$ar.treat$date, td = tunisia.es$ar.treat$td,  
                         car = tunisia.es$car, car.se = tunisia.es$car.se)
tunisia.es[, lcar := car - qnorm(.975) * car.se]
tunisia.es[, ucar := car + qnorm(.975) * car.se]

# Plot 
p <- ggplot(tunisia.es, aes(x = date, y = car)) + geom_line() +
  geom_point(size = .8) +
  geom_ribbon(aes(ymin = lcar, ymax = ucar), alpha=0.2) +
  xlab("Date") + ylab("CAR (%)") +
  geom_hline(aes(yintercept = 0), linetype = 2) + 
  annotate("text", x = as.Date("12/18/2010", "%m/%d/%Y"), 
           y = -10, label = "Dec 18: Protests begin", size = 3) +
  geom_segment(aes(x = as.Date("12/18/2010", "%m/%d/%Y"), y = -9, 
                   xend = as.Date("12/18/2010", "%m/%d/%Y") , 
                   yend = -0.5),
               arrow = arrow(length = unit(0.01, "npc"))) +
  annotate("text", x = as.Date("01/25/2011", "%m/%d/%Y"), 
           y = -3, label = "Jan 14: Ben Ali flees", size = 3) +
  geom_segment(aes(x = as.Date("01/15/2011", "%m/%d/%Y"), y = -4, 
                   xend = as.Date("01/15/2011", "%m/%d/%Y") , 
                   yend = -13),
               arrow = arrow(length = unit(0.01, "npc")))

ggsave("figs/tunisia-2011.pdf", p, height = 5, width = 7)

# EFFECT OF PUBLIC PROTESTS ON STOCK PRICES ------------------------------------
# data
index.rev <- vector(mode = "list", length(rev$ticker))
for (i in 1:length(rev$ticker)){
  index.rev[[i]] <- index[ticker == rev$ticker[i] & date >= (rev$start_date[i] - 250)
                          & date <= (rev$end_date[i] + 250)]
  index.rev[[i]][, rev_no := i]
  index.rev[[i]][, event := ifelse(date >= rev$start_date[i] & 
                                     date <= rev$end_date[i], 1, 0)]
  index.rev[[i]] <- index.rev[[i]][!is.na(dr)]
}

# volatility
garch.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                         distribution.model = "norm")
for (i in 1:length(rev$ticker)){
  rc.garchfit <- ugarchfit(spec = garch.spec, data = index.rev[[i]]$dr,
                           solver.control = list(tol = 1e-12))
  index.rev[[i]]$garch_volatility <- rc.garchfit@fit$sigma
  print(i)
}
index.rev <- rbindlist(index.rev)

# panel data
index.rev <- pdata.frame(index.rev, index = c("rev_no", "date"))

# regressions
revfit1 <- plm(dr ~ event, data = index.rev, model = "within")
revfit2 <- plm(abs_dr ~ event, data = index.rev, model = "within")
revfit3 <- plm(abs_dr ~ event + emerg_abs_dr, data = index.rev, model = "within")
revfit4 <- plm(garch_volatility ~ event, data = index.rev, model = "within")
revfit <- list(revfit1, revfit2, revfit3, revfit4)
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

