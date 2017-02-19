rm(list = ls())
load("data/data-clean.RData")
source("code/func.R")
library("data.table")
library("plm")
library("lmtest")
library("broom")
theme_set(theme_bw())

# EGYPTIAN REVOLUTION ----------------------------------------------------------
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

# EFFECT OF PUBLIC PROTESTS ON STOCK PRICES ------------------------------------
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

