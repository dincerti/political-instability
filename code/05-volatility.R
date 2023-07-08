rm(list = ls())
library("xtable")
library("data.table")
library("ggplot2")
library("rugarch")
load("data/data-clean.RData")
theme_set(theme_bw())
source("code/func.R")

# DATA SETUP -------------------------------------------------------------------
post.days <- pre.days <- 1000
n.rc <- nrow(regime.change)
dat <- vector(mode = "list", n.rc)
for (i in 1:n.rc){
  dat[[i]] <- index[ticker == regime.change$ticker[i], .(ticker, date, dr)]
  dat[[i]] <- calc_td(stockdata = dat[[i]], 
                            event_date = regime.change$stock_date[i])
  dat[[i]] <- dat[[i]][td >= -pre.days & td <= post.days]
  dat[[i]] <- dat[[i]][complete.cases(dat[[i]])]
  dat[[i]]$event_date <- regime.change$stock_date[i]
  dat[[i]]$event_num <- i
} 
nobs <- do.call("c", lapply(dat, nrow))

# GARCH MODELS -----------------------------------------------------------------
# Garch (1,1)
# model specs
garch.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                         distribution.model = "norm")

# run models
for (i in 1:n.rc){
  rc.garchfit <- ugarchfit(spec = garch.spec, data = dat[[i]]$dr,
                           solver.control = list(tol = 1e-6))
  dat[[i]]$garch_volatility <- rc.garchfit@fit$sigma
  print(i)
}

# EGARCH
# model specs
egarch.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)), 
                         mean.model=list(armaOrder=c(0,0))) 

# run models
for (i in 1:n.rc){
  rc.egarchfit <- ugarchfit(spec = egarch.spec, data = dat[[i]]$dr)
  dat[[i]]$egarch_volatility <- rc.egarchfit@fit$sigma
  print(i)
}

#TGARCH
tgarch.spec = ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(0,0)))

for (i in 1:n.rc){
  rc.tgarchfit <- ugarchfit(spec = tgarch.spec, data = dat[[i]]$dr)
  dat[[i]]$tgarch_volatility <- rc.tgarchfit@fit$sigma
  print(i)
}

#FIGARCH
figarch.spec <-ugarchspec(variance.model = list(model = "fiGARCH", garchOrder = c(1, 1))) 

for (i in 1:n.rc){
  rc.figarchfit <- ugarchfit(spec = figarch.spec, data = dat[[i]]$dr)
  dat[[i]]$figarch_volatility <- rc.figarchfit@fit$sigma
  print(i)
}

# VOLATILITY PLOTS -------------------------------------------------------------
dat <- rbindlist(dat, fill = TRUE)

# GARCH
volatility.mean <- dat[, .(mean_garch_volatility = mean(garch_volatility, na.rm = TRUE)), by = c("td")]

p.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_garch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = round(seq(min(1), max(3), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility.pdf", p.volatility, height = 5, width = 7)

# EGARCH
volatility.mean <- dat[, .(mean_egarch_volatility = mean(egarch_volatility, na.rm = TRUE)),
                       by = c("td")]

e.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_egarch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = round(seq(min(1), max(3), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility-egarch.pdf", e.volatility, height = 5, width = 7)

# TGARCH
volatility.mean <- dat[, .(mean_tgarch_volatility = mean(tgarch_volatility, na.rm = TRUE)),
                       by = c("td")]

t.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_tgarch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = round(seq(min(1), max(3), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility-tgarch.pdf", t.volatility, height = 5, width = 7)

# FiGARCH
volatility.mean <- dat[, .(mean_figarch_volatility = mean(figarch_volatility, na.rm = TRUE)),
                       by = c("td")]

fi.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_figarch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = round(seq(min(1), max(3), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility-figarch.pdf", fi.volatility, height = 5, width = 7)

