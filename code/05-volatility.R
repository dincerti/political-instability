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

# GARCH MODEL ------------------------------------------------------------------
# model spects
garch.spec <- ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                         distribution.model = "norm")

# run models
for (i in 1:n.rc){
  rc.garchfit <- ugarchfit(spec = garch.spec, data = dat[[i]]$dr,
                           solver.control = list(tol = 1e-6))
  dat[[i]]$garch_volatility <- rc.garchfit@fit$sigma
  print(i)
}

# VOLATILITY PLOTS -------------------------------------------------------------
dat <- rbindlist(dat, fill = TRUE)
volatility.mean <- dat[, .(mean_garch_volatility = mean(garch_volatility, na.rm = TRUE)),
                             by = c("td")]
p.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_garch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 2.5),
                     breaks = round(seq(min(1), max(2.5), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility.pdf", p.volatility, height = 5, width = 7)
