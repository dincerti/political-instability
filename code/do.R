
# SET UP -----------------------------------------------------------------------
regime.change <- event[type == "Coup" | type == "Assassination" |
                         type == "Resignation", 
                       .(country, ticker, stock_date, type)]

# ABSOLUTE VALUE OF DAILY STOCK RETURNS BY DAYS FROM EVENT ---------------------
pre <- post <- 200
dr <- matrix(NA, nrow = pre + post + 1, ncol = nrow(regime.change))
for (i in 1:nrow(regime.change)){
  dr[, i] <- ReturnByTD(stockdata = index[ticker == regime.change$ticker[i], .(date, dr)],
                        event.date = regime.change$stock_date[i], pre.event = pre, 
                        post.event = post)  
}
# note: 2 events don't have complete data and are excluded
dr.na <- apply(dr, 2, function (x) sum(1 * is.na(x)))
abs.dr.mean <- apply(abs(dr[, which(dr.na==0)]), 1, mean)
tmp <- data.table(dr = abs.dr.mean, td = seq(-pre, post))
p <- ggplot(tmp, aes(x = td, y = dr)) + 
  geom_segment(mapping = aes(xend = td, yend = 0)) +
  xlab("Trading days") + ylab("Absolute value of daily return (%)")
ggsave("figs/daily_mean_absreturn.pdf", p, height = 5, width = 7)

# EVENT STUDY ------------------------------------------------------------------
# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
sigma <- rep(NA, nrow(regime.change))
ar <- matrix(NA, nrow = 2 * event.window, ncol = nrow(regime.change))
dtr <- rep(NA, nrow(regime.change))
for (i in 1:ncol(ar)){
  tmp <-  EventStudy(stockdata = index[ticker == regime.change$ticker[i], 
                                       .(date, dr)],
                     event.window = event.window, estimation.window = est.window,
                     event.date = regime.change$stock_date[i], model = "constant")   
  sigma[i] <- tmp$sigma
  ar[, i] <- tmp$ar
  dtr[i] <- DaysToRebound(stockdata = index[ticker == regime.change$ticker[i], 
                                            .(date, p)],
                          event.date = regime.change$stock_date[i])
}
td.ew <- seq(-event.window, event.window -1 )

# ABNORMAL RETURNS TABLES ------------------------------------------------------
# coups
coup.index <- which(regime.change$type == "Coup")
ar.table.coups <- ARTable(td = td.ew, ar = ar[, coup.index],
                          sigma = sigma[coup.index], dtr = dtr[coup.index],
                          country = regime.change[coup.index, country],
                          date = regime.change[coup.index, stock_date], 
                          coup = TRUE)

# assassinations
ass.index <- which(regime.change$type == "Assassination")
ar.table.ass <- ARTable(td = td.ew, ar = ar[, ass.index],
                          sigma = sigma[ass.index], dtr = dtr[ass.index],
                          country = regime.change[ass.index, country],
                          date = regime.change[ass.index, stock_date])

# resignations
res.index <- which(regime.change$type == "Resignation")
ar.table.res <- ARTable(td = td.ew, ar = ar[, res.index],
                        sigma = sigma[res.index], dtr = dtr[res.index],
                        country = regime.change[res.index, country],
                        date = regime.change[res.index, stock_date])

# VENEZUELA PARTIAL COUP -------------------------------------------------------
ven <- event[ticker == "_IBCD" & stock_date == "2002-04-12"]
ven.es <- EventStudy(stockdata = index[ticker == ven$ticker, 
                             .(date, dr)],
           event.window = event.window, estimation.window = est.window,
           event.date = ven$stock_date, model = "constant")  
ven.es <- c(ven.es, Car(td = td.ew, ar = ven.es$ar, sigma = ven.es$sigma))
tmp <- data.table(td = td.ew,  ar = ven.es$ar, ar.se = ven.es$sigma)
tmp[, lower := ar - qnorm(.975) * ar.se]
tmp[, upper := ar + qnorm(.975) * ar.se]
p <- ggplot(tmp[abs(td) <= 10], aes(x = td, y = ar)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  xlab("Days") + ylab("AR (%)")
ggsave("figs/venezuela_coup_attempt_2002.pdf", p, height = 5, width = 7)

# VOLATILITY -------------------------------------------------------------------
