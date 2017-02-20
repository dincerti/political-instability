rm(list = ls())
library("data.table")
library("ggplot2")
load("data/data-clean.RData")
source("code/func.R")
theme_set(theme_bw())

# ABSOLUTE VALUE OF DAILY STOCK RETURNS BY DAYS FROM EVENT ---------------------
pre <- post <- 200
dr <- matrix(NA, nrow = pre + post + 1, ncol = nrow(regime.change))
for (i in 1:nrow(regime.change)){
  dr[, i] <- return_by_td(stockdata = index[ticker == regime.change$ticker[i], .(date, dr)],
                          event_date = regime.change$stock_date[i], pre_event = pre, 
                          post_event = post)  
}

# note: 2 events don't have complete data and are excluded
dr.na <- apply(dr, 2, function (x) sum(1 * is.na(x)))
abs.dr.mean <- apply(abs(dr[, which(dr.na==0)]), 1, mean)
tmp <- data.table(dr = abs.dr.mean, td = seq(-pre, post))

# Create plot
p <- ggplot(tmp, aes(x = td, y = dr)) + 
  geom_area(color="gray48", fill='gray48', size = .1) +
  xlab("Trading days") + ylab("Absolute value of daily return (%)") +  
  
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks = element_blank()) +
  
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

ggsave("figs/daily_mean_absreturn.pdf", p, height = 5, width = 7)