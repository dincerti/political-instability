rm(list = ls())

# Libraries
library("xtable")
library("data.table")
library("ggplot2")
library("rugarch")
library("changepoint")
library("strucchange")
library("tidyverse")
source("code/func.R")

# Import data
load("data/data-clean.RData")

# Global options
theme_set(theme_bw())

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

# EXAMINE CHANGE POINTS --------------------------------------------------------
# Examine mean across all indices
index_mean <- index %>% 
  group_by(date) %>% 
  summarize(dr = mean(dr)) %>%
  filter(!is.na(dr))

# Examine change points: mean and variance
meanvar <- cpt.meanvar(index_mean$dr, method = "PELT")
plot(meanvar, type = "l", col = "grey", cpt.col = "steelblue2", 
     xlab = "Date", ylab = "Daily return", cpt.width = 2, ylim = c(-10, 10))

# Loop through all indices: Binary segmentation
par(mfrow=c(6,6), mai = c(0.3, 0.3, 0.3, 0.3))
for (i in 1:n.rc){
  data = dat[[i]]
  m_binseg <- cpt.mean(data$dr, penalty = "BIC", method = "BinSeg", Q = 5)
  plot(m_binseg, type = "l", col = "grey", cpt.col = "steelblue2", 
       xlab = "Date", ylab = "Daily return", cpt.width = 2)
  abline(v=as.Date(data$event_date[[i]]), col = 'firebrick2')
}

# Loop through all indices: PELT
par(mfrow=c(6,6), mai = c(0.3, 0.3, 0.3, 0.3))
for (i in 1:n.rc){
  data = dat[[i]]
  m_pelt <- cpt.meanvar(data$dr, penalty = "BIC", method = "PELT")
  plot(m_pelt, type = "l", col = "grey", cpt.col = "steelblue2", 
       xlab = "Date", ylab = "Daily return", cpt.width = 2)
}

# STRUCTURAL BREAKS ------------------------------------------------------------
# Find structural breaks
for (i in 1:n.rc){
  data = dat[[i]]
  # Create lagged variable
  data <- data %>% mutate(y = dr, ylag = lag(y)) %>% drop_na()
  
  # Test
  sa_bp <- breakpoints(y ~ ylag, data = data, breaks = 5)
  
  # Find break dates
  breaks <- data %>%
    select(date, dr) %>%
    mutate(y = dr, ylag = lag(y)) %>%
    drop_na() %>%
    slice(sa_bp$breakpoint) %>%
    mutate(break_dummy = 1) %>%
    select(date, break_dummy)
  
  # Merge breaks with index
  dat[[i]] <- left_join(data, breaks) %>%
    mutate(
      break_dummy = ifelse(is.na(break_dummy), 0, 1),
      break_dummy2 = cumsum(break_dummy),
      break_dummy2 = case_when(
        break_dummy2 == 0 ~ 0,
        break_dummy2 == 1 ~ 1,
        break_dummy2 == 2 ~ 0,
        break_dummy2 == 3 ~ 1
      ))
}

# Run GARCH models with structural break dummies
for(i in 1:n.rc){
  data = dat[[i]]
  if(mean(data$break_dummy2) > 0){
    ext.reg <- data$break_dummy2
    garch.spec <- ugarchspec(
      variance.model = list(external.regressors = data.matrix(ext.reg)),
      mean.model = list(armaOrder = c(0,0)), 
      distribution.model = "norm")
    
    rc.garchfit <- ugarchfit(spec = garch.spec, data = data$dr,
                             solver.control = list(tol = 1e-6)) 
  } else if(mean(data$break_dummy2) == 0){
    garch.spec <- ugarchspec(
      mean.model = list(armaOrder = c(0,0)), distribution.model = "norm")
    
    rc.garchfit <- ugarchfit(spec = garch.spec, data = data$dr,
                             solver.control = list(tol = 1e-6)) 
  } 
  dat[[i]]$garch_volatility <- rc.garchfit@fit$sigma
  print(i)
}

# Plot
dat <- rbindlist(dat, fill = TRUE)
volatility.mean <- dat[, .(mean_garch_volatility = mean(garch_volatility, na.rm = TRUE)), by = c("td")]

sb.volatility <- 
  ggplot(volatility.mean[td >= -250 & td <= 250],
         aes(x = td, y = mean_garch_volatility)) + 
  geom_line(color = "grey48") + 
  xlab("Trading days") + 
  ylab("Mean volatility") +
  scale_y_continuous(limits = c(1, 3),
                     breaks = round(seq(min(1), max(3), by = 0.5),1)) +
  theme_classic()

ggsave("figs/mean-volatility-sb.pdf", sb.volatility, height = 5, width = 7)
