rm(list = ls())
library("xtable")
library("data.table")
library("Synth")
library("lubridate")
load("data/data-clean.RData")
source("code/func.R")

# EVENT STUDY ------------------------------------------------------------------
# Lag date of event by 60 days
regime.change$date_lag = regime.change$stock_date %m-% months(3)

# No data for Phillipines prior to 1986: cannot lag data 3 months
regime.change <- regime.change[stock_date != '1986-02-26'] 

# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
n.es <- nrow(regime.change)
es <- es.abs <- vector(mode = "list", n.es)
dtr.treat <- rep(NA, n.es)

# loop
for (i in 1:n.es){
  # # no controls for 1901
  # if(regime.change$date_lag[i] == mydate("06/07/1901")){
  #     controli <- FALSE
  # } else{
  #     controli <- TRUE
  # }
  # 
  # # use custom V in synth to deal with singular matrix in optim
  # if (regime.change$date_lag[i] == mydate("7/4/2002") |
  #     regime.change$date_lag[i] == mydate("2/3/1993")){
  #     custom.v.i <- rep(1, 2)
  # } else{
  #     custom.v.i <- NULL
  # }
  
  # run event study
  es[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                     event_ticker = regime.change$ticker[i],
                     event_window = event.window, estimation_window = est.window,
                     event_date = regime.change$date_lag[i], model = "constant",
                     control = FALSE, custom_v = NULL) 
  es.abs[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = abs(index$dr),
                         event_ticker = regime.change$ticker[i],
                         event_window = event.window, estimation_window = est.window,
                         event_date = regime.change$date_lag[i], model = "constant",
                         control = FALSE, custom_v = NULL)
  
  # days to rebound
  dtr.treat[i] <- days_to_rebound(ticker = index$ticker, date = index$date, price = index$p,
                                  event_ticker = regime.change$ticker[i],
                                  event_date = regime.change$date_lag[i])
  
  # print
  print(i)
}

# combine event studies
rc.es <- combine_event_studies(es, event_country = regime.change$country,
                                          event_type = regime.change$type, 
                                          event_date = regime.change$date_lag)
rc.es <- c(rc.es, list(dtr.treat = dtr.treat))
rc.abs.es <- combine_event_studies(es.abs, event_country = regime.change$country,
                                          event_type = regime.change$type, 
                                          event_date = regime.change$stock_date)

# save 
#save(rc.es, rc.abs.es, file = "output/regime-change-event-study.RData")