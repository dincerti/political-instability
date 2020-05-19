rm(list = ls())
library("xtable")
library("data.table")
library("Synth")
load("data/data-clean.RData")
source("code/func.R")

# EVENT STUDY ------------------------------------------------------------------
# Add authoritarian versus democratic shift information to data
auth_dem <- event[type == "Coup" | type == "Assassination" |
                  type == "Resignation", 
                  .(country, ticker, stock_date, `auth shift`, `dem shift`)]

regime.change <- merge(regime.change, auth_dem, 
                       by = c("country", "ticker", "stock_date"))

# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
n.es <- nrow(regime.change)
es <- es.abs <- vector(mode = "list", n.es)
dtr.treat <- rep(NA, n.es)

# loop
for (i in 1:n.es){
  # no controls for 1901
  if(regime.change$stock_date[i] == mydate("09/07/1901")){
      controli <- FALSE
  } else{
      controli <- TRUE
  }
  
  # use custom V in synth to deal with singular matrix in optim
  if (regime.change$stock_date[i] == mydate("10/4/2002") |
      regime.change$stock_date[i] == mydate("5/3/1993")){
      custom.v.i <- rep(1, 2)
  } else{
      custom.v.i <- NULL
  }
  
  # run event study
  es[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                     event_ticker = regime.change$ticker[i],
                     event_window = event.window, estimation_window = est.window,
                     event_date = regime.change$stock_date[i], model = "constant",
                     control = controli, custom_v = custom.v.i) 
  es.abs[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = abs(index$dr),
                         event_ticker = regime.change$ticker[i],
                         event_window = event.window, estimation_window = est.window,
                         event_date = regime.change$stock_date[i], model = "constant",
                         control = controli, custom_v = custom.v.i)
  
  # days to rebound
  dtr.treat[i] <- days_to_rebound(ticker = index$ticker, date = index$date, price = index$p,
                                  event_ticker = regime.change$ticker[i],
                                  event_date = regime.change$stock_date[i])
  
  # print
  print(i)
}

# combine event studies: by type of regime change
rc.es <- combine_event_studies(es, event_country = regime.change$country,
                                          event_type = regime.change$type, 
                                          event_date = regime.change$stock_date)
rc.es <- c(rc.es, list(dtr.treat = dtr.treat))
rc.abs.es <- combine_event_studies(es.abs, event_country = regime.change$country,
                                          event_type = regime.change$type, 
                                          event_date = regime.change$stock_date)

# combine event studies: by authoritarian shift
rc.es.auth <- combine_event_studies(es, event_country = regime.change$country,
                                          event_type = regime.change$`auth shift`, 
                                          event_date = regime.change$stock_date)
rc.es.auth <- c(rc.es, list(dtr.treat = dtr.treat))
rc.abs.es.auth <- combine_event_studies(es.abs, event_country = regime.change$country,
                                          event_type = regime.change$`auth shift`, 
                                          event_date = regime.change$stock_date)

# combine event studies: by democratic shift
rc.es.dem <- combine_event_studies(es, event_country = regime.change$country,
                                          event_type = regime.change$`dem shift`, 
                                          event_date = regime.change$stock_date)
rc.es.dem <- c(rc.es, list(dtr.treat = dtr.treat))
rc.abs.es.dem <- combine_event_studies(es.abs, event_country = regime.change$country,
                                          event_type = regime.change$`dem shift`, 
                                          event_date = regime.change$stock_date)

# save 
save(rc.es, rc.abs.es, rc.es.auth, rc.abs.es.auth, rc.es.dem, rc.abs.es.dem,
     file = "output/regime-change-event-study.RData")