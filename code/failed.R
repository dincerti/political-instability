rm(list = ls())
library("xtable")
library("dplyr")
library("data.table")
library("Synth")
load("data/data-clean.RData")
source("code/func.R")


# EVENT STUDY: FAILED COUPS ONLY -----------------------------------------------
# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
n.es <- nrow(failed)
es <- es.abs <- vector(mode = "list", n.es)
dtr.treat <- rep(NA, n.es)

# loop
for (i in 1:n.es){
  # no controls for 1901 or 2016
  if(failed$stock_date[i] == mydate("07/18/2016")){
    controli <- FALSE
  } else{
    controli <- TRUE
  }
  
  # use custom V in synth to deal with singular matrix in optim
  if (failed$stock_date[i] == mydate("10/4/2002") |
      failed$stock_date[i] == mydate("5/3/1993")){
    custom.v.i <- rep(1, 2)
  } else{
    custom.v.i <- NULL
  }
  
  # run event study
  es[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = failed$ticker[i],
                         event_window = event.window, estimation_window = est.window,
                         event_date = failed$stock_date[i], model = "constant",
                         control = controli, custom_v = custom.v.i) 
  es.abs[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = abs(index$dr),
                             event_ticker = failed$ticker[i],
                             event_window = event.window, estimation_window = est.window,
                             event_date = failed$stock_date[i], model = "constant",
                             control = controli, custom_v = custom.v.i)
  
  # days to rebound
  dtr.treat[i] <- days_to_rebound(ticker = index$ticker, date = index$date, price = index$p,
                                  event_ticker = failed$ticker[i],
                                  event_date = failed$stock_date[i])
  
  # print
  print(i)
}

# combine event studies for failed coups
failed.index <- which(failed$type == "Failed coup")
failed.es <- combine_event_studies(es, event_country = failed$country,
                               event_type = failed$type, 
                               event_date = failed$stock_date)
failed.es <- c(failed.es, list(dtr.treat = dtr.treat))
failed.abs.es <- combine_event_studies(es.abs, event_country = failed$country,
                                   event_type = failed$type, 
                                   event_date = failed$stock_date)

artable.failed <- ar_table(td = failed.es$td, ar = failed.es$ar.treat[, failed.index],
                          sigma = failed.es$sigma.treat, 
                          dtr = failed.es$dtr.treat,
                          country = failed[failed.index, country],
                          date = failed[failed.index, stock_date], 
                          coup = FALSE)

# Print table of failed coups
myprint.xtable(artable.failed$car, file = "tables/artable-failed-car.txt")
myprint.xtable(artable.failed$car.mean, file = "tables/artable-failed-car-mean.txt")

# EVENT STUDY: FAILED AND SUCCESSFUL COUPS -------------------------------------
# Combine failed coups with successful coups
coup <- regime.change %>% filter(type == "Coup")
failed <- failed %>% select(country, ticker, stock_date, type)
failed <- rbind(failed, coup, fill = TRUE)

# calculate abnormal returns and days to rebound
event.window <- 20
est.window <- 200
n.es <- nrow(failed)
es <- es.abs <- vector(mode = "list", n.es)
dtr.treat <- rep(NA, n.es)

# loop
for (i in 1:n.es){
  # no controls for 1901 or 2016
  if(failed$stock_date[i] == mydate("07/18/2016")){
    controli <- FALSE
  } else{
    controli <- TRUE
  }
  
  # use custom V in synth to deal with singular matrix in optim
  if (failed$stock_date[i] == mydate("10/4/2002") |
      failed$stock_date[i] == mydate("5/3/1993")){
    custom.v.i <- rep(1, 2)
  } else{
    custom.v.i <- NULL
  }
  
  # run event study
  es[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = index$dr,
                         event_ticker = failed$ticker[i],
                         event_window = event.window, estimation_window = est.window,
                         event_date = failed$stock_date[i], model = "constant",
                         control = controli, custom_v = custom.v.i) 
  es.abs[[i]] <- event_study(ticker = index$ticker, date = index$date, dr = abs(index$dr),
                             event_ticker = failed$ticker[i],
                             event_window = event.window, estimation_window = est.window,
                             event_date = failed$stock_date[i], model = "constant",
                             control = controli, custom_v = custom.v.i)
  
  # days to rebound
  dtr.treat[i] <- days_to_rebound(ticker = index$ticker, date = index$date, price = index$p,
                                  event_ticker = failed$ticker[i],
                                  event_date = failed$stock_date[i])
  
  # print
  print(i)
}

# combine event studies for failed and successful coups
failed.index <- which(failed$type == "Failed coup" | failed$type == "Coup")
failed.es <- combine_event_studies(es, event_country = failed$country,
                                   event_type = failed$type, 
                                   event_date = failed$stock_date)
failed.es <- c(failed.es, list(dtr.treat = dtr.treat))
failed.abs.es <- combine_event_studies(es.abs, event_country = failed$country,
                                       event_type = failed$type, 
                                       event_date = failed$stock_date)

artable.failed <- ar_table(td = failed.es$td, ar = failed.es$ar.treat[, failed.index],
                           sigma = failed.es$sigma.treat, 
                           dtr = failed.es$dtr.treat,
                           country = failed[failed.index, country],
                           date = failed[failed.index, stock_date], 
                           coup = FALSE)

# Print table of failed and successful coups
myprint.xtable(artable.failed$car, file = "tables/artable-failed-success-car.txt")
myprint.xtable(artable.failed$car.mean, file = "tables/artable-failed-success-car-mean.txt")
