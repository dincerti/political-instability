# CONVERT STRING DATE TO R DATE ------------------------------------------------
mydate <- function(strdate){
  return(as.Date(strdate, "%m/%d/%Y"))
}

# xtable PRINT -----------------------------------------------------------------
myprint.xtable <- function(x, file){
  print(xtable(x), 
        include.rownames = FALSE, include.colnames = FALSE,
        only.contents = TRUE, sanitize.text.function = identity, hline.after = NULL,
        file = file)
}

# MATRIX WITH PARENTHESES IN STANDARD ERRORS -----------------------------------
matrix_se <- function(est, se){
  est <- gsub(" ", "", est)
  se <- gsub(" ", "", se)
  m <- matrix(as.vector(rbind(as.vector(est), 
                              paste0("(", as.vector(se), ")"))),
              nrow = 2 * nrow(est))
  m <- gsub("\\()", "", m)
  return(m)
}

# CALCULATE TRADING DAYS IN EVENT WINDOW ---------------------------------------
calc_td <- function(stockdata, event_date){
  stockdata[, n := seq(1, .N), by = "ticker"]
  if (length(unique(stockdata$ticker)) == 1) {
      # scenario 1: only 1 ticker 
      event.td <- which.max(stockdata$date - event_date >= 0) # first trading day on or after event
      stockdata[, td := n - event.td]
  } else { 
      # scenario 2: many tickers (i.e. for control portfolio)
      stockdata[, event_td := which.max(date - event_date >= 0), by = "ticker"]
      stockdata[, td := n - event_td]
      stockdata[, date_diff := date - event_date]
      stockdata[, date_diff0 := ifelse(td == 0, date_diff, NA)]
      stockdata[, date_diff0 := mean(date_diff0, na.rm = TRUE), by = "ticker"]
      stockdata <- stockdata[abs(date_diff0) <= 5]
      stockdata[, c("event_td", "date_diff", "date_diff0") := NULL]
  }
  return(stockdata)
}

# DAILY RETURNS BY TRADING DAY -------------------------------------------------
return_by_td <- function(stockdata, event_ticker, event_date, pre_event, post_event){
  # Note: first column of stockdata must be a date variable
  stockdata <- stockdata[ticker == event_ticker, .(ticker, date, dr)]
  
  # trading days
  stockdata <- calc_td(stockdata, event_date)
  
  # trading window surrounding event
  stockdata <- stockdata[td >= -pre_event & td <= post_event]
  
  # deal with countries with insufficient observations (i.e. fill in missing td with NAs)
  if (min(stockdata$td) > - pre_event){
    tmp <- data.table(matrix(NA, nrow = pre - abs(min(stockdata$td)), 
                             ncol = ncol(stockdata)))
    setnames(tmp, colnames(stockdata))
    tmp$td <- seq(-pre, min(stockdata$td) - 1)
    tmp$date <- as.Date(tmp$date)
    stockdata <- rbind(tmp, stockdata)
  }
  return(stockdata$dr)
}

# DAYS TO REBOUND --------------------------------------------------------------
days_to_rebound <- function(ticker, date, price, event_ticker, event_date){
  stockdata <- data.table(ticker = ticker, date = date, p = price)
  stockdata <- stockdata[ticker == event_ticker]
  
  # trading days
  stockdata <- calc_td(stockdata, event_date)
  
  # days to rebound
  p.init <- stockdata[td == -1, p]
  if (stockdata[td == 0, p]/p.init < 1){
    return(which.max(stockdata[td >= 0, p] >= p.init))
  } else{
    return(NA)
  }
}

# EVENT STUDY REGRESSION MODEL -------------------------------------------------
event_study_mod <- function(stockdata, event_window, estimation_window,
                            model = c("constant", "maarket")){
  model <- match.arg(model)
  if (model == "constant"){
    lm <- lm(dr ~ 1, stockdata[td >= -estimation_window & td < -event_window]) 
  } else if (model == "market"){
    print ("Code has not yet been set up for market model")
  }
  
  # abnormal returns
  event.data <- stockdata[td >= -event_window & td < event_window, .(td, dr)]
  ar <- event.data$dr - predict(lm, event.data)
  sigma <- summary(lm)$sigma
  return(list(event_data = event.data, ar = ar, sigma = sigma))
}

# EVENT STUDY ------------------------------------------------------------------
# Returns: list of date, trading day, standard error of regression, and
#          abnormal return
event_study <- function(ticker, date, dr, event_ticker, event_window, estimation_window, 
                        event_date, model = "constant", control = FALSE,
                        custom_v = NULL){
  # Note: first column of stockdata must be a date variable
  stockdata <- data.table(ticker = ticker, date = date, dr = dr)
  
  # treatment portfolio
  treatdata <- stockdata[ticker == event_ticker]
  treatdata <- calc_td(treatdata, event_date)
  treatdata <- treatdata[td >= -estimation_window & td < event_window]
  mod.treat <- event_study_mod(treatdata, event_window, 
                               estimation_window, "constant")
  ar.treat <- data.table(date = treatdata[td >= -event_window & td < event_window,
                                          date],
                        td = mod.treat$event_data$td,
                         ar = mod.treat$ar)
  
  # control portfolio
  if (control == TRUE){
    synthdata <- calc_td(stockdata, event_date)
    synth.control <- synth_control(synthdata, event_ticker, event_window,
                                   estimation_window, custom_v = custom_v)
    mod.control <- event_study_mod(synth.control$dr, event_window, 
                                   estimation_window, "constant")
    ar.control <- data.table(td = mod.control$event_data$td,
                             ar = mod.control$ar)
  }
  
  # return
  l <- list(dr.treat = treatdata[, .(ticker, date, td, dr)],
            ar.treat = ar.treat, sigma.treat = mod.treat$sigma)
  if (control == TRUE){
    l <- c(l, list(ar.control = ar.control, sigma.control = mod.control$sigma,
                   synth.info = synth.control))
  }
  return(l)
}  

# ABNORMAL RETURN TABLE --------------------------------------------------------
# LaTeX table of abnormal returns
#
# Args:
#   td: vector of trading days relative to event day for abnormal returns.
#   ar: matrix of abnormal returns.
#   sigma: standard error returns.
#   dtr: days to rebound.
#   country: country in which event occured.
#   date: date of the event.
#   coup: Was the event a coup? If yes then TRUE, else FALSE.
ar_table <- function(td, ar, sigma, dtr, country, date, coup = FALSE){
  
  # cumulative abnormal returns for selected time periods
  car <- matrix(NA, nrow = ncol(ar), ncol = 5)
  car[, 1] <- ar[which(td == 0), ]
  car[, 2] <- apply(ar[which(td >= 0 & td <= 6), ], 2, sum)
  car[, 3] <- apply(ar[which(td >= 0 & td <= 19), ], 2, sum)
  car[, 4] <- apply(ar[which(td <= -1 & td >= -7), ], 2, sum)
  car[, 5] <- apply(ar[which(td <= -1 & td >= -20), ], 2, sum)
  
  # standard errors of cumulative abnormal returns
  car.se <- as.matrix(sigma) %*% sqrt(t(as.matrix(c(1, 7, 20, 7, 20))))
  
  # means
  car.mean <- t(as.matrix(apply(car, 2, mean)))
  car.mean.se <- t(as.matrix(sqrt(1/nrow(car)^2 * sum(sigma^2) *
                             c(1, 7, 20, 7, 20))))
  if(coup == TRUE){
    exclude <- which(country == "Argentina" & date == "1976-04-05")
    car.mean <- rbind(car.mean, apply(car[-c(exclude), ], 2, mean))
    car.mean.se <- rbind(car.mean.se,
                         sqrt(1/nrow(car[-c(exclude), ])^2 * sum(sigma[-exclude]^2) *
                               c(1, 7, 20, 7, 20)) )   
  }
  
  # table
  car <- formatC(car, format="f", digits=3)
  car.se <- formatC(car.se, format="f", digits=3)
  country <- as.vector(rbind(country, ""))
  date <- as.vector(rbind(format(date, format="%m/%d/%Y"), ""))
  dtr.str <- ifelse(is.na(dtr), "", formatC(dtr, format = "d", big.mark = ","))
  dtr.str <- as.vector(rbind(dtr.str, ""))
  table <-  matrix_se(car, car.se)
  table <- cbind(country, date,  table, dtr.str)
  
  # table for means
  car.mean <- formatC(car.mean, format="f", digits=3)
  car.mean.se <- formatC(car.mean.se, format="f", digits=3)
  table.means <-  matrix_se(car.mean, car.mean.se)
  if (coup == TRUE){
    name1 <- "\\multicolumn{2}{l}{\\hspace{.2cm}All Coups}"
    name2 <- "\\multicolumn{2}{l}{\\hspace{.2cm}Excluding 4/5/1976}"
    name.se <- "\\multicolumn{2}{l}{}"
    names <- c(name1, name.se, name2, name.se) 
    dtr.str <- c(mean(dtr, na.rm = TRUE), mean(dtr[-exclude], na.rm = TRUE))
    dtr.str <- formatC(dtr.str, format = "d", big.mark = ",")
    dtr.str <- c(dtr.str[1], "", dtr.str[2], "")
  } else{
    dtr.str <- formatC(mean(dtr, na.rm = TRUE), format = "d", big.mark = ",")
    dtr.str <- c(dtr.str, "")
    name <- "\\multicolumn{2}{l}{\\textbf{Mean}}"
    name.se <- "\\multicolumn{2}{l}{}"
    names <- as.vector(rbind(c(name, name.se)))
  }
  table.means <- cbind(names, table.means, dtr.str)
  
  # return
  return(list(car = table, car.mean = table.means))
}

# CUMULATIVE ABNORMAL RETURNS --------------------------------------------------
car_prepost <- function(td, ar, sigma){
  car.b <- rev(cumsum(rev(ar[which(td < -1)])))
  car.b.se <- rev(sigma * sqrt(seq(1, length(car.b))))
  car.f <- cumsum(ar[which(td >= 0)])
  car.f.se <- sigma * sqrt(seq(1, length(car.f)))
  car.fixed <- ar[which(td == -1)]
  car.fixed.se <- 0
  car <- c(car.b, car.fixed, car.f)
  car <- car - car.fixed
  car.se <- c(car.b.se, car.fixed.se, car.f.se)
  return(list(car = car, car.se =  car.se))
}

# MEAN CAR ---------------------------------------------------------------------
car_mean <- function(ar, sigma) {
  ar.mean <- apply(ar, 1, mean)
  car.mean <- cumsum(ar.mean)
  ar.mean.var <- 1/nrow(ar)^2 * sum(sigma^2)
  car.mean.se <- sqrt(ar.mean.var * seq(1, nrow(ar)))
  return(data.table(car_mean = car.mean, car_mean_se = car.mean.se))
}

# MEAN CAR PRE AND POST --------------------------------------------------------
mean_car_prepost <- function(ar, sigma, td){
  ar.b <-  ar[rev(which(td <= -1)), ]
  car.mean.b <- data.table(car_mean(ar.b, sigma), td = rev(td[td <= -1]))
  ar.f <-  ar[which(td >= 0), ]
  car.mean.f <- data.table(car_mean(ar.f, sigma), td = td[td >= 0])
  car.mean.f[, td := td + 1]
  car.mean.0 <- data.table(car_mean = 0,
                           car_mean_se = 0, td = 0)
  car.mean <- rbind(car.mean.b[nrow(car.mean.b):1 ], car.mean.0, car.mean.f)
  return(car.mean)
}

# REGRESSION TABLE WITH STANDARD ERRORS IN PARENTEHSES -------------------------
# Regression table with standard errors in parentheses
#
# Args:
#   models: List of models
#   lookup: Option data.frame to lookup variable names. First column is term, the name
#           of variables in the model. Second column is varname, the name of the variable
#           to display in the LaTex table. Third column is id, which is the order to display
#           the variables.
#   digits: Digits to round coefficients and standard errors to
#
# Returns: 
#   LaTeX table with coefficients (se's in parentheses) and number of observations for each
#   model listed.
reg_table <- function(models, lookup = NULL, digits = 3){
  n.est <- length(models)
  coef <- se <- vector(mode = "list", length = n.est)
  obs <- rep(NA, n.est)
  #r2 <- rep(NA, n.est)
  for (i in 1:n.est){
    est <- coeftest(models[[i]], vcov = vcovHC(models[[i]], type = "HC0",
                                                   cluster="group"))
    est <- data.frame(tidy(est))
    colnames(est) <- c("term", paste0(colnames(est)[-1], i))
    if (!is.null(lookup)){
      est <- merge(est, lookup, by = "term", all.x = TRUE)
      est$term <- est$varname
      est$varname <- NULL
    }
    coef[[i]] <- est[, c("term", "id", paste0("estimate", i))]
    se[[i]] <- est[, c("term", "id", paste0("std.error", i))]
    obs[i] <- nobs(models[[i]])
  }
  coef <- Reduce(function(x, y) merge(x, y, by = c("term", "id"), all = TRUE), coef)
  se <- Reduce(function(x, y) merge(x, y, by = c("term", "id"), all = TRUE), se)
  coef <- coef[order(coef$id), ]
  se <- se[order(se$id), ]
  se$id <- coef$id <- NULL
  coef.str <- formatC(as.matrix(coef[, -1]), format = "f", digits = digits)
  se.str <- formatC(as.matrix(se[, -1]), format = "f", digits = digits)
  coef.str <- gsub("NA", "", coef.str)
  se.str <- gsub("NA", "", se.str)
  table <-  matrix_se(coef.str, se.str)
  obs <- formatC(obs, format = "d", big.mark = ",")
  obs <- paste0("\\multicolumn{1}{c}{", obs, "}")
  table <- rbind(table, obs)
  names <- c(as.vector(rbind(as.character(coef$term), "")), "Observations")
  table <- cbind(names, table)
  rownames(table) <- seq(1, nrow(table))
  return(table)
}
  
# RANK TEST --------------------------------------------------------------------
rank_test <- function(ar, td){
  n.na <- apply(ar, 2, function(x) sum(is.na(x)))
  cols <- which(n.na == 0)
  ar <- ar[, cols]
  L2 <- nrow(ar)
  exp.rank <- (L2 + 1)/2
  ar.rank <- apply(ar, 2, rank)
  diff <- ar.rank - exp.rank
  diff.mean <- apply(diff, 1, mean)
  sk <- sqrt(mean(diff.mean^2))
  theta <- diff.mean[which(td == 0)]/sk
  pval <- 2 * (1 - pnorm(abs(theta)))
  return(pval)
}

# SIGN TEST --------------------------------------------------------------------
sign_test <- function(ar0, direction = c("positive", "negative"),
                      alternative = c("two.sided", "less", "greater")){
  direction = match.arg(direction)
  ar0 <- ar0[!is.na(ar0)]
  n <- length(ar0)
  if (direction == "positive"){
    x <- sum(ar0 > 0)
  } else{
    x <- sum(ar0 < 0)
  }
  binom.test <- binom.test(x, n, p = .5, alternative = alternative)
  return(binom.test$p.value)
}

# SYNTHETIC CONTROL GROUP ------------------------------------------------------
synth_control <- function(stockdata, event_ticker, event_window, 
                          estimation_window, custom_v = NULL){
  stockdata <- stockdata[td >= -estimation_window & td < event.window]
  stockdata[, id := .GRP, by = "ticker"]
  stockdata <- stockdata[!is.na(dr)]

  # number of observations by group
  max.n <- estimation_window + event_window
  nobs <- stockdata[, .(N = .N, td1 = td[1]), by = c("id", "ticker")]
  
  # first trading day for control must be same or before that of control
  td1.treat <- nobs[ticker == event_ticker, td1]
  id.control <- nobs[ticker != event_ticker & td1 <= td1.treat, id]
  id.event <- nobs[ticker == event_ticker, id]
  stockdata <- stockdata[td >= td1.treat]
  
  ## synth data prep
  dataprep.out  <-  dataprep(
    foo  =  stockdata,
    predictors  =  c("dr"),
    time.predictors.prior  = td1.treat:(-event.window - 1),
    special.predictors  =  list(
      list("dr", td1.treat:(-event.window - 1), "var")),
    dependent  =  "dr",
    unit.variable  =  "id",
    time.variable  =  "td",
    treatment.identifier  =  id.event,
    controls.identifier  =  id.control,
    time.optimize.ssr  =  td1.treat:(-event.window - 1),
    time.plot  =  td1.treat:(event.window - 1),
    unit.names.variable = "ticker")
  synth.out  <-  synth(data.prep.obj  =  dataprep.out, custom.v = custom_v,
                       optimxmethod  =  c("Nelder-Mead", "BFGS"))
  
  ## pred
  tab <- synth.tab(synth.out, dataprep.out)
  
  ## daily returns
  dr.control.estimation <- dataprep.out$Z0 %*% synth.out$solution.w
  dr.control.event <- dataprep.out$Y0 %*% synth.out$solution.w
  dr.control <- data.table(td = as.numeric(c(rownames(dr.control.estimation),
                                  rownames(dr.control.event))),
                           dr = c(dr.control.estimation, dr.control.event))
  
 ## return
  return(list(dr = dr.control, tab.pred = tab$tab.pred, tab.v = tab$tab.v,
              tab.w = tab$tab.w, tab.loss = tab$tab.loss))
}

# COMBINE EVENT STUDIES --------------------------------------------------------
combine_event_studies <- function(x, event_country, event_type, event_date){
  # initialize
  n <- length(x)
  td.ew <- x[[1]]$ar.treat$td
  sigma.treat <- sigma.control <- rep(NA, n)
  ar.treat <- ar.control <- matrix(NA, nrow = nrow(x[[1]]$ar.treat), ncol = n)
  car.treat <- car.control <- car.treat.se <- car.control.se <- ar.treat
  dr.treat <- dr.control <- vector(mode = "list", length = n)

  # store
  for (i in 1:n){
    # treatment
    sigma.treat[i] <- x[[i]]$sigma.treat
    ar.treat[, i] <- x[[i]]$ar.treat$ar
    tmp.car.treat <- car_prepost(x[[i]]$ar.treat$td, x[[i]]$ar.treat$ar,
                             x[[i]]$sigma.treat)
    car.treat[, i] <- tmp.car.treat$car
    car.treat.se[, i] <- tmp.car.treat$car.se
    dr.treat[[i]] <- x[[i]]$dr.treat
    dr.treat[[i]]$country <- event_country[i]
    dr.treat[[i]]$type <- event_type[[i]]
    dr.treat[[i]]$event_date <- event_date[[i]]
    
    # control
    if (!is.null(x[[i]]$ar.control)){
        sigma.control[i] <- x[[i]]$sigma.control
        ar.control[, i] <- x[[i]]$ar.control$ar
        tmp.car.control <- car_prepost(x[[i]]$ar.control$td, x[[i]]$ar.control$ar, 
                                       x[[i]]$sigma.control)
        car.control[, i] <- tmp.car.control$car
        car.control.se[, i] <- tmp.car.control$car.se
        dr.control[[i]] <- x[[i]]$synth.info$dr
        dr.control[[i]]$country <- event_country[i]
        dr.control[[i]]$type <- event_type[[i]]
        dr.control[[i]]$event_date <- event_date[[i]]
    } 
  }
  dr.treat <- rbindlist(dr.treat)
  dr.treat[, lab := "Treatment"]
  dr.control <- rbindlist(dr.control)
  dr.control[, lab := "Control"]
  dr <- rbind(dr.treat, dr.control, fill = TRUE)
  
  # store
  l <- list(dr = dr, td = td.ew, sigma.treat = sigma.treat, 
            ar.treat = ar.treat, car.treat = car.treat, car.treat_se = car.treat.se,
            sigma.control = sigma.control, ar.control = ar.control, 
            car.control = car.control, car.control.se = car.control.se)
  return(l)
}

# VOLATILITY DATA --------------------------------------------------------------
volatility_data <- function(stockdata, event_ticker, event_date, 
                            date_start = NULL, date_end = NULL<
                            pre_days, post_days){
  n <- length(event_ticker)
  dat <- vector(mode = "list", n)
  for (i in 1:n){
    dat[[i]] <- stockdata[ticker == event_ticker[i], .(ticker, date, dr)]
    dat[[i]] <- calc_td(dat[[i]], 
                        event_date = event_date[i])
    dat[[i]] <- dat[[i]][td >= -pre_days & td <= post_days]
    dat[[i]] <- dat[[i]][complete.cases(dat[[i]])]
    dat[[i]]$event_date <- event_date[i]
    dat[[i]]$event_num <- i
  }
  return(dat)
}