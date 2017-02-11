# xtable print -----------------------------------------------------------------
myprint.xtable <- function(x, file){
  print(xtable(x), 
        include.rownames = FALSE, include.colnames = FALSE,
        only.contents = TRUE, sanitize.text.function = identity, hline.after = NULL,
        file = file)
}

# MATRIX WITH PARENTHESES IN STANDARD ERRORS -----------------------------------
matrix_se <- function(est, se){
  m <- matrix(as.vector(rbind(as.vector(est), 
                              paste0("(", as.vector(se), ")"))),
              nrow= 2 * nrow(est))
  return(m)
}

# DAILY RETURNS BY TRADING DAY -------------------------------------------------
return_by_td <- function(stockdata, event_date, pre_event, post_event){
  # Note: first column of stockdata must be a date variable
  setnames(stockdata, c("date", "r"))
  
  # trading days
  stockdata[, n := seq(1, .N)]
  event.td <- which.max(stockdata$date - event_date >= 0) # first trading day on or after event
  stockdata[, td := n - event.td]
  
  # trading window surrounding event
  stockdata <- stockdata[td >= -pre_event & td <= post_event]
  
  # deal with countries with insufficient observations
  if (min(stockdata$td) > - pre_event){
    tmp <- data.table(matrix(NA, nrow = pre - abs(min(stockdata$td)), 
                             ncol = ncol(stockdata)))
    setnames(tmp, colnames(stockdata))
    tmp$td <- seq(-pre, min(stockdata$td) - 1)
    tmp$date <- as.Date(tmp$date)
    stockdata <- rbind(tmp, stockdata)
  }
  return(stockdata$r)
}

# DAYS TO REBOUND --------------------------------------------------------------
days_to_rebound <- function(stockdata, event_date){
  # Note: first column of stockdata is a date and second is price
  x = copy(stockdata)
  setnames(x, c("date", "p"))
  
  # trading days
  x[, n := seq(1, .N)]
  event.td <- which.max(x$date - event_date >= 0) 
  x[, td := n - event.td]
  
  # days to rebound
  p.init <- x[td == -1, p]
  if (x[td == 0, p]/p.init < 1){
    return(which.max(x[td >= 0, p] >= p.init))
  } else{
    return(NA)
  }
}

# EVENT STUDY ------------------------------------------------------------------
event_study <- function(stockdata, event_window, estimation_window, event_date,
                       model = "constant"){
  # Note: first column of stockdata must be a date variable
  setnames(stockdata, c("date", "r"))
  
  # trading days (i.e. time between event date and stock date)
  stockdata[, n := seq(1, .N)]
  event.td <- which.max(stockdata$date - event_date >= 0) 
  stockdata[, td := n - event.td]
  
  # model
  if (model == "constant"){
    lm <- lm(r ~ 1, stockdata[td >= -estimation_window & td < -event_window]) 
  } else if (model == "market"){
    print ("market")
  }
  
  # abnormal returns
  event.data <- stockdata[td >= -event_window & td < event_window, .(td, r)]
  ar <- event.data$r - predict(lm, event.data)
  sigma <- summary(lm)$sigma
  return(list(ar = ar, sigma = sigma))
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
Car <- function(td, ar, sigma){
  car.b <- rev(cumsum(rev(ar[which(td < 0)])))
  car.b.se <- rev(sigma * sqrt(seq(1, length(car.b))))
  car.f <- cumsum(ar[which(td >= 0)])
  car.f.se <- sigma * sqrt(seq(1, length(car.f)))
  car <- c(car.b, car.f)
  car.se <- c(car.b.se, car.f.se)
  return(list(car = car, car.se = car.se))
}
  
