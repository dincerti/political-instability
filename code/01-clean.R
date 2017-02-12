rm(list = ls())
library("data.table")
library("chron") # day.of.week command

# LOAD DATA --------------------------------------------------------------------
# ticker id's
country.lookup <- read.csv("data-raw/country_lookup.csv")

# daily stock returns
# countries
f <- list.files("data-raw/indices")
indices <- lapply(f, function (x) fread(paste0("data-raw/indices/", x)))
indices <- do.call("rbind", indices)
indices[, Period_Change := NULL]
setnames(indices, c("date","ticker","p","dr","real_p"))

# regime changes
event <- fread("data-raw/event_list.csv")

# leader duration
leaders <- fread("data-raw/leaders_duration.csv")

# revolution dates
rev <- fread("data-raw/revolutions.csv")
rev[, num := seq(1, length(country), 1)]
rev[, start_date := as.Date(start_date,"%m/%d/%Y")]
rev[, end_date := as.Date(end_date,"%m/%d/%Y")]

# CLEAN DATA -------------------------------------------------------------------
# inflation adjusted returns
#indices[, dr := (real_p - shift(real_p))/shift(real_p), by = "ticker"]

## continuously compounded returns
indices[, dr := 100 * log(dr+1)]
indices[, abs_dr := abs(dr)]

## dates 
indices[, date := as.Date(date,"%m/%d/%Y")]
indices[, month := as.numeric(format(date, format = "%m"))]
indices[, day := as.numeric(format(date, format = "%d"))]
indices[, year := as.numeric(format(date, format = "%Y"))]
indices[, dow := day.of.week(month, day, year)]
event[, stock_date := as.Date(stock_date,"%m/%d/%Y")]

## market indices
ticker.market <- c("_MIWO00D", "_IPDCPD", "_A31", "_P31")
index.market <- indices[ticker %in% ticker.market, 
                        .(date, ticker, dow, p, dr, real_p, abs_dr)]

# use friday market indices for countries that trade on sunday's. note that
# i am only doing this for emerging market and world indices
index.market.0 <- index.market[dow == 5 & ticker %in% c("_MIWO00D", "_IPDCPD")]
index.market.0[, date := date + 2] 
index.market <- rbind(index.market.0, index.market)
index.market[, dow := NULL]

# wide form 
index.market <- melt(index.market, id.vars = c("date", "ticker"))
index.market <- dcast.data.table(index.market, date  ~ ticker + variable, value.var = "value")
market.names <- gsub("_MIWO00D", "msci_wi", colnames(index.market))
market.names <- gsub("_IPDCPD", "emerg", market.names)
market.names <- gsub("_A31", "latamer", market.names)
market.names <- gsub("_P31", "seasia", market.names)
setnames(index.market, market.names)

## country indices
index <- indices[!ticker %in% ticker.market]
index <- merge(index, index.market, all.x = TRUE, by = "date")
index <- index[order(ticker, date)]
index[, td := seq(1, .N), by = "ticker"]

# regime change
regime.change <- event[type == "Coup" | type == "Assassination" |
                         type == "Resignation", 
                       .(country, ticker, stock_date, type)]

# SAVE DATA --------------------------------------------------------------------
save(country.lookup, event, index, regime.change, rev,
     file = "data/data-clean.RData")
