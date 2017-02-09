rm(list = ls())
library("data.table")

# LOAD DATA --------------------------------------------------------------------
# ticker id's
country.lookup <- read.csv("data/country_lookup.csv")

# daily stock returns
# countries
f <- list.files("data/indices")
indices <- lapply(f, function (x) fread(paste0("data/indices/", x)))
indices <- do.call("rbind", indices)
indices[, Period_Change := NULL]
setnames(indices, c("date","ticker","p","dr","real_p"))

# regime changes
event <- fread("data/event_list.csv")

# leader duration
leaders <- fread("data/leaders_duration.csv")

# revolution dates
rev <- fread("data/revolutions.csv")
rev[, num := seq(1, length(Country), 1)]