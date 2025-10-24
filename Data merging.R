
# clear workspace
rm(list = ls())

library(readxl)
library(dplyr)
library(lubridate)
library(writexl)
library(data.table)

# I removed this as this path only works on your (Lorenzos) computer. 
# setwd("~/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Aeris GHG Graes")

# General recommendations: 
# have separate scripts for N2O and CH4 data. 


aeris <- read_excel("Excel Aeris.xlsx")
da <- as.data.table(aeris)

dc <- fread("chamber_data_complete.csv")
setnames(dc, as.character(dc[1, ]))
dc <- dc[-1, ]


# splitting dc in two data.tables, one for N2O and one for CH4 

dc$start.N2O <- as.numeric(dc$start.N2O)
dc$end.N2O <- as.numeric(dc$end.N2O)

dc <- dc[! is.na(start.N2O), ]

# Lorenzo, I am unsure if the data there is in the dc is Aeries time or 'real time'? 
# if it is Aeris time then the next line correting the time should not be done
# here, but you should correct time time after mearging and before including 
# temperature data. 

# correcting Aeris time
# N2O aeris is 1 hour behind real time
da[ , date.time := Datatime + 1 * 3600]

# making a date.time column for mearing, rounded to nearest minute
da[, begin.time := as.POSIXct(round(Datatime, "mins"), tzone = 'UTC')]
da[, final.time := as.POSIXct(round(Datatime, "mins"), tzone = 'UTC')]

# change dc begin time to same format as date.time in da
dc[, begin.time := as.POSIXct(begin.time, format = "%d-%m-%Y %H:%M", tzone = 'UTC')]
dc[, final.time := as.POSIXct(final.time, format = "%d-%m-%Y %H:%M", tzone = 'UTC')]

dt <- merge(da, dc, by = "begin.time", all.x = TRUE)
dt <- merge(dt, dc, by = "final.time", all.x = TRUE)



# 251024, code below not used, code below not done JP

# Create a column Datetime in the new file
aeris <- aeris %>%
  mutate(Datetime = parse_date_time(paste(Day, Hour), orders = c("mdy HMS", "ymd HMS", "dmy HMS", "mdy HM", "ymd HM", "dmy HM")))

# Associate Chamber and ID according to the time interval
aeris <- aeris %>%
  rowwise() %>%
  mutate(
    match = list(
      chambers %>%
        filter(Datetime >= Begin_time & Datetime <= Final_time)
    ),
    Chamber = ifelse(length(match$Chamber) > 0, match$Chamber[1], NA),
    ID = ifelse(length(match$ID) > 0, match$ID[1], NA)
  ) %>%
  select(-match) %>%
  ungroup()

# Save the file
write_xlsx(aeris, "Aeris_con_chamber_ID.xlsx")
