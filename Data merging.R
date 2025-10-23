
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)
library(data.table)

# I removed this as this path only works on your (Lorenzos) computer. 
# setwd("~/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Aeris GHG Graes")

# General recommendations: 
# have separate scripts for N2O and CH4 data. 

# clear workspace
rm(list = ls())


aeris <- read_excel("Excel Aeris.xlsx")
da <- as.data.table(aeris)

dc <- fread("chamber_data_complete.csv")
setnames(dc, as.character(dc[1, ]))
dc <- dc[-1, ]


# splitting dc in two data.tables, one for N2O and one for CH4 
dc.n2o <- dc[! is.na(start.N2O), ]
dc.ch4 <- dc[! is.na(start.CH4), ]

# correcting Aeris time
# N2O aeris is 1 hour behind real time
da[ , Datatime := Datatime + 1 * 3600]

# making a date.time column for mearing, rounded to nearest minute
da[, date.time := as.POSIXct(round(Datatime, "mins"))]






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
