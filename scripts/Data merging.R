
# clear workspace
rm(list = ls())

library(readxl)
library(dplyr)
library(lubridate)
library(writexl)
library(data.table)

# I removed this as this path only works on your (Lorenzos) computer. 
# setwd("~/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Aeris GHG Graes")

# loading data
da <- read_excel("Excel Aeris.xlsx")
dc <- fread("chamber_data_complete.csv")

# cleaning data
setnames(dc, as.character(dc[1, ]))
dc <- dc[-1, ]
dc$start.N2O <- as.numeric(dc$start.N2O)
dc$end.N2O <- as.numeric(dc$end.N2O)

# selecting dc data for N2O 
dc <- dc[! is.na(start.N2O), c('chamber', 'ID', 'begin.time', 'final.time', 'start.N2O', 'end.N2O')]

da <- as.data.table(da)

# Lorenzo, I am unsure if the data there is in the dc is Aeries time or 'real time'? 
# if it is Aeris time then the next line correting the time should not be done
# here, but you should correct time time after mearging and before including 
# temperature data. 

# correcting Aeris time
# N2O aeris is 1 hour behind real time
da[ , date.time := Datatime + 1 * 3600]

# making a date.time column for mearing, rounded to nearest minute
da[, date.time := as.POSIXct(round(Datatime, "mins"), tz = 'UTC')]


# change dc begin time to same format as date.time in da
dc[, begin.time := as.POSIXct(begin.time, format = '%d-%m-%Y %H:%M', tz = 'UTC')]
dc[, final.time := as.POSIXct(final.time, format = '%d-%m-%Y %H:%M', tz = 'UTC')]

# adding rows with chamber, id and N2O info for between begin.time and final.time
# sequence function for one row
safe_seq <- function(from, to) {
  # if to < from, return NA (or return reversed/empty based on preference)
   if (to < from) return(as.POSIXct(NA_real_))
  seq(from, to, by = '1 min')
}

# Expand row-by-row. Use rowid to ensure seq() gets scalars.
dc[, rowid := .I]

dcl <- dc[, {dcthis <- safe_seq(begin.time[1], final.time[1])
    .(date.time = dcthis)},
  by = .(rowid, chamber, ID, start.N2O, end.N2O)]

# drop helper rowid
dcl[, rowid := NULL]

# challenge that we have some measurements that start and stop on the same times.stamp
# because the resolution is minutes, not seconds.... 
# the times we have dublicates: 
dcl[duplicated(date.time), unique(date.time)]

# making a full mearge, mening that when we have the same date.time for start and stop
# it will be dublicated
dt <- merge(da, dcl, by = "date.time", all.x = TRUE, allow.cartesian = TRUE)

# dropping Aeris data when not measured on chamber: 
dt <- dt[!is.na(start.N2O)]


# change header in da

# calculate elapsed time pr measurement in minutes

# plotting all
# adding chamber number data to HMRds
dt$date <- as.Date(dt$date.time)
unique_dates <- unique(dt$date)

for (d in unique_dates){
  df_subset <- dt[dt$date == d, ]

p <- ggplot(df_subset, aes(elapsed.time, n2o)) + 
    geom_point() + 
    facet_wrap(~ chamber, scales = 'free_y') + 
    ggtitle(paste('N2O on', d)) +
    theme_bw()
ggsave(filename = paste0('../plots/N2O check/N2O_free_y_', d, '.png'), plot = p, width = 10, height = 10)
}



# # Create a column Datetime in the new file
# aeris <- aeris %>%
#   mutate(Datetime = parse_date_time(paste(Day, Hour), orders = c("mdy HMS", "ymd HMS", "dmy HMS", "mdy HM", "ymd HM", "dmy HM")))
# 
# # Associate Chamber and ID according to the time interval
# aeris <- aeris %>%
#   rowwise() %>%
#   mutate(
#     match = list(
#       chambers %>%
#         filter(Datetime >= Begin_time & Datetime <= Final_time)
#     ),
#     Chamber = ifelse(length(match$Chamber) > 0, match$Chamber[1], NA),
#     ID = ifelse(length(match$ID) > 0, match$ID[1], NA)
#   ) %>%
#   select(-match) %>%
#   ungroup()
# 
# # Save the file
# write_xlsx(aeris, "Aeris_con_chamber_ID.xlsx")
