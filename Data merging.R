
library(readxl)
library(dplyr)
library(lubridate)
library(writexl)

setwd("~/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Aeris GHG Graes")


aeris <- read_excel("Excel Aeris.xlsx")
chambers <- read_excel("chamber_data_complete.xlsx")

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
