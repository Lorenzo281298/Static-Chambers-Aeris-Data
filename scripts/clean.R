
# Chamber data
setnames(dc, as.character(dc[1, ]))
dc <- dc[-1, ]
dc$start.N2O <- as.numeric(dc$start.N2O)
dc$end.N2O <- as.numeric(dc$end.N2O)

# selecting dc data for N2O 
dc <- dc[! is.na(start.N2O), c('chamber', 'ID', 'begin.time', 'final.time', 'start.N2O', 'end.N2O')]


# Aeris data
da <- as.data.table(da)

setnames(da, old = c('CO2 (ppm)', 'H2O (ppm)', 'N2O (ppm)'), 
         new = c( 'CO2', 'H2O', 'N2O'))

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