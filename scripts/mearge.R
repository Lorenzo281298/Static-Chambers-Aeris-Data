
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

# calculate elapsed time pr measurement
# Making elapsed.time fit with the first measurement of each valve = 0
dt[, elapsed.time := difftime(Datatime, min(Datatime), units = "secs"), by = .(chamber, ID)]
dt$elapsed.time <- as.numeric(dt$elapsed.time)
