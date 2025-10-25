
# plotting all
# adding chamber number data to HMRds
dt$date <- as.Date(dt$date.time)
unique_dates <- as.character(unique(dt$date))

for (d in unique_dates){
  df_subset <- dt[dt$date == d, ]
  
  p <- ggplot(df_subset, aes(elapsed.time, N2O)) + 
    geom_point() + 
    facet_wrap(~ chamber, scales = 'free') + 
    ggtitle(paste('N2O on', d)) +
    theme_bw()
  ggsave(filename = paste0('../plots/N2O check free y/N2O_free_y_', d, '.png'), plot = p, width = 10, height = 10)
}

for (d in unique_dates){
  df_subset <- dt[dt$date == d, ]
  
  p <- ggplot(df_subset, aes(elapsed.time, N2O)) + 
    geom_point() + 
    facet_wrap(~ chamber, scales = 'free_x') + 
    ggtitle(paste('N2O on', d)) +
    theme_bw()
  ggsave(filename = paste0('../plots/N2O check fixed y/N2O_fixed_y_', d, '.png'), plot = p, width = 10, height = 10)
}

for (d in unique_dates){
  df_subset <- dt[dt$date == d, ]
  
  p <- ggplot(df_subset, aes(elapsed.time, CO2)) + 
    geom_point() + 
    facet_wrap(~ chamber, scales = 'free') + 
    ggtitle(paste('CO2 on', d)) +
    theme_bw()
  ggsave(filename = paste0('../plots/CO2 check/CO2_free_y_', d, '.png'), plot = p, width = 10, height = 10)
}
