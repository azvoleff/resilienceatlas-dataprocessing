# For full year, set end_day to 365. Leap years are handled automatically.
start_days <- seq(as.Date('2001/1/1'), as.Date('2001/12/1'), by='month')
end_days <- c(start_days[2:length(start_days)] - 1, as.Date('2001/12/31'))
start_days <- as.numeric(format(start_days, '%j'))
end_days <- as.numeric(format(end_days, '%j'))
seasons <- data.frame(start_day=start_days,
                      end_day=end_days,
                      name=month.abb)
stopifnot(sum(seasons[1] < seasons[2]) == nrow(seasons))
write.csv(seasons, file='seasons.csv', row.names=FALSE)
