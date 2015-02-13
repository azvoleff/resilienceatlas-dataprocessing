source('../0_settings.R')

library(dplyr)
library(ggplot2)
library(scales)

spi <- brick(file.path(prefix, "CHIRPS", "TEAM_monthly", 
                             "monthly_BIF_SPI_12.tif"))

# Select the start and end dates for the data to include in this analysis
start_date <- as.Date('1981/1/1') # Inclusive
end_date <- as.Date('2014/4/1') # Exclusive
dates <- seq(start_date, end_date, by='months')
spi_period <- 12

spi_ts <- data.frame(obsdate=dates, spi=cellStats(spi, "mean"), 
                     period=spi_period)

# Filter to only include valid periods
p1 <- ggplot(spi_ts) +
    theme_bw() +
    geom_line(aes(obsdate, spi), colour="blue") +
    xlab('Year') + ylab(expression('Std. Precip. Index')) +
    scale_x_date(breaks=c(as.Date('1990/1/1'), as.Date('2000/1/1'),
                          as.Date('2010/1/1')), labels=date_format("%Y"))
ggsave("Bwindi_spi12_meantimeseries.png", p1, width=4, height=2, dpi=PLOT_DPI)
ggsave("Bwindi_spi12_meantimeseries.eps", p1, width=4, height=2, dpi=PLOT_DPI)
