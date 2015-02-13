source('../0_settings.R')

library(dplyr)
library(ggplot2)
library(scales)

wpi <- read.csv("Bwindi Impenetrable ForestAllspecies_dataonly.csv")

# Filter to only include valid periods
p1 <- ggplot(filter(wpi, YEAR >= 2009, YEAR <=2012), aes(x=YEAR, y=WPI)) +
    theme_bw() +
    geom_line(colour="blue") +
    geom_ribbon(aes(ymin=WPI_LO, ymax=WPI_HI), alpha=.25, fill="blue") +
    ylim(0, 3) +
    xlab('Year') + ylab(expression('Wildlife Picture Index'))
ggsave("Bwindi_wpi_meantimeseries.png", p1, width=4, height=2, dpi=PLOT_DPI)
ggsave("Bwindi_wpi_meantimeseries.eps", p1, width=4, height=2, dpi=PLOT_DPI)
