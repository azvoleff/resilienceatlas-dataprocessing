library(ggplot2)
library(stringr)
library(dplyr)
library(reshape2)
library(lubridate)
library(lme4)

source('../0_settings.R')

###############################################################################
# Load agb_data
agb <- read.csv('CarbonAllSitesAllYears.csv')
agb$Cstorage <- (agb$Cstorage * 2) / 1000 # Puts in units of Mg / ha
agb$sitecode <- str_extract(agb$Plot, '[A-Z]{2,3}')
agb$year <- as.numeric(str_extract(agb$Plot, '[0-9]{4}'))
agb$plot_num <- as.numeric(str_extract(agb$Plot, '[0-9]$'))
agb$plot_ID <- paste0('VG', agb$sitecode, agb$plot_num)
agb <- agb[with(agb, order(sitecode, plot_num, year)), ]

agb_chg <- mutate(group_by(agb, sitecode, plot_num),
                  Cstorage_chg=c(NA, diff(Cstorage)))
agb_chg <- agb_chg[complete.cases(agb_chg), ]

agb_chg <- filter(agb_chg, sitecode == "BIF")

p1 <- group_by(agb_chg, year) %>%
    summarise(mean_agb=mean(Cstorage)) %>%
    ggplot() + geom_line(aes(year, mean_agb)) +
    theme_bw() +
    xlab('Year') + ylab(expression('Carbon Storage'))
ggsave("Bwindi_ccstorage_meantimeseries.png", p1, width=4, height=2, dpi=PLOT_DPI)
ggsave("Bwindi_ccstorage_meantimeseries.eps", p1, width=4, height=2, dpi=PLOT_DPI)
