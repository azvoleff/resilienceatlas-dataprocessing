source('../0_settings.R')

library(dplyr)
library(ggplot2)

forest <- read.csv(file.path(prefix, "GFC_TEAM_Analysis_Lydia", "wgs84_75pct", 
                             "BIF_gfcextract_wgs84_stats_loss.csv"))

forest$loss_km2 <- forest$loss * .01

forest <- group_by(forest, aoi) %>%
    mutate(cum_loss_km2=c(NA, cumsum(loss_km2[2:length(loss)])))

p1 <- ggplot(filter(forest, aoi %in% c("ZOI_0", "PA_0"))) +
        theme_bw() +
        geom_line(aes(year, cum_loss_km2, colour=aoi)) +
        xlab('Year') + ylab(expression("Total forest loss (km"^2*")")) +
        scale_x_continuous(breaks=c(2000, 2004, 2008, 2012)) +
        scale_colour_discrete("Region", breaks=c("PA_0", "ZOI_0"),
                              labels=c("Protected Area", "ZOI")) +
        theme(legend.position="bottom")
ggsave('Bwindi_forest_loss_km2.png', p1, width=5, height=3, dpi=PLOT_DPI)
ggsave('Bwindi_forest_loss_km2.eps', p1, width=5, height=3, dpi=PLOT_DPI)
