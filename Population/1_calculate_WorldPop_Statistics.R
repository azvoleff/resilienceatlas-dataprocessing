###############################################################################
# Crops GPW population data to cover the spatial extent of each geography.
###############################################################################

source('../0_settings.R')

library(ggplot2)
library(stringr)
library(plyr)
library(foreach)
library(gridExtra)
library(dplyr)
library(RColorBrewer)

dpi <- 500
width <- 6
height <- 3

d <- foreach(year=c(2000, 2005, 2010, 2015), .combine=rbind) %do% {
    read.csv(paste0("M:/Vital_Signs/GRP/data/regional/basedata/Horn of Africa/Population Data/HornOfAfrica_", year, "Population.csv"))
}
d$COUNTRY <- ordered(d$COUNTRY, levels=sort(unique(d$COUNTRY)))

d$year <- as.numeric(paste0('20', gsub('(^ap)|(v4_$)', '', str_extract(d$VARIABLE, '^ap[01][05]v4_'))))
d$Adjusted <- grepl('_adj$', d$VARIABLE)
d$VARIABLE <- gsub('(^ap[01][05]v4_)|(_adj$)', '', d$VARIABLE)
d$Category <- gsub('_', '', str_extract(d$VARIABLE, "^[a-zA-Z0-9]*"))
d$Gender <- gsub('_', '', str_extract(d$VARIABLE, "(_F$)|_M$()"))

###############################################################################
# Make age plots
filter(d, !(Category %in%c("TOTAL", "WOCBA"))) %>%
    select(COUNTRY, year, SUM, Category, Gender) -> d_age

d_age$lower <- as.numeric(gsub('A', '', str_extract(d_age$Category, "[0-9]{2}")))
d_age$upper <- as.numeric(str_extract(d_age$Category, "[0-9]{2}$"))
d_age$upper[is.na(d_age$upper)] <- "+"

select(d_age, -Category) -> d_age

d_age$Label <- paste(d_age$lower, d_age$upper, sep='-')
d_age$Label[d_age$Label == "65-+"] <- "65+"
d_age$Label <- ordered(d_age$Label, levels=unique(d_age$Label))

d_age$Gender <- factor(d_age$Gender, levels=c("M", "F"),
                       labels=c("Male", "Female"))

# Make male population negative show they show on left of pyramid
d_age$SUM[d_age$Gender == "Male"] <- -1 * d_age$SUM[d_age$Gender == "Male"]

label_formatter <- function(x) {
    if (max(x, na.rm=TRUE) > 1e6) {
        x <- paste0((abs(x / 1e6)), "m")
    } else if (max(x, na.rm=TRUE) > 1e4) {
        x <- paste0((abs(x / 1e3)), "k")
    } else {
        x <- abs(x, na.rm=TRUE)
    }
    x <- gsub('^0((k)|(m))$', 0, x)
    format(x)
}

ps <- foreach(country=levels(d_age$COUNTRY)) %do% {
    p <- ggplot(filter(d_age, COUNTRY == country, year == 2015),
           aes(x=Label, y=SUM, fill=Gender)) +
        geom_bar(subset=.(Gender == "Female"), stat="identity") +
        geom_bar(subset=.(Gender == "Male"), stat="identity") +
        scale_y_continuous(labels = function(x) label_formatter(x)) +
        coord_flip() + 
        scale_fill_brewer(palette = "Paired") + 
        xlab("Age Group") + ylab("Population") +
        theme_bw(base_size=8) +
        ggtitle(country) +
        guides(fill=FALSE) +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.y=element_blank())
    # ggsave(paste0('pop_pyramid_', country, '.png'), p, dpi=dpi, width=width, height=height)
    # ggsave(paste0('pop_pyramid_', country, '.eps'), p, dpi=dpi, width=width, height=height)
    return(p)
}

p <- arrangeGrob(ps[[1]], ps[[2]], ps[[3]], ps[[4]], ps[[5]], ps[[6]], ncol=length(ps))
ggsave(paste0('pop_pyramid_HornCountries.png'), p, dpi=300, width=10, height=2)
ggsave(paste0('pop_pyramid_HornCountries.eps'), p, dpi=300, width=10, height=2)

# Make overall Horn pyramid
horn_d <- group_by(d_age, Label, Gender) %>%
    summarise(SUM=sum(SUM))
p <- ggplot(horn_d, aes(x=Label, y=SUM, fill=Gender)) +
    geom_bar(subset=.(Gender == "Female"), stat="identity") +
    geom_bar(subset=.(Gender == "Male"), stat="identity") +
    scale_y_continuous(labels = function(x) label_formatter(x)) +
    coord_flip() + 
    scale_fill_brewer(palette = "Paired") + 
    xlab("Age Group") + ylab("Population") +
    theme_bw() +
    theme(legend.position=c(.15, .8),
          legend.title=element_blank())
ggsave(paste0('pop_pyramid_HornOfAfrica.png'), p, dpi=dpi, width=width, height=height)
ggsave(paste0('pop_pyramid_HornOfAfrica.eps'), p, dpi=dpi, width=width, height=height)

###############################################################################
# Make pop growth plots
filter(d, Category %in% "TOTAL", Adjusted == TRUE) %>%
    select(COUNTRY, year, SUM, Category) -> d_total

group_by(d_total, COUNTRY) %>%
    arrange(year) %>%
    mutate(pop_start=lag(SUM),
           pop_end=SUM,
           growth=c(NA, diff(SUM)),
           period=paste(lag(year), year, sep="-")) %>%
    filter(!is.na(growth)) %>%
    select(-SUM) -> pop_growth
# Calculate growth rate on an annualized basis
pop_growth$growth_rate <- ((pop_growth$pop_end / pop_growth$pop_start)^(1/5) - 1) * 100

pop_growth$period <- factor(pop_growth$period)

p <- ggplot(pop_growth, aes(x=as.integer(period), y=growth_rate, 
                            colour=COUNTRY, shape=COUNTRY)) +
    theme_bw(base_size=10) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=1:length(levels(pop_growth$period)),
                       labels=levels(pop_growth$period)) +
    scale_colour_manual(values=rep(brewer.pal(3, "Paired"), 2)) +
    xlab("Period") + ylab("Annual rate of population growth") +
    ylim(c(0, 4.5)) +
    theme(legend.title=element_blank(),
          legend.key.size=unit(1.5, "cm"),
          legend.key.height=unit(.5, "cm"))
ggsave(paste0('pop_growth_HornOfAfrica.png'), p, dpi=dpi, width=width, height=height*1.5)
ggsave(paste0('pop_growth_HornOfAfrica.eps'), p, dpi=dpi, width=width, height=height*1.5)
