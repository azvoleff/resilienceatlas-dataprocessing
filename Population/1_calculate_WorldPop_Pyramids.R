###############################################################################
# Crops GPW population data to cover the spatial extent of each geography.
###############################################################################

source('../0_settings.R')

library(ggplot2)
library(stringr)
library(plyr)
library(foreach)
library(dplyr)

dpi <- 500
width <- 6
height <- 3

d <- read.csv("M:/Vital_Signs/GRP/data/regional/basedata/Horn of Africa/Pop Stats by Age/HornPopMergeFinal.csv")
d$Adjusted <- grepl('_adj$', d$Age_Range_Gender)
d$Age_Range_Gender <- gsub('(^ap00v4_)|(_adj$)', '', d$Age_Range_Gender)
d$Category <- gsub('_', '', str_extract(d$Age_Range_Gender, "^[a-zA-Z0-9]*"))
d$Gender <- gsub('_', '', str_extract(d$Age_Range_Gender, "(_F$)|_M$()"))

filter(d, !(Category %in%c("TOTAL", "WOCBA"))) %>%
    select(COUNTRY, SUM, Category, Gender) -> d_age

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

foreach(country=unique(d_age$COUNTRY)) %do% {
    p <- ggplot(filter(d_age, COUNTRY == country),
           aes(x=Label, y=SUM, fill=Gender)) +
        geom_bar(subset=.(Gender == "Female"), stat="identity") +
        geom_bar(subset=.(Gender == "Male"), stat="identity") +
        scale_y_continuous(labels = function(x) label_formatter(x)) +
        coord_flip() + 
        scale_fill_brewer(palette = "Set1") + 
        xlab("Age Group") + ylab("Population") +
        theme_bw() +
        theme(legend.position=c(.15, .8),
              legend.title=element_blank())
    ggsave(paste0('pop_pyramid_', country, '.png'), p, dpi=dpi, width=width, height=height)
    ggsave(paste0('pop_pyramid_', country, '.eps'), p, dpi=dpi, width=width, height=height)

}

# Make overall Horn pyramid
horn_d <- group_by(d_age, Label, Gender) %>%
    summarise(SUM=sum(SUM))
p <- ggplot(horn_d, aes(x=Label, y=SUM, fill=Gender)) +
    geom_bar(subset=.(Gender == "Female"), stat="identity") +
    geom_bar(subset=.(Gender == "Male"), stat="identity") +
    scale_y_continuous(labels = function(x) label_formatter(x)) +
    coord_flip() + 
    scale_fill_brewer(palette = "Set1") + 
    xlab("Age Group") + ylab("Population") +
    theme_bw() +
    theme(legend.position=c(.15, .8),
          legend.title=element_blank())
ggsave(paste0('pop_pyramid_HornOfAfrica.png'), p, dpi=dpi, width=width, height=height)
ggsave(paste0('pop_pyramid_HornOfAfrica.eps'), p, dpi=dpi, width=width, height=height)
