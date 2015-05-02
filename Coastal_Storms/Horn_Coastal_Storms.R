library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

d <- read.csv("Future_Affects_of_Storm_Surges.csv", skip=6)

d <- filter(d, Country %in% c('Eritrea',
                              'Djibouti',
                              'Ethiopia',
                              'Somalia',
                              'Uganda',
                              'Kenya'))
