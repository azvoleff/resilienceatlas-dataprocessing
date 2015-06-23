source('../0_settings.R')

library(tools)
library(dplyr)
library(reshape2)

countries <- read.csv(file.path(prefix, "GRP", "DataTables", 
                                "GRP_Countries.csv"), stringsAsFactors=FALSE)

d <- read.csv(file.path(prefix, "GRP", "GlobalPeaceIndex", 
                        'GPI_Scores_2008-2014_reformatted.csv'))
names(d)[names(d) == 'ISO_3'] <- 'ISO3'

countries$ISO3[!(countries$ISO3 %in% d$ISO3)]

d <- reshape(d, idvar=c('ISO3', 'Country'), varying=c(3:ncol(d)), sep='_',
             direction='long', timevar='YEAR')

d <- filter(d, ISO3 %in% countries$ISO3)
d <- select(d, -Country)

write.csv(d,file.path(prefix, "GRP", "GlobalPeaceIndex", 
                      'GPI_Scores_longformat.csv'), row.names=FALSE)

