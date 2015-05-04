library(dplyr)
library(rgdal)
library(reshape2)

source('../0_settings.R')

load(file.path(prefix, "GRP", 
               "Resilience_Indicator","resil_raw_DHS_data.RData"))

key <- data.frame(Indicator=unique(indic$Indicator))
indic$IndicatorShort <- gsub('[ ,.-]', '', indic$Indicator)
indic$IndicatorShort <- gsub('Household', 'HH', indic$IndicatorShort)
indic$IndicatorShort <- gsub('possessing', 'Pss', indic$IndicatorShort)
indic$IndicatorShort <- abbreviate(indic$IndicatorShort, 10)
key$IndicatorShort <- indic$IndicatorShort[match(key$Indicator, indic$Indicator)]

write.csv(key, file='DHS_indicator_variable_key.csv', row.names=FALSE)

indic <- dcast(indic, RegionId ~ IndicatorShort, value.var='Value')
indic <- tbl_df(indic)

dhs_regions <- readOGR(file.path(prefix, "GRP", "DHS"), "DHS_Regions_HornOfAfrica")

dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% indic$RegionId, ]
indic <- indic[indic$RegionId %in% dhs_regions$REG_ID, ]

indic <- rename(indic, REG_ID=RegionId)
dhs_regions@data <- left_join(dhs_regions@data, indic, by='REG_ID')

writeOGR(dhs_regions, file.path(prefix, "GRP", "DHS"), 
         "DHS_Regions_HornOfAfrica_merged", driver="ESRI Shapefile", 
         overwrite=TRUE)

##############
# Health
# Children stunting
spplot(dhs_regions, 'Chldrnstnt')

# Total fertility rate
spplot(dhs_regions, 'Ttlfrt1549')

# Infant mortality rate
spplot(dhs_regions, 'Infntmrtlt')

##############
# Education

# Access to mass media
spplot(dhs_regions, 'Wmnwthccss')

# Women who are literate
spplot(dhs_regions, 'Wmnwhrltrt')

