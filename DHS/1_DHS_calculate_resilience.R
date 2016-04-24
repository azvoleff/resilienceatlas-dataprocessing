library(rgdal)
library(raster)
library(dplyr)
library(rgeos)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

source('../0_settings.R')

# TODO: Add in gridded data
# load(file.path(prefix, "GRP", "Resilience_Indicator", 
#                "dhs_regions_merged_with_Justin_data.RData"))
#
# gis_data <- dhs_regions@data
# gis_data <- gis_data[c("REG_ID", "maize_per_person", "calories_per_person")]
# gis_data <- tbl_df(gis_data)
#
# dim(indic)
# indic <- left_join(indic, gis_data, by=list("RegionId"="REG_ID"))
# dim(indic)

# TODO: Ensure all indicators are in regional format

load(file.path(prefix, "GRP", 
               "Resilience_Indicator","resil_raw_DHS_data.RData"))

# Calculate quantiles
probs <- seq(0, 1, .2)
# Note the subtraction of 1 in calaculation of value quantile is so that the 
# final scores will start from 0 after normalization
indic <- group_by(indic, IndicatorId) %>%
    mutate(ValueQuantile=as.numeric(cut(Value,
                                        quantile(Value, probs, na.rm=TRUE), 
                                        include.lowest=TRUE)) - 1) %>%
    group_by(RegionId, Facet) %>%
    mutate(NumValidValue=n() - sum(is.na(Value)))

# Reverse quantiles where high numbers are BAD
rev_index <- function(x) {
    max(x) - x
}
unique(indic$Indicator)
stunt_rows <- indic$Indicator == "Children stunted"
#indic[stunt_rows,]$ValueQuantile
indic$ValueQuantile[stunt_rows] <- rev_index(indic$ValueQuantile[stunt_rows])
imr_rows <- indic$Indicator == "Infant mortality rate"
indic$ValueQuantile[imr_rows] <- rev_index(indic$ValueQuantile[imr_rows])

# group_by(filter(indic, RegionGRP %in% c("Horn of Africa", "Sahel", "Southeast Asia")),
#                 RegionGRP, CountryName, Facet, Indicator) %>%
#     summarise(mean_quantile=mean(ValueQuantile)) %>%
#     ggplot()  +
#     geom_bar(aes(Indicator, mean_quantile, group=CountryName, fill=RegionGRP), position="dodge", 
#              stat="identity") +
#     facet_wrap(~ Facet, scales="free", drop=TRUE, nrow=3)

resil <- group_by(indic, DHS_CountryCode, CharacteristicLabel, CountryName, RegionGRP, RegionId, Facet) %>%
    summarise(Score=sum(ValueQuantile) / (NumValidValue[1]*(length(probs) - 2)) * 10)

# data.frame(select(filter(indic, DHS_CountryCode == "ET"), Indicator, 
# CharacteristicLabel, Value, ValueQuantile, NumValidValue))
#
# data.frame(filter(resil, DHS_CountryCode == "ET"))

# group_by(resil, CountryName, Facet) %>%
#     summarize(mean_score=mean(Score)) %>%
#     ggplot() +
#     geom_bar(aes(CountryName, mean_score, fill=Facet), position="dodge", stat="identity") +
#     xlab("Country") + ylab("Score") + ylim(0, 10) +
#     scale_fill_discrete("Facet of resilience")

resil$Facet <- factor(resil$Facet,
                      levels=c('Buffer capacity', 'Self-organization', 
                               'Capacity for learning'),
                      labels=c('BuffCap', 'SelfOrg', 'CapLearn'))

resil_wide <- dcast(resil, CountryName + RegionGRP + RegionId ~ Facet)

load(file.path(prefix, "GRP", "Resilience_Indicator", 
               "dhs_regions_merged_with_Justin_data.RData"))

dhs_regions$RegionId_short <- gsub('^[A-Z]{5}[0-9]{4}', '', dhs_regions$REG_ID)
resil_wide$RegionId_short <- gsub('^[A-Z]{5}[0-9]{4}', '', resil_wide$RegionId)

# table(resil_wide$RegionId %in% dhs_regions$REG_ID)
# resil[!(resil$RegionId %in% dhs_regions$REG_ID), ]
#
# table(resil_wide$RegionId_short %in% dhs_regions$RegionId_short)
# resil_wide[!(resil_wide$RegionId_short %in% dhs_regions$RegionId_short), ]

# Filter GIS dataset to only include regions that are in the resilience dataset
dhs_regions <- dhs_regions[which(dhs_regions$RegionId_short %in% resil_wide$RegionId_short), ]
resil_wide <- resil_wide[which(resil_wide$RegionId_short %in% dhs_regions$RegionId_short), ]

# Ensure the datasets match
stopifnot(nrow(dhs_regions) == nrow(resil_wide))
stopifnot(length(unique(dhs_regions$RegionId_short)) == nrow(dhs_regions))

# Order the datasets in the same way
resil_wide <- resil_wide[order(resil_wide$RegionId_short), ]
dhs_regions <- dhs_regions[order(dhs_regions$RegionId_short), ]

dhs_regions@data <- dhs_regions@data['ISO']

# Add columns from resil_wide
dhs_regions$CountryName <- resil_wide$CountryName
dhs_regions$RegionGRP <- resil_wide$RegionGRP
dhs_regions$RegionId <- resil_wide$RegionId
dhs_regions$SelfOrg <- resil_wide$SelfOrg
dhs_regions$CapLearn <- resil_wide$CapLearn
dhs_regions$BuffCap <- resil_wide$BuffCap

dhs_regions@data[100,]

#dhs_regions@data[dhs_regions@data$CountryName == "Ethiopia", ]

# dhs_regions$maize_per_person <- as.numeric(dhs_regions$maize_per_person)
# dhs_regions$calories_per_person <- as.numeric(dhs_regions$calories_per_person)
writeOGR(dhs_regions, file.path(prefix, "GRP", "Resilience_Indicator"), 
         "resil_indicator", driver="ESRI Shapefile", overwrite=TRUE)

# display.brewer.all(n=10, exact.n=FALSE)

png('global_resilience_map.png', width=1500, heigh=1200)
print(spplot(dhs_regions, c('BuffCap', 'CapLearn', 'SelfOrg'),
       col.regions=brewer.pal(10, "PuOr"), at=seq(0, 10, 2),
       par.settings=list(fontsize=list(text=20))))
dev.off()

for (region in unique(dhs_regions$RegionGRP)) {
    these_regions <- dhs_regions[which(dhs_regions$RegionGRP == region), ]
    png(paste0(gsub(' ', '', region), '_resilience_map.png'), width=1200, 
        heigh=900)
    print(spplot(these_regions,
           c('BuffCap', 'CapLearn', 'SelfOrg'),
           col.regions=brewer.pal(11, "PuOr"),
           at=seq(0, 10, 2), rows=3))
    dev.off()
}
