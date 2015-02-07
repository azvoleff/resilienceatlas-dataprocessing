library(rgdal)
library(raster)
library(dplyr)
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
    max(x) + 1 - x
}
# Children stunted
# Infant mortality rate

filter(indic, CountryName == "Indonesia")

resil <- group_by(indic, CountryName, RegionGRP, RegionId, Facet) %>%
    summarise(Score=sum(ValueQuantile) / (NumValidValue[1]*(length(probs) - 2)) * 10)

group_by(resil, CountryName, Facet) %>%
    summarize(mean_score=mean(Score)) %>%
    ggplot() +
    geom_bar(aes(CountryName, mean_score, fill=Facet), position="dodge", stat="identity") +
    xlab("Country") + ylab("Score") + ylim(0, 10) +
    scale_fill_discrete("Facet of resilience")

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
dhs_regions <- dhs_regions[dhs_regions$RegionId_short %in% resil_wide$RegionId_short, ]

dhs_regions@data <- select(dhs_regions@data, RegionId_short, maize_per_person, 
                           calories_per_person)
dhs_regions@data <- merge(dhs_regions@data, resil_wide)

# display.brewer.all(n=10, exact.n=FALSE)

dhs_regions$maize_per_person <- as.numeric(dhs_regions$maize_per_person)
dhs_regions$calories_per_person <- as.numeric(dhs_regions$calories_per_person)
writeOGR(dhs_regions, file.path(prefix, "GRP", "Resilience_Indicator"), 
         "resil_indicator", driver="ESRI Shapefile")

png('GRP_resilience_map.png', width=1500, heigh=1200)
spplot(dhs_regions, c('BuffCap', 'CapLearn', 'SelfOrg'),
       col.regions=brewer.pal(11, "PuOr"), at=seq(0, 10),
       par.settings=list(fontsize=list(text=20)))
dev.off()

#
# for (RegionGRP in unique(dhs_regions$RegionGRP)) {
#     png(paste0(gsub(' ', '', RegionGRP), '_resilience_map.png'), width=1200, 
#         heigh=900)
#     print(spplot(dhs_regions[which(dhs_regions$RegionGRP == RegionGRP), ],
#            c('BuffCap', 'CapLearn', 'SelfOrg'),
#            col.regions=brewer.pal(11, "PuOr"),
#            at=seq(0, 10)))
#     dev.off()
# }
