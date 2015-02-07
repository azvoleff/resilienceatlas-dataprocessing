library(rgdal)
library(raster)
library(dplyr)

source('../0_settings.R')

#### TODO: Add yields and yield potential from Justin
# Realized yields / person
# Yield potential / person

dhs_regions <- readOGR(file.path(prefix, "GRP", "DHS", "Regions"), 
                                 "sdr_subnational_data_quickstats")

pop <- raster(file.path(prefix, "GRP", "From_Justin", "population", 
                        "2010_pop_gpw4_5m.tif"))

maize_yield <- raster(file.path(prefix, "GRP", "From_Justin", "earthstat", 
                                "crops", "maize", "maize_YieldPerHectare.tif"))
maize_yield_per_person <- maize_yield / pop
maize_yield <- extract(maize_yield_per_person, dhs_regions, fun=mean, na.rm=TRUE)
maize_yield[is.na(maize_yield)] <- 0
dhs_regions$maize_per_person <- maize_yield

calories <- raster(file.path(prefix, "GRP", "From_Justin", "tradeoffs", 
                             "all_GridCal.tif"))
calories_per_person <- calories / pop
calories <- extract(calories_per_person, dhs_regions, fun=mean, na.rm=TRUE)
calories[is.na(calories)] <- 0
dhs_regions$calories_per_person <- calories

# spplot(dhs_regions, "maize_yield_per_person")
# spplot(dhs_regions, "calories_per_person")

hist(calories[calories< 1e5])

save(dhs_regions, file=file.path(prefix, "GRP", "Resilience_Indicator",
     "dhs_regions_merged_with_Justin_data.RData"))

