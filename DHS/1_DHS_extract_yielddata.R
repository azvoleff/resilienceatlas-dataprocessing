library(rgdal)
library(raster)
library(dplyr)

source('../0_settings.R')

dhs_regions <- readOGR(file.path(prefix, "GRP", "DHS", "Regions"), 
                                 "sdr_subnational_data_quickstats")

pop <- raster(file.path(prefix, "GRP", "From_Justin", "population", 
                        "2010_pop_gpw4_5m.tif"))
region_pop <- extract(pop, dhs_regions, fun=sum, na.rm=TRUE, small=TRUE)

maize <- raster(file.path(prefix, "GRP", "From_Justin", "earthstat", 
                                "crops", "maize", "maize_YieldPerHectare.tif"))
maize_per_person <- maize / pop
maize <- extract(maize_per_person, dhs_regions, fun=sum, na.rm=TRUE, small=TRUE)
dhs_regions$maize_per_person <- maize / region_pop

calories <- raster(file.path(prefix, "GRP", "From_Justin", "tradeoffs", 
                             "all_GridCal.tif"))
calories <- extract(calories, dhs_regions, fun=sum, na.rm=TRUE, small=TRUE)
calories[is.na(calories)] <- 0
dhs_regions$calories_per_person <- calories / region_pop

# spplot(dhs_regions, "maize_per_person")
# spplot(dhs_regions, "calories_per_person")
# hist(calories[calories< 1e5])

save(dhs_regions, file=file.path(prefix, "GRP", "Resilience_Indicator",
     "dhs_regions_merged_with_Justin_data.RData"))

