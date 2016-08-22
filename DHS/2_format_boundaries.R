library(rgdal)
library(dplyr)

#data_base <- 'H:/Data'
data_base <- 'O:/Data/Resilience_Atlas'

r <- readOGR(file.path(data_base, "/DHS/Boundaries/shps"), "sdr_subnational_data_quickstats")

r@data <- select(r@data, RegionID=REG_ID,DHSRegion=DHSREGEN, 
                 Multilevel=MULTLEVEL, LevelRank=LEVELRNK)

writeOGR(r, 'dhs_regions.geojson', 'regions', driver='GeoJSON')
