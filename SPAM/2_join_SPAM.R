source('../0_settings.R')

library(rgdal)
library(raster)
library(gdalUtils)
library(dplyr)

in_folder <- file.path(prefix, "GRP", "SPAM")
in_folder <- file.path(prefix, "GRP", "SPAM")
out_folder <- file.path(prefix, "GRP", "SPAM")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

spam_in_file <- file.path(in_folder, "Regions", "reg6id_Horn.shp")

spam <- readOGR(file.path(in_folder, "Regions"), "reg6id_Horn")
spam@data$ALLOC_ID <- as.integer(spam@data$ALLOC_ID)

# Join in production data
P <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_P.csv"))
P <- rename(P, ALLOC_ID=alloc_key)
spam@data <- left_join(spam@data, P)

# Join in aggregated data
V_agg  <- read.csv(file.path(in_folder, "Sub-Saharan Africa", 
                             "spam2005V2r0_SSA_V_agg.csv"))
V_agg <- rename(V_agg, ALLOC_ID=alloc_key)
spam@data <- left_join(spam@data, V_agg)

writeOGR(spam, file.path(in_folder, "Regions"),
         "reg6id_Horn_Joined", driver="ESRI Shapefile",
         overwrite=TRUE)

dim(spam)
dim(spam)

spplot(spam, "acof")

spam$coffee_rainfed <- spam$acof_
