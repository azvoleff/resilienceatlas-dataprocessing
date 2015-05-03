source('../0_settings.R')

library(rgdal)
library(raster)
library(foreach)
library(doParallel)
library(dplyr)

in_folder <- file.path(prefix, "GRP", "SPAM")
in_folder <- file.path(prefix, "GRP", "SPAM")
out_folder <- file.path(prefix, "GRP", "SPAM")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

# Function to ensure allocation IDs are properly formatted for joins
format_alloc_ID <- function(ids) {
    sprintf('%08i', as.numeric(as.character(ids)))
}

spam_in_file <- file.path(in_folder, "Regions", "reg6id_Horn.shp")

spam <- readOGR(file.path(in_folder, "Regions"), "reg6id_Horn")
spam$ALLOC_ID <- format_alloc_ID(spam$ALLOC_ID)

# # Join production data
# P <- read.csv(file.path(in_folder, "Sub-Saharan Africa", "spam2005V2r0_SSA_P.csv"))
# P <- rename(P, ALLOC_ID=alloc_key)
# P$ALLOC_ID <- format_alloc_ID(P$ALLOC_ID)
# spam@data <- left_join(spam@data, P, by="ALLOC_ID")

# Join harvested area data
H  <- read.csv(file.path(in_folder, "Sub-Saharan Africa", 
                             "spam2005V2r0_SSA_H.csv"))
H <- rename(H, ALLOC_ID=alloc_key)
H$ALLOC_ID <- format_alloc_ID(H$ALLOC_ID)
# Calculate total coffee (Arabica and Robusta) area harvested
H$area_coffee <- H$acof + H$rcof
spam@data <- left_join(spam@data, select(H, ALLOC_ID, area_coffee), 
                       by="ALLOC_ID")

# Join value data
V  <- read.csv(file.path(in_folder, "Sub-Saharan Africa", 
                             "spam2005V2r0_SSA_V.csv"))
V <- rename(V, ALLOC_ID=alloc_key)
V$ALLOC_ID <- format_alloc_ID(V$ALLOC_ID)
# Calculate total coffee (Arabica and Robusta) production value
V$vp_coffee <- V$acof + V$rcof
spam@data <- left_join(spam@data, select(V, ALLOC_ID, vp_coffee), 
                       by="ALLOC_ID")

# Join aggregated data
V_agg  <- read.csv(file.path(in_folder, "Sub-Saharan Africa", 
                             "spam2005V2r0_SSA_V_agg.csv"))
V_agg <- rename(V_agg, ALLOC_ID=alloc_key)
V_agg$ALLOC_ID <- format_alloc_ID(V_agg$ALLOC_ID)
spam@data <- left_join(spam@data, select(V_agg, ALLOC_ID, area_cr_h, area_cr_i, 
                                         area_cr_l, area_cr_s, vp_crop), 
                       by="ALLOC_ID")

# writeOGR(spam, file.path(in_folder, "Regions"),
#          "reg6id_Horn_Joined", driver="ESRI Shapefile",
#          overwrite=TRUE)

# rasterize spam
base_rast <- raster()
extent(base_rast) <- extent(spam)
res(base_rast) <- 5/60

cl <- makeCluster(3)
registerDoParallel(cl)
foreach (field=names(spam)[2:length(names(spam))],
         .packages=c("raster", "rgdal")) %dopar% {
    out_file <- file.path(in_folder, "Regions", paste0("Horn_", field, ".tif"))
    rastered <- rasterize(spam, base_rast, field,
                          filename=out_file, overwrite=TRUE)
    return(TRUE)
}
stopCluster(cl)
