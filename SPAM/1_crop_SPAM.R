source('../0_settings.R')

library(rgdal)
library(raster)
library(gdalUtils)

in_folder <- file.path(prefix, "GRP", "SPAM")
in_folder <- file.path(prefix, "GRP", "SPAM")
out_folder <- file.path(prefix, "GRP", "SPAM")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))
stopifnot(file_test("-d", shp_folder))

# Crop SPAM African region to Horn
spam_in_file <- file.path(in_folder, "Regions", "reg6id.shp")
spam_out_file <- file.path(in_folder, "Regions", "reg6id_Horn.shp")

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Name == "Horn of Africa", ]

# Round extent so that pixels are aligned properly
clipsrc <- as.numeric(bbox(aoi_polygons))
clipsrc[1:2] <- floor(clipsrc[1:2])
clipsrc[3:4] <- ceiling(clipsrc[3:4])
clipsrc <- paste0('[', paste(clipsrc, collapse=' '), ']')
clipsrc <- paste(clipsrc, collapse=' ')

# Crop SPAM region to Horn - to get this to work the output of "clipsrc" 
# shouldn't be qutoed
a <- ogr2ogr(spam_in_file, spam_out_file, clipsrc=clipsrc, verbose=TRUE)
