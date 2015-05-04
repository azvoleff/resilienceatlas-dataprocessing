library(dplyr)
library(raster)
library(gdalUtils)
library(rgdal)
library(rgeos)

source('../0_settings.R')

out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
shp_folder <- file.path(prefix, "GRP", "Boundaries")
stopifnot(file_test('-d', out_folder))
stopifnot(file_test("-d", shp_folder))

# Load the DEM extents needed for the auto_setup_dem function
load('dem_extents.RData')
dem_path <- file.path(prefix, 'CGIAR_SRTM', 'Tiles')
dem_extents$filename <- gsub('H:\\\\Data\\\\CGIAR_SRTM', dem_path, dem_extents$filename)
dem_extents$filename <- gsub('\\\\', '/', dem_extents$filename)

aoi_polygons <- readOGR(shp_folder, 'Analysis_Areas')
aoi_polygons <- aoi_polygons[aoi_polygons$Name == "Horn of Africa", ]
aoi_polygons <- spTransform(aoi_polygons, CRS(proj4string(dem_extents)))
intersecting <- as.logical(gIntersects(dem_extents, gConvexHull(aoi_polygons), byid=TRUE))

if (sum(intersecting) == 0) {
    stop('no intersecting dem extents found')
} else {
    dem_extents <- dem_extents[intersecting, ]
}

dem_list <- dem_extents$filename
dem_rasts <- lapply(dem_list, raster)

if (length(dem_list) > 1) {
    dem_prj <- projection(dem_rasts[[1]])
    if (any(lapply(dem_rasts, projection) != dem_prj)) {
        stop("each DEM in dem_list must have the same projection")
    }
    mosaic_file <- extension(rasterTmpFile(), '.tif')
    # Calculate minimum bounding box coordinates:
    mosaic_te <- as.numeric(bbox(aoi_polygons))
    # Expand bbox slightly:
    mosaic_te[1] <- mosaic_te[1] - .05
    mosaic_te[2] <- mosaic_te[2] - .05
    mosaic_te[3] <- mosaic_te[3] + .05
    mosaic_te[4] <- mosaic_te[4] + .05
    # Use mosaic_rasters from gdalUtils for speed:
    mosaic_rasters(dem_list, mosaic_file, te=mosaic_te, of="GTiff", 
                   overwrite=TRUE, ot='Int16')
    dem_mosaic <- raster(mosaic_file)
} else {
    dem_mosaic <- dem_rasts[[1]]
    mosaic_file <- filename(dem_mosaic)
}
