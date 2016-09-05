library(raster)

data_base <- 'O:/Data/Resilience_Atlas'

a <- raster(file.path(data_base, 'Aridity/Global_Aridity.tif'))

a <- aggregate(a, fact=10, fun=mean)

writeRaster(a, 'global_aridity_10km.tif', overwrite=TRUE)

drylands <- calc(a, function(x) {
        x[x < 300 | x > 6500] <- NA
        x[x >= 300 & x <= 6500] <- 1
        x
    }, filename='global_aridity_10km_drylands.tif', overwrite=TRUE)

# Use gdal_polygonize to convert drylands to a polygon (needed to run on Linux 
# machine). Command: gdal_polygonize.py global_aridity_10km_drylands.tif -f 
# "GeoJSON" global_aridity_10km_drylands_poly.json"

#a_polys <- rasterToPolygons(drylands, fun=function(x) {x > 300 & x < 6500}, 
#dissolve=TRUE)

# Topcode aridity at 2.5, then convert to byte image
a <- calc(a, function(x) {
        x <- x * .0001
        # Top code at 2.5
        x[x > 2.5] <- 2.5
        # Scale by range between 0 and 250
        x <- round(x * 100)
        x
    }, filename='global_aridity_rescaled.tif', datatype='INT1U', overwrite=TRUE)
