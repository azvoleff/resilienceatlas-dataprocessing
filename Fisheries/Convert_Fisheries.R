library(raster)

fish <- stack("H:/Data/GRP/Fisheries_catch/4096.png",
              bands=as.integer(c(1, 2, 3)))
proj4string(fish) <- '+init=epsg:4326'
extent(fish) <- c(-180, 180, -90, 90)

writeRaster(fish, "Change_In_Fisheries_Catch_Potential.tif", overwrite=TRUE)

# 50% increase is equal to RGB: 80, 81, 161


plot(fish)
plotRGB(fish)
