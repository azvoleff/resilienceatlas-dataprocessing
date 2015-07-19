library(raster)

fish_raw <- read.csv("O:/Data/GRP/Fisheries_catch/CPSRESA1B_converted.csv")
fish_raw <- as.matrix(fish_raw[, 2:3])
#fish_raw[fish_raw <= .1] <- NA

fish <- brick(nrows=360, ncols=720, nl=2, xmn=-180, xmx=180, 
              ymn=-90, ymx=90, crs='+init=epsg:4326')
fish <- setValues(fish, fish_raw)

fish$Year2055DiffPct <- ((fish$Year2055 - fish$Year2000s) / fish$Year2000s) * 100

writeRaster(fish$Year2000s, "O:/Data/GRP/Fisheries_catch/Catch_Potential_2000.tif", overwrite=TRUE)
writeRaster(fish$Year2055DiffPct, "O:/Data/GRP/Fisheries_catch/Catch_Potential_2055_ChangePercentFrom2000.tif", overwrite=TRUE)
