###############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

source('../0_settings.R')

library(rgdal)

in_folder <- file.path(prefix, "GRP", "GLASOD", "Original", "glasod_shapefile")
out_folder <- file.path(prefix, "GRP", "GLASOD", "Recoded")
stopifnot(file_test('-d', in_folder))
stopifnot(file_test('-d', out_folder))

glasod <- readOGR(in_folder, "Glasod")

glasod$MAJOR_CAT <- 'None'

glasod$MAJOR_CAT[glasod$MAJOR %in% c("C3", "C4")] <- "Chemical degradation"
glasod$MAJOR_CAT[glasod$MAJOR %in% c("E3", "E4")] <- "Wind erosion"
glasod$MAJOR_CAT[glasod$MAJOR %in% c("P3", "P4")] <- "Physical erosion"
glasod$MAJOR_CAT[glasod$MAJOR %in% c("W3", "W4")] <- "Water erosion"

glasod <- glasod[glasod$MAJOR_CAT != "None", ]

#spplot(glasod, "MAJOR_CAT")

writeOGR(glasod, out_folder, "Glasod", driver="ESRI Shapefile", overwrite=TRUE)

