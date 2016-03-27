##############################################################################
# Crops CHIRPS pentad or monthly precipitation data to cover the spatial extent 
# of each site.
###############################################################################

prefix <- "O:/Data"

library(raster)
library(tools)

# Over what period should the calculations be made?
mean_monthly_period <- '198501-201412'

in_file <- file.path(prefix, "GRP", "CHIRPS-2.0", "CHIRPS_198501-201412_annualtotal.tif")
out_folder <- file.path(prefix, "GRP", "CHIRPS-2.0")
stopifnot(file_test('-f', in_file))
stopifnot(file_test('-d', out_folder))

base_name <- file_path_sans_ext(in_file)

annual_total <- stack(in_file)

coef_var <- cv(annual_total)

writeRaster(coef_var,
            filename=paste0(base_name, '_interannualvariability_pct.tif'),
            overwrite=TRUE)
