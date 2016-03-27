library(raster)

base_dir <- "H:/Data/CHPclim"

chpclim <- stack(list.files(base_dir, pattern='^CHPclim.(0[0-9])|(1[0-2]).tif', 
                            full.names=TRUE))

ann <- calc(chpclim, function(x) {
        x[x < 0] <- NA
        x <- sum(x)
        if ((x < 0) | is.na(x)) {
            return(-9999)
        } else {
            return(x)
        }
    })
writeRaster(ann, file.path(base_dir, 'total_annual_rainfall.tif'), overwrite=TRUE)

# Drop table on CartoDB
#https://grp.cidata.io/user/grp/api/v1/sql?q=SELECT+initcap%28s_name%29+as+name,+bbox,+iso3+as+iso+FROM+grpcountries_250k_polygon

# Upload new data
