library(raster)
library(foreach)
library(dplyr)
library(iterators)
library(RColorBrewer)

base_path <- 'O:/Data/GAEZ'

###############################################################################
# Calculate projected changes in yields due to climate change
crops <- c('maize', 'sorghum', 'pearlmillet')
codes <- c('maiz', 'sorg', 'pmlt')
codes_short <- c('mze', 'sorg', NA)

stopifnot(length(crops) == length(codes))

foreach (crop=crops, code=codes, code_short=codes_short) %do% {
    # Future agroclimatic yields. Remember that agro-climatic yields are in 
    # kg/ha and need to be converted to tons/ha.
    fut2020_acy_r_h <- raster(file.path(base_path, paste0('res02_csa22020h_', code, '150b_yld.tif'))) / 1000
    fut2020_acy_r_i <- raster(file.path(base_path, paste0('res02_csa22020i_', code, '150b_yld.tif'))) / 1000
    fut2020_acy_r_l <- raster(file.path(base_path, paste0('res02_csa22020l_', code, '150b_yld.tif'))) / 1000

    # Current agroclimatic yields.
    # Future agroclimatic yields. Remember that agro-climatic yields are in 
    # kg/ha and need to be converted to tons/ha.
    fut2050_acy_r_h <- raster(file.path(base_path, paste0('res02_csa22050h_', code, '150b_yld.tif'))) / 1000
    fut2050_acy_r_i <- raster(file.path(base_path, paste0('res02_csa22050i_', code, '150b_yld.tif'))) / 1000
    fut2050_acy_r_l <- raster(file.path(base_path, paste0('res02_csa22050l_', code, '150b_yld.tif'))) / 1000

    # Current agroclimatic yields.
    cur_acy_r_h <- raster(file.path(base_path, paste0('res02_crav6190h_', code, '150b_yld.tif'))) / 1000
    cur_acy_r_i <- raster(file.path(base_path, paste0('res02_crav6190i_', code, '150b_yld.tif'))) / 1000
    cur_acy_r_l <- raster(file.path(base_path, paste0('res02_crav6190l_', code, '150b_yld.tif'))) / 1000

    calc_y_diff <- function(fut, cur) {
        d <- fut - cur
        d[d == 0] <- NA
        d[is.na(d)] <- -999
        d
    }

    writeRaster(calc_y_diff(fut2020_acy_r_l, cur_acy_r_l), 
                paste0('y_chg_2020_', crop, '_rainfed_lowinput.tif'), overwrite=TRUE)
    writeRaster(calc_y_diff(fut2020_acy_r_i, cur_acy_r_i),
                paste0('y_chg_2020_', crop, '_rainfed_medinput.tif'), overwrite=TRUE)
    writeRaster(calc_y_diff(fut2020_acy_r_h, cur_acy_r_h),
                paste0('y_chg_2020_', crop, '_rainfed_highinput.tif'), overwrite=TRUE)

    writeRaster(calc_y_diff(fut2050_acy_r_l, cur_acy_r_l), 
                paste0('y_chg_2050_', crop, '_rainfed_lowinput.tif'), overwrite=TRUE)
    writeRaster(calc_y_diff(fut2050_acy_r_i, cur_acy_r_i),
                paste0('y_chg_2050_', crop, '_rainfed_medinput.tif'), overwrite=TRUE)
    writeRaster(calc_y_diff(fut2050_acy_r_h, cur_acy_r_h),
                paste0('y_chg_2050_', crop, '_rainfed_highinput.tif'), overwrite=TRUE)

    ###############################################################################
    # No current yield data for pearl millet, so only calculate yield gap for 
    # sorghum and maize.
    if (code %in% c('sorg', 'maiz')) {
        # Current yields are in ton/ha
        cur_y_r <- raster(file.path(base_path, paste0('act2000_r_', code_short, '_2000_yld.tif')))

        writeRaster(cur_acy_r_l - cur_y_r, paste0('yg_', crop, '_rainfed_lowinput.tif'), overwrite=TRUE)
        writeRaster(cur_acy_r_i - cur_y_r, paste0('yg_', crop, '_rainfed_medinput.tif'), overwrite=TRUE)
        writeRaster(cur_acy_r_h - cur_y_r, paste0('yg_', crop, '_rainfed_highinput.tif'), overwrite=TRUE)
    }
}

# r <- raster('y_chg_2050_sorghum_rainfed_highinput.tif')
# p
# \aalot(r, col=brewer.pal(7,"RdYlGn"), zlim=c(-5, 5))
