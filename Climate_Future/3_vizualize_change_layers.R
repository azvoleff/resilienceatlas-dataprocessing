library(raster)
library(rasterVis)
library(dplyr)
library(foreach)
library(doParallel)
library(RColorBrewer)
library(tools)
library(maps)

source('../0_settings.R')

source('../ec2/s3_ls.R')

cl <- makeCluster(4)
registerDoParallel(cl)

s3_in <- 's3://ci-vsdata/CMIP5/results/'
s3_out <- 's3://ci-vsdata/CMIP5/results_plots/'

# Below is needed on windows
Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')

s3_files <- s3_ls(s3_in, 'C:/Users/azvoleff/.aws/config')
s3_files$variable <- str_extract(s3_files$file, '^[a-z]*')

melt_cmip <- function(r) {
    xy <- coordinates(r)
    data.frame(x=xy[, 1], y=xy[, 2], value=getValues(r))
}

# Plot percent diffs
foreach (f=iter(filter(s3_files, grepl('multimodelmean', file)), by='row'),
         .packages=c('raster', 'tools', 'RColorBrewer', 'maps')) %dopar% {
    Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')
    r_tmp_file <- tempfile(fileext='.tif')
    system2('aws', args=c('s3', 'cp', paste0(s3_in, f$file), r_tmp_file))
    r <- raster(r_tmp_file)
    out_file <- paste0(file_path_sans_ext(f$file), '_rplot.png')
    png_tmp_file <- tempfile(fileext='.png')
    png_tmp_file <- 'test.png'

    # r[r > 100] <- 100
    # r[r < -100] <- 100
    # r[is.na(r)] <- 0
    png(png_tmp_file, width=900, height=600)
    if (f$variable == 'pr') {
        plot(r, zlim=c(-50, 50), col=brewer.pal(11,"RdBu"), main=f$file, 
             axes=FALSE, box=FALSE)
        map("world", add=TRUE)
    } else {

        plot(r, col=rev(brewer.pal(11,"RdBu")), main=f$file, axes=FALSE, box=FALSE)
        map("world", add=TRUE)

    }
    dev.off()

    system2('aws', args=c('s3', 'cp', png_tmp_file, paste0(s3_out, out_file)))
}

# Plot multimodel means for temp

# Plot percent diffs
foreach (f=iter(filter(s3_files, grepl('pctdiff', file)), by='row'),
         .packages=c('raster', 'tools', 'RColorBrewer', 'maps')) %dopar% {
    Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')
    r_tmp_file <- tempfile(fileext='.tif')
    system2('aws', args=c('s3', 'cp', paste0(s3_in, f$file), r_tmp_file))
    r <- raster(r_tmp_file)
    out_file <- paste0(file_path_sans_ext(f$file), '_rplot.png')
    png_tmp_file <- tempfile(fileext='.png')
    png_tmp_file <- 'test.png'

    # r[r > 100] <- 100
    # r[r < -100] <- 100
    # r[is.na(r)] <- 0
    png(png_tmp_file, width=900, height=600)
    if (f$variable == 'pr') {
        plot(r, zlim=c(-50, 50), col=brewer.pal(11,"RdBu"), main=f$file, 
             axes=FALSE, box=FALSE)
        map("world", add=TRUE)
    } else {

        plot(r, zlim=c(0, 4), col=rev(brewer.pal(11,"RdBu")), main=f$file, 
             axes=FALSE, box=FALSE)
        map("world", add=TRUE)

    }
    dev.off()

    system2('aws', args=c('s3', 'cp', png_tmp_file, paste0(s3_out, out_file)))
}
