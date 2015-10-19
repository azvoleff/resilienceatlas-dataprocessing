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

# Plot percent diffs (precipitation only)
pr_palette <- c(rev(brewer.pal(9, 'YlOrBr')), brewer.pal(9,"Blues"))
foreach (f=iter(filter(s3_files, grepl('pctdiff', file)), by='row'),
         .packages=c('raster', 'tools', 'RColorBrewer', 'maps')) %dopar% {
    Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')
    r_tmp_file <- tempfile(fileext='.tif')
    system2('aws', args=c('s3', 'cp', paste0(s3_in, f$file), r_tmp_file))
    r <- raster(r_tmp_file)
    out_file <- paste0(file_path_sans_ext(f$file), '_rplot.png')
    png_tmp_file <- tempfile(fileext='.png')
    lim <- max(abs(cellStats(r, 'min')), abs(cellStats(r, 'max')))
    png(png_tmp_file, width=900, height=600)
    plot(r, zlim=c(-lim, lim), main=f$file, col=pr_palette, axes=FALSE, 
         box=FALSE)
    map("world", add=TRUE)
    dev.off()
    system2('aws', args=c('s3', 'cp', png_tmp_file, paste0(s3_out, out_file)))
}

temp_palette <- c(rev(brewer.pal(9,"Blues")), brewer.pal(9, 'Reds'))
temp_breaks <- seq(-13,13, length.out=length(temp_palette))

# Plot absolute differences
foreach (f=iter(filter(s3_files, grepl('absdiff', file)), by='row'),
         .packages=c('raster', 'tools', 'RColorBrewer', 'maps')) %dopar% {
    Sys.setenv(AWS_CONFIG_FILE='C:/Users/azvoleff/.aws/config')
    r_tmp_file <- tempfile(fileext='.tif')
    system2('aws', args=c('s3', 'cp', paste0(s3_in, f$file), r_tmp_file))
    r <- raster(r_tmp_file)
    out_file <- paste0(file_path_sans_ext(f$file), '_rplot.png')
    png_tmp_file <- tempfile(fileext='.png')
    png_tmp_file <- 'test.png'
    png(png_tmp_file, width=900, height=600)
    if (f$variable == 'pr') {
        plot(r, breaks=pr_breaks, col=pr_palette, main=f$file, 
             axes=FALSE, box=FALSE)
        map("world", add=TRUE)
    } else {
        plot(r, breaks=temp_breaks, col=temp_palette, main=f$file, 
             axes=FALSE, box=FALSE)
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
