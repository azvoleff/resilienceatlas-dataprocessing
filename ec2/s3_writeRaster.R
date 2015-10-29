# Function to write an in-memory raster to S3, removing temp files after 
# finished
s3_writeRaster <- function(r, S3_loc) {
    require(raster)
    temp_file <- tempfile(fileext='.tif')
    writeRaster(r, filename=temp_file)
    system2('aws', args=c('s3', 'cp', temp_file, S3_loc))
    unlink(temp_file)
}
