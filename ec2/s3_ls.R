# Function to parse output of S3 ls command
# Note env is needed on windows to 
s3_ls <- function(s3_folder, config=NULL) {
    require(stringr)
    if (!is.null(config)) {
        # Needed on Windows
        Sys.setenv(AWS_CONFIG_FILE=config)
    }
    s3_ls <- system2('aws', args=c('s3', 'ls', s3_folder), stdout=TRUE)
    parse_out <- function(x) {
        # Replace any combination of repeated spaces with a single space
        x <- gsub(' [ ]*', ' ', x)
        x <- unlist(str_split(x, ' '))
        data.frame(date=x[1], time=x[2], size=x[3], file=x[4])
    }
    s3_files <- plyr::ldply(s3_ls, parse_out)
    # Filter out empty placeholder file with size 1 and no filename:
    s3_files <- s3_files[s3_files$file != '', ]
    s3_files$file <- as.character(s3_files$file)
    return(s3_files)
}
