# Function to parse output of S3 ls command
#
# config is needed on windows to allow aws CLI to find access key
s3_ls <- function(s3_folder, config=NULL) {
    require(stringr)
    if (!is.null(config)) {
        # Needed on Windows
        Sys.setenv(AWS_CONFIG_FILE=config)
    }
    s3_ls <- system2('aws', args=c('s3', 'ls', s3_folder), stdout=TRUE)
    s3_files <- gsub(' [ ]*', ' ', s3_ls)
    s3_files <- str_split(s3_files, ' ')
    # Drop empty placeholder file with size 1 and no filename:
    s3_files <- s3_files[-1]
    s3_files <- data.frame(matrix(unlist(s3_files), nrow=length(s3_files), byrow=T))
    names(s3_files) <- c("date", "time", "size", "file")
    s3_files$file <- as.character(s3_files$file)
    return(s3_files)
}
