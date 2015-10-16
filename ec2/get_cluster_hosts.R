library(stringr)

get_cluster_hosts <- function() {
    require(stringr)
    con  <- file('/etc/hosts', open="r")
    cluster_hosts <- c()
    while (length(this_line <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if(grepl('(node[0-9]{3})|(master)', this_line)) {
            cluster_hosts <- c(cluster_hosts, str_extract(this_line, '(node[0-9]{3})|(master)'))
        }
    } 
    close(con)
    return(cluster_hosts)
}
