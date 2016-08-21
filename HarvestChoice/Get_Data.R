# NOTE: Abandoned this code since the API doesn't really give access to any new 
# data beyond what we already have independently from SPAM and DHS.
library(httr)
library(stringr)
library(jsonlite)

url_base <- "http://hcapi.harvestchoice.org"

api_url <- paste0(url_base, "/ocpu/library/hcapi3/R/")

# Get list of all indicators
i <- POST(paste0(api_url, "category/json"), body=list(), encode="json")
indic <- content(i, simplifyDataFrame=TRUE)

# Get list of demographic indicators
r <- POST(paste0(api_url, "category/json"), body=list(q="demographics"), encode="json")
r_df <- content(r, simplifyDataFrame=TRUE)
