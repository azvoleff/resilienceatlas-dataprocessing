require(dplyr)
require(RJSONIO)
 
get_indic <- function(indicator_IDs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    ccs <- c("ER", "ET", "ID", "NI", "UG")
    cc_string <- paste0("countryIds=", paste(ccs, collapse=","))
    indicators_string <- paste0("indicatorIds=", paste(indicator_IDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational"
    api_call <- paste0(api_base, "&", indicators_string, "&", cc_string, "&f=json")
    # Import DHS Indicator data for TFR for each survey
    json_file <- fromJSON(api_call)
    # Unlist the JSON file entries
    d <- lapply(json_file$Data, function(x) {unlist(x)})
    # Convert JSON input to a data frame
    d <- as.data.frame(do.call("rbind", d), stringsAsFactors=FALSE)
    d <- tbl_df(d)
    return(d)
}

buff_indicators <- c(214527002, # Children stunted
                     72254002, # Infant mortality rate
                     6136002, # Population age 6 and over who attended secondary education
                     120147001, # Household size
                     10485003, # Percent in middle wealth quintile or above (women)
                     10485004, # Percent in fourth quintile or above (women)
                     10485005) # Percent in highest quintile or above (women)
buff <- get_indic(buff_indicators)

# Tabulate the TFR values by the survey IDs
xtabs(as.numeric(Value) ~ SurveyId, data=buff)
                  
#### TODO: Add yields and yield potential from Justin
# Realized yields / person
# Yield potential / person

indicator_list <- fromJSON('http://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition')
# Unlist the JSON file entries
indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

grep_list <- function(x) {
    def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
    label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
    return(indicator_list[(def_rows | label_rows), ])
}
grep_list('elec')
grep_list('stunt')
grep_list('mort')

indicator_list
