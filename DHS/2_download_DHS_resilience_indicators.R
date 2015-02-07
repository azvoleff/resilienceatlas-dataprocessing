require(dplyr)
require(RJSONIO)

###############################################################################
### Pick variables

indicator_list <- fromJSON('http://api.dhsprogram.com/rest/dhs/indicators?APIkey=CVINTL-877527?returnFields=IndicatorId,Label,Definition')
# Unlist the JSON file entries
indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

grep_list <- function(x) {
    def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
    label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
    return(indicator_list[(def_rows | label_rows), 1:3])
}

grep_list('water')

###############################################################################
###  Build indicator
 
get_indic <- function(indicator_IDs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    ccs <- c("ER", "ET", "ID", "NI", "UG")
    cc_string <- paste0("countryIds=", paste(ccs, collapse=","))
    indicators_string <- paste0("indicatorIds=", paste(indicator_IDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
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

buff_indic <- c(214527002, # Children stunted
                72254002, # Infant mortality rate
                6136002, # Population age 6 and over who attended secondary education
                120147001, # Household size
                10485003, # Percent in middle wealth quintile or above (women)
                10485004, # Percent in fourth quintile or above (women)
                10485005) # Percent in highest quintile or above (women)
buff <- get_indic(buff_indic)
buff$facet <- "Buffer capacity"

sorg_indic <- c(8139010, # Piped water
                8142001) # Access to electricity
sorg <- get_indic(sorg_indic)
sorg$facet <- "Self-organization"

learn_indic <- c(127383002, # women who are literate
                 7135002) #  pop 6-15 who are attending school
learn <- get_indic(learn_indic)
learn$facet <- "Capacity for learning"


indic <- full_join(buff, sorg)
indic <- full_join(indic, learn)

# Drop all but most recent survey data, and take mean when both men/women are 
# supplied
indic <- group_by(indic, CountryName) %>%
    filter(SurveyYear == max(SurveyYear)) %>%
    group_by(facet, DHS_CountryCode, CountryName, SurveyYear, SurveyId, IndicatorId, RegionId, CharacteristicLabel) %>%
    summarize(Value=mean(as.numeric(Value)))

# Make the income variables a sum
    
# 10485003, # Percent in middle wealth quintile or above (women)
# 10485004, # Percent in fourth quintile or above (women)
# 10485005) # Percent in highest quintile or above (women)
                #
save(indic, file=file.path(prefix, "GRP", 
                           "Resilience_Indicator","resil_raw_DHS_data.RData"))
write.csv(indic, file=file.path(prefix, "GRP", 
                           "Resilience_Indicator","resil_raw_DHS_data.csv", 
                           row.names=FALSE))
