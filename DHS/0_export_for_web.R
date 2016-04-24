source('../0_settings.R')

library(RJSONIO)
library(dplyr)

###############################################################################
### Pick variables

indicator_list <- fromJSON('http://api.dhsprogram.com/rest/dhs/indicators?APIkey=CVINTL-877527?returnFields=IndicatorId,Label,Definition')
# Unlist the JSON file entries
indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

country_list <- fromJSON('http://api.dhsprogram.com/rest/dhs/countries?APIkey=CVINTL-877527?returnFields=countryId,Label,Definition')
# Unlist the JSON file entries
country_list <- lapply(country_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
country_list <- as.data.frame(do.call("rbind", country_list), stringsAsFactors=FALSE)

grep_list <- function(x) {
    def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
    label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
    return(indicator_list[(def_rows | label_rows), 1:3])
}

###############################################################################
###  Build indicator

get_indic <- function(indicator_IDs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    indicators_string <- paste0("indicatorIds=", paste(indicator_IDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
    api_call <- paste0(api_base, "&", indicators_string, "&f=json")
    # Make repeated calls to retrieve all records as there is a 5000 record 
    # limit per request
    nrecs <- 5000
    i <- 1
    while (nrecs == 5000) {
        this_call <- paste0(api_call, '&page=', i)
        json_file <- fromJSON(this_call)
        # Unlist the JSON file entries
        this_d <- lapply(json_file$Data, function(x) {unlist(x)})
        # Convert JSON input to a data frame
        this_d <- as.data.frame(do.call("rbind", this_d), stringsAsFactors=FALSE)
        if (i == 1) d <- this_d
        else d <- rbind(d, this_d)
        nrecs <- nrow(this_d)
        i <-  i + 1
    }
    d <- tbl_df(d)
}

vars <- c('CN_NUTS_C_HA2', # Children stunted
          'CM_ECMR_C_IMR', # IMR
          'FE_FRTR_W_TFR', # Total fertility rate
          'ED_EDAT_M_CSC', # Males 6 and over with completed secondary ed.
          'ED_EDAT_W_CSC', # Females 6 and over with completed secondary ed.
          'ED_LITR_W_LIT', # Women who are literate
          'ED_LITR_M_LIT', # Men who are literate
          'ED_MDIA_W_N3M', # Women with no access to mass media
          'ED_MDIA_M_N3M', # Men with no access to mass media
          'HC_ELEC_H_ELC', # Households with electricity
          'HC_HEFF_H_NPH', # Households possessing a telephone
          'HC_HEFF_H_MPH', # Households possessing a mobile phone
          'WS_SRCE_H_IMP') # Households with an improved water source

dhs_vars <- get_indic(vars)

unique(dhs_vars$Indicator)

dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)

vars <- group_by(dhs_vars, IndicatorId, ByVariableId) %>%
    summarise(Indicator=Indicator[1],
              ByVariableLabel=ByVariableLabel[1])
write.csv(vars, file='DHS_Variables_Key.csv', row.names=FALSE)

names(dhs_vars)[names(dhs_vars == "RegionId")] <- dhs_vars$REG_ID

save(dhs_vars, file=file.path(prefix, "Resilience_Atlas", "DHS", 
                              "DHS_indicators.RData"))
write.csv(dhs_vars, file=file.path(prefix, "Resilience_Atlas", 
                                   "DHS", "DHS_indicators.csv"), 
          row.names=FALSE)
