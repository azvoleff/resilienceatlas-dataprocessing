library(rgdal)
library(raster)
library(RJSONIO)
library(dplyr)
library(tidyr)

#data_base <- 'H:/Data'
data_base <- 'O:/Data'

dhs_api_key <- Sys.getenv('dhs_api_key')
indicator_list <- fromJSON(paste0('http://api.dhsprogram.com/rest/dhs/indicators?APIkey=',
                                  dhs_api_key, '?returnFields=IndicatorId,Label,Definition'))
# Unlist the JSON file entries
indicator_list <- lapply(indicator_list$Data, function(x) {unlist(x)})
# Convert JSON input to a data frame
indicator_list <- as.data.frame(do.call("rbind", indicator_list), stringsAsFactors=FALSE)

grep_list <- function(x) {
    def_rows <- grepl(tolower(x), tolower(indicator_list$Label))
    label_rows <- grepl(tolower(x), tolower(indicator_list$Definition))
    return(indicator_list[(def_rows | label_rows), 1:3])
}

get_indic <- function(indicatorIDs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    indicators_string <- paste0("indicatorIds=", paste(indicatorIDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
    api_call <- paste(api_base, indicators_string, "f=json", sep='&')
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

vars <- c('CN_NUTS_C_HA2', # percent children stunted
          'CN_NUTS_C_WA2', # percent children underweight
          'CN_NUTS_C_WH2', # percent children wasted
          'HC_ELEC_H_ELC', # access to electricity
          'HC_HEFF_H_NPH', # households possessing a phone
          'HC_HEFF_H_MPH', # households possessing a mobile phone
          'WS_TLET_H_FSW', # households with flush toilet
          'ED_LITR_W_NRD', # illiteracy (women)
          'ED_LITR_M_NRD', # illiteracy (men)
          'ED_MDIA_W_N3M', # access to mass media (women)
          'ED_MDIA_M_N3M', # access to mass media (men)
          'ED_EDAT_W_CSC', # secondary education (women)
          'ED_EDAT_M_CSC') # secondary education (men)

'WS_SRCE_H_NIM' # Households using an unimproved water source


Households using an unprotected well water

dhs_vars <- get_indic(vars)

dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)
dhs_vars$Value <- as.numeric(dhs_vars$Value)

names(dhs_vars)[names(dhs_vars) == "RegionId"] <- 'REG_ID'

# Filter to only include most recent data from each country
dhs_vars <- group_by(dhs_vars, DHS_CountryCode, Indicator) %>%
    filter(SurveyYear == max(SurveyYear))

# Only include data collected within past 10 years
dhs_vars <- filter(dhs_vars, SurveyYear > 2005)

#ggplot(dhs_vars) + geom_point(aes(Indicator, Value))

# Join polygons
dhs_regions <- readOGR(file.path(data_base, 'DHS', 'DHS_Regions'), 'DHS_Regions')

dim(dhs_regions)
dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% dhs_vars$REG_ID, ]
dim(dhs_regions)

dhs_vars <- spread(select(dhs_vars, Indicator, Value, REG_ID), Indicator, Value)

dhs_regions@data <- left_join(dhs_regions@data, dhs_vars)

writeOGR(dhs_regions, 'DHS_indicators.geojson', 
         'DHS_indicators', driver='GeoJSON')
