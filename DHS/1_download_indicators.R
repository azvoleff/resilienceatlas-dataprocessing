library(rgdal)
library(raster)
library(RJSONIO)
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)

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
    registerDoParallel(4)
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
    r <- foreach(indicatorID=indicatorIDs, .combine=rbind, .inorder=FALSE,
                 .packages='RJSONIO') %dopar% {
        api_call <- paste(api_base,
                          paste0("indicatorIDs=", indicatorID),
                          "f=json", sep='&')
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
        d
    }
    r <- tbl_df(r)
    r
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
          'ED_EDAT_M_CSC', # secondary education (men)
          'CM_ECMR_C_IMR', # Infant mortality rate (per 1000 live births)
          'CM_ECMR_C_U5M', # Under-five mortality rate
          'FE_FRTR_W_TFR', # total fertility rate
          'FP_CUSM_W_ANY', # Married women currently using any method of contraception
          'FP_CUSM_W_MOD', # Married women currently using any modern method of contraception
          'RH_DELP_C_DHF', # Place of delivery: Health facility
          'CH_VACS_C_BAS', # Received all vaccinations
          'WS_WTRT_H_NTR', # Households not treating water
          'WS_SRCE_H_PIP') # Households using water piped into dwelling

dhs_vars <- get_indic(vars)

dhs_vars$SurveyYear <- as.numeric(dhs_vars$SurveyYear)
dhs_vars$Value <- as.numeric(dhs_vars$Value)

# Place of delivery is coded for both three years (coded as ByVariableId
# equal to 14000) and five years preceding survey (coded as ByVariableId equal 
# to 14001). Use last five years.
dhs_vars <- dhs_vars[!(dhs_vars$IndicatorId == 'RH_DELP_C_DHF' & dhs_vars$ByVariableId == 14000), ]

# NOTE: Below lines are commented out because for some countries (Indonesia, 
# for example), we don't yet have region boundaries for the most recent survey.  
# So we load all the data into CartoDB, then join with a regions file that has 
# regions only for the most recent available survey from each country - this 
# will filter to the most recent data, and we can do a filter there to only 
# include data from the past 10 years.
#
# Filter to only include most recent data from each country
#dhs_vars <- group_by(dhs_vars, DHS_CountryCode, IndicatorId) %>%
#    filter(SurveyYear == max(SurveyYear))
# Only include data collected within past 10 years
#dhs_vars <- filter(dhs_vars, SurveyYear > 2005)

dhs_key <- group_by(dhs_vars, IndicatorId, Indicator, ByVariableId, 
                    ByVariableLabel) %>%
    summarise(n=n())
write.csv(dhs_key, 'dhs_indicators_key.csv', na="", row.names=FALSE)

dhs_vars <- select(dhs_vars, DHS_CountryCode, SurveyYear, SurveyId, RegionId, 
                   IndicatorId, Value) %>%
    group_by(DHS_CountryCode, SurveyYear, SurveyId, RegionId) %>%
    spread(IndicatorId, Value)

# Use blanks for NAs so CartoDB will correctly format numeric columns as 
# numeric and not as text
write.csv(dhs_vars, 'dhs_indicators.csv', na="", row.names=FALSE)

# # Join polygons
# dhs_regions <- readOGR(file.path(data_base, 'DHS', 'DHS_Regions'), 'DHS_Regions')
#
# dim(dhs_regions)
# dhs_regions <- dhs_regions[dhs_regions$REG_ID %in% dhs_vars$REG_ID, ]
# dim(dhs_regions)
#
# dhs_vars <- spread(select(dhs_vars, Indicator, Value, REG_ID), Indicator, Value)
#
# dhs_regions@data <- left_join(dhs_regions@data, dhs_vars)
#
# writeOGR(dhs_regions, 'DHS_indicators.geojson', 
#          'DHS_indicators', driver='GeoJSON')
