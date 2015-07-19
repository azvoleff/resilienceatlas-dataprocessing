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

grp_list <- read.csv(file.path(prefix, "GRP", "DataTables", "GRP_Countries.csv"))
# Filter for only countries with DHS data available
grp_list <- grp_list[grp_list$ISO3 %in% country_list$ISO3_CountryCode, ]

grp_list <- merge(grp_list, country_list, by.x="ISO3", by.y="ISO3_CountryCode")

ccs <- as.character(grp_list$DHS_CountryCode)

get_indic <- function(indicator_IDs, ccs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    cc_string <- paste0("countryIds=", paste(ccs, collapse=","))
    indicators_string <- paste0("indicatorIds=", paste(indicator_IDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
    api_call <- paste0(api_base, "&", indicators_string, "&", cc_string, "&f=json")
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

buff_indic <- c(214527002, # Children stunted
                70254002, # Infant mortality rate
                6136002, # Population age 6 and over who attended secondary education
                120147001) # Household size)
#TODO: Make assets index
buff <- get_indic(buff_indic, ccs)
buff$Facet <- "Buffer capacity"
unique(buff$Indicator)

#TODO: Make assets index
own_indic <-  c('9124001', # Households possessing a radio
                '9124002', # Households possessing a television
                '9124003', # Households possessing a telephone
                '9124004', # Households possessing a refrigerator
                '9124005', # Households possessing a bicycle
                '9124006', # Households possessing a motorcycle
                '9124007', # Households possessing a private car
                '9124008') # Households possessing none of the previous possessions
own <- get_indic(own_indic, ccs)
own$Facet <- "own"
unique(own$Indicator)

#TODO: Figure out wealth - doesn't appear to be available from the API
# wealth_indic <- c('10485001', # Women in the lowest wealth quintile
#                   '10485002', # Women in the second wealth quintile
#                   '10485003', # Women in the middle wealth quintile
#                   '10485004', # Women in the fourth wealth quintile
#                   '10485005', # Women in the highest wealth quintile
#                   '152485001', # Men in the lowest wealth quintile
#                   '152485002', # Men in the second wealth quintile
#                   '152485003', # Men in the middle wealth quintile
#                   '152485004', # Men in the fourth wealth quintile
#                   '152485005') # Men in the highest wealth quintile
# wealth <- get_indic(wealth_indic, ccs)
# wealth$Facet <- "wealth"
# unique(wealth$Indicator)

tfr_indic <-  c(19170000, # Total fertility rate for 15-49
                216236002, # Unmet need for contraception
                20171000) # Total fertility rate
tfr <- get_indic(tfr_indic, ccs)
tfr$Facet <- "TFR"
unique(tfr$Indicator)

sorg_indic <- c(8139010, # Piped water
                8142001) # Access to electricity
sorg <- get_indic(sorg_indic, ccs)
sorg$Facet <- "Self-organization"
unique(sorg$Indicator)

learn_indic <- c(127383002, # women who are literate
                 127384003, # percentage of women who cannot read at all
                 7135002, # pop 6-15 who are attending school
                 13126000, # women with no access to mass media
                 155126001, # men with no access to mass media
                 13126005) # percentage women with access to newspaper, television, and radio at least once per week
learn <- get_indic(learn_indic, ccs)
learn$Facet <- "Capacity for learning"
unique(learn$Indicator)

indic <- full_join(buff, sorg)
indic <- full_join(indic, learn)
indic <- full_join(indic, tfr)
indic <- full_join(indic, own)
# indic <- full_join(indic, wealth)

indic$SurveyYear <- as.numeric(indic$SurveyYear)

dim(indic)

vars <- group_by(indic, IndicatorId, ByVariableId) %>%
    summarise(Indicator=Indicator[1],
              ByVariableLabel=ByVariableLabel[1])
write.csv(vars, file='DHS_Variables_Key.csv', row.names=FALSE)

names(indic)[names(indic == "Region_ID")] <- indic$REG_ID

save(indic, file=file.path(prefix, "GRP", 
                           "Resilience_Indicator", "DHS_Export.RData"))
write.csv(indic, file=file.path(prefix, "GRP", 
                                "Resilience_Indicator", "DHS_Export.csv"), 
          row.names=FALSE)
