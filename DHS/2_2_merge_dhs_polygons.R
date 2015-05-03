library(RJSONIO)
library(dplyr)

source('../0_settings.R')

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

grep_list('water')

###############################################################################
###  Build indicator

grp_list <- read.csv(file.path(prefix, "GRP", "GRP_Countries.csv"))
# Filter for only GRP countries
grp_list <- grp_list[grp_list$GRP == "Yes", ]
# Filter for only countries with DHS data available
grp_list <- grp_list[grp_list$ISO_Code %in% country_list$ISO2_CountryCode, ]

grp_list <- merge(grp_list, country_list, by.x="ISO_Code", by.y="ISO2_CountryCode")

ccs <- as.character(grp_list$DHS_CountryCode)

get_indic <- function(indicator_IDs, ccs) {
    # Note DHS uses "NI" for Niger instead of the correct, "NE")
    cc_string <- paste0("countryIds=", paste(ccs, collapse=","))
    indicators_string <- paste0("indicatorIds=", paste(indicator_IDs, collapse=","))
    api_base <- "http://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&APIkey=CVINTL-877527&perpage=5000"
    api_call <- paste0(api_base, "&", indicators_string, "&", cc_string, "&f=json")
    json_file <- fromJSON(api_call)
    # Unlist the JSON file entries
    d <- lapply(json_file$Data, function(x) {unlist(x)})
    # Convert JSON input to a data frame
    d <- as.data.frame(do.call("rbind", d), stringsAsFactors=FALSE)
    d <- tbl_df(d)
    return(d)
}

buff_indic <- c(214527002, # Children stunted
                70254002, # Infant mortality rate
                6136002, # Population age 6 and over who attended secondary education
                120147001) # Household size)
#TODO: Make assets index
buff <- get_indic(buff_indic, ccs)
stopifnot(nrow(buff) < 5000)
buff$Facet <- "Buffer capacity"
unique(buff$Indicator)

sorg_indic <- c(8139010, # Piped water
                8142001) # Access to electricity
sorg <- get_indic(sorg_indic, ccs)
stopifnot(nrow(sorg) < 5000)
sorg$Facet <- "Self-organization"
unique(sorg$Indicator)

learn_indic <- c(127383002, # women who are literate
                 7135002, # pop 6-15 who are attending school
                 9124003, # households possessing a phone
                 13126005) # percentage women with access to newspaper, television, and radio at least once per week
learn <- get_indic(learn_indic, ccs)
stopifnot(nrow(learn) < 5000)
learn$Facet <- "Capacity for learning"
unique(learn$Indicator)

indic <- full_join(buff, sorg)
indic <- full_join(indic, learn)

table(indic$CountryName == "Niger")
table(indic$CountryName == "Niger", indic$SurveyYear)

indic$SurveyYear <- as.numeric(indic$SurveyYear)

indic <- indic[indic$SurveyYear >= 2002, ]
table(indic$SurveyYear)
table(indic$CountryName)

dim(indic)
# Drop all but most recent survey data, and take mean when both men/women are 
# supplied
indic <- group_by(indic, CountryName) %>%
    filter(SurveyYear == max(SurveyYear)) %>%
    group_by(Facet, DHS_CountryCode, CountryName, SurveyYear, SurveyId, 
             Indicator, IndicatorId, RegionId, CharacteristicLabel) %>%
    summarize(Value=mean(as.numeric(Value)))
dim(indic)
table(indic$SurveyYear)
table(indic$CountryName)

hist(indic$SurveyYear)

# Add in my region names
indic <- left_join(indic, dplyr::select(grp_list, DHS_CountryCode, Region))

# Make the income variables a sum
    
# 10485003, # Percent in middle wealth quintile or above (women)
# 10485004, # Percent in fourth quintile or above (women)
# 10485005) # Percent in highest quintile or above (women)

names(indic)[names(indic) == "Region"] <- "RegionGRP"

save(indic, file=file.path(prefix, "GRP", 
                           "Resilience_Indicator","resil_raw_DHS_data.RData"))
write.csv(indic, file=file.path(prefix, "GRP", 
                                "Resilience_Indicator","resil_raw_DHS_data.csv"), 
          row.names=FALSE)
