###############################################################################
###############################################################################
# Process Uganda LSMS data
###############################################################################
###############################################################################

source('../0_settings.R')

library(arm) # for se.coef
library(foreign)
library(spgwr)
library(raster)
library(spgwr)
library(scales)
library(reshape2)
library(sp)
library(dplyr)
library(ggplot2)
library(lme4)
library(lubridate)

PLOT_DPI <- 600

base_dir <- "~/Documents/GRP"

lsms_folder <- file.path(prefix, "LSMS")

### do I need to keep these lines that relate to .spss data? I am using .dta
# Below removes the value.labels returned by read.spss that mess up dplyr
clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]], "label") <- NULL
    for(i in 1 : length(x)) attr(x[[i]], "value.labels") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
    attr(x, "value.labels") <- NULL
  }
  return(x)
}

# Load geo-referencing data
ug_hh_geo <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/UNPS_Geovars_1112.dta"), convert.factors = TRUE)
ug_hh_geo <- clear.labels(ug_hh_geo)
ug_hh_geo <- tbl_df(ug_hh_geo)

## Load data on agricultural and labor inputs 
ag_sec3a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC3A.dta"), convert.factors = TRUE)
ag_sec3a <- clear.labels(ag_sec3a)
ag_sec3a <- tbl_df(ag_sec3a)

## Load post-planting data on crop planted and seeds
ag_sec4a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC4A.dta"), convert.factors = TRUE)
ag_sec4a <- clear.labels(ag_sec4a)
ag_sec4a <- tbl_df(ag_sec4a)

## Load education data from household questionaire
g_sec4 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC4.dta"), convert.factors = TRUE)
g_sec4 <- clear.labels(g_sec4)
g_sec4 <- tbl_df(g_sec4)

## Load pre-planting data on field sizes
ag_sec2a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC2A.dta"), convert.factors = TRUE)
ag_sec2a <- clear.labels(ag_sec2a)
ag_sec2a <- tbl_df(ag_sec2a)

ag_sec2b <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC2B.dta"), convert.factors = TRUE)
ag_sec2b <- clear.labels(ag_sec2b)
ag_sec2b <- tbl_df(ag_sec2b)

# Load extension services data
ag_sec9 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC9.dta"), convert.factors = TRUE)
ag_sec9 <- clear.labels(ag_sec9)
ag_sec9 <- tbl_df(ag_sec9)

# Load post harvest data section 5A - data on harvest by field
ag_sec5a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC5A.dta"), convert.factors = TRUE)
ag_sec5a <- clear.labels(ag_sec5a)
ag_sec5a <- tbl_df(ag_sec5a)

dim(ag_sec5a)
dim(ug_hh_geo)

ag_sec5a <- merge(ag_sec5a, dplyr::select(ug_hh_geo, HHID, lon_mod, 
                                                lat_mod, srtm_uga), all.x=TRUE)

dim(ag_sec5a)
table(is.na(ag_sec5a$lon_mod))

##### UPDATE LINE 88-100
# # was crop cut
# table(sect9_ph_w1$ph_s9q01)
# # when was crop cut
# table(sect9_ph_w1$ph_s9q02_a)
# table(sect9_ph_w1$ph_s9q02_b)
#
# table(sect9_ph_w1$ph_s9q02_b)
#
# # n households with crop cut
# length(unique(sect9_ph_w1[sect9_ph_w1$ph_s9q01 == "Yes", ]$household_id))
#
# # n crop cuts
# nrow(unique(sect9_ph_w1[sect9_ph_w1$ph_s9q01 == "Yes", ]))

###############################################################################
# Calculate yields in kg

## change conversion factor to 1 for Kilograms then calculate total kilos for self-reported yield

ag_sec5a$a5aq6d[ag_sec5a$a5aq6c == 'Kilogram (KG)' & is.na(ag_sec5a$a5aq6d)] <-1
ag_sec5a$yld_self_kg <- (ag_sec5a$a5aq6a) * (ag_sec5a$a5aq6d)
hist(ag_sec5a$yld_self_kg)
hist(filter(ag_sec5a, yld_self_kg < 2000)$yld_self_kg)
mean(ag_sec5a$yld_self_kg, na.rm=TRUE)

# Add up kg and grams for cut (fresh weight)

fresh_raw_rows <- !is.na(ag_sec5a$a5aq6b) & ( ("Fresh/raw harvested \x96 state not applicable" == ag_sec5a$a5aq6b) | 
  ("Green harvested \x96 with shell/cob and with stalk" == ag_sec5a$a5aq6b) | 
  ("Green harvested \x96 in the cob" == ag_sec5a$a5aq6b) | 
  ("Fresh/raw harvested \x96 with shell/cob without stalk" == ag_sec5a$a5aq6b) | 
  ("Fresh/raw harvested \x96 in pods or shell/husks" == ag_sec5a$a5aq6b) | 
  ("Green harvested \x96 in the pods" == ag_sec5a$a5aq6b) | 
  ("Fresh/raw harvested \x96 with shell/cob and with stalk" == ag_sec5a$a5aq6b) | 
  ("Fresh/raw harvested \x96 in the cob" == ag_sec5a$a5aq6b) )
ag_sec5a$yld_cut_fresh_kg <- NA
ag_sec5a$yld_cut_fresh_kg[fresh_raw_rows] <-  ag_sec5a$yld_self_kg[fresh_raw_rows]
### not sure what is the plot size
hist(ag_sec5a$yld_cut_fresh_kg)
hist(filter(ag_sec5a, yld_cut_fresh_kg < 5000)$yld_cut_fresh_kg)
mean(ag_sec5a$yld_cut_fresh_kg, na.rm=TRUE)

# Add up kg and grams for cut (dry weight)
dry_rows <- !is.na(ag_sec5a$a5aq6b) & ( ("Dry at harvest \x96 with shell/cob without stalk" == ag_sec5a$a5aq6b) | 
                                                ("Dry at harvest \x96 grain" == ag_sec5a$a5aq6b) | 
                                                ("Dry \x96 grain" == ag_sec5a$a5aq6b) | 
                                                ("Dry at harvest \x96 with shell/cob and with stalk" == ag_sec5a$a5aq6b) | 
                                                ("Dry at harvest  \x96 in the cob" == ag_sec5a$a5aq6b) )
ag_sec5a$yld_cut_dry_kg <- NA
ag_sec5a$yld_cut_dry_kg[dry_rows] <-  ag_sec5a$yld_self_kg[dry_rows]
## again not sure what is plot size 
hist(ag_sec5a$yld_cut_dry_kg)
hist(filter(ag_sec5a, yld_cut_dry_kg < 5000)$yld_cut_dry_kg)
mean(ag_sec5a$yld_cut_dry_kg, na.rm=TRUE)


###############################################################################
# Crop areas - to get crop area need to first load field areas, then figure out 
# what percentage of each field was planted with each crop.

# Limit analysis to only include fields that were measured with GPS either map and 
# compass (pp_s3q08_a == Yes) or GPS (pp_s3q04 == Yes)

dim(ag_sec2b)
ag_sec2b <- filter(ag_sec2b, (Visit_GPS_Parcel == "Yes"))
dim(ag_sec2b)

# TODO: use area conversions here to also include fields without GPS or 
# map/compass measurement that were reported in local units.

# Use GPS area when available (measurements only include GPS or respondet's estimations)
ag_sec2b$field_area_sq_m <- ifelse(ag_sec2b$Visit_GPS_Parcel == "Yes",
                                      ag_sec2b$a2bq4 * 4046.856) # GPS measurement
ag_sec2b <- filter(ag_sec2b, !is.na(field_area_sq_m))


# Trim top 1% and bottom 1% of fields by area
dim(ag_sec2b)
ag_sec2b <- filter(ungroup(ag_sec2b),
                      (field_area_sq_m < quantile(field_area_sq_m, .99)) &
                        (field_area_sq_m > quantile(field_area_sq_m, .01)))
dim(ag_sec2b)
hist(ag_sec2b$field_area_sq_m)

ag_sec2b$field_area_ha <- ag_sec2b$field_area_sq_m / 10000
hist(ag_sec2b$field_area_ha)

###############################################################################
# Ensure pre and post-harvest crop codes match(I have codes for post hervest and names for pre harvest; 
####do I have to change names to codes?)

## set up crop_ID numbers

# Convert self-reported yields to yields per ha

# Look at top 3 crops by region? Could model effect of drought on yields for 
# top 3 crops in each region.
dim(ag_sec2b)
dim(ag_sec4a)
dim(ag_sec5a)

## rename respondentID
names(ag_sec4a)[names(ag_sec4a) == "a4aq6_1"] <- "respondentID"
names(ag_sec5a)[names(ag_sec5a) == "a5aq5_1"] <- "respondentID"
names(ag_sec2b)[names(ag_sec2b) == "a2bq5_1"] <- "respondentID"

## ag_sec5a
table(with(ag_sec4a, paste0(respondentID, parcelID, plotID)) %in%
        with(ag_sec5a, paste0(respondentID, parcelID, plotID)))

table(with(ag_sec2b, paste0(respondentID, parcelID)) %in%
        with(ag_sec5a, paste0(respondentID, parcelID)))

# Merge the by-crop post-harvest dataset (ag_sec5a) with by-field pre-planting 
# dataset (ag_sec2b)
yield <- left_join(ag_sec5a, ag_sec2b)
dim(yield)

# Add in the by-crop post-planting dataset on crops planted (sect4)
## rename cropID in ag_sec4a to cropCODE in order to allow join
names(ag_sec4a)[names(ag_sec4a) == "cropID"] <- "cropCODE"
yield <- left_join(yield, ag_sec4a)
dim(yield)

###Add in the data on holder use of extension 
##(no information available but there is info about receiving advice on extension service)

yield <- left_join(yield, ag_sec9)
table(yield$a5aq17)

## Add in data on use of fertlizers and pesticides

yield <- left_join(yield, ag_sec3a)
table(yield$a3aq13)
table(yield$a3aq22)

# ag_sec4a$a4aq8 is was field a pure stand
# ag_sec4a$a4aq9 is percent of field that was planted with this crop

table(yield$a4aq8, useNA="always")
table(yield$a4aq9, useNA="always")
plot(yield$a5aq17)

yield$field_pct_planted <- ifelse(yield$a4aq8 == "Pure Stand",
                                  100,
                                  yield$a4aq9)
table(yield$field_pct_planted)


# Check for consistency - how much of field is planted?
chk <- group_by(yield, respondentID, parcelID, plotID) %>%
  summarize(pct=sum(field_pct_planted, na.rm=TRUE))
mean(chk$pct)
min(chk$pct)
max(chk$pct)
table(chk$pct > 100)

# Delete fields that were more than 100 percent planted
yield <- filter(yield, field_pct_planted <= 100)

# yld_self_kg_ha is equal to the self-reported yield in kg multiplied by the 
# percent of the field planted with that crop, divided by the field area in ha
yield <- mutate(yield, yld_self_kg_ha=(yld_self_kg * (field_pct_planted/100) / field_area_ha))
hist(yield$yld_self_kg_ha)

table(!(is.na(yield$yld_self_kg_ha)) | !(is.na(yield$yld_cut_dry_kg))) 


##### Preferentially use fields from crop-cutting
yield$yld_kg_ha <- ifelse(!is.na(yield$yld_cut_dry_kg),
                          yield$yld_cut_dry_kg,
                          yield$yld_self_kg_ha)
yield$yld_source <- ifelse(!is.na(yield$yld_cut_dry_kg),
                           "Crop cut",
                           "Self-report")
mean(yield$yld_kg_ha, na.rm=TRUE)
mean(yield$yld_self_kg_ha, na.rm=TRUE)
mean(yield$yld_cut_dry_kg, na.rm=TRUE)
mean(yield$yld_cut_fresh_kg, na.rm=TRUE)
table(yield$yld_source)

yield <- filter(yield, !is.na(yld_kg_ha))
dim(yield)

hist(yield$yld_kg_ha)

quantile(yield$yld_kg_ha, .99)
quantile(yield$yld_kg_ha, .01)

table((yield$yld_kg_ha < quantile(yield$yld_kg_ha, .99)) & (yield$yld_kg_ha > quantile(yield$yld_kg_ha, .01)))

# Trim top 1% and bottom 1% of yields
dim(yield)
yield <- filter(ungroup(yield), (yld_kg_ha < quantile(yld_kg_ha, .99)) & (yld_kg_ha > quantile(yld_kg_ha, .01)))
dim(yield)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
        sep="", collapse=" ")
}
yield$Crop <- unlist(lapply(as.character(yield$crop_name), simpleCap))

###############################################################################
# Now model yields of Maize, Sorghum, Teff, Barley

# Extension services
table(yield$a9q3)
yield$land_ext <- yield$a9q3 == "Yes"

# Irrigation
table(yield$a2bq16)
yield$irrigation <- yield$a2bq16 == "Irrigated"

# Chem Fertilizer
table(yield$a3aq13)
yield$chem_fertilizer <- yield$a3aq13 == "Yes"

# Pesticides

table(yield$a3aq22)
yield$pesticide <- yield$a3aq22 == "Yes"
table(yield$pesticide, useNA="always")

yield$insecticide <- yield$a3aq23 == "Insecticides"
table(yield$insecticide, useNA="always")

yield$fungicide <- yield$a3aq23 == "Fungicides"
table(yield$fungicide, useNA="always")

yield$growth_regulator <- yield$a3aq23 == "Growth regulators and harvest aids"
table(yield$growth_regulator, useNA="always")

# See section 4A for info on improved seeds
yield$improved_seed <- ifelse(yield$a4aq13 == "Improved",
                              TRUE,
                              FALSE)
table(yield$a4aq13, useNA="always")

# Calculate summary stats on percent with irrigation, improved seeds, and 
# extension services

# See section 7 for more on extension program, and whether farmer gets advisory 
# services or credit services.
## no answers from holders; the information looks the same as the one on fertilizers above

# TODO: Education

# TODO: Soil quality - see the geo variables accompanying dataset (only 
# available for GPS'ed fields)
### table(ag_seca2$a2aq17)

###############################################################################
# Add harvest start dates

yield$harvest_start_month <- yield$a5aq6e
table(yield$harvest_start_month, useNA="always")

# Eliminate households who stated harvest after month 6 (February 2012)
## not sure what this line is doing; the cropping season was from Jan-June 2011; 
##which months should I eliminate?

dim(yield)
yield <- filter(yield, harvest_start_month < 6)
dim(yield)

yield$harvest_start_date <- ymd("2011/8/1") %m+% months(yield$harvest_start_month)
yield$harvest_start_date <- as.Date(yield$harvest_start_date)
table(yield$harvest_start_month, useNA="always")
table(yield$harvest_start_date, useNA="always")