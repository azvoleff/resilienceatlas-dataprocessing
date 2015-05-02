###############################################################################
###############################################################################
# Process Ethiopia LSMS data
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

base_dir <- "O:/Data/Vital Signs/LSMS_Data"

lsms_folder <- file.path(prefix, "LSMS")

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
hh_geo <- read.spss(file.path(prefix, 
                              "GRP/LSMS/ET/2011/Pub_ETH_HouseholdGeovariables_Y1.sav"),
                    to.data.frame=TRUE)
hh_geo <- clear.labels(hh_geo)
hh_geo <- tbl_df(hh_geo)

# Load pre-planting data on field sizes
sect3_pp_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect3_pp_w1.sav"), 
                         to.data.frame=TRUE)
sect3_pp_w1 <- clear.labels(sect3_pp_w1)
sect3_pp_w1 <- tbl_df(sect3_pp_w1)

# Load education data from household questionnaire
sect2_hh_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect2_hh_w1.sav"), 
                         to.data.frame=TRUE)
sect2_hh_w1 <- clear.labels(sect2_hh_w1)
sect2_hh_w1 <- tbl_df(sect2_hh_w1)

# Load post-planting data on crops planted
sect4_pp_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect4_pp_w1.sav"), 
                         to.data.frame=TRUE)
sect4_pp_w1 <- clear.labels(sect4_pp_w1)
sect4_pp_w1 <- tbl_df(sect4_pp_w1)

# Load post-planting data on seeds
sect5_pp_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect5_pp_w1.sav"), 
                         to.data.frame=TRUE)
sect5_pp_w1 <- clear.labels(sect5_pp_w1)
sect5_pp_w1 <- tbl_df(sect5_pp_w1)

# Load post-planting data section 7 - data on holder (use of extension, etc)
sect7_pp_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect7_pp_w1.sav"), 
                 to.data.frame=TRUE)
sect7_pp_w1 <- clear.labels(sect7_pp_w1)
sect7_pp_w1 <- tbl_df(sect7_pp_w1)

# Load post harvest data section 9 - data on harvest by field
sect9_ph_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect9_ph_w1.sav"), 
                 to.data.frame=TRUE)
sect9_ph_w1 <- clear.labels(sect9_ph_w1)
sect9_ph_w1 <- tbl_df(sect9_ph_w1)

# Load post harvest data section 9 - data on harvest by field
area_conv <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/ET_local_area_unit_conversion.sav"), 
                 to.data.frame=TRUE)
area_conv <- clear.labels(area_conv)
area_conv <- tbl_df(area_conv)

# dim(sect9_ph_w1)
# dim(hh_geo)
sect9_ph_w1 <- merge(sect9_ph_w1, select(hh_geo, household_id, LON_DD_MOD, 
                                         LAT_DD_MOD, srtm), all.x=TRUE)
# dim(sect9_ph_w1)
# table(is.na(sect9_ph_w1$LON_DD_MOD))

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

# Add kilos and grams for self-reported yield
sect9_ph_w1$yld_self_kg <- sect9_ph_w1$ph_s9q12_a + sect9_ph_w1$ph_s9q12_b/1000
hist(sect9_ph_w1$yld_self_kg)
hist(filter(sect9_ph_w1, yld_self_kg < 2000)$yld_self_kg)
mean(sect9_ph_w1$yld_self_kg, na.rm=TRUE)

# Add up kg and grams for cut (fresh weight)
sect9_ph_w1$yld_cut_fresh_kg <- sect9_ph_w1$ph_s9q03_a + sect9_ph_w1$ph_s9q03_b/1000
# Crop cutting is from 4 m sq. (2m x 2m plots). Convert to per ha.
sect9_ph_w1$yld_cut_fresh_kg_ha <- (sect9_ph_w1$yld_cut_fresh_kg / 4) * 10000
hist(sect9_ph_w1$yld_cut_fresh_kg_ha)
hist(filter(sect9_ph_w1, yld_cut_fresh_kg_ha < 5000)$yld_cut_fresh_kg_ha)
mean(sect9_ph_w1$yld_cut_fresh_kg_ha, na.rm=TRUE)

# Add up kg and grams for cut (dry weight)
sect9_ph_w1$yld_cut_dry_kg <- sect9_ph_w1$ph_s9q05_a + sect9_ph_w1$ph_s9q05_b/1000
# Crop cutting is from 4 m sq. (2m x 2m plots). Convert to per ha.
sect9_ph_w1$yld_cut_dry_kg_ha <- (sect9_ph_w1$yld_cut_dry_kg / 4) * 10000
hist(sect9_ph_w1$yld_cut_dry_kg_ha)
hist(filter(sect9_ph_w1, yld_cut_dry_kg_ha < 5000)$yld_cut_dry_kg_ha)
mean(sect9_ph_w1$yld_cut_dry_kg_ha, na.rm=TRUE)

###############################################################################
# Crop areas - to get crop area need to first load field areas, then figure out 
# what percentage of each field was planted with each crop.

# Limit analysis to only include fields that were measured with either map and 
# compass (pp_s3q08_a == Yes) or GPS (pp_s3q04 == Yes)
dim(sect3_pp_w1)
sect3_pp_w1 <- filter(sect3_pp_w1, (pp_s3q04 == "Yes") | (pp_s3q08_a == "Yes"))
dim(sect3_pp_w1)

# TODO: use area conversions here to also include fields without GPS or 
# map/compass measurement that were reported in local units.

# Use GPS area when available:
sect3_pp_w1$field_area_sq_m <- ifelse(sect3_pp_w1$pp_s3q04 == "Yes",
                                sect3_pp_w1$pp_s3q05_c, # GPS measurement
                                sect3_pp_w1$pp_s3q08_b) # map/compass measurement
sect3_pp_w1 <- filter(sect3_pp_w1, !is.na(field_area_sq_m))

# Trim top 1% and bottom 1% of fields by area
dim(sect3_pp_w1)
sect3_pp_w1 <- filter(ungroup(sect3_pp_w1),
                      (field_area_sq_m < quantile(field_area_sq_m, .99)) &
                      (field_area_sq_m > quantile(field_area_sq_m, .01)))
dim(sect3_pp_w1)
hist(sect3_pp_w1$field_area_sq_m)

sect3_pp_w1$field_area_ha <- sect3_pp_w1$field_area_sq_m / 10000
hist(sect3_pp_w1$field_area_ha)

###############################################################################
# Ensure pre and post-harvest crop codes match

sect4_pp_w1$crop_name <- gsub(' *$', '', as.character(sect4_pp_w1$crop_name))
sect5_pp_w1$crop_name <- gsub(' *$', '', as.character(sect5_pp_w1$crop_name))
sect9_ph_w1$crop_name <- gsub(' *$', '', as.character(sect9_ph_w1$crop_name))

# sort(unique(c(sect4_pp_w1$crop_name, sect5_pp_w1$crop_name, sect9_ph_w1$crop_name)))
# sort(unique(sect4_pp_w1$crop_name))
# sort(unique(sect5_pp_w1$crop_name))
# sort(unique(sect9_ph_w1$crop_name))

sect4_pp_w1$crop_name[sect4_pp_w1$crop_name == ""] <- "UNKNOWN"
sect5_pp_w1$crop_name[sect5_pp_w1$crop_name == ""] <- "UNKNOWN"
sect9_ph_w1$crop_name[sect9_ph_w1$crop_name == ""] <- "UNKNOWN"

# Fix errors
sect4_pp_w1[sect4_pp_w1$crop_name == "OTHER ROOT C", ]$crop_name <- "OTHER ROOT CROPS"
sect5_pp_w1[sect5_pp_w1$crop_name == "OTHER ROOT C", ]$crop_name <- "OTHER ROOT CROPS"
sect4_pp_w1[sect4_pp_w1$crop_name == "OTHER CASE CROPS", ]$crop_name <- "OTHER CASH CROPS"
sect5_pp_w1[sect5_pp_w1$crop_name == "OTHER CASE CROPS", ]$crop_name <- "OTHER CASH CROPS"

crop_names <- sort(unique(c(sect4_pp_w1$crop_name,
                            sect5_pp_w1$crop_name,
                            sect9_ph_w1$crop_name)))
crop_codes_numeric <- sprintf('%03i', seq(1:length(crop_names)))

sect4_pp_w1$crop_code <- crop_codes_numeric[match(sect4_pp_w1$crop_name, crop_names)]
sect5_pp_w1$crop_code <- crop_codes_numeric[match(sect5_pp_w1$crop_name, crop_names)]
sect9_ph_w1$crop_code <- crop_codes_numeric[match(sect9_ph_w1$crop_name, crop_names)]

sect4_pp_w1$crop_code <- factor(sect4_pp_w1$crop_code,
                                levels=crop_codes_numeric, labels=crop_names)
sect5_pp_w1$crop_code <- factor(sect5_pp_w1$crop_code,
                                levels=crop_codes_numeric, labels=crop_names)
sect9_ph_w1$crop_code <- factor(sect9_ph_w1$crop_code,
                                levels=crop_codes_numeric, labels=crop_names)

sect3_pp_w1$parcel_id <- sprintf('%02i', sect3_pp_w1$parcel_id)
sect4_pp_w1$parcel_id <- sprintf('%02i', sect4_pp_w1$parcel_id)
sect5_pp_w1$parcel_id <- sprintf('%02i', sect5_pp_w1$parcel_id)
sect9_ph_w1$parcel_id <- sprintf('%02i', sect9_ph_w1$parcel_id)

###############################################################################
# Setup crop_id numbers

sect4_pp_w1 <- group_by(sect4_pp_w1, holder_id, parcel_id, field_id) %>%
    mutate(crop_id=paste0(holder_id, parcel_id, field_id, crop_code))
dim(sect4_pp_w1)
length(unique(sect4_pp_w1$crop_id))
# Have more than one of the same crop_code in a single parcel 4 times.  Ignore 
# for now, need to fix this later.

sect5_pp_w1 <- group_by(sect5_pp_w1, holder_id, parcel_id, field_id) %>%
    mutate(crop_id=paste0(holder_id, parcel_id, field_id, crop_code))
dim(sect5_pp_w1)
length(unique(sect5_pp_w1$crop_id))
# Have more than one of the same crop_code in a single parcel 6 times.  Ignore 
# for now, need to fix this later.

sect9_ph_w1 <- group_by(sect9_ph_w1, holder_id, parcel_id, field_id) %>%
    mutate(crop_id=paste0(holder_id, parcel_id, field_id, crop_code))
dim(sect9_ph_w1)
length(unique(sect9_ph_w1$crop_id))
# Have more than one of the same crop_code in a single parcel 2 times.  Ignore 
# for now, need to fix this later.

###############################################################################
# Convert self-reported yields to yields per ha

# Look at top 3 crops by region? Could model effect of drought on yields for 
# top 3 crops in each region.
dim(sect3_pp_w1)
dim(sect4_pp_w1)
dim(sect9_ph_w1)

# sect9_ph

table(with(sect4_pp_w1, paste0(holder_id, parcel_id, field_id)) %in%
      with(sect9_ph_w1, paste0(holder_id, parcel_id, field_id)))

table(with(sect3_pp_w1, paste0(holder_id, parcel_id, field_id)) %in%
      with(sect9_ph_w1, paste0(holder_id, parcel_id, field_id)))

# Merge the by-crop post-harvest dataset (sect9) with by-field pre-planting 
# dataset (sect3)
yield <- left_join(sect9_ph_w1, sect3_pp_w1)
dim(yield)

# Add in the by-crop post-planting dataset on crops planted (sect4)
yield <- left_join(yield, sect4_pp_w1)
dim(yield)

# Add in the by-crop post-planting dataset on seeds (sect5)
yield <- left_join(yield, sect5_pp_w1)
dim(yield)

# Add in the data on holder (use of extension, etc.)
yield <- left_join(yield, sect7_pp_w1)
dim(yield)

table(yield$pp_s4q09)

# pp_s4q02 is was field a pure stand
# pp_s4q03 is percent of field that was planted with this crop

table(yield$pp_s4q02, useNA="always")
table(yield$pp_s4q03, useNA="always")
hist(yield$pp_s4q09)

yield$field_pct_planted <- ifelse(yield$pp_s4q02 == "Pure Stand",
                                  100,
                                  yield$pp_s4q03)
table(yield$field_pct_planted)


# Check for consistency - how much of field is planted?
chk <- group_by(yield, holder_id, parcel_id, field_id) %>%
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

table(!(is.na(yield$yld_self_kg_ha)) | !(is.na(yield$yld_cut_dry_kg_ha)))

# Preferentially use fields from crop-cutting
yield$yld_kg_ha <- ifelse(!is.na(yield$yld_cut_dry_kg_ha),
                          yield$yld_cut_dry_kg_ha,
                          yield$yld_self_kg_ha)
yield$yld_source <- ifelse(!is.na(yield$yld_cut_dry_kg_ha),
                           "Crop cut",
                           "Self-report")

mean(yield$yld_kg_ha, na.rm=TRUE)
mean(yield$yld_self_kg_ha, na.rm=TRUE)
mean(yield$yld_cut_dry_kg_ha, na.rm=TRUE)
mean(yield$yld_cut_fresh_kg_ha, na.rm=TRUE)
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
table(yield$pp_s3q11)
yield$land_ext <- yield$pp_s3q11 == 1

# Irrigation
table(yield$pp_s3q12)
yield$irrigation <- yield$pp_s3q12 == "Yes"

# Fertilizer
table(yield$pp_s3q14)
yield$fertilizer <- yield$pp_s3q14 == "Yes"

# Chemicals

table(yield$pp_s4q04)
yield$pesticide <- ifelse(yield$pp_s4q04 == "Yes",
                          yield$pp_s4q05 == "Yes",
                          FALSE)
table(yield$pesticide, useNA="always")

yield$herbicide <- ifelse(yield$pp_s4q04 == "Yes",
                          yield$pp_s4q06 == "Yes",
                          FALSE)
table(yield$herbicide, useNA="always")

yield$fungicide <- ifelse(yield$pp_s4q04 == "Yes",
                          yield$pp_s4q07 == "Yes",
                          FALSE)
table(yield$fungicide, useNA="always")

# See section 5 for info on improved seeds or not
yield$improved_seed <- ifelse(yield$pp_s5q01 == "Improved",
                              TRUE,
                              FALSE)
table(yield$pp_s5q01, useNA="always")

# Calculate summary stats on percent with irrigation, improved seeds, and 
# extension services


# See section 7 for more on extension program, and whether farmer gets advisory 
# services or credit services.

yield$hldr_crop_rotation <- yield$pp_s7q01 == "Yes"
table(yield$hldr_crop_rotation, useNA="always")

yield$hldr_chem_fert <- yield$pp_s7q01 == "Yes"
table(yield$hldr_chem_fert, useNA="always")

yield$hldr_ext <- yield$pp_s7q04 == "Yes"
table(yield$hldr_ext, useNA="always")

yield$hldr_svc_credit <- yield$pp_s7q06 == "Yes"
table(yield$hldr_svc_credit, useNA="always")

yield$hldr_svc_advisory <- yield$pp_s7q08 == "Yes"
table(yield$hldr_svc_advisory, useNA="always")

# TODO: Education

# TODO: Soil quality - see the geo variables accompanying dataset (only 
# available for GPS'ed fields)

###############################################################################
# Add harvest start dates

yield$harvest_start_month <- yield$ph_s9q13_a
table(yield$harvest_start_month, useNA="always")

# Eliminate households who stated harvest after month 6 (February 2012)
dim(yield)
yield <- filter(yield, harvest_start_month < 6)
dim(yield)

yield$harvest_start_date <- ymd("2011/8/1") %m+% months(yield$harvest_start_month)
yield$harvest_start_date <- as.Date(yield$harvest_start_date)
table(yield$harvest_start_month, useNA="always")
table(yield$harvest_start_date, useNA="always")

###############################################################################
# Merge climate data

# Can only merge yield data for points that are GPS'ed
dim(yield)
yield <-  filter(ungroup(yield), !is.na(LON_DD_MOD))
dim(yield)
yield <-  filter(ungroup(yield), !is.na(LAT_DD_MOD))
dim(yield)

# Extract SPI for LSMS fields
precip_anom_12 <- brick(file.path(prefix, "GRP", "CHIRPS", 'ET_v1p8chirps_monthly_12mth_anom.tif'))

# Load climatology so it can be controlled for
precip_mean_annual <- raster(file.path(prefix, "GRP", "CHIRPS", 
                                       'ET_v1p8chirps_monthly_ppt_mean_12mth.tif'))

yield$temp_id <- seq(1:nrow(yield))

yield_sp <- SpatialPointsDataFrame(cbind(yield$LON_DD_MOD, 
                                         yield$LAT_DD_MOD),
                                   data=data.frame(yield), 
                                   proj4string=CRS("+init=epsg:4326"))

# Note the below code is INCLUSIVE of the start date
chirps_start_date <- as.Date('1981/1/1')
# Note the below code is EXCLUSIVE of the end date
chirps_end_date <- as.Date('2014/5/1')
yrs <- seq(year(chirps_start_date), year(chirps_end_date))
chirps_dates <- seq(chirps_start_date, chirps_end_date, by='months')
chirps_dates <- chirps_dates[chirps_dates < chirps_end_date]


# Extract SPI for LSMS fields
precip_anom_12_wide <- data.frame(extract(precip_anom_12, yield_sp))
names(precip_anom_12_wide) <- chirps_dates
precip_anom_12_wide$temp_id <- seq(1:nrow(precip_anom_12_wide))
precip_anom_12_long <- melt(precip_anom_12_wide, id.vars="temp_id", variable.name="date", 
                       value.name="precip_anom_12")
precip_anom_12_long$date <- as.Date(precip_anom_12_long$date)

# Extract mean annual precip for LSMS fields
precip_mean_annual_long <- data.frame(extract(precip_mean_annual, yield_sp))
names(precip_mean_annual_long) <- "precip_mean_annual"
precip_mean_annual_long$temp_id <- seq(1:nrow(precip_mean_annual_long))

dim(yield)
yield <- left_join(yield, precip_anom_12_long,
                   by=c("temp_id", "harvest_start_date" = "date"))
yield <- left_join(yield, precip_mean_annual_long, by=c("temp_id"))
dim(yield)
hist(yield$precip_anom_12)

# Convert precip measurements to units of 100s of mm
yield$precip_anom_12 <- yield$precip_anom_12 / 100
yield$precip_mean_annual <- yield$precip_mean_annual / 100

yield$srtm_cut <- cut(yield$srtm, c(0, 1000, 1500, 2000, 2500, 3500))

# Remake the spatial dataframe with the new variables included
yield_sp <- SpatialPointsDataFrame(cbind(yield$LON_DD_MOD, 
                                         yield$LAT_DD_MOD),
                                   data=data.frame(yield), 
                                   proj4string=CRS("+init=epsg:4326"))

###############################################################################
# Plots

ggplot(filter(yield, Crop %in% c("Maize", "Sorghum", "Teff", "Barley"))) +
    geom_bar(aes(yld_kg_ha, fill=Crop), type="identity") +
    facet_wrap(~Crop)

###############################################################################
# Now model yields

# Restrict to only include fields with planting date and harvest date w/in 
# particular period (need to convert Ethiopian dates to Gregorian calendar)

# TODO: consider the impact of join by=() on sample size

lm.beta.lmer <- function(mod) {
   b <- fixef(mod)[-1]
   stderror <- se.coef(mod)$fixef[-1]
   sd.x <- apply(getME(mod,"X")[,-1], 2, sd)
   sd.y <- sd(getME(mod, "y"))
   data.frame(coef=b*sd.x/sd.y, stderr=stderror*sd.x/sd.y)
}

yield_maize <- filter(yield, Crop == "Maize") %>%
    select(yld_kg_ha, fertilizer, improved_seed, hldr_ext, 
           hldr_svc_credit, srtm_cut, precip_mean_annual, 
           precip_anom_12,  ea_id)
maize_model <- lmer(yld_kg_ha ~ . - ea_id + srtm_cut + (1 | ea_id), data=yield_maize)
summary(maize_model)
lm.beta.lmer(maize_model)
summary(yield_maize)

# maize_sp <- subset(yield_sp, Crop == "Maize")
# maize_sp <- maize_sp[, c("yld_kg_ha", "fertilizer", "improved_seed", 
#                          "hldr_ext", "hldr_chem_fert", "hldr_svc_credit", 
#                          "srtm_cut", "precip_mean_annual", "precip_anom_12", 
#                          "ea_id")]
# maize_sp <- subset(maize_sp, complete.cases(maize_sp@data))

# yield_maize_gwr_bw <- gwr.sel(yld_kg_ha ~ fertilizer + improved_seed + hldr_ext 
#                               + hldr_chem_fert + hldr_svc_credit + srtm_cut + 
#                               precip_mean_annual + precip_anom_12,
#                               data=maize_sp,
#                               gweight=gwr.Gauss)
#
# yield_maize_gwr <- gwr(yld_kg_ha ~ fertilizer + improved_seed + hldr_ext + 
#                        hldr_chem_fert + hldr_svc_credit + srtm_cut + 
#                        precip_mean_annual + precip_anom_12,
#                        data=maize_sp,
#                        bandwidth=500)
#

# spplot(yield_maize_gwr$SDF, c("precip_mean_annual"))
# spplot(yield_maize_gwr$SDF, c("precip_anom_12"))
# spplot(yield_maize_gwr$SDF, c("fertilizerTRUE"))
#
# summary(lm(yld_kg_ha ~ . - ea_id, data=yield_maize))

yield_barley <- filter(yield, Crop == "Barley") %>%
    select(yld_kg_ha, fertilizer, hldr_ext, 
           hldr_svc_credit, srtm_cut, precip_mean_annual, 
           precip_anom_12,  ea_id)
barley_model <- lmer(yld_kg_ha ~ . - ea_id + (1 | ea_id), data=yield_barley)
summary(barley_model)
lm.beta.lmer(barley_model)
summary(yield_barley)

yield_teff <- filter(yield, Crop == "Teff") %>%
    select(yld_kg_ha, fertilizer, hldr_ext, 
           hldr_svc_credit, srtm_cut, precip_mean_annual, precip_anom_12,  
           ea_id)
teff_model <- lmer(yld_kg_ha ~ . - ea_id + (1 | ea_id), data=yield_teff)
summary(teff_model)
lm.beta.lmer(teff_model)
summary(yield_teff)

yield_sorghum <- filter(yield, Crop == "Sorghum") %>%
    select(yld_kg_ha, fertilizer, hldr_ext, 
           hldr_chem_fert, hldr_svc_credit, precip_anom_12,  ea_id)
sorghum_model <- lmer(yld_kg_ha ~ . - ea_id + (1 | ea_id), data=yield_sorghum)
# summary(lm(yld_kg_ha ~ . - ea_id, data=yield_sorghum))
summary(yield_sorghum)

# spplot(subset(yield_sp, yield_sp$Crop == "Maize"), "yld_kg_ha")
# spplot(subset(yield_sp, yield_sp$Crop == "Teff"), "yld_kg_ha")
# spplot(subset(yield_sp, yield_sp$Crop == "Barley"), "yld_kg_ha")
# spplot(subset(yield_sp, yield_sp$Crop == "Sorghum"), "yld_kg_ha")

get_std_error <- function(model, term) {
    term_num <- which(names(fixef(model)) == term)
    stopifnot(length(term_num) == 1)
    se.coef(model)$fixef[term_num]
}


# Plot coefficients from all three models for report
yield_sensitivity <- c((fixef(maize_model)["precip_mean_annual"]/fixef(maize_model)["(Intercept)"]),
                       (fixef(maize_model)["precip_anom_12"]/fixef(maize_model)["(Intercept)"]),
                       (fixef(teff_model)["precip_mean_annual"]/fixef(teff_model)["(Intercept)"]),
                       (fixef(teff_model)["precip_anom_12"]/fixef(teff_model)["(Intercept)"]),
                       (fixef(barley_model)["precip_mean_annual"]/fixef(barley_model)["(Intercept)"]),
                       (fixef(barley_model)["precip_anom_12"]/fixef(barley_model)["(Intercept)"]))

std_errors <- c((get_std_error(maize_model, "precip_mean_annual")/fixef(maize_model)["(Intercept)"]),
                (get_std_error(maize_model, "precip_anom_12")/fixef(maize_model)["(Intercept)"]),
                (get_std_error(teff_model, "precip_mean_annual")/fixef(teff_model)["(Intercept)"]),
                (get_std_error(teff_model, "precip_anom_12")/fixef(teff_model)["(Intercept)"]),
                (get_std_error(barley_model, "precip_mean_annual")/fixef(barley_model)["(Intercept)"]),
                (get_std_error(barley_model, "precip_anom_12")/fixef(barley_model)["(Intercept)"]))

sensitivity <- data.frame(crop=c('Maize', 'Maize', 'Teff',
                                 'Teff', 'Barley', 'Barley'),
                          type=c('Long-term', 'Short-term', 'Long-term',
                                 'Short-term', 'Long-term', 'Short-term'),
                          yield_sensitivity=yield_sensitivity,
                          std_err=std_errors)

sensitivity$crop <- ordered(sensitivity$crop, level=c('Maize', 'Teff', 
                                                      'Barley'))

# Note: these results are consistent with FAO data - Barley needs less water 
# than maize, so would tend to be growing in drier areas

p <- ggplot(sensitivity, aes(type, yield_sensitivity, colour=type)) + 
    theme_bw(base_size=8) +
    geom_point(size=1) + facet_grid(~crop) +
    geom_errorbar(aes(ymin=-1.96*std_err+yield_sensitivity,
                      ymax=1.96*std_err+yield_sensitivity, width=.1), size=.4) +
    geom_errorbar(aes(ymin=-1*std_err+yield_sensitivity,
                      ymax=1*std_err+yield_sensitivity, width=.25), size=.4, 
                  linetype=2) +
    ylab("Change in yield (%)") +
    scale_y_continuous(labels=percent_format()) +
    theme(axis.title.x=element_blank(),
          legend.position="none")
ggsave(paste0("ET", "_yield_sensitivity.png"), p, width=4, height=2, 
       dpi=PLOT_DPI)
ggsave(paste0("ET", "_yield_sensitivity.eps"), p, width=4, height=2, 
       dpi=PLOT_DPI)

p <- ggplot(filter(sensitivity, type == "Short-term"),
            aes(type, yield_sensitivity, colour=type)) + 
    theme_bw(base_size=8) +
    geom_point(size=1) + facet_grid(~crop) +
    geom_errorbar(aes(ymin=-1.96*std_err+yield_sensitivity,
                      ymax=1.96*std_err+yield_sensitivity, width=.1), size=.4) +
    geom_errorbar(aes(ymin=-1*std_err+yield_sensitivity,
                      ymax=1*std_err+yield_sensitivity, width=.25), size=.4, 
                  linetype=2) +
    ylab("Change in yield (%)") +
    scale_y_continuous(labels=percent_format()) +
    theme(axis.title.x=element_blank(),
          legend.position="none")
ggsave(paste0("ET", "_yield_sensitivity_no_seasonal.png"), p, width=4, height=2, 
       dpi=PLOT_DPI)
ggsave(paste0("ET", "_yield_sensitivity_no_seasonal.eps"), p, width=4, height=2, 
       dpi=PLOT_DPI)
