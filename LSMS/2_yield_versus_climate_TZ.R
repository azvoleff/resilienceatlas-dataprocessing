###############################################################################
###############################################################################
# Process Tanzania 2012 LSMS data
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
tz_hh_geo <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HouseholdGeovars_Y3.sav"),
                       to.data.frame=TRUE)
tz_hh_geo <- clear.labels(tz_hh_geo)
tz_hh_geo <- tbl_df(tz_hh_geo)

# Load pre-planting data on field sizes (long rainy season)
tz_ag_pp_lr <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/TZ/2012/AG_SEC_2A.SAV"),
                         to.data.frame=TRUE)
tz_ag_pp_lr <- clear.labels(tz_ag_pp_lr)
tz_ag_pp_lr <- tbl_df(tz_ag_pp_lr)

# Load education data from household questionnaire
tz_hh_edu <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HH_SEC_C.SAV"),
                       to.data.frame=TRUE)
tz_hh_edu <- clear.labels(tz_hh_edu)
tz_hh_edu <- tbl_df(tz_hh_edu)

# Load post-planting data on crops planted (long rainy season)
tz_ag_pop_lr <- read.spss(file.path(prefix, 
                                    "GRP/LSMS/TZ/2012/AG_SEC_3A.SAV"),
                          to.data.frame=TRUE)
tz_ag_pop_lr <- clear.labels(tz_ag_pop_lr)
tz_ag_pop_lr <- tbl_df(tz_ag_pop_lr)

# Load post-planting data on seeds (long rainy season)
tz_ag_sd <- read.spss(file.path(prefix, 
                                "GRP/LSMS/TZ/2012/AG_SEC_4A.SAV"),
                      to.data.frame=TRUE)
tz_ag_sd <- clear.labels(tz_ag_sd)
tz_ag_sd <- tbl_df(tz_ag_sd)

# Load post-planting data section 7 - data on holder (use of extension, etc)
tz_ag_ext <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/AG_SEC_12A.SAV"),
                       to.data.frame=TRUE)
tz_ag_ext <- clear.labels(tz_ag_ext)
tz_ag_ext <- tbl_df(tz_ag_ext)

# Load post harvest data section 9 - data on harvest by field (long rainy season)
tz_ag_hv <- read.spss(file.path(prefix, 
                                "GRP/LSMS/TZ/2012/AG_SEC_5A.SAV"),
                      to.data.frame=TRUE)
tz_ag_hv <- clear.labels(tz_ag_hv)
tz_ag_hv <- tbl_df(tz_ag_hv)

# Load local unit conversion information 
tz_conv <- read.spss(file.path(prefix, 
                                "GRP/LSMS/TZ/2012/COM_SEC_CG.SAV"),
                      to.data.frame=TRUE)
tz_conv <- clear.labels(tz_conv)
tz_conv <- tbl_df(tz_conv)

###############################################################################
# Calculate yields in kg

tz_ag_sd$yld_self_kg <- tz_ag_sd$ag4a_28
hist(tz_ag_sd$yld_self_kg)
hist(filter(tz_ag_sd, yld_self_kg < 2000)$yld_self_kg)
mean(tz_ag_sd$yld_self_kg, na.rm=TRUE)

## no information on cut fresh/cut dry quantity

###############################################################################
# Crop areas - to get crop area need to first load field areas, then figure out 
# what percentage of each field was planted with each crop.

## consult Alex but there is information on % of the field that was planted 
## in tz_ag_sd$ag4a_02


# Limit analysis to only include fields that were measured with GPS
## only GPS was used to measure the field, no compas was used here
dim(tz_ag_pp_lr)
tz_ag_pp_lr <- filter(tz_ag_pp_lr, (ag2a_07 == "YES"))
dim(tz_ag_pp_lr)

tz_ag_pp_lr$field_area_sq_m <- ifelse(tz_ag_pp_lr$ag2a_07 == "YES",
                                      tz_ag_pp_lr$ag2a_09) # GPS measurement
                                      
tz_ag_pp_lr <- filter(tz_ag_pp_lr, !is.na(field_area_sq_m))

# Trim top 1% and bottom 1% of fields by area
dim(tz_ag_pp_lr)
tz_ag_pp_lr <- filter(ungroup(tz_ag_pp_lr),
                      (field_area_sq_m < quantile(field_area_sq_m, .99)) &
                        (field_area_sq_m > quantile(field_area_sq_m, .01)))
dim(tz_ag_pp_lr)
hist(tz_ag_pp_lr$field_area_sq_m)

tz_ag_pp_lr$field_area_ha <- tz_ag_pp_lr$field_area_sq_m / 10000
hist(tz_ag_pp_lr$field_area_ha)

###############################################################################
# Ensure pre and post-harvest crop codes match
tz_ag_hv$zaocode <- gsub(' *$', '', as.character(tz_ag_hv$zaocode))
tz_ag_sd$zaocode <- gsub(' *$', '', as.character(tz_ag_sd$zaocode))

sort(unique(c(tz_ag_hv$zaocode, tz_ag_sd$zaocode)))
sort(unique(tz_ag_hv$zaocode))
sort(unique(tz_ag_hv$zaocode))
