###############################################################################
###############################################################################
# Process Tanzania 2012 LSMS data
###############################################################################
###############################################################################
###
### TODO: Instead of aggregating at the EA level, need to aggregate at the 
### lowest level at which sample estimates are representative - based on the 
### data book

source('../0_settings.R')

library(foreign)
library(stringr)
library(dplyr)
library(ggplot2)

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

# Change a string to title case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", 
        collapse=" ")
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

# Load pre-planting data on field sizes (short rainy season)
tz_ag_pp_sr <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/TZ/2012/AG_SEC_2B.SAV"),
                         to.data.frame=TRUE)
tz_ag_pp_sr <- clear.labels(tz_ag_pp_sr)
tz_ag_pp_sr <- tbl_df(tz_ag_pp_sr)

# Load demographic data from household questionnaire
tz_hh_demo <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/TZ/2012/HH_SEC_B.SAV"),
                         to.data.frame=TRUE)
tz_hh_demo <- clear.labels(tz_hh_demo)
tz_hh_demo <- tbl_df(tz_hh_demo)

# Load education data from household questionnaire
tz_hh_edu <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/TZ/2012/HH_SEC_C.SAV"),
                        to.data.frame=TRUE)
tz_hh_edu <- clear.labels(tz_hh_edu)
tz_hh_edu <- tbl_df(tz_hh_edu)

# Load housingdata from household questionnaire
tz_hh_hs <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HH_SEC_I.SAV"),
                       to.data.frame=TRUE)
tz_hh_hs <- clear.labels(tz_hh_hs)
tz_hh_hs <- tbl_df(tz_hh_hs)

# Load time use data from household questionnaire (labour)
tz_hh_lab <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HH_SEC_E.SAV"),
                       to.data.frame=TRUE)
tz_hh_lab <- clear.labels(tz_hh_lab)
tz_hh_lab <- tbl_df(tz_hh_lab)

# Load time use data from household questionnaire (non-farm enterprises)
tz_hh_ent <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HH_SEC_N.SAV"),
                       to.data.frame=TRUE)
tz_hh_ent <- clear.labels(tz_hh_ent)
tz_hh_ent <- tbl_df(tz_hh_ent)

# Load post-planting data on crops planted (long rainy season)
tz_ag_pop_lr <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/AG_SEC_3A.SAV"),
                       to.data.frame=TRUE)
tz_ag_pop_lr <- clear.labels(tz_ag_pop_lr)
tz_ag_pop_lr <- tbl_df(tz_ag_pop_lr)

# Load post-planting data on crops planted (short rainy season)
tz_ag_pop_sr <- read.spss(file.path(prefix, 
                                    "GRP/LSMS/TZ/2012/AG_SEC_3B.SAV"),
                          to.data.frame=TRUE)
tz_ag_pop_sr <- clear.labels(tz_ag_pop_sr)
tz_ag_pop_sr <- tbl_df(tz_ag_pop_sr)

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

# Community-level data on basic service access
tz_com_bsa <- read.spss(file.path(prefix, 
                                "GRP/LSMS/TZ/2012/COM_SEC_CB.SAV"),
                      to.data.frame=TRUE)
tz_com_bsa <- clear.labels(tz_com_bsa)
tz_com_bsa <- tbl_df(tz_com_bsa)

# Community-level data on economic activities (investment projects)
tz_com_inv <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/TZ/2012/COM_SEC_CC.SAV"),
                        to.data.frame=TRUE)
tz_com_inv <- clear.labels(tz_com_inv)
tz_com_inv <- tbl_df(tz_com_inv)

## Load data on household assets (access to mobile phone)
tz_hh_ast <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/TZ/2012/HH_SEC_M.SAV"),
                       to.data.frame=TRUE)
tz_hh_ast <- clear.labels(tz_hh_ast)
tz_hh_ast <- tbl_df(tz_hh_ast)

# Community-level data on agriculture, including trends in crop buyers, seed, 
# etc (market prices)
tz_com_mp <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/TZ/2012/COM_SEC_CF.SAV"),
                        to.data.frame=TRUE)
tz_com_mp <- clear.labels(tz_com_mp)
tz_com_mp <- tbl_df(tz_com_mp)

##################################
###  Education
indiv <- select(tz_hh_edu, y3_hhid)
indiv$edu <- tz_hh_edu$hh_c07
table(indiv$edu)

indiv$edu <- as.character(indiv$edu)
indiv$edu[indiv$edu == "PP"] <- 0
indiv$edu[indiv$edu == "ADULT"] <- 0
indiv$edu[indiv$edu == "D1"] <- 0
indiv$edu[indiv$edu == "D2"] <- 0
indiv$edu[indiv$edu == "D3"] <- 0
indiv$edu[indiv$edu == "D4"] <- 0
indiv$edu[indiv$edu == "D5"] <- 0
indiv$edu[indiv$edu == "D6"] <- 0
indiv$edu[indiv$edu == "D7"] <- 1
indiv$edu[indiv$edu == "D8"] <- 1
indiv$edu[indiv$edu == "PREFORM 1"] <- 1
indiv$edu[indiv$edu == "MS+COURSE"] <- 1
indiv$edu[indiv$edu == "F1"] <- 2
indiv$edu[indiv$edu == "F2"] <- 2
indiv$edu[indiv$edu == "F3"] <- 2
indiv$edu[indiv$edu == "F4"] <- 2
indiv$edu[indiv$edu == "'O'+COURSE"] <- 2
indiv$edu[indiv$edu == "F5"] <- 2
indiv$edu[indiv$edu == "F6"] <- 2
indiv$edu[indiv$edu == "'A'+COURSE"] <- 2
indiv$edu[indiv$edu == "DIPLOMA"] <- 2
indiv$edu[indiv$edu == "U1"] <- 3
indiv$edu[indiv$edu == "U2"] <- 3
indiv$edu[indiv$edu == "U3"] <- 3
indiv$edu[indiv$edu == "U4"] <- 3
indiv$edu[indiv$edu == "U5&+"] <- 4

# Set individuals under 5 to NA for education
indiv$edu[tz_hh_edu$hh_c01 == "NO"] <- NA

indiv$edu <- ordered(indiv$edu, levels=c(0, 1, 2, 3, 4),
                     labels=c("<P7", "Primary", "Secondary", "College", "Graduate"))
indiv$in_school <- tz_hh_edu$hh_c05 == "YES"
indiv$not_in_school_why <- tz_hh_edu$hh_c18

indiv$never_attended_school <- tz_hh_edu$hh_c03 == "NO"
## no information available on why indiv never attended school
#indiv$never_attended_school_why <- sect2_hh_w1$hh_s2q04
table(indiv$never_attended_school)
#table(indiv$never_attended_school_why)

table(indiv$not_in_school_why)

plot(indiv$edu)

##################################
###  Occupation
job_data <- select(tz_hh_lab, y3_hhid)

# Record job code
job_data$job_perm_type <- as.character(tz_hh_lab$hh_e18)

# Code as "None" those who weren't asked job type since they have no 
# off-household job:
job_data$job_perm_type[tz_hh_lab$hh_e04b == "NO"] <- "None"
table(job_data$job_perm_type, useNA="always")

# Code people with no off household job as 0, with any off household job as 1
job_data$job_perm_any <- job_data$job_perm_type != "None"
table(job_data$job_perm_any, useNA="always")

### no information on labor exchange but plenty of info on unpaid apprenticeship
#job_data$labor_exchange <- sect4_hh_w1$hh_s4q37 == "Yes"
#table(job_data$labor_exchange)

## to do later
#indiv <- full_join(indiv, job_data)

##################################
###  Demographic information
demo_data <- select(tz_hh_demo, y3_hhid, indidy3)
demo_data$sex <- tz_hh_demo$hh_b02

# Age is included in hh_s1q04_a for those over five years of age. For those 
# under five years of age, months are given in hh_s1q04_b.
demo_data$age <- tz_hh_demo$hh_b04
plot(demo_data$sex ~ demo_data$age)

# Household head always has individual_id == 1
names(demo_data)[names(demo_data) == "indidy3"] <- "individual_id"
demo_data$hh_head <- demo_data$individual_id == "1"
table(demo_data$hh_head)

## to do later
#indiv <- full_join(indiv, demo_data)

# Setup a measure of number of children not in school that should be, based on 
# ages 8-16
##revisit this section to chage demo_data to indiv
#indiv$of_age_not_in_school_why <- indiv$not_in_school_why
#indiv$of_age_not_in_school_why[indiv$age < 8] <- NA
#indiv$of_age_not_in_school_why[indiv$age > 16] <- NA
#indiv$of_age_not_in_school_noint <- (indiv$not_in_school_why == "CHILD REFUSED")
#indiv$of_age_not_in_school_noint[indiv$age < 8] <- NA
#indiv$of_age_not_in_school_noint[indiv$age > 16] <- NA

#indiv$of_age_in_school <- indiv$in_school
#indiv$of_age_in_school[indiv$age < 8] <- NA
#indiv$of_age_in_school[indiv$age > 16] <- NA

#indiv$of_age_never_attended <- indiv$never_attended_school
#indiv$of_age_never_attended[indiv$age < 8] <- NA
#indiv$of_age_never_attended[indiv$age > 16] <- NA
#indiv$of_age_never_attended_why <- indiv$never_attended_school_why
#indiv$of_age_never_attended_why[indiv$age < 8] <- NA
#indiv$of_age_never_attended_why[indiv$age > 16] <- NA

#table(indiv$of_age_never_attended)
#table(indiv$of_age_never_attended_why)

#table(indiv$of_age_in_school)
#table(indiv$of_age_not_in_school_why)
#table(indiv$of_age_not_in_school_why)
#table(indiv$of_age_not_in_school_noint)

##################################
### Farming inputs

inputs <- select(tz_ag_pop_lr, y3_hhid)

#inputs$hldr_crop_rotation <- sect7_pp_w1$pp_s7q01 == "Yes"
#table(inputs$hldr_crop_rotation, useNA="always")

inputs$hldr_chem_fert <- tz_ag_pop_lr$ag3a_47 == "YES"
table(inputs$hldr_chem_fert, useNA="always")

ext <- select(tz_ag_ext, y3_hhid)
ext$hldr_ext_agr <- tz_ag_ext$ag12a_02_1 == "YES"
table(ext$hldr_ext_agr, useNA="always")
ext$hldr_ext_agp <- tz_ag_ext$ag12a_02_2 == "YES"
table(ext$hldr_ext_agp, useNA="always")

## credits here refers to getting input on credit
inputs$hldr_svc_credit <- tz_ag_pop_lr$ag3a_66 == "YES"
table(inputs$hldr_svc_credit, useNA="always")

#inputs$hldr_svc_advisory <- sect7_pp_w1$pp_s7q08 == "Yes"
#table(inputs$hldr_svc_advisory, useNA="always")

#inputs$ignorance_fert <- sect7_pp_w1$pp_s7q03 == 1
#inputs$ignorance_ext <- sect7_pp_w1$pp_s7q05 == "Ignorance"
#inputs$ignorance_credit <- sect7_pp_w1$pp_s7q07 == 4
#inputs$ignorance_advisory <- sect7_pp_w1$pp_s7q09 == 3

#table(inputs$ignorance_fert)
#table(inputs$ignorance_ext)
#table(inputs$ignorance_credit)
#table(inputs$ignorance_advisory)

#dim(indiv)
#indiv <- full_join(indiv, inputs)
#dim(indiv)

##################################
### Household-level, time in residence (not available, but there is ownership status and how long the 
# household has been living in the community)

hs <- select(tz_hh_hs, y3_hhid)
hs$own <- tz_hh_hs$hh_i01
table(hs$own)

##################################
### Household-level, household size

hh_size <- group_by(indiv, y3_hhid) %>%
  summarize(hh_size=n())
hh <- full_join(hs, hh_size)
