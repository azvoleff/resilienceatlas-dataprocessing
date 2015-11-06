###############################################################################
###############################################################################
# Process Niger LSMS data
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

# Load household geo-referencing data
ni_hh_geo <- read.spss(file.path(prefix, 
                              "GRP/LSMS/NI/2011/NER_HouseholdGeovars_Y1.sav"),
                    to.data.frame=TRUE)
ni_hh_geo <- clear.labels(ni_hh_geo)
ni_hh_geo <- tbl_df(ni_hh_geo)

# Load plot geo-referencing data
ni_pt_geo <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/NI/2011/NER_PlotGeovariables_Y1.sav"),
                       to.data.frame=TRUE)
ni_pt_geo <- clear.labels(ni_pt_geo)
ni_pt_geo <- tbl_df(ni_pt_geo)

# Load pre-planting data on field sizes (access to land)
ni_ac_land <- read.spss(file.path(prefix, 
                                      "GRP/LSMS/NI/2011/ecvmamen_p1_en.sav"),
                            to.data.frame=TRUE)
ni_ac_land <- clear.labels(ni_ac_land)
ni_ac_land <- tbl_df(ni_ac_land)

# Load demographic, education and time use data from household questionnaire
ni_hh_demo_edu <- read.spss(file.path(prefix, 
                                 "GRP/LSMS/NI/2011/ecvmaind_p1p2_en.sav"),
                       to.data.frame=TRUE)
ni_hh_demo_edu <- clear.labels(ni_hh_demo_edu)
ni_hh_demo_edu <- tbl_df(ni_hh_demo_edu)

## Load agriculture input data
ni_agr_inp <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/NI/2011/ecvmaas1_p1_en.sav"),
                        to.data.frame=TRUE)
ni_agr_inp <- clear.labels(ni_agr_inp)
ni_agr_inp <- tbl_df(ni_agr_inp)

## Load data on cost of inputs
ni_agr_inp_co <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/NI/2011/ecvmaas2c_p1_en.sav"),
                        to.data.frame=TRUE)
ni_agr_inp_co <- clear.labels(ni_agr_inp_co)
ni_agr_inp_co <- tbl_df(ni_agr_inp_co)
# Load post-planting data on crops planted and seeds used (ni_crp_sed)
ni_crp_sed <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/NI/2011/ecvmaas2b_p1_en.sav"),
                         to.data.frame=TRUE)
ni_crp_sed <- clear.labels(ni_crp_sed)
ni_crp_sed <- tbl_df(ni_crp_sed)

# Load post-planting data section 7 - data on holder (use of extension, etc)
## the available data are form the second visit. is it ok if I use them?
ni_ext <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/NI/2011/ecvmaas07_p2_en.sav"),
                        to.data.frame=TRUE)
ni_ext <- clear.labels(ni_ext)
ni_ext <- tbl_df(ni_ext)

# Load post harvest data on harvest by field
ni_crp_prod <- read.spss(file.path(prefix, 
                              "GRP/LSMS/NI/2011/ecvmaas2e_p2_en.sav"),
                    to.data.frame=TRUE)
ni_crp_prod <- clear.labels(ni_crp_prod)
ni_crp_prod <- tbl_df(ni_crp_prod)

# Community-level data on basic service access
ni_com_serv <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/NI/2011/ecvmacoms01_p1_en.sav"),
                         to.data.frame=TRUE)
ni_com_serv <- clear.labels(ni_com_serv)
ni_com_serv <- tbl_df(ni_com_serv)

# Community-level data on economic activities and agriculture including trends in crop buyers, seeds, etc
ni_com_econ <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/NI/2011/ecvmacoms02_p1_en.sav"),
                         to.data.frame=TRUE)
ni_com_econ <- clear.labels(ni_com_econ)
ni_com_econ <- tbl_df(ni_com_econ)

# household assets
ni_hh_assets <- read.spss(file.path(prefix, 
                                   "GRP/LSMS/NI/2011/ecvmaactif_p1_en.sav"),
                         to.data.frame=TRUE)
ni_hh_assets <- clear.labels(ni_hh_assets)
ni_hh_assets <- tbl_df(ni_hh_assets)

## Load data on agricultural extension services
ni_agr_ext <- read.spss(file.path(prefix, 
                                  "GRP/LSMS/NI/2011/ecvmaas07_p2_en.sav"),
                        to.data.frame=TRUE)
ni_agr_ext <- clear.labels(ni_agr_ext)
ni_agr_ext <- tbl_df(ni_agr_ext)

##################################
###  Education
indiv <- select(ni_hh_demo_edu, hid, passage1)
names(indiv)[names(indiv) == "passage1"] <- "wave_1"
indiv$edu <- ni_hh_demo_edu$ms02q23
table(indiv$edu)

indiv$edu <- as.character(indiv$edu)
indiv$edu[indiv$edu == "Preschool"] <- 0
indiv$edu[indiv$edu == "Primary"] <- 1
indiv$edu[indiv$edu == "Secondary first cycle-general"] <- 2
indiv$edu[indiv$edu == "Secondary first cycle technical & professional"] <- 2
indiv$edu[indiv$edu == "Secondary second cycle general"] <- 3
indiv$edu[indiv$edu == "Secondary second cycle technical& professional"] <- 3
indiv$edu[indiv$edu == "Superior"] <- 4

indiv$edu <- ordered(indiv$edu, levels=c(0, 1, 2, 3, 4),
                     labels=c("Preschool", "Primary", "Secondary", "College", "Graduate"))

indiv$in_school <- ni_hh_demo_edu$ms02q10 == "Yes"
indiv$not_in_school_why <- ni_hh_demo_edu$ms02q11

indiv$never_attended_school <- ni_hh_demo_edu$ms02q04 == "No"
indiv$never_attended_school_why <- ni_hh_demo_edu$ms02q05
table(indiv$never_attended_school)
table(indiv$never_attended_school_why)

table(indiv$not_in_school_why)
plot(indiv$edu)

##################################
###  Occupation
job_data <- select(ni_hh_demo_edu, hid, passage1)
names(job_data)[names(job_data) == "passage1"] <- "wave_1"

# Record job code
job_data$job_perm_type <- as.character(ni_hh_demo_edu$ms04q26)

# Code as "None" those who weren't asked job type since they have no 
# off-household job:
job_data$job_perm_type[ni_hh_demo_edu$ms04q02 == "No"] <- "None"
table(job_data$job_perm_type, useNA="always")

# Code people with no off household job as 0, with any off household job as 1
job_data$job_perm_any <- job_data$job_perm_type != "None"
table(job_data$job_perm_any, useNA="always")

#### consult the questionaire again to see if
## this question is available
##job_data$labor_exchange <- sect4_hh_w1$hh_s4q37 == "Yes"
##table(job_data$labor_exchange)

indiv <- full_join(indiv, job_data)

##################################
###  Demographic information
demo_data <- select(ni_hh_demo_edu, hid)
demo_data$sex <- ni_hh_demo_edu$ms01q01
# Age is included in hh_s1q04_a for those over five years of age. For those 
# under five years of age, months are given in hh_s1q04_b.
demo_data$age <- ifelse(is.na(ni_hh_demo_edu$ms01q06b),
                        ni_hh_demo_edu$ms01q06a,
                        ni_hh_demo_edu$ms01q06b/12)
plot(demo_data$sex ~ demo_data$age)

demo_data$individual_id <- ni_hh_demo_edu$ms01q00

# Household head always has individual_id == 1
demo_data$hh_head <- demo_data$individual_id == "1"
table(demo_data$hh_head)

indiv <- full_join(indiv, demo_data)

# Setup a measure of number of children not in school that should be, based on 
# ages 8-16
indiv$of_age_not_in_school_why <- indiv$not_in_school_why
indiv$of_age_not_in_school_why[indiv$age < 8] <- NA
indiv$of_age_not_in_school_why[indiv$age > 16] <- NA
indiv$of_age_not_in_school_noint <- (indiv$not_in_school_why == "Wants to be self-taught")
indiv$of_age_not_in_school_noint[indiv$age < 8] <- NA
indiv$of_age_not_in_school_noint[indiv$age > 16] <- NA

indiv$of_age_in_school <- indiv$in_school
indiv$of_age_in_school[indiv$age < 8] <- NA
indiv$of_age_in_school[indiv$age > 16] <- NA

indiv$of_age_never_attended <- indiv$never_attended_school
indiv$of_age_never_attended[indiv$age < 8] <- NA
indiv$of_age_never_attended[indiv$age > 16] <- NA
indiv$of_age_never_attended_why <- indiv$never_attended_school_why
indiv$of_age_never_attended_why[indiv$age < 8] <- NA
indiv$of_age_never_attended_why[indiv$age > 16] <- NA

table(indiv$of_age_never_attended)
table(indiv$of_age_never_attended_why)

table(indiv$of_age_in_school)
table(indiv$of_age_not_in_school_why)
table(indiv$of_age_not_in_school_why)
table(indiv$of_age_not_in_school_noint)

##################################
### Farming inputs
inputs <- select(ni_agr_inp, hid)

## check again questionaire for this question
#inputs$hldr_crop_rotation <- sect7_pp_w1$pp_s7q01 == "Yes"
#table(inputs$hldr_crop_rotation, useNA="always")

inputs$hldr_chem_fert <- ni_agr_inp$as02aq10 == "Yes"
table(inputs$hldr_chem_fert, useNA="always")

ext <- select(ni_agr_ext, hid)
ext$hldr_ext <- ni_agr_ext$as07q03 == "Yes"
inputs <- full_join (inputs, ext)
table(inputs$hldr_ext, useNA="always")

inp_co <- select(ni_agr_inp_co, hid)
inp_co$hldr_svc_credit <- ni_agr_inp_co$as02cq10 == "1"
inputs <- full_join (inputs, inp_co)
table(inputs$hldr_svc_credit, useNA="always")


##line 255 through line 266, no information available in the questionaires
##inputs$hldr_svc_advisory <- sect7_pp_w1$pp_s7q08 == "Yes"
#table(inputs$hldr_svc_advisory, useNA="always")

#inputs$ignorance_fert <- sect7_pp_w1$pp_s7q03 == 1
#inputs$ignorance_ext <- sect7_pp_w1$pp_s7q05 == "Ignorance"
#inputs$ignorance_credit <- sect7_pp_w1$pp_s7q07 == 4
#inputs$ignorance_advisory <- sect7_pp_w1$pp_s7q09 == 3

#table(inputs$ignorance_fert)
#table(inputs$ignorance_ext)
#table(inputs$ignorance_credit)
#table(inputs$ignorance_advisory)

dim(indiv)
## unable to join, maybe too many entries?
# indiv <- full_join(indiv, inputs)
# dim(indiv)

##################################
### Household-level, time in residence (for NI, months in residence) 
## but there is residence information at the community level

hh <- select(ni_hh_demo_edu, hid)
# Number of residence years is in hh_s9q02_a, and months is in hh_s9q02_b if it 
# was given (often it is NA).
hh$resid_duration <- ni_hh_demo_edu$ms01q19

table(hh$resid_duration < 1, useNA="always")
hist(hh$resid_duration)

##################################
### Household-level, household size

hh_size <- group_by(indiv, hid) %>%
  summarize(hh_size=n())
hh <- full_join(hh, hh_size)

dep_ratio <- group_by(indiv, hid) %>%
  summarize(dep_ratio=(sum(age < 14) + sum(age >= 64) /
                         sum((age >= 14) & (age < 64))))
hh <- full_join(hh, dep_ratio)
