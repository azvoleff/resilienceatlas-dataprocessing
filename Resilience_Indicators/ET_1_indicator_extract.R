###############################################################################
###############################################################################
# Process Ethiopia LSMS data
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

# Change a string to title case
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)), sep="", 
        collapse=" ")
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

# Load demographic data from household questionnaire
sect1_hh_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect1_hh_w1.sav"), 
                         to.data.frame=TRUE)
sect1_hh_w1 <- clear.labels(sect1_hh_w1)
sect1_hh_w1 <- tbl_df(sect1_hh_w1)

# Load education data from household questionnaire
sect2_hh_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect2_hh_w1.sav"), 
                         to.data.frame=TRUE)
sect2_hh_w1 <- clear.labels(sect2_hh_w1)
sect2_hh_w1 <- tbl_df(sect2_hh_w1)

# Load time use data from household questionnaire
sect4_hh_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect4_hh_w1.sav"), 
                         to.data.frame=TRUE)
sect4_hh_w1 <- clear.labels(sect4_hh_w1)
sect4_hh_w1 <- tbl_df(sect4_hh_w1)

# Load time use data from household questionnaire
sect9_hh_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect9_hh_w1.sav"), 
                         to.data.frame=TRUE)
sect9_hh_w1 <- clear.labels(sect9_hh_w1)
sect9_hh_w1 <- tbl_df(sect9_hh_w1)

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

# Community-level data on basic service access
sect4_com_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect4_com_w1.sav"), 
                 to.data.frame=TRUE)
sect4_com_w1 <- clear.labels(sect4_com_w1)
sect4_com_w1 <- tbl_df(sect4_com_w1)

# Community-level data on economic activities
sect5_com_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect5_com_w1.sav"), 
                 to.data.frame=TRUE)
sect5_com_w1 <- clear.labels(sect5_com_w1)
sect5_com_w1 <- tbl_df(sect5_com_w1)

# Community-level data on agriculture, including trends in crop buyers, seed, 
# etc.
sect6_com_w1 <- read.spss(file.path(prefix, "GRP/LSMS/ET/2011/sect6_com_w1.sav"), 
                 to.data.frame=TRUE)
sect6_com_w1 <- clear.labels(sect6_com_w1)
sect6_com_w1 <- tbl_df(sect6_com_w1)

# sect9_ph_w1 <- merge(sect9_ph_w1, select(hh_geo, household_id, LON_DD_MOD, 
# LAT_DD_MOD, srtm), all.x=TRUE)

##################################
###  Education
indiv <- select(sect2_hh_w1, household_id, individual_id, ea_id, rural, pw, saq01)
names(indiv)[names(indiv) == "saq01"] <- "region_id"
indiv$edu <- sect2_hh_w1$hh_s2q05

#table(indiv$edu)

indiv$edu <- as.character(indiv$edu)
indiv$edu[indiv$edu == "Kindergarden"] <- 0
indiv$edu[indiv$edu == "1st Grade Complete"] <- 0
indiv$edu[indiv$edu == "2nd Grade Complete"] <- 0
indiv$edu[indiv$edu == "3rd Grade Complete"] <- 0
indiv$edu[indiv$edu == "4th Grade Complete"] <- 0
indiv$edu[indiv$edu == "5th Grade Complete"] <- 1
indiv$edu[indiv$edu == "6th Grade Complete"] <- 1
indiv$edu[indiv$edu == "7th Grade Complete"] <- 1
indiv$edu[indiv$edu == "8th Grade Complete"] <- 1
indiv$edu[indiv$edu == "9th Grade Complete"] <- 2
indiv$edu[indiv$edu == "10th Grade Complete"] <- 2
indiv$edu[indiv$edu == "11th Grade Complete"] <- 2
indiv$edu[indiv$edu == "12th Grade Complete"] <- 2
indiv$edu[indiv$edu == "Certificate"] <- 2
indiv$edu[indiv$edu == "Teacher Training Certificate"] <- 2
indiv$edu[indiv$edu == "1st Year College Complete"] <- 3
indiv$edu[indiv$edu == "2nd Year College Complete"] <- 3
indiv$edu[indiv$edu == "Diploma"] <- 3
indiv$edu[indiv$edu == "3rd Year University"] <- 3
indiv$edu[indiv$edu == "1st Degree (BA/BSc)"] <- 3
indiv$edu[indiv$edu == "Graduate Degree (MA/MS/Mphil/PhD)"] <- 4
indiv$edu[indiv$edu == "NC 9th Grade Complete"] <- 2
indiv$edu[indiv$edu == "NC 10th Grade Complete"] <- 2
indiv$edu[indiv$edu == "NC 11th Grade Complete"] <- 2
indiv$edu[indiv$edu == "NC 12th Grade Complete"] <- 2
indiv$edu[indiv$edu == "NC Certificate (10+1)- Technical/Vocational"] <- 2
indiv$edu[indiv$edu == "NC Certificate (10+2)- level 2 Incomplete"] <- 2
indiv$edu[indiv$edu == "NC Certificate (10+2) Complete"] <- 2
indiv$edu[indiv$edu == "NC Certificate (10+3) Level 3 One Year Complete"] <- 2
indiv$edu[indiv$edu == "NC Certificate (10+3) Level 3 Two Years Complete"] <- 2
indiv$edu[indiv$edu == "NC Diploma (10+3) Level 3"] <- 2
indiv$edu[indiv$edu == "1st Year College Complete"] <- 3
indiv$edu[indiv$edu == "2nd Year College Complete"] <- 3
indiv$edu[indiv$edu == "3rd Year College Complete"] <- 3
indiv$edu[indiv$edu == "1st Degree or Level 4 Complete"] <- 3
indiv$edu[indiv$edu == "Above 1st Degree / Level 4"] <- 3
indiv$edu[indiv$edu == "Basic education - can read and write"] <- 2
indiv$edu[indiv$edu == "Adult educatioin - can read and write"] <- 2
indiv$edu[indiv$edu == "Alternative education - can rea and write"] <- 2
indiv$edu[indiv$edu == "Informal education (relegious)"] <- 2
indiv$edu[indiv$edu == "Not educated - Can not read and write"] <- 0

# Set individuals under 5 to NA for education
indiv$edu[sect2_hh_w1$hh_s2q01 != "X"] <- NA

indiv$edu <- ordered(indiv$edu, levels=c(0, 1, 2, 3, 4),
                    labels=c("<=4", "5-8", "8-12", "College", "Graduate"))


indiv$in_school <- sect2_hh_w1$hh_s2q06 == 1
indiv$not_in_school_why <- sect2_hh_w1$hh_s2q07

indiv$never_attended_school <- sect2_hh_w1$hh_s2q03 == "No"
indiv$never_attended_school_why <- sect2_hh_w1$hh_s2q04
table(indiv$never_attended_school)
table(indiv$never_attended_school_why)

table(indiv$not_in_school_why)
plot(indiv$edu)

##################################
###  Occupation
job_data <- select(sect4_hh_w1, individual_id)

# Record job code
job_data$job_perm_type <- as.character(sect4_hh_w1$hh_s4q12)

# Code as "None" those who weren't asked job type since they have no 
# off-household job:
job_data$job_perm_type[sect4_hh_w1$hh_s4q09 == "No"] <- "None"
table(job_data$job_perm_type, useNA="always")

# Code people with no off household job as 0, with any off household job as 1
job_data$job_perm_any <- job_data$job_perm_type != "None"
table(job_data$job_perm_any, useNA="always")

job_data$labor_exchange <- sect4_hh_w1$hh_s4q37 == "Yes"
table(job_data$labor_exchange)

indiv <- full_join(indiv, job_data)

##################################
###  Demographic information
demo_data <- select(sect1_hh_w1, individual_id)
demo_data$sex <- sect1_hh_w1$hh_s1q03
# Age is included in hh_s1q04_a for those over five years of age. For those 
# under five years of age, months are given in hh_s1q04_b.
demo_data$age <- ifelse(is.na(sect1_hh_w1$hh_s1q04_b),
                        sect1_hh_w1$hh_s1q04_a,
                        sect1_hh_w1$hh_s1q04_b/12)
plot(demo_data$sex ~ demo_data$age)

demo_data$individual_id_2char <- str_extract(demo_data$individual_id, '[0-9]{2}$')

# Household head always has individual_id == 1
demo_data$hh_head <- demo_data$individual_id_2char == "01"
table(demo_data$hh_head)

indiv <- full_join(indiv, demo_data)

# Setup a measure of number of children not in school that should be, based on 
# ages 8-16
indiv$of_age_not_in_school_why <- indiv$not_in_school_why
indiv$of_age_not_in_school_why[indiv$age < 8] <- NA
indiv$of_age_not_in_school_why[indiv$age > 16] <- NA
indiv$of_age_not_in_school_noint <- (indiv$not_in_school_why == 4) # no interest/no time
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

inputs <- select(sect7_pp_w1, household_id, holder_id)
names(inputs)[names(inputs) == "holder_id"] <- "individual_id"

inputs$hldr_crop_rotation <- sect7_pp_w1$pp_s7q01 == "Yes"
table(inputs$hldr_crop_rotation, useNA="always")

inputs$hldr_chem_fert <- sect7_pp_w1$pp_s7q02 == "Yes"
table(inputs$hldr_chem_fert, useNA="always")

inputs$hldr_ext <- sect7_pp_w1$pp_s7q04 == "Yes"
table(inputs$hldr_ext, useNA="always")

inputs$hldr_svc_credit <- sect7_pp_w1$pp_s7q06 == "Yes"
table(inputs$hldr_svc_credit, useNA="always")

inputs$hldr_svc_advisory <- sect7_pp_w1$pp_s7q08 == "Yes"
table(inputs$hldr_svc_advisory, useNA="always")

inputs$ignorance_fert <- sect7_pp_w1$pp_s7q03 == 1
inputs$ignorance_ext <- sect7_pp_w1$pp_s7q05 == "Ignorance"
inputs$ignorance_credit <- sect7_pp_w1$pp_s7q07 == 4
inputs$ignorance_advisory <- sect7_pp_w1$pp_s7q09 == 3

table(inputs$ignorance_fert)
table(inputs$ignorance_ext)
table(inputs$ignorance_credit)
table(inputs$ignorance_advisory)

dim(indiv)
indiv <- full_join(indiv, inputs)
dim(indiv)

##################################
### Household-level, time in residence

hh <- select(sect9_hh_w1, household_id, ea_id, rural, pw)
# Number of residence years is in hh_s9q02_a, and months is in hh_s9q02_b if it 
# was given (often it is NA).
hh$resid_duration <- ifelse(is.na(sect9_hh_w1$hh_s9q02_b),
                                      sect9_hh_w1$hh_s9q02_a,
                                      sect9_hh_w1$hh_s9q02_a + sect9_hh_w1$hh_s9q02_b/12)

table(hh$resid_duration < 1, useNA="always")
hist(hh$resid_duration)

##################################
### Household-level, household size

hh_size <- group_by(indiv, household_id) %>%
    summarize(hh_size=n())
hh <- full_join(hh, hh_size)

dep_ratio <- group_by(indiv, household_id) %>%
    summarize(dep_ratio=(sum(age < 14) + sum(age >= 64) /
              sum((age >= 14) & (age < 64))))
hh <- full_join(hh, dep_ratio)
# Code infinite dependency ratios as 10
hh$dep_ratio[hh$dep_ratio > 10] <- 10

hh$dep_ratio_cut <- cut(hh$dep_ratio, c(0, 2, 4, Inf), right=FALSE)
table(hh$dep_ratio_cut)
plot(hh$dep_ratio_cut)

mean(hh$resid_duration, na.rm=TRUE)
mean(hh$hh_size, na.rm=TRUE)
group_by(hh, rural) %>%
    summarize(mean(hh_size, na.rm=TRUE))

##################################
### Community-level distance metrics as measures of resource independence

com <- select(sect4_com_w1, ea_id, rural)

# Distance to bus
com$dist_bus_km <- sect4_com_w1$cs4q06_1

# Distance to major urban center
com$dist_urban_km <- sect4_com_w1$cs4q12_b1

# Code areas that are IN the major urban center with dist_urban_km = 0
com$dist_urban_km[sect4_com_w1$cs4q11 == "Yes"] <- 0

com_econ <- select(sect5_com_w1, ea_id, rural)

com_econ$micro_enterprise <- sect5_com_w1$cs5q09 == "Yes"
com_econ$micro_enterprise_pct_male <- sect5_com_w1$cs5q10
com_econ$micro_enterprise_pct_female <- sect5_com_w1$cs5q11

com_ag <- select(sect4_com_w1, ea_id, rural)

# Distance to ag extension officer
com_ag$dist_ext_km <- sect6_com_w1$cs6q09_1

# Code communities with a extension specialist resident in the area with 
# distance zero
com_ag$dist_ext_km[sect6_com_w1$cs6q08 == "Yes"] <- 0
com_ag$dist_ext_km

com_ag$chg_fert_distr <- sect6_com_w1$cs6q16_a
com_ag$chg_seed_distr <- sect6_com_w1$cs6q16_b
com_ag$chg_crop_buyer <- sect6_com_w1$cs6q16_c
com_ag$chg_pestherb_distr <- sect6_com_w1$cs6q16_d

table(com_ag$chg_fert_distr)
table(com_ag$chg_seed_distr)
table(com_ag$chg_crop_buyer)
table(com_ag$chg_pestherb_distr)

com_ag$chg_ext_crop_prod <- sect6_com_w1$cs6q17_a
com_ag$chg_ext_nat_resources <- sect6_com_w1$cs6q17_b
com_ag$chg_ext_agric_credit <- sect6_com_w1$cs6q17_c
com_ag$chg_ext_livestock <- sect6_com_w1$cs6q17_d

table(com_ag$chg_ext_crop_prod)
table(com_ag$chg_ext_nat_resources)
table(com_ag$chg_ext_agric_credit)
table(com_ag$chg_ext_livestock)

com <- full_join(com, com_econ)
com <- full_join(com, com_ag)

###############################################################################
# Aggregate metrics

cats <- c("None", "Very low", "Low", "Average", "High", "Very high")
# Choose quantiles
qs <- c(0, .333, .666, 1)

rev_index <- function(x) {
    max(x) + 1 - x
}

calc_score <- function(x, name) {
    raw_score <- apply(select(x, -ea_id), 1, sum)
    score <- raw_score / max(raw_score) * 10
    score <- data.frame(x$ea_id, score)
    names(score) <- c("ea_id", name)
    score
}

##########################
# Buffer capacity
buff_indiv_stats <- group_by(indiv, ea_id, household_id) %>%
    summarize(job_perm_any=sum(job_perm_any, na.rm=TRUE) > 1,
              edu_gt8=sum(edu > "5-8", na.rm=TRUE)/sum(!is.na(edu))) %>%
    group_by(ea_id) %>%
    summarize(job_perm_any=mean(job_perm_any, na.rm=TRUE),
              edu_gt8=mean(edu_gt8, na.rm=TRUE))

buff_hh_stats <- group_by(hh, ea_id) %>%
    summarize(mean_resid_duration=mean(resid_duration, na.rm=TRUE),
              mean_dep_ratio=mean(dep_ratio, na.rm=TRUE),
              mean_hh_size=mean(hh_size))

buff_stats <- full_join(buff_indiv_stats, buff_hh_stats)

# Delete NAs in last row. TODO: why are they there??
buff_stats <- buff_stats[-nrow(buff_stats), ]

buff_quants <- ungroup(buff_stats) %>%
    select(-job_perm_any) %>%
    mutate_each(funs(as.integer(cut(., quantile(., c(0, .33, .666, 1), na.rm=TRUE),
                                    include.lowest=TRUE))), -ea_id)

# Reverse the order of mean_dep_ratio - higher is less resilient
buff_quants$mean_dep_ratio <- rev_index(buff_quants$mean_dep_ratio)

buff_score <- calc_score(buff_quants, 'buff_score')

##########################
# Self-organization

# Farm inputs
buff_input_stats <- group_by(indiv, ea_id, household_id) %>%
    summarize(crop_rotation=sum(hldr_crop_rotation, na.rm=TRUE)/sum(!is.na(hldr_crop_rotation)),
              chem_fert=sum(hldr_chem_fert, na.rm=TRUE)/sum(!is.na(hldr_chem_fert)),
              ext=sum(hldr_ext, na.rm=TRUE)/sum(!is.na(hldr_ext)),
              svc_credit=sum(hldr_svc_credit, na.rm=TRUE)/sum(!is.na(hldr_svc_credit)),
              svc_advisory=sum(hldr_svc_advisory, na.rm=TRUE)/sum(!is.na(hldr_svc_advisory)))

# Labor exchange
job_stats <- group_by(job_data, household_id) %>%
    summarize(labor_exchange=sum(hldr_labor_exchange, na.rm=TRUE)/sum(!is.na(hldr_labor_exchange)))


# Microenterprise


# Distance to resources/services


##########################
# Capacity for learning

###############################################################################
# Plots

ggplot(filter(yield, Crop %in% c("Maize", "Sorghum", "Teff", "Barley"))) +
    geom_bar(aes(yld_kg_ha, fill=Crop), type="identity") +
    facet_wrap(~Crop)
