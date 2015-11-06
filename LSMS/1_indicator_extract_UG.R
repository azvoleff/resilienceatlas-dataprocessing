 ###############################################################################
###############################################################################
# Process Uganda LSMS data
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

###################################################
# Load Uganda geo-referencing data
ug_hh_geo <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/UNPS_Geovars_1112.dta"), convert.factors = TRUE)
ug_hh_geo <- clear.labels(ug_hh_geo)
ug_hh_geo <- tbl_df(ug_hh_geo)


## Load pre-planting data on field sizes
ag_sec2a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC2A.dta"), convert.factors = TRUE)
ag_sec2a <- clear.labels(ag_sec2a)
ag_sec2a <- tbl_df(ag_sec2a)


## Load demographic data from household questionnaire (HH Roster data)
g_sec2 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC2.dta"), convert.factors = TRUE)
g_sec2 <- clear.labels(g_sec2)
g_sec2 <- tbl_df(g_sec2)

## Load general household information
g_sec3 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC3.dta"), convert.factors = TRUE)
g_sec3 <- clear.labels(g_sec3)
g_sec3 <- tbl_df(g_sec3)

## Load Education data
g_sec4 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC4.dta"), convert.factors = TRUE)
g_sec4 <- clear.labels(g_sec4)
g_sec4 <- tbl_df(g_sec4)


## load time use data
g_sec8 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC8.dta"), convert.factors = TRUE)
g_sec8 <- clear.labels(g_sec8)
g_sec8 <- tbl_df(g_sec8)


# Load time use data from household questionnaire
g_sec9a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC9A.dta"), convert.factors = TRUE)
g_sec9a <- clear.labels(g_sec9a)
g_sec9a <- tbl_df(g_sec9a)

## load HH indentification Particulars
g_sec1 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC1.dta"), convert.factors = TRUE)
g_sec1 <- clear.labels(g_sec1)
g_sec1 <- tbl_df(g_sec1)

## Load post-planting data (AG data on seeds)
ag_sec4a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC4A.dta"), convert.factors = TRUE)
ag_sec4a <- clear.labels(ag_sec4a)
ag_sec4a <- tbl_df(ag_sec4a)

## Load post-planting data (AG data on chemical inputs)
ag_sec3a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC3A.dta"), convert.factors = TRUE)
ag_sec3a <- clear.labels(ag_sec3a)
ag_sec3a <- tbl_df(ag_sec3a)


## Load agricultural and labor inputs data
ag_sec3b <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC3B.dta"), convert.factors = TRUE)
ag_sec3b <- clear.labels(ag_sec3b)
ag_sec3b <- tbl_df(ag_sec3b)

## Load post-planting data (AG data on extension services)
ag_sec9 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC9.dta"), convert.factors = TRUE)
ag_sec9 <- clear.labels(ag_sec9)
ag_sec9 <- tbl_df(ag_sec9)

## Load post harvest data (by field)
ag_sec5a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC5A.dta"), convert.factors = TRUE)
ag_sec5a <- clear.labels(ag_sec5a)
ag_sec5a <- tbl_df(ag_sec5a)

## Community-level data on basic service access
c_sec1 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/CSEC1.dta"), convert.factors = TRUE)
c_sec1 <- clear.labels(c_sec1)
c_sec1 <- tbl_df(c_sec1)

## load HH indentification (agriculture)
ag_sec1 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/AGSEC1.dta"), convert.factors = TRUE)
ag_sec1 <- clear.labels(ag_sec1)
ag_sec1 <- tbl_df(ag_sec1)

## load WSEC2A data
w_sec2a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/WSEC2A.dta"), convert.factors = TRUE)
w_sec2a <- clear.labels(w_sec2a)
w_sec2a <- tbl_df(w_sec2a)


## load Community indentification Particulars
c_sec1 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/CSEC1.dta"), convert.factors = TRUE)
c_sec1 <- clear.labels(c_sec1)
c_sec1 <- tbl_df(c_sec1)

## Load HH transportation data
g_sec18 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC18.dta"), convert.factors = TRUE)
g_sec18 <- clear.labels(g_sec18)
g_sec18 <- tbl_df(g_sec18)

## Load HH transportation data
g_sec18 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC18.dta"), convert.factors = TRUE)
g_sec18 <- clear.labels(g_sec18)
g_sec18 <- tbl_df(g_sec18)

## Load HH more transportation data
g_sec18a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC18A.dta"), convert.factors = TRUE)
g_sec18a <- clear.labels(g_sec18a)
g_sec18a <- tbl_df(g_sec18a)

## Load community data
c_sec6a <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/CSEC6A.dta"), convert.factors = TRUE)
c_sec6a <- clear.labels(c_sec6a)
c_sec6a <- tbl_df(c_sec6a)

c_sec6b <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/CSEC6B.dta"), convert.factors = TRUE)
c_sec6b <- clear.labels(c_sec6b)
c_sec6b <- tbl_df(c_sec6b)

c_sec6d <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/CSEC6D.dta"), convert.factors = TRUE)
c_sec6d <- clear.labels(c_sec6d)
c_sec6d <- tbl_df(c_sec6d)

## Load HH assets (cellphone)
g_sec14 <- read.dta(file.path(prefix, "GRP/LSMS/UG/2011/GSEC14.dta"), convert.factors = TRUE)
g_sec14 <- clear.labels(g_sec14)
g_sec14 <- tbl_df(g_sec14)

##############################################
### Education
indiv <- select (g_sec1, HHID, urban, region)
names(indiv)[names(indiv) == "region"] <- "region_id"
edu <- select(g_sec4, HHID, PID, h4q7)
indiv_edu <- full_join(indiv, edu)


#Plot individu education level

indiv_edu <- as.character(indiv_edu$h4q7)
indiv_edu[indiv_edu == 10] <- 0
indiv_edu[indiv_edu == 11] <- 0
indiv_edu[indiv_edu == 12] <- 0
indiv_edu[indiv_edu == 13] <- 0
indiv_edu[indiv_edu == 14] <- 0
indiv_edu[indiv_edu == 15] <- 1
indiv_edu[indiv_edu == 16] <- 1
indiv_edu[indiv_edu == 17] <- 1
indiv_edu[indiv_edu == 21] <- 2
indiv_edu[indiv_edu == 22] <- 2
indiv_edu[indiv_edu == 23] <- 2
indiv_edu[indiv_edu == 31] <- 2
indiv_edu[indiv_edu == 32] <- 2
indiv_edu[indiv_edu == 33] <- 3
indiv_edu[indiv_edu == 34] <- 3
indiv_edu[indiv_edu == 35] <- 3
indiv_edu[indiv_edu == 36] <- 3
indiv_edu[indiv_edu == 41] <- 2
indiv_edu[indiv_edu == 51] <- 3
indiv_edu[indiv_edu == 61] <- 4
indiv_edu[indiv_edu == 99] <- 0

indiv_edu <- ordered(indiv_edu, levels=c(0, 1, 2, 3, 4),
                     labels=c("<=4", "5-7", "8-12", "College", "Graduate"))

edu$in_school <- g_sec4$h4q5 == "Currently attending school"
edu$not_in_school_why <- g_sec4$h4q6
edu$never_attended_school <- g_sec4$h4q5 == "Never attended"
table(edu$never_attended_school)
table(edu$not_in_school_why)

plot(indiv_edu)

### Occupation
job_data <- select(g_sec8, PID)

# Record job code
job_data$job_perm_type <- as.character(g_sec8$h8q35)

# Code as "None" those who weren't asked job type since they have no 
# off-household job (to be completed)

job <- select (ag_sec3b, HHID, A3BQ46)
job$labor_exchange <- ag_sec3b$A3BQ46 == "Yes"
table(job$labor_exchange)

individual <- select(g_sec1, HHID, urban, region)
edu_ind <- select(g_sec4, HHID, PID)
indiv_id <- full_join(individual, edu_ind)
indiv <- full_join(indiv_id, job_data)

### Demographic information
demo_data <- select(g_sec2, PID)
demo_data$sex <- g_sec2$h2q3
### complete demo_data age (where the total number of years is missing but there is some 
###information on the date of birth)

demo_data$age <- g_sec2$h2q8
plot(demo_data$sex ~ demo_data$age)

demo_data$PID_2char <- str_extract(demo_data$PID, '[0-9]{2}$')

# Household head always has individual_id == 1
demo_data$hh_head <- demo_data$PID_2char == "01"
table(demo_data$hh_head)

indiv <- full_join(indiv, demo_data)

# Setup a measure of number of children not in school that should be, based on 
# ages 8-16
indiv_edu <- full_join(indiv, edu)
indiv_edu$of_age_not_in_school_why <- indiv_edu$not_in_school_why
indiv_edu$of_age_not_in_school_why[indiv_edu$age < 8] <- NA
indiv_edu$of_age_not_in_school_why[indiv_edu$age > 16] <- NA
indiv_edu$of_age_not_in_school_noint <- (indiv_edu$not_in_school_why == "Not willing to attend")
indiv_edu$of_age_not_in_school_noint[indiv_edu$age < 8] <- NA
indiv_edu$of_age_not_in_school_noint[indiv_edu$age > 16] <- NA

indiv_edu$of_age_in_school <- indiv_edu$in_school
indiv_edu$of_age_in_school[indiv_edu$age < 8] <- NA
indiv_edu$of_age_in_school[indiv_edu$age > 16] <- NA

indiv_edu$of_age_never_attended <- indiv_edu$never_attended_school
indiv_edu$of_age_never_attended[indiv_edu$age < 8] <- NA
indiv_edu$of_age_never_attended[indiv_edu$age > 16] <- NA
indiv_edu$of_age_never_attended_why <- indiv_edu$not_in_school_why
indiv_edu$of_age_never_attended_why[indiv_edu$age < 8] <- NA
indiv_edu$of_age_never_attended_why[indiv_edu$age > 16] <- NA

table(indiv_edu$of_age_never_attended)
table(indiv_edu$of_age_never_attended_why)

table(indiv_edu$of_age_in_school)
table(indiv_edu$of_age_not_in_school_why)
table(indiv_edu$of_age_not_in_school_why)
table(indiv_edu$of_age_not_in_school_noint)

################################################
##### Farming inputs

inputs <- select(ag_sec3a, HHID, a3aq3_1)
names(inputs)[names(inputs) == "a3aq3_1"] <- "individual_id"

#### no information on crop rotation

##chemical fertilizers
inputs$hldr_chem_fert <- ag_sec3a$a3aq13 == "Yes"
table(inputs$hldr_chem_fert, useNA="always")

##extension services
extension <- select(ag_sec9, HHID, a9q2, a9q3)
input_ext <- full_join(inputs, extension)
input_ext$hldr_ext <- input_ext$a9q3 == "Yes"
table(input_ext$hldr_ext, useNA="always")

### no information on service credits

### no information on advisory services (or similar to extension question above)

dim(indiv)
####unable to join indiv and inputs??

indiv <-full_join(indiv, input_ext)

#########################
### Household level, time in residence

hh <- select(g_sec3, HHID)
## number of residence is in years or =100 if individual has lived there since birth or 0 of <1 year
hh$resid_duration <- (g_sec3$h3q15)
                            
table(hh$resid_duration < 1, useNA="always")
hist(hh$resid_duration)

#########################
## Household level, Household size
hh_size <- group_by(indiv, HHID) %>%
  summarize(hh_size=n())
hh <- full_join(hh, hh_size)

dep_ratio <- group_by(indiv, HHID) %>%
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

hh <-full_join(indiv, hh)
group_by(hh, urban) %>%
  summarize(mean(hh_size, na.rm=TRUE))

##################################
### Community-level distance metrics as measures of resource independence

com <- select (c_sec1, c1aq4, comcod)
names(com)[names(com) == "c1aq4"] <- "HHID"

### combine transportation data

trans <- full_join (g_sec18, g_sec18a)


## Commonest mode of transportation
common_trans <- (trans$h18q3)
table(common_trans)

## Distance to bus (distance to public transportation)
dist_bus_km <- (trans$h18q7)
table(dist_bus_km)

## Distance to major urban centers (instead distance to pop and administration centers )
com_dist <- select (ug_hh_geo, HHID)
com_dist$pop_center <- ug_hh_geo$dist_popcenter
com_dist$administration <- ug_hh_geo$dist_admctr

hist(com_dist$pop_center)
hist(com_dist$administration)
### community characyeristics

com_econ <- select (c_sec6b, c1aq4, ac6b, c6bq1)
names(com_econ)[names(com_econ) == "c1aq4"] <- "HHID"

com_econ$micro_entreprise <- (com_econ$ac6b == "Saving & Credit Coop") & (com_econ$c6bq1 == "Yes")

## total number of females in the group
com_econ$micro_entreprise_female <-c_sec6b$c6bq5

## community's need for extension services
com_needs_ag <- select (c_sec6d, c1aq4, c6d, c6dq1)
names(com_needs_ag)[names(com_needs_ag) == "c1aq4"] <- "HHID"

com_needs_ag$ext <- (com_needs_ag$c6d == "Agricultural/Fishery/Livestock Extension Services: Initiation/Improvement") & (com_needs_ag$c6dq1 == "Yes")

## hh assets (mobilephone)

## household assets
hh_assets <- select(g_sec14, HHID)
hh_assets$mobilephone <- (g_sec14$h14q2 == "Mobile phone") &
                                      (g_sec14$h14q3 == "Yes")
hh_assets$mobilephone_items <- ifelse(hh_assets$mobilephone == "TRUE", g_sec14$h14q4, NA)
hist(hh_assets$mobilephone_items)

###############################################################################
# Aggregate metrics

cats <- c("None", "Very low", "Low", "Average", "High", "Very high")
# Choose quantiles
qs <- c(0, .333, .666, 1)

#### Scores???

############################################
### Buffer capacity

#### Buffer job not working
buff_indiv_stats <- group_by(indiv, HHID) %>%
  summarize(job_perm_type=sum(job_perm_type, na.rm=TRUE) > 1,
            edu_gt8=sum(edu > "5-8", na.rm=TRUE)/sum(!is.na(edu))) %>%
  group_by(HHID) %>%
  summarize(job_perm_type=mean(job_perm_type, na.rm=TRUE),
            edu_gt8=mean(edu_gt8, na.rm=TRUE))

buff_hh_stats <- group_by(hh, HHID) %>%
  summarize(mean_resid_duration=mean(resid_duration, na.rm=TRUE),
            mean_dep_ratio=mean(dep_ratio, na.rm=TRUE),
            mean_hh_size=mean(hh_size))

#### (run this line when buff_indiv_stats works) buff_stats <- full_join(buff_indiv_stats, buff_hh_stats)

### finish buffer section

#######################################
### Self organization

# Farm inputs (only have chemical fertilizers)

## Labor exchange

job_stats <- group_by(job, HHID) %>%
  summarize(job_labor_exchange=sum(labor_exchange, na.rm=TRUE)/sum(!is.na(labor_exchange)))

# Microenterprise


# Distance to resources/services

## available data (distance to markets, to administration center, to health facilities, to schools)

##########################
# Capacity for learning

###############################################################################
# Plots

