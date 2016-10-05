##################### Calculates mean of the total rainfall using the WorldClim CMIPS 30 seconds data########################
########################### Climate data can be found at: http://www.worldclim.org/cmip5_30s#################################
############################# Country admin data can be found at: http://gadm.org/country####################################
#############################################################################################################################
##Notes: Run lines 24-119 for current and 2050 climate models and 24-93 and uncomment 2070 in line 41 for 2070 models
#####################################################Part 1 & 3#################################################################
##Download WorldClim data (into WorldClim folder) and unzip to separate into unique folders: CMIPS_30s_2050 & CMIPS_30s_2070
##Download the country boundary and place shapefile in unique folder eg for Madagascar: folder = MDG_adm_shp & shp = MDG_adm0
##Inputs: climate models and country boundary shapefile
##Output: Total annual rainfall saved for each model/scenario into its individiual folder
#####################################################Part 2 # 4##################################################################
##Calculates the mean from all climate scenarios from output of the code: "climate_worldclim_MDG.R"
##Each folder within the CMIPS_30s_2050 & CMIPS_30s_2070 folders has a unique output from the above code 
## for example: for each model scenario a unique model is named "total_annual_rainfall_ac45pr50"
## saved within the folder: E:\WorldClim\CMIPS_30s_2050\ac45pr50
##Inputs: Total annual rainfall output from various climate model/scenarios
##Output: Mean, min and max of the total annual rainfall saved for the year
#####################################################Part 5##################################################################
##Calculates the total annual rainfall for the current WorldClim data
##Data available from: http://www.worldclim.org/current
##Inputs: climate model (.bil) and country boundary shapefile
##Output: Total annual rainfall saved for current climate model into its individiual folder
#####################################################Part 6-8##################################################################
##Calculates the difference of the mean (current rainfall-model rainfall/current rainfall)*100% to get difference in percent
##Inputs: Mean results from all models/scenarios and mean of current rainfall
##Output: Difference in percent for each of the model summaries for each scenario
#####################################################Part 0##################################################################
###Assigning variables and set up your workspace
###Load libraries
library(raster)
library(stringr)
library(foreach)
library(rgdal)

##Assigns temp folder for data processing (insert drive on your computer with extra memory)
rasterOptions(tmpdir='D:/temp')

## Assign variable for shapefile of the country boundary
## Note: This is a shapefile for the country boundary of Madagascar
MDG <- readOGR('E:/Data/MDG_adm_shp','MDG_adm0')

### Enter in folder containing all scenarios
base_dir50 <- "E:/WorldClim/CMIPS_30s_2050"
base_dir70 <- "E:/WorldClim/CMIPS_30s_2070"
base_dircur <- "E:/WorldClim/Current/prec_30s_bil"

##Assign model variables and naming scheme
##Note: for a model named "ac45pr50"; Model Code = "ac", scenario = "45", variable = "pr" and year = "50"
models <- list.dirs(base_dir70, recursive=FALSE)
models <- data.frame(matrix(unlist(str_match_all(models, '([a-z]{2})([0-9]{2})([a-z]{2})([0-9]{2})')), ncol=5, byrow=TRUE))
names(models) <- c("folder", "model", "scenario", "variable", "year")

#######################################################Part 5##################################################################
##Calculate the total annual rainfall for the current WorldClim data
#Stacks the images
r_monthly <- stack(list.files(base_dircur, 
                              pattern='prec_[0-9]{3,4}.bil$', 
                              full.names=TRUE, recursive=TRUE))

#Crops to the shapefile listed above
r_monthly <- crop(r_monthly, MDG)

#Assigns a variable for the total annual rainfall 
curclim <- calc(r_monthly, function(x) {
  x[x < 0] <- NA
  x <- sum(x)
  if ((x < 0) | is.na(x)) {
    return(-9999)
  } else {
    return(x)
  }
}, filename=file.path(base_dircur, paste0('total_annual_rainfall_current.tif')), overwrite=TRUE)

curclm <- file.path(base_dircur, "/total_annual_rainfall_current.tif")
curclim <-raster(curclm)

#####################################################Part 1##################################################################
###Pulls monthly climate data and creates an output for each model/scenario, saved into each model/scenario folder
##foreach loop to stack each model and scenario
foreach(scenario=unique(models$scenario)) %do% {
  these_models <- models[models$scenario == scenario, ]
  
  foreach(year=unique(these_models$year)) %do% {
    these_models <- these_models[these_models$year == year, ]
    
    foreach(folder=these_models$folder) %do% {
      monthly_files <- list.files(file.path(base_dir70, folder),
                                  pattern=paste0('[a-z]{2}', scenario, '[a-z]{2}', year, '[0-9]{1,2}.tif'),
                                  full.names=TRUE)
      stopifnot(length(monthly_files) == 12)
      r_monthly <- stack(monthly_files)
      
      # NOTE THAT LAYERS ARE NOT IN THE CORRECT ORDER BY MONTH
      
      #Clip to country boundary
      r_monthly <- crop(r_monthly, MDG)
      
      #Function to sum all raster (tif) files monthly to return annual output
      ann <- calc(r_monthly, function(x) {
        x[x < 0] <- NA
        x <- sum(x)
        if ((x < 0) | is.na(x)) {
          return(-9999)
        } else {
          return(x)
        }
      }, filename=file.path(base_dir70, folder, paste0('total_annual_rainfall_', folder, '.tif')), overwrite=TRUE)
    }
    
    r_anns <- stack(list.files(base_dir70,
                               pattern=paste0('total_annual_rainfall_[a-z]{2}', scenario, '[a-z]{2}', year, '.tif'),
                               full.names=TRUE, recursive=TRUE))
    r_mean <- calc(r_anns, function(x) {
      mean(x)
    }, filename=file.path(base_dir70, paste0('total_annual_rainfall_multimodel_mean_', scenario, '_', year, '.tif')),
    overwrite=TRUE)
    
    r_min <- calc(r_anns, function(x) {
      min(x)
    }, filename=file.path(base_dir70, paste0('total_annual_rainfall_multimodel_min_', scenario, '_', year, '.tif')),
    overwrite=TRUE)
    
    r_max <- calc(r_anns, function(x) {
      max(x)
    }, filename=file.path(base_dir70, paste0('total_annual_rainfall_multimodel_max_', scenario, '_', year, '.tif')),
    overwrite=TRUE)
    
    ##Raster calculator to get the mean of all scenarios
    pct_diff <- ((curclim - r_mean)/curclim)*100

    pct_diff <- mask(crop(pct_diff, MDG), MDG, updateValue=-9999, updateNA=TRUE,
                     filename=paste0(base_dir70, '/total_annual_rainfall_multimodel_pctdiff_mean_', scenario, '_', year, '.tif'),
                     overwrite=TRUE)
    
  }
}

#######################################################Part 7##################################################################
##Difference between models from 45 scenario in year 2050
##Assigns the output from the mean of all 45_pr_50 scenarios 
r_mn45_50 <- file.path(base_dir50, "/total_annual_rainfall_multimodel_mean_45_50.tif")
r_mean45_50 <-raster(r_mn45_50)

##Raster calculator to get the mean of all scenarios
pct_diff45_50 <- ((curclim - r_mean45_50)/curclim)*100

##Crops output to the country boundary and plots output
pct_diff45_50 <- crop(pct_diff45_50, MDG)
#plot(pct_diff45_50, zlim=c(-35, 50))

##Writes raster to folder
writeRaster(pct_diff45_50, filename=paste0(base_dir50, '/total_annual_rainfall_multimodel_pctdiff_mean_45_50.tif'), overwrite=TRUE)

##Repeats for the another set of scenarios
##Difference between models from 85 scenario in year 2050
r_mn85_50 <- file.path(base_dir50, "/total_annual_rainfall_multimodel_mean_85_50.tif")
r_mean85_50 <-raster(r_mn85_50)

##Raster calculator to get the mean of all scenarios
pct_diff85_50 <- ((curclim - r_mean85_50)/curclim)*100

##Crops output to the country boundary and plots output
pct_diff85_50 <- crop(pct_diff85_50, MDG)
#plot(pct_diff85_50, zlim=c(-35, 50))

##Writes raster to folder
writeRaster(pct_diff85_50, filename=paste0(base_dir50, '/total_annual_rainfall_multimodel_pctdiff_mean_85_50.tif'), overwrite=TRUE)
#######################################################Part 8##################################################################
##Difference between models from 45 scenario in year 2070
r_mn45_70 <- file.path(base_dir70, "/total_annual_rainfall_multimodel_mean_45_70.tif")
r_mean45_70 <-raster(r_mn45_70)

##Raster calculator to get the mean of all scenarios
pct_diff45_70 <- ((curclim - r_mean45_70)/curclim)*100

##Crops output to the country boundary and plots output
pct_diff45_70 <- crop(pct_diff45_70, MDG)
#plot(pct_diff45_70, zlim=c(-35, 50))

##Writes raster to folder
writeRaster(pct_diff45_70, filename=paste0(base_dir70,  '/total_annual_rainfall_multimodel_pctdiff_mean_45_70.tif'), overwrite=TRUE)

##Difference between models from 85 scenario in year 2070
r_mn85_70 <- file.path(base_dir70, "/total_annual_rainfall_multimodel_mean_85_70.tif")
r_mean85_70 <-raster(r_mn85_70)

##Raster calculator to get the mean of all scenarios
pct_diff85_70 <- ((curclim - r_mean85_70)/curclim)*100

##Crops output to the country boundary and plots output
pct_diff85_70 <- crop(pct_diff85_70, MDG)
#plot(pct_diff85_70, zlim=c(-35, 50))

##Writes raster to folder
writeRaster(pct_diff85_70, filename=paste0(base_dir70, '/total_annual_rainfall_multimodel_pctdiff_mean_85_70.tif'), overwrite=TRUE)
#######################################################END##################################################################

