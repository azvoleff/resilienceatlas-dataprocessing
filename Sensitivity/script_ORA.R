library("raster")

source('../0_settings.R')

# Set the main directory
mainDir <- file.path(prefix, "GRP", "Sensitivity")
setwd(mainDir) 

# Import the landsea mask and plot it
landsea<-read.table("landsea.txt", header=F)
landsea<-c(as.matrix(landsea))
length(landsea)

landsea.matrix <- matrix(landsea, ncol=7200, byrow=FALSE)
lsRaster <- raster(landsea.matrix)
plot(lsRaster)


mask<-c(which(landsea==1))
length(mask)

# Need this landsea.tile.matrix for later on in the script
landsea.tile.matrix <- matrix(landsea, ncol= 100, byrow = FALSE)




###################################################################################
############################ DATA ASSEMBLY FUNCTIONS ##############################
###################################################################################

######  first split the data for each variable up into 100 'tiles' due to large size of files.

makeTiles <- function(var= "evi"){
	
	dir <- paste(mainDir, "/", var, sep="")
	dirNew <- 	paste(mainDir, "/", var,"_tiled", sep="")
	dir.create(dirNew) 

	fileNames <- list.files(path= dir, pattern =".txt")
	nTiles <- 100
	tileMatrix <- matrix(c(1:21600000), ncol= nTiles, byrow=FALSE) # Make a matrix used to subset the different tiles in the variable
	
	print("Splitting data into tiles")
	progBar <- txtProgressBar(min = 0, max = length(fileNames), style = 3)
 	
	
	for(k in 1: length(fileNames)){
		varName <- unlist(strsplit(fileNames[k], split=".", fixed=TRUE))[1]
		dirName <- paste(mainDir,"/", var, "_tiled/", varName, sep="")
		
		# make a new directory for each month for the variable of interest
		dir.create(dirName) 
		
		#Read in the data
		fullData <- read.table(paste(dir, "/", fileNames[k], sep=""), header=FALSE)[,1]		

		# For each of the 100 tiles grap the observations from the tile of interest (defined within tile matrix) and then save in the new directory
		for(i in 1: nTiles){
		dataToGet <- tileMatrix[,i]	
            tiledData <- fullData[dataToGet]
			write.table(tiledData, 
					file=paste(dirName, "/",varName, "_", i, ".csv", sep=""), 
					sep=",", 
					col.names=FALSE, row.names= FALSE
					)
	
		} # End of nTiles loop[i]
	
	setTxtProgressBar(progBar, k)
	} # End of fileNames [k] loop
} # End of function




####### Run the function to make the tiles for the variable. All the data will be separated into tiles for each month
makeTiles(var="evi")
makeTiles(var="temp")
makeTiles(var="aetpet")
makeTiles(var="cld")


#########################################################################################
## Sort all the tiled data into a new  directory, (eg. evi_sorted)

#########################################################################################

moveTileData<-function(var="evi"){

	dir <- paste(mainDir, "/", var, "_tiled", sep="") # Directory with the tiled data from above function
	fileNamesMonth <- list.files(path= dir)
	
	dirNew <- paste(mainDir, "/", var, "_sorted", sep="")
	dir.create(dirNew)
	
	print("Moving data into tiles")
	progBar <- txtProgressBar(min = 0, max = 100, style = 3)
 	
	for( j in 1: 100){
		tileName <- paste(var, "_", j, sep="") # Name for directory for tile 
		dir.create(paste(dirNew,"/", tileName, sep="")) # Create a new directory for the file	
	
		for(i in 1:length(fileNamesMonth)){
		
			fileToMove <- paste(dir,"/", fileNamesMonth[i],"/", fileNamesMonth[i], "_", j, ".csv", sep="") 
			fileToPlace <- paste(dirNew, "/", tileName, "/",  fileNamesMonth[i], "_", j, ".csv", sep="")
			file.rename(from = fileToMove, to = fileToPlace)
		}
	setTxtProgressBar(progBar, j)
	}
	unlink(dir, recursive = TRUE) # Deletes directory where the moved files were placed
}
nTiles=100

moveTileData(var="evi")
moveTileData(var="cld")
moveTileData(var="aetpet")
moveTileData(var="temp")


##########################################################################################
## PREPARE DATA FUNCTION TO MAKE TIMESERIES TILE BY TILE.

## NB. ONLY DOING THIS FOR LAND AREAS TO SAVE COMPUTiNG TIME LATER, SO USE 'landsea.tile.matrix' OBJECT TO DO THIS

##########################################################################################

prepareData <- function (var = "evi", tile = 1, mask.matrix = landsea.tile.matrix){
	
	newDir <-  paste(mainDir, "/", var, "_ts", sep="")
	dir.create(newDir)

	#Import each monthly tiled image, mask it and find the land areas in the specific tile from the mask
  	tilePath <- paste(mainDir, "/", var,"_sorted/", var, "_", tile, sep="")
	filenames<-list.files(path=tilePath, pattern=".csv") # reads the filenames in the directory

	mask.full <- mask.matrix[,tile] # select which column in the matrix you are interested in 
	mask <- which(mask.full!= -9999) # select the land areas

	resultMatrix <- matrix(NA, ncol=length(filenames), nrow= length(mask), dimnames= list(seq(1:length(mask)), filenames))  # Months/ years will be in columns, pixels from the landsea mask will be in rows
	setwd(tilePath)
	
	print("Compiling data into time series within a tile")
	progBar <- txtProgressBar(min = 0, max = length(filenames), style = 3)
 	
	for(j in 1: length(filenames)){
	
		data1<-read.csv(file=filenames[j], header=F) 
		data1<-c(as.matrix(data1))
		resultMatrix[,j]<-data1[mask]
		setTxtProgressBar(progBar, j)
	}
	write.csv(resultMatrix, file= paste(newDir, "/", var, "_ts_", tile,".csv", sep=""))
}

# THEN USE THE PREPARE DATA FUNCTION TO MAKE THE TIME SERIES FOR EACH MONTH (OK to ignore warnings when this function completed)
 
for(i in 1: 100) prepareData(var = "evi", tile =i, mask.matrix = landsea.tile.matrix)
for(i in 1: 100) prepareData(var = "cld", tile =i, mask.matrix = landsea.tile.matrix)
for(i in 1: 100) prepareData(var = "aetpet", tile =i, mask.matrix = landsea.tile.matrix)
for(i in 1: 100) prepareData(var = "temp", tile =i, mask.matrix = landsea.tile.matrix)


###################################################################################
## SCRIPT TO CALCULATE VSI SCORE 

## Functions for this stored in another script file (xxx). 
## Load this script then run it for each tile individually in the loop. 
## Stores the result as an .Rdata file. Then use the separate mapping script functions to make the maps.

###################################################################################

### Load the function
source(paste(mainDir, "/script_vsiFun.R", sep ="" ))

dir.create(paste(mainDir, "/", "results", sep=""))

for(tile in 1:100){

	evi <- read.csv(paste(mainDir, "/evi_ts/evi_ts_",tile, ".csv", sep=""), header = TRUE, row.names=1)
	aet <- read.csv(paste(mainDir, "/aetpet_ts/aetpet_ts_",tile, ".csv", sep=""), header = TRUE, row.names=1)
	temp <- read.csv(paste(mainDir, "/temp_ts/temp_ts_",tile, ".csv", sep=""), header = TRUE, row.names=1)
	cld <- read.csv(paste(mainDir, "/cld_ts/cld_ts_",tile, ".csv", sep=""), header = TRUE, row.names=1)


vsiResult<-vector("list", nrow(evi))
for(pixel in 1: nrow(evi)){
	
	evi1 <- c(NA, as.numeric(evi[pixel, ]))# Adds an extra value to the start of the time series so it begins in Jan 2000
	temp1 <- c(NA, as.numeric(temp[pixel, ]))
	cld1 <- c(NA, as.numeric(cld[pixel, ]))
	aet1 <- c(NA, as.numeric(aet[pixel, ]))
	
	output<-vsi(evi1, temp1, cld1, aet1) 
	vsiResult[[pixel]]<-output		
}	

save(vsiResult, file=paste(mainDir, "/", "results/tile_", tile, ".RData", sep=""))	# summaryPCA saved as an RData file
}




###################################################################################
## ESTIMATE THE CLIMATE COEFFICIENT WEIGHTS / CIs (EXTENDED DATA 2,3, 10)

# First you compile the results from the tiles, then you project it back to the global grid and store as a csv file for use.

###################################################################################

dir.create(paste(mainDir, "/rasters", sep=""))
source(paste(mainDir, "/script_mappingFun.R", sep ="" ))

# Calculates the mean coefficients of the 3 climate weights and the t-1 variable
coefResults <- compileCoefTiles(tilePath = paste(mainDir, "/results", sep = ""), absVal = TRUE) # Compile your results

# Makes a .csv matrix which can be turned into a raster. Extended data figure 2 is the file "coeft1.csv". Standardises scores in each variable between 0-100
landSeaProj(resultMatrix = coefResults, landsea = landsea, fullStand = FALSE, resultPath = paste(mainDir, "/rasters/coef", sep = ""))

# Extended Data Figure 2 (climate weights). Calculate the coefficients without the t-1 variable (raw coefficients and standardised on own, and against full dataset)
coefNonT1 <- compileCoefTilesNONT1(tilePath = paste(mainDir, "/results", sep = ""), absVal <- TRUE)
landSeaProj(resultMatrix = coefNonT1, landsea = landsea, fullStand = TRUE, resultPath = paste(mainDir, "/rasters/coefNonT1_global_", sep = ""))
landSeaProj(resultMatrix = coefNonT1, landsea = landsea, fullStand = FALSE, resultPath = paste(mainDir, "/rasters/coefNonT1", sep = "")) #  coefficients from regression for a given variable - T1 not accounted in the weight - Locally weighted) for Individual display of each coefficient.

# Get the climate / t-1 as a percentage for making Extended Data Figure 3
coefPerc <- compileCoefTilesPERCENTAGE(tilePath = paste(mainDir, "/results", sep = ""))
landSeaProj(resultMatrix = coefPerc, landsea = landsea, fullStand = FALSE, resultPath = paste(mainDir, "/rasters/coefPerc", sep=""))


# Extended Data Figure 10. Get the width of the confidence intervals of the coefficients for each variable
CI <- compileCITiles(tilePath = paste(mainDir, "/results", sep = "")) # Compile your results
landSeaProj(resultMatrix = CI, landsea = landsea, fullStand = FALSE, raw = TRUE, resultPath = paste(mainDir, "/rasters/cI", sep ="")) # IS THIS CORRECT? HAVE HAD TO ADD RAW - TRUE

# Makes a .csv file with the raw, untransformed values of the coefficients in the regressions- in order to divide cIs by them to scale them.
landSeaProj(resultMatrix = coefResults, landsea = landsea, fullStand = FALSE, raw = TRUE, resultPath = paste(mainDir, "/rasters/cI_coef", sep = ""))



###################################################################################
## ESTIMATE THE MEAN VARIANCE ANOMALIES FOR EACH VARIABLE
###################################################################################

# Get the mean EVIs in each
meanSig <- compileVarTilesAnon(tilePath = paste(mainDir, "/results", sep= ""), var= "meanVecSig")
landSeaProj(resultMatrix = meanSig, landsea = landsea, fullStand = TRUE, resultPath = paste(mainDir, "/rasters/meanVec_", sep =""))

# Get the variance
sdSig <- compileVarTilesAnon(tilePath = paste(mainDir, "/results", sep= ""), var= "sdDetVecSig")
landSeaProj(resultMatrix = sdSig, landsea = landsea, fullStand = TRUE, resultPath = paste(mainDir, "/rasters/sdDetVec_", sep =""))

# Calculate the anomalies for each variable. 
temp <- anomCalcNew(var = "temp", filePath = paste(mainDir, "/rasters/anom", sep = ""))
aet <- anomCalcNew(var = "aet", filePath = paste(mainDir, "/rasters/anom", sep = ""))
cld <- anomCalcNew(var = "cld", filePath = paste(mainDir, "/rasters/anom", sep = ""))
evi <- anomCalcNew(var = "evi", filePath = paste(mainDir, "/rasters/anom", sep = ""))


# Make the mean variance plots (Extended Data 8).

makeAnomPlot<- function(var = temp, xlabel = "Mean", label = "a"){
	varPlot <- var
	colList <- list(evi = rgb(100,100 ,100, 70, maxColorValue = 255), temp =  rgb(100, 0 ,0, 70, maxColorValue = 255),
			    aet = rgb(0, 0 , 100, 70, maxColorValue = 255), cld = rgb(0, 100 ,0, 70, maxColorValue = 255))
	
	plot(varPlot$x, varPlot$y, col= eval(parse(text = paste("colList$", varPlot$varName, sep=""))), pch=16, xlab = xlabel, ylab = "Variance")
	lines(varPlot$xPred, varPlot$yPred)
	
	xLab <- min(varPlot$x, na.rm = TRUE) + (max(varPlot$x, na.rm= TRUE) - min(varPlot$x, na.rm = TRUE))*0.025
	yLab <- min(varPlot$y, na.rm= TRUE) + (max(varPlot$y, na.rm = TRUE) - min(varPlot$y, na.rm = TRUE))*0.975
	text(xLab, yLab, labels= label, cex = 1.1) 
}

pdf(paste(mainDir, "/ExDataFig8.pdf", sep= ""))
par(mfrow= c(2,2))
makeAnomPlot(var = evi, label= "a", xlabel = "Mean EVI")
makeAnomPlot(var = temp, label = "b", xlabel =expression (paste("Mean Temperature (", degree ~K, ")", sep ="")))
makeAnomPlot(var = aet, label= "c", xlabel = "Mean AET")
makeAnomPlot(var = cld, label = "d", xlabel = "Mean Cloudiness Index")
dev.off()

dataToSave <- list(temp = temp, aet = aet, cld = cld, evi = evi)
save(dataToSave, file = paste(mainDir, "/ExDataFig8.RData", sep =""))




###################################################################################
# THEN M.M.F. TOOK THESE CSV FILES AND MADE THE FINAL CALCULATIONS IN MATLAB FOR FIGURE 1, 
# MAKING ALL THE MAPS IN ARCGIS.

# SEE THE "matlabMappingscript.m" MATLAB SCRIPT FOR DETAILS

###################################################################################




###################################################################################
## FIND AND COUNT MONTHS WITH SIGNIFICANT PCA/ CLIMATE COEFFICIENTS IN THE REGRESSIONS (EXTENDED DATA 7)
###################################################################################

sigMonth <- compileSigMonthTiles(tilePath = paste(mainDir, "/results", sep =""), var = "sig.var.inf.sum")

# Project to landsea matrix
sigMonth1 <- landsea
sigMonth1[landsea== 1] <- sigMonth
sigMonth.m <- matrix(sigMonth1, ncol = 7200, nrow= 3000, byrow= FALSE)
rSigmonth <- raster(sigMonth.m)
plot(rSigmonth)
write.table(sigMonth.m, file = paste(mainDir, "/rasters/sigMonth.csv", sep='"), sep= ",", col.names=FALSE, row.names= FALSE)


###################################################################################
# STANDARD ERROR OF MEAN PLOT (EXTENDED DATA 9)
###################################################################################

# Make rasters of mean monthly standard deviations of EVI first...
var <- "stdev"
library("raster")

setwd(paste(mainDir,"/", var, sep=""))
dir.create(paste(mainDir, "/", var, "_raster", sep="")) 

# convert text strings to raster
toImp <- list.files() # read the filenmaes from the directory

for(i in 1: length(toImp)){
	sd1 <- read.table(toImp[i], header = FALSE)[,1]
	sd1 <- matrix(sd1, ncol = 7200, nrow = 3000, byrow = TRUE)
	sd1[sd1 == -9999] <- NA
	sd1[sd1 == -999] <- NA
	sd1[sd1 == 0] <- NA
	r <- raster(sd1/ 10000)

	rName <- strsplit(toImp[i], ".txt")[[1]][1]
	setwd(paste(mainDir, "/", var, "_raster", sep = ""))
	writeRaster(r, filename = paste(rName, ".grd", sep = ""), format = "raster", overwrite = TRUE )
	setwd(paste(mainDir, "/", var, sep = ""))
}

# Calculate the mean sd for the different months from the grd files
setwd(paste(mainDir, "/", var, "_raster", sep= ""))
rImp <- list.files()
want <- grep(".grd", rImp)
rImp <- rImp[want]
rImp <- rImp
select <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for(i in 1:length(select)){
	want <- grep(paste(select[i], var, sep ="_"), rImp)
	rImp1 <- rImp[want]
	s1 <- stack(rImp1)
	meanS1 <- calc(s1, mean, na.rm = TRUE)
	writeRaster(meanS1, filename = paste("mean_", var, "_",select[i], ".grd", sep = ""), format = "raster", overwrite = TRUE )
}



## MAke the plot of the PDF
rImp <- list.files()
want <- grep(".grd", rImp)
rImp <- rImp[want]
rImp <- rImp[168:179]
for(i in 1: 12){
	r <- raster(rImp[i])
	pdf(paste(rImp[i], ".pdf", sep = ""))
	plot(r)
	dev.off()
}

### Calculate the mean of the monthly SDs  (sdYR). sd1 is a stack of the monthly means
# rImp <- list.files()
# want <- grep(".grd", rImp)
# rImp <- rImp[want]
# rImp <- rImp[168:179]

sd1 <- stack(rImp)
sdYr <- calc(sd1, mean, na.rm = TRUE)
writeRaster(sdYr, filename = "sdYr.grd", format = "raster", overwrite = TRUE )

sdYr <- raster("sdYr")
plot(sdYr)

# sdYr.mat <- as.matrix(sdYr)
# write.table(sdYr.mat, file = "sdYr.csv", sep= "", row.names= FALSE, col.names= FALSE)





###################################################################
# Now do the same with with the monthly means of npixels

var <- "numpxl"
setwd(paste(mainDir,"/", var, sep=""))
dir.create(paste(mainDir, "/", var, "_raster", sep="") 

# convert text strings to raster
toImp <- list.files() # read the filenmaes from the directory

for(i in 1: length(toImp)){
	nP1 <- read.table(toImp[i], header = FALSE)[,1]
	nP1 <- matrix(nP1, ncol = 7200, nrow = 3000, byrow = TRUE)
	nP1[nP1 == -9999] <- NA
	nP1[nP1 == -999] <- NA
	r <- raster(nP1)

	rName <- strsplit(toImp[i], ".txt")[[1]][1]
	setwd(paste(mainDir, "/", var, "_raster", sep = ""))
	writeRaster(r, filename = paste(rName, ".grd", sep = ""), format = "raster", overwrite = TRUE )
	setwd(paste(mainDir, "/", var, sep = ""))
}


setwd(paste(mainDir, "/numpxl_raster", sep= ""))
rImp <- list.files()
want <- grep(".grd", rImp)
rImp <- rImp[want]
select <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

for(i in 1:length(select)){
	want <- grep(paste(select[i], "numpxl", sep ="_"), rImp)
	rImp1 <- rImp[want]
	s1 <- stack(rImp1)
	meanS1 <- calc(s1, mean, na.rm = TRUE)
	writeRaster(meanS1, filename = paste("mean_numpxl_", select[i], ".grd", sep = ""), format = "raster", overwrite = TRUE )
}


rImp <- list.files()
want <- grep(".grd", rImp)
rImp <- rImp[want]
rImp <- rImp[168:179]

nPxMean <- stack(rImp)
nPxAMean <- calc(nPxMean, mean, na.rm = TRUE)

### Can then calculate mean annual nPix and sd and then use this to find the standard error of the mean (not used in the paper)...

sqNPx <- calc(nPxAMean, sqrt)
plot(sqNPx)

stderr <- overlay(sdYr, sqNPx, fun = function(x, y){return(x/y)}) 
par(mfrow=c(1,1))
plot(stderr)
writeRaster(stderr, filename = paste(mainDir, "/", "stderror.grd", sep= ""), format= "raster", overwrite= TRUE)

## Or find the standard error of the mean for each month and then use that instead

# Get the monthly number of pixels
numPxmonth <- stack(rImp)
nPxMnth_sqrt <- calc(numPxmonth, sqrt, forceapply= TRUE)
plot(nPxMnth_sqrt)

	
# Get the monthly sds
var <- "stdev_raster"
setwd(paste(mainDir, "/", var, sep=""))
rImp <- list.files()
want <- grep(".grd", rImp)
rImp <- rImp[want]
notwant <- grep(".pdf", rImp)
rImp <- rImp[-notwant]
rImp <- rImp[168:179]
stdevMonth <- stack(rImp)

monthStErr <- overlay(stdevMonth, nPxMnth_sqrt, fun= function(x,y){return(x/y)})
writeRaster(monthStErr, filename = paste(mainDir, "/", "stderror_month.grd", sep = ""), format= "raster", overwrite= TRUE)

monthStErr_year <- calc(monthStErr, mean, na.rm= TRUE)
writeRaster(monthStErr_year, filename = paste(mainDir, "/", "month_yr_stderror.grd", sep = ""), format= "raster", overwrite= TRUE)





###################################################################################
# EXTENDED DATA 5

###################################################################################

# Get Worldclim prec data

totPrec <- raster(paste(mainDir, "/totPrecProj.grd", sep = ""))


# Crop to the warmer latitudes. Also remove very higher precipitation regions
totPrec1 <-totPrec
totPrec1[totPrec > 800] <- NA
totPrec1[totPrec < 100] <- NA
totPrec1[1:800, ] <- NA
totPrec1[2800:3000, ] <- NA
plot(totPrec1)


### Get the aetpet and t-1 values
t1 <- read.csv(paste(mainDir, "/rasters/cI_coeft1.csv", sep = ""), header = FALSE)
t1 <- as.matrix(t1)
t1R <- raster(t1)
t1R[t1R == -9999] <- NA
t1R[t1R ==-8888] <- NA
plot(t1R)

aet1 <- read.csv(paste(mainDir, "/rasters/cI_coefaet.csv", sep = ""), header = FALSE)
aet1 <- as.matrix(aet1)
aet1R <- raster(aet1)
aet1R[aet1R == -9999] <- NA
aet1R[aet1R ==-8888] <- NA
plot(aet1R)


##### USED A HACK FROM THIS BLOG POST TO ENABLE YOU TO CHECK FOR AUTOCORRELATION IN RESIDUALS ACROSS GEOGRAPHIC DISTANCE IN NLME- NEED TO DOWNLOAD THIS SOURCE CODE AND IMPORT INTO R
# THE CODE FOR THE HACK IS HERE: http://stackoverflow.com/questions/18857443/specifying-a-correlation-structure-for-a-linear-mixed-model-using-the-ramps-pack

source(paste(mainDir, "/corHaversine_nlmeHack.R", sep = ""))

r <- totPrec
r[is.na(totPrec)]<- 0
latlon <- data.frame(rasterToPoints(r))
dry <- data.frame(aet = aet1R@data@values, t1 = t1R@data@values, prec = totPrec1@data@values, lon = latlon$x, lat = latlon$y)

want <- with(dry, !is.na(prec))
drySub <- dry[want,]
with(drySub, hist(prec))
library("nlme")
library("gstat")
library("rgdal")
library("gtools")
# Get random subset
smp <- sample(x = 1: nrow(drySub), size = 1000)

par(mfrow= c(2,2))
drySmp <- drySub[smp,]
drySmp <- drySmp[complete.cases(drySmp),]
dim(drySmp)

# Fit the model of t-1 coefficient against prec
with(drySmp, plot(prec, t1, col = "darkred", pch = 1, ylab = "Coefficient in linear model", xlab = "Total Annual Precipitation (mm)", cex = 0.5))
mT1 <- lm(t1 ~ prec, data= drySmp)

# And with spatial autocorrelation strucutre
mT1.spat <- gls(t1 ~ prec, data= drySmp,  method= "REML", correlation = corHaversine(value = c(1000, 0.5), form = ~ lon + lat, mimic = "corExp", nugget = TRUE, fixed = FALSE))
summary(mT1.spat)
abline(mT1.spat, lwd= 2)

# Check the variagram of the residuals
# foo <- structure(list(z = residuals(mT1.spat, type="normalized"), lon = drySmp$lon, lat = drySmp$lat), .Names = c("z", "lon", "lat"), row.names = c(1:nrow(drySmp)), class = "data.frame")
# coordinates(foo) <- ~lon+lat
# proj4string(foo) <- CRS("+proj=longlat")
# v <- variogram(z~1,foo, cutoff= 10000)
# vParam <- na.omit(as.numeric(strsplit(capture.output(summary(mT1.spat)$modelStruct$corStruct)[3], "\\ ")[[1]]))
# vFit <- fit.variogram(v, vgm(1, model = "Exp", range = vParam[1], nugget = vParam[2]))
# plot(v, vFit)


# Do the same for aet-pet coefficient against prec
with(drySmp, plot(prec, aet, col = "darkblue", pch = 2, cex = 0.5, ylab = "Coefficient in linear model", xlab = "Total Annual Precipitation (mm)", ylim= (c(0.05, 0.9))))
mPrec <- lm(aet~ prec, data= drySmp)
mPrec.spat <- gls(aet ~ prec, data= drySmp,  method= "REML", correlation = corHaversine(value = c(1000, 0.5), form = ~ lon + lat, mimic = "corExp", nugget = TRUE, fixed = FALSE))
# foo <- structure(list(z = residuals(mPrec.spat, type="normalized"), lon = drySmp$lon, lat = drySmp$lat), .Names = c("z", "lon", "lat"), row.names = c(1:nrow(drySmp)), class = "data.frame")
# coordinates(foo) <- ~lon+lat
# proj4string(foo) <- CRS("+proj=longlat")
# v <- variogram(z~1,foo, cutoff= 10000)
# library(gtools)
# vParam <- na.omit(as.numeric(strsplit(capture.output(summary(mPrec.spat)$modelStruct$corStruct)[3], "\\ ")[[1]]))
# vFit <- fit.variogram(v, vgm(1, model = "Exp", range = vParam[1], nugget = vParam[2]))
# plot(v, vFit)

summary(mPrec.spat)
abline(mPrec.spat, lwd= 2)
legend(x = 52, y =0.94, legend = c("t-1", "AET"), pch= c(20, 2), col = c("red", "blue"), cex= 0.75)
dev.off()


###################################################################################
# EXTENDED DATA 4

###################################################################################


# First need to make raster stack of EVI time series so that rasters for a given lat long can be extracted
dir.create(paste(mainDir, "/eviRaster", sep=""))
filenames <- list.files(path = paste(mainDir, "/evi", sep= ""))

for(i in 1:length(filenames)){
	eviMonth <- read.table( paste(mainDir, "/evi/",filenames[i], sep =""), header= FALSE)
	eviMonthMat <- matrix(eviMonth[,1], nrow= 3000, ncol = 7200, byrow= FALSE)

	r <- raster(eviMonthMat, xmn = -180, xmx = 180, ymn = -60, ymx = 90)
	writeRaster(r, filename = paste(mainDir, "/eviRaster/", filenames[i], ".grd", sep = ""), overwrite = TRUE)
}


s.file <-  list.files(path = paste(mainDir, "/eviRaster", sep= ""))
want <- grep(".grd", s.file)
setwd(paste(mainDir, "/eviRaster", sep= ""))
wantEVI <- s.file[want]  
eviStack <- stack(wantEVI ) # EVI stack

# Get the coordinates
coord <- coordinates(eviStack[[1]])
coord <- cbind(1:nrow(coord), coord)


# Then set the boundaries of the box that you want to get the time series for. To make extended data figure 4 need to repeat this for:

# xMin = -100, 30, 115, -38, 10, 127  
# yMin = 47, 48, 44, -9, 13, -28

# e.g. 
xMin <-30 
xMax <- xMin+ 1
yMin <- 48
yMax <- yMin+1

coord.x <- coord[which(coord[,2] > xMin & coord[,2] < xMax ), ]
dim(coord.x)
coord.y <- coord.x[which(coord.x[,3] > yMin & coord.x[,3] < yMax), ]
dim(coord.y)

eviTSall <- extract(eviStack, coord.y[, 1])
# dir.create(paste(mainDir, "/eviTSExFig4", sep= ""))
# write.csv(eviTSall, file = paste(mainDir, "/eviTSExFig4/eviall_",xMin, yMin, ".csv", sep= ""))

# NB two lines above not run. Already created the directory and placed the extracted time series in there. Prefixed the files with 1-6 in order of plotting in Extended Data Figure 4. 


# Can Then import the data to make the plots
mnt <- c("jan", "feb", "mar", "apr", "may","jun", "jul", "aug", "sep", "oct", "nov", "dec")
yr <- 2000:2013

tsDir <- paste(mainDir, "/eviTSExFig4", sep="")
var1 = "evi"

monthAnomCalc <- function(x){
		xMat <- matrix(	c(NA, x), ncol = 12, nrow = 14, dimnames = list(yr, mnt), byrow = TRUE)
		xMat.scale <- apply(xMat, 2, scale)
		x.scale <- c(t(xMat.scale))
		return(x.scale)
	}

tsToMake1 <- list.files(tsDir)


nObsTab <- matrix(NA, nrow= length(tsToMake1), ncol=1, dimnames= list(tsToMake1,"nObs"))

m <- matrix(c(1,3,5,2,4,6,13,13,13,13,13,13,7,9,11,8,10,12), ncol = 3, byrow = TRUE)
par(mar= c(3,3,1,4), bty= "n", mgp = c(0,0.5,0), cex = 0.5)
layout(m)


for(i in 1: length(tsToMake1)){
	
	tsPlot1 <- as.matrix(read.csv(paste(tsDir, "/", tsToMake1[i], sep=""), row.names= 1))/10000
	if(any(tsPlot1 == -9999))  tsPlot1[tsPlot1==-9999] <- NA
	if(any(tsPlot1 ==-999)) tsPlot1[tsPlot1==-999] <- NA
	
	# Make anomalies 
	tsAnom1 <- t(apply(tsPlot1,1 , monthAnomCalc))[,-1]
			
	meanTs1 <- colMeans(tsPlot1, na.rm= TRUE)
	meanTs1.ts <- ts(meanTs1, start = c(2000, 2), end = c(2013,12), frequency =12)	
	
	sdTs1 <- 2*(apply(tsPlot1, 2, sd, na.rm =TRUE))
	
	minTs1 <- meanTs1.ts-sdTs1
	minTs1[minTs1<0] <- 0
	maxTs1 <- meanTs1.ts + sdTs1	
	
			
	n <- length(meanTs1.ts)+2

	plot(1:n, 1:n, type = "n", pch= 20, col = "darkgreen", xlab = "", ylab = "", yaxt = "n", xaxt = "n", ylim = c(min(minTs1, na.rm= TRUE), max(maxTs1, na.rm= TRUE)))	
	polygon(c(2:(n-1), (n-1):2), c(minTs1, rev(maxTs1)), col = "lightgreen", border= NA)
	points(2:(n-1), meanTs1.ts, type = "o", pch= 20, col = "darkgreen", xlab = "Year", ylab = "mean.EVI", xaxt = "n", ylim = c(0, max(maxTs1)))	
	axis(1, at = seq(1, n, by = 12), labels = c(2000:2014), cex.axis=0.75, tck = -0.05)
	mtext(side = 1, line = 1.5, "Year", cex = 0.6)
	axis(2,  cex.axis=0.75, tck = -0.05)
	mtext(side = 2, line = 1.5, "mean EVI", cex = 0.6)		
	
	meanTs2 <- colMeans(tsAnom1, na.rm = TRUE)
	
	meanTs2.ts <- ts(meanTs2, start = c(2000, 2), end = c(2013,12), frequency =12)

	plot(1:n, 1:n, type = "n", pch= 20, col = "blue", xlab = "", ylab = "", yaxt = "n", xaxt = "n", ylim = c(min(meanTs2, na.rm = TRUE), max(meanTs2, na.rm = TRUE)))	
	points(2:(n-1), meanTs2.ts, type = "h", pch= 20, col = "darkblue", xlab = "Year", ylab = "", xaxt = "n")	
	axis(1, at = seq(1, n, by = 12), labels = c(2000:2014), cex.axis=0.75, tck = -0.05)
	mtext(side = 1, line = 1.5, "Year", cex = 0.6)
	axis(2,  cex.axis=0.75, tck = -0.05)
	mtext(side = 2, line = 1.5, "EVI anom.", cex = 0.6)		
		
	
		
}


## Then add the map in the centre. Note that I could not get the legend to plot correctly using the layout function so ended up saving this separatly and then adding to pdf.

### Get the aetpet and t-1 values
#t1 <- read.csv(paste(mainDir, "/results/cI_coeft1.csv",sep =""), header = FALSE)
t1 <- as.matrix(t1)
t1R <- raster(t1, xmn = -180, xmx = 180, ymn = -60 , ymx = 90)
t1R[t1R == -9999] <- NA
t1R[t1R ==-8888] <- 0
aggregate(t1R, scale= 10)

t1Q <- quantile(t1R, probs= 0.75)

t1R[t1R < t1Q] <- 0
t1R[t1R > 0 ] <- 2
# plot(t1R)


#aet1 <- read.csv(paste(mainDir, "/results/cI_coefprec.csv", sep="") header = FALSE)
aet1 <- as.matrix(aet1)
aet1R <- raster(aet1, xmn = -180, xmx = 180, ymn = -60 , ymx = 90)
aet1R[aet1R == -9999] <- NA
aet1R[aet1R ==-8888] <- 0
# plot(aet1R)
aggregate(aet1R, scale= 10)

aetQ <- quantile(aet1R, probs= 0.75)

aet1R[aet1R < aetQ] <- 0
aet1R[aet1R > 0 ] <- 1
# plot(aet1R)

dry <- overlay(aet1R, t1R, fun = function(x,y) {return(x+y)})

breakpoints <- c(0, 0.5,1.5,2.5, 3.5)
colors <- c("darkgrey","darkblue","darkred","lightblue")


par(mar = c(0,0,0,0))
plot(dry,breaks=breakpoints,col=colors, legend = FALSE,xaxt = "n", yaxt = "n", bty= "n")
legend(x= -90, y = 0, legend = c("Upper t-1 quartile", "Upper AET quartile", "Overlapping quartiles"), col = c("darkred", "darkblue", "lightblue"), pch = 15, cex = 0.5)










