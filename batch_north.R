#example for using bfast spatial, taken from http://www.loicdutrieux.net/bfastSpatial/
#.libPaths("D:/R_library")
#.libPaths("P:/R/win-library/3.3")
#.libPaths("https://github.com")
#source("P:/Calltobegin.r")
#detach_package(raster)

loadandinstall <- function(mypkg) {if (!is.element(mypkg, installed.packages()[,1]))
{install.packages(mypkg)}; library(mypkg, character.only = T)}
#loadandinstall("installr")
#updateR()


# load the package
#devtools::install('loicdtx/bfastSpatial', ref = 'develop')
#devtools::install_github('loicdtx/bfastSpatial', ref='develop')

loadandinstall("rstudioapi")
loadandinstall("bfastSpatial")
loadandinstall("devtools")
loadandinstall("RStoolbox")
loadandinstall("raster")
loadandinstall("Hmisc") ###attention::Mask!!
loadandinstall("compiler")
loadandinstall("doParallel")
loadandinstall("bfast")


#set wd
setwd('E:/Mosaic')

####################################################################
####################################################################

# Get list of input data files. It is expected that you copy all .tar.gz archives to this folder:

#Running Landsat import in batch mode
# Get the directory where the Landsat archives are stored

###################################
#         batch process           #
###################################
dir_batch <- c("E:/Mosaic/Landsat_8","E:/Mosaic/ls_07_north","E:/Mosaic/ls_05_north")

#compile batch function!! (package compiler)    (#for LAndsat x (see LEDAPS product guide for details about the pixel_qa layer))
batch <- function(x, e, v){bfastSpatial::processLandsatBatch(x=x, pattern = NULL, outdir=dirout, srdir=NULL, vi='ndvi', e = e, mask = 'pixel_qa', keep = v, overwrite=TRUE)}
batch <- cmpfun(batch)
#############################
###doParallel###

no_cores<-detectCores()-1
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)
#stopCluster(cl)


foreach (i=1:length(dir_batch))%dopar%{
  library(bfastSpatial)
  library(raster)
  
  #batch <- function(x, e, v){bfastSpatial::processLandsatBatch(x=x, pattern = NULL, outdir=dirout, srdir=NULL, vi='ndvi', e = e, mask = 'pixel_qa', keep = v, overwrite=TRUE)}
  #dir_batch <- c("E:/Mosaic/Landsat_8","E:/Mosaic/ls_07_north","E:/Mosaic/ls_07_south","E:/Mosaic/ls_05_north","E:/Mosaic/ls_05_south")
  # Set the location of output and intermediary directories (everything in tmpdir in that case)
  # We use dirname(rasterTmpFile()) instead of rasterOptions()$tmpdir to reduce verbose
  rasterOptions(tmpdir = paste0("E:/Mosaic/north",i,sep=""))
  
  # Create a directory to store the output files.
  srdir <- dirout <- file.path(dirname(rasterTmpFile()), 'processed_batch')
  dir.create(dirout, showWarning=FALSE)
  
  #Define extent within Landsat image to be processed (in UTM coordinates):
  ex <- extent(c(602862.3 , 613640.1, -1537215, -1510000))
  
  # keep values LS5-7 or LS8
  #ls8 kls08 <- c(322, 386) & ls 5-7 kls_5_7 <- c(66, 130)
  if(i==1){v <- c(322, 386)}else {v <-  c(66, 130)}
  
  # batch process
  batch(dir_batch[i], ex, v)
  
  # list output directories 
  assign(paste0("list",i,sep=""), list.files(paste0("E:/Mosaic/north",i,"/processed_batch/ndvi",sep=""), pattern=glob2rx('*.grd'),  full.names = TRUE))
  
}
stopCluster(cl)


# Stack the layers
ts <- bfastSpatial::timeStack(x=c(list1, list2, list3), filename=ts_north, datatype='INT2S', overwrite=TRUE))
writeRaster(ts, filename = "TS_Cerrad_1983_2018.tif")

# set season
seasonStart <- as.numeric(format(as.Date('2000-09-30'), format = '%j'))
seasonEnd <- as.numeric(format(as.Date('2000-05-15'), format = '%j'))


annualComposite <- cmpfun(annualComposite)
#annual sd
annualSd <- annualComposite(ts, fun=sd, period=c(seasonStart:seasonEnd), na.rm=TRUE)
plot(annualSd, 8)
writeRaster(annualSd, filename = "L_P137r26_annualSd_1986_2017.tif")

# mask fragments
fragment <- shapefile("C:/Geo/Landsat/fragment_Cerrado.shp")
# match coordinates shapefile-raster
fragment <- spTransform(fragment, CRS = crs(ts[[1]]))
identicalCRS(fragment, ts)

ts_fragment <- crop(ts, fragment, snap="out")
plot(ts_fragment[[2]])

