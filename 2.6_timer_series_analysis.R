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
loadandinstall("lubridate")
loadandinstall("ggplot2")
loadandinstall("entropy")

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
  ex <- extent(c(598750.8 , 613857.2, -1526392, -1516472))
  
  # keep values LS5-7 or LS8
  #ls8 kls08 <- c(322, 386) & ls 5-7 kls_5_7 <- c(66, 130)
  if(i==1){v <- c(322, 386)}else {v <-  c(66, 130)}
  
  # batch process
  batch(dir_batch[i], ex, v)
  
  # list output directories 
  assign(paste0("list",i,sep=""), list.files(paste0("E:/Mosaic/north",i,"/processed_batch/ndvi",sep=""), pattern=glob2rx('*.grd'),  full.names = TRUE))
  
}
stopCluster(cl)

list1 <- list.files("E:/Mosaic/north1/processed_batch/ndvi", pattern=glob2rx('*.grd'), full.names=TRUE)
list2 <- list.files("E:/Mosaic/north2/processed_batch/ndvi", pattern=glob2rx('*.grd'), full.names=TRUE)
list3 <- list.files("E:/Mosaic/north3/processed_batch/ndvi", pattern=glob2rx('*.grd'), full.names=TRUE)

# Stack the layers
ts <- bfastSpatial::timeStack(x=c(list1, list2, list3), datatype='INT2S', overwrite=TRUE)
writeRaster(ts, filename = "TS_Cerrad_1983_2018.tif")

# show the layer names
names(ts)
s <- bfastSpatial::getSceneinfo(names(ts)) #date

# subset dates
gt <- as.data.frame(s$date)
str(gt)
names(gt) <- "date"
gt$year <- as.numeric(format(gt$date, "%Y"))
gt$month <- as.numeric(format(gt$date, "%m"))

dat <- gt[gt$month %in% c(5, 6, 7, 8, 9),]
dat$month
hist(dat$year, breaks=c(1982:2019), main="Scenes per Year (May-September) ", xlab="year", ylab="number of scenes")



# per month
s$month <- as.numeric(substr(s$date, 6, 7))
hist(s$month, breaks=c(0:12), main="p137r26: Scenes per month", xlab="month", ylab="# of scenes")

#Valid Observations: countObs
data(ts)
obs <- bfastSpatial::countObs(ts)
plot(obs)
summary(obs)
#Values can also be expressed as a percentage if as.perc is set to TRUE.
# valid observations
obs <- countObs(ts, as.perc=TRUE)
summary(obs)
# % NA per pixel
percNA <- 100 - countObs(ts, as.perc=TRUE)
plot(percNA, main="percent NA per pixel")
summary(percNA)

# set season
seasonStart <- as.numeric(format(as.Date('2000-05-01'), format = '%j'))
seasonEnd <- as.numeric(format(as.Date('2000-09-30'), format = '%j'))

#compile fkt to C+
annualComposite <- cmpfun(annualComposite)

#annual max
annualmax <- annualComposite(ts, fun=max, na.rm=TRUE)
#annualmax <- annualComposite(ts, fun=max, period=c(seasonStart:seasonEnd), na.rm=TRUE)

plot(annualmax$layer.8)
#writeRaster(annualmax, filename = "L_P137r26_annualmax_1986_2017.tif", overwrite=TRUE)

#annual min
annualmin <- annualComposite(ts, fun=min, na.rm=TRUE)
#annualmin <- annualComposite(ts, fun=min, period=c(seasonStart:seasonEnd), na.rm=TRUE)

#writeRaster(annualmin, filename = "L_P137r26_annualr.range_1986_2017.tif", overwrite=TRUE)

#annual mean
annualmean <- annualComposite(ts, fun=mean, period=c(seasonStart:seasonEnd), na.rm=TRUE)
#writeRaster(annualmean, filename = "L_P137r26_annualmean_1986_2017.tif", overwrite=TRUE)

#annualrange
annualraneg3 <- stack()
tes2t <- seq(1,35, 1)
tes3t <- seq(1,35, 1)
for(i in 1:nlayers(annualmax)){
  print(i)
  aa <- subset(annualmax,i) # aa = annualMAX
  id <- which(names(aa) == names(annualmin))
  if((length(id) == !0)){
    bb <- subset(annualmin,id) # bb = annualMIN
    cc <- subset(annualmean,id) # cc = annualMEAN
    
    dd <- aa - bb # dd range
    dd@z <- bb@z
    
    dd <- dd %in% 0:3000 # filter range 0:3000
    dd <- raster::mask(aa, dd, maskvalue = 0)
    dd <- dd %in% 7000:10000 # filter max 8250:10000
    dd <- raster::mask(cc, dd, maskvalue = 0)
    #dd <- dd %in% 3000:10000
    #dd <- raster::mask(cc, dd, maskvalue = 0)
    plot(dd, main=paste(i))
    
    names(dd) <- names(bb)
    annualraneg3 <- stack(annualraneg3,dd)
    tes2t[i] <- sd(getValues(annualraneg3[[i]]), na.rm=TRUE)
    tes3t[i] <- mean(getValues(annualraneg3[[i]]), na.rm=TRUE)
    
  }}

#writeRaster(annualraneg3, filename = "anRange.tif", overwrite=TRUE)




# plotting forest classification
#jpeg("TS_1985.jpg")
par(mfrow=c(2,2))
plot(p1985 <- annualraneg3$layer.3, main = "1985")
plot(p1991 <- annualraneg3$layer.9, main = "1991")
plot(p2008 <- annualraneg3$layer.26, main = "2008")
plot(p2018 <- annualraneg3$layer.35, main = "2018")
#dev.off()

# subset valid years (complete data)
annualmean_sub <- dropLayer(annualraneg3, c(2, 8, 10, 11, 12, 15, 16, 17, 18, 20, 21, 23, 24, 27, 29, 32, 33))
#writeRaster(annualmean_sub, filename = "anRangsub.tif", overwrite=TRUE)

#############
# buffer forest edge and extract sd and mean per buffer zone

for (i in 1:nlayers(annualmean_sub)){
  test <- annualmean_sub[[i]]
  test[is.na(test) == TRUE] <- 1 
  getValues(test)
  test2 <- test == 1
  values(test2)[values(test2) == 0] = NA
  res1 <- distance(test2)
  
list <- seq(90,300,30)
for (k in 1:length(list)){
  assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(annualmean_sub[[i]], get(paste("res1")), maskvalue = list[k], inverse = TRUE))
  if(!exists(paste0("sdF",names(annualmean_sub[[i]])))){assign(paste0("sdF",names(annualmean_sub[[i]])), sd(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
    eval(parse(text=c(paste0("sdF",names(annualmean_sub[[i]]),"[",k,"] <- sd(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
  }}
for (k in 1:length(list)){
  if(!exists(paste0("MeanF",names(annualmean_sub[[i]])))){assign(paste0("MeanF",names(annualmean_sub[[i]])), mean(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
    eval(parse(text=c(paste0("MeanF",names(annualmean_sub[[i]]),"[",k,"] <- mean(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
  }}
}


#writeRaster(res1, filename = "mask35.tif", overwrite=TRUE)

# buid data.frame
sdF <- as.data.frame(cbind(list, sdFlayer.1, sdFlayer.3, sdFlayer.4, sdFlayer.5, sdFlayer.6, sdFlayer.9,  sdFlayer.13, sdFlayer.14, sdFlayer.19, sdFlayer.22, sdFlayer.25, sdFlayer.26, sdFlayer.28, sdFlayer.30, sdFlayer.31, sdFlayer.34, sdFlayer.35))
MeanF <- as.data.frame(cbind(list, MeanFlayer.1, MeanFlayer.3, MeanFlayer.4, MeanFlayer.5, MeanFlayer.6, MeanFlayer.9,  MeanFlayer.13, MeanFlayer.14, MeanFlayer.19, MeanFlayer.22, MeanFlayer.25, MeanFlayer.26, MeanFlayer.28, MeanFlayer.30, MeanFlayer.31, MeanFlayer.34, MeanFlayer.35))


# standart deviation lregsummary
(summary(m1sd <- lm(sdF$sdFlayer.1[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m3sd <- lm(sdF$sdFlayer.3[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m4sd <- lm(sdF$sdFlayer.4[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m5sd <- lm(sdF$sdFlayer.5[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m6sd <- lm(sdF$sdFlayer.6[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m9sd <- lm(sdF$sdFlayer.9[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m13sd <- lm(sdF$sdFlayer.13[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m14sd <- lm(sdF$sdFlayer.14[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m19sd <- lm(sdF$sdFlayer.19[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m22sd <- lm(sdF$sdFlayer.22[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m25sd <- lm(sdF$sdFlayer.25[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m26sd <- lm(sdF$sdFlayer.26[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m28sd <- lm(sdF$sdFlayer.28[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m30sd <- lm(sdF$sdFlayer.30[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m31sd <- lm(sdF$sdFlayer.31[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m34sd <- lm(sdF$sdFlayer.34[sdF$list < 310] ~ sdF$list[sdF$list < 310])))
(summary(m35sd <- lm(sdF$sdFlayer.35[sdF$list < 310] ~ sdF$list[sdF$list < 310])))

# mean lregsummary
(summary(m1Mean <- lm(MeanF$MeanFlayer.1[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m3Mean <- lm(MeanF$MeanFlayer.3[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m4Mean <- lm(MeanF$MeanFlayer.4[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m5Mean <- lm(MeanF$MeanFlayer.5[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m6Mean <- lm(MeanF$MeanFlayer.6[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m9Mean <- lm(MeanF$MeanFlayer.9[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m13Mean <- lm(MeanF$MeanFlayer.13[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m14Mean <- lm(MeanF$MeanFlayer.14[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m19Mean <- lm(MeanF$MeanFlayer.19[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m22Mean <- lm(MeanF$MeanFlayer.22[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m25Mean <- lm(MeanF$MeanFlayer.25[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m26Mean <- lm(MeanF$MeanFlayer.26[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m28Mean <- lm(MeanF$MeanFlayer.28[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m30Mean <- lm(MeanF$MeanFlayer.30[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m31Mean <- lm(MeanF$MeanFlayer.31[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m34Mean <- lm(MeanF$MeanFlayer.34[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))
(summary(m35Mean <- lm(MeanF$MeanFlayer.35[MeanF$list < 310] ~ MeanF$list[MeanF$list < 310])))

par(mfrow=c(1,2))
par(oma=c(0, 0, 0, 5))
plot(NULL, xlim = c(90,300), ylim = c(150,500), type="p", pch=8, cex=0.4, col=1, main = "Standart deviation of NDVI", xlab = "Edge distance [m]", ylab = "sd NDVI")
abline(m1sd, lty=1, col=1)
abline(m3sd, lty=2, col=2)
abline(m4sd, lty=3, col=3)
abline(m5sd, lty=4, col=4)
abline(m6sd, lty=5, col=5)
abline(m9sd, lty=6, col=6)
abline(m13sd, lty=7, col=7)
abline(m14sd, lty=8, col=8)
abline(m19sd, lty=9, col=9)
abline(m22sd, lty=10, col=10)
abline(m25sd, lty=11, col=11)
abline(m26sd, lty=12, col(13))
abline(m28sd, lty=13, col=14)
abline(m30sd, lty=14, col=15)
abline(m31sd, lty=15, col=16)
abline(m34sd, lty=16, col=17)
abline(m35sd, lty=17, col=18)
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c("1983", "1985", "1986", "1987", "1988", "1991", "1995", "1996", "2001","2004", "2007", "2010", "2012", "2013", "2017" ,"2018"), horiz = FALSE, lty =  seq(1,16,1), col = seq(1,16,1), cex = 0.5)


plot(NULL, xlim = c(90,300), ylim = c(6000,9000), type="p", pch=8, cex=0.4, col=1, main = "Mean of NDVI", xlab = "Edge distance", ylab = "Mean NDVI")
abline(m1Mean, lty=1, col=1)
abline(m3Mean, lty=2, col=2)
abline(m4Mean, lty=3, col=3)
abline(m5Mean, lty=4, col=4)
abline(m6Mean, lty=5, col=5)
abline(m9Mean, lty=6, col=6)
abline(m13Mean, lty=7, col=7)
abline(m14Mean, lty=8, col=8)
abline(m19Mean, lty=9, col=9)
abline(m22Mean, lty=10, col=10)
abline(m25Mean, lty=11, col=11)
abline(m26Mean, lty=12, col=12)
abline(m28Mean, lty=13, col=13)
abline(m30Mean, lty=14, col=14)
abline(m31Mean, lty=15, col=15)
abline(m34Mean, lty=16, col=16)
abline(m35Mean, lty=17, col=17)
#legend("topright", c("1983", "1985", "1986", "1987", "1988", "1991", "1995", "1996", "2001","2004", "2007", "2008", "2010", "2012", "2013", "2017" ,"2018"), xpd = TRUE, horiz = TRUE, inset = c(0,0), bty = "n", lty =  seq(1,16,1), col = seq(1,16,1), cex = 0.4)



####################################################
#pixel-based analysis of plot G and H
###################################################
for (i in 1:nlayers(annualmean_sub)){
  mask_buf_Fragment_G <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_G.tif")
  mask_buf_Fragment_H <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_H.tif")
  mask_buf_Fragment_A <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_A.tif")
  mask_buf_Fragment_B <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_B.tif")
  mask_buf_Fragment_A <- (crop(mask_buf_Fragment_A, annualmean_sub$layer.1))
  mask_buf_Fragment_B <- (crop(mask_buf_Fragment_B, annualmean_sub$layer.1))
  mask_buf_Fragment_G <- (crop(mask_buf_Fragment_G, annualmean_sub$layer.1))
  mask_buf_Fragment_H <- (crop(mask_buf_Fragment_H, annualmean_sub$layer.1))
  
  
  list <- seq(40,100,10)
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_G), mask_buf_Fragment_G, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_G_sdF",names(annualmean_sub[[i]])))){assign(paste0("Frag_G_sdF",names(annualmean_sub[[i]])), sd(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_G_sdF",names(annualmean_sub[[i]]),"[",k,"] <- sd(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}

  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_H), mask_buf_Fragment_H, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_H_sdF",names(annualmean_sub[[i]])))){assign(paste0("Frag_H_sdF",names(annualmean_sub[[i]])), sd(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_H_sdF",names(annualmean_sub[[i]]),"[",k,"] <- sd(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_A), mask_buf_Fragment_A, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_A_sdF",names(annualmean_sub[[i]])))){assign(paste0("Frag_A_sdF",names(annualmean_sub[[i]])), sd(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_A_sdF",names(annualmean_sub[[i]]),"[",k,"] <- sd(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_B), mask_buf_Fragment_B, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_B_sdF",names(annualmean_sub[[i]])))){assign(paste0("Frag_B_sdF",names(annualmean_sub[[i]])), sd(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_B_sdF",names(annualmean_sub[[i]]),"[",k,"] <- sd(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}

}

Frag_A_sdF_temp <- as.data.frame(rbind(Frag_A_sdFlayer.1, Frag_A_sdFlayer.3, Frag_A_sdFlayer.4, Frag_A_sdFlayer.5, Frag_A_sdFlayer.6, Frag_A_sdFlayer.9,  Frag_A_sdFlayer.13, Frag_A_sdFlayer.14, Frag_A_sdFlayer.19, Frag_A_sdFlayer.22, Frag_A_sdFlayer.25, Frag_A_sdFlayer.26, Frag_A_sdFlayer.28, Frag_A_sdFlayer.30, Frag_A_sdFlayer.31, Frag_A_sdFlayer.34, Frag_A_sdFlayer.35))
Frag_B_sdF_temp <- as.data.frame(rbind(Frag_B_sdFlayer.1, Frag_B_sdFlayer.3, Frag_B_sdFlayer.4, Frag_B_sdFlayer.5, Frag_B_sdFlayer.6, Frag_B_sdFlayer.9,  Frag_B_sdFlayer.13, Frag_B_sdFlayer.14, Frag_B_sdFlayer.19, Frag_B_sdFlayer.22, Frag_B_sdFlayer.25, Frag_B_sdFlayer.26, Frag_B_sdFlayer.28, Frag_B_sdFlayer.30, Frag_B_sdFlayer.31, Frag_B_sdFlayer.34, Frag_B_sdFlayer.35))
Frag_G_sdF_temp <- as.data.frame(rbind(Frag_G_sdFlayer.1, Frag_G_sdFlayer.3, Frag_G_sdFlayer.4, Frag_G_sdFlayer.5, Frag_G_sdFlayer.6, Frag_G_sdFlayer.9,  Frag_G_sdFlayer.13, Frag_G_sdFlayer.14, Frag_G_sdFlayer.19, Frag_G_sdFlayer.22, Frag_G_sdFlayer.25, Frag_G_sdFlayer.26, Frag_G_sdFlayer.28, Frag_G_sdFlayer.30, Frag_G_sdFlayer.31, Frag_G_sdFlayer.34, Frag_G_sdFlayer.35))
Frag_H_sdF_temp <- as.data.frame(rbind(Frag_H_sdFlayer.1, Frag_H_sdFlayer.3, Frag_H_sdFlayer.4, Frag_H_sdFlayer.5, Frag_H_sdFlayer.6, Frag_H_sdFlayer.9,  Frag_H_sdFlayer.13, Frag_H_sdFlayer.14, Frag_H_sdFlayer.19, Frag_H_sdFlayer.22, Frag_H_sdFlayer.25, Frag_H_sdFlayer.26, Frag_H_sdFlayer.28, Frag_H_sdFlayer.30, Frag_H_sdFlayer.31, Frag_H_sdFlayer.34, Frag_H_sdFlayer.35))

names(Frag_A_sdF_temp) <- list
names(Frag_B_sdF_temp) <- list
names(Frag_G_sdF_temp) <- list
names(Frag_H_sdF_temp) <- list


Frag_A_sdF_spatial <- as.data.frame(cbind(list, Frag_A_sdFlayer.1, Frag_A_sdFlayer.3, Frag_A_sdFlayer.4, Frag_A_sdFlayer.5, Frag_A_sdFlayer.6, Frag_A_sdFlayer.9,  Frag_A_sdFlayer.13, Frag_A_sdFlayer.14, Frag_A_sdFlayer.19, Frag_A_sdFlayer.22, Frag_A_sdFlayer.25, Frag_A_sdFlayer.26, Frag_A_sdFlayer.28, Frag_A_sdFlayer.30, Frag_A_sdFlayer.31, Frag_A_sdFlayer.34, Frag_A_sdFlayer.35))
Frag_B_sdF_spatial <- as.data.frame(cbind(list, Frag_B_sdFlayer.1, Frag_B_sdFlayer.3, Frag_B_sdFlayer.4, Frag_B_sdFlayer.5, Frag_B_sdFlayer.6, Frag_B_sdFlayer.9,  Frag_B_sdFlayer.13, Frag_B_sdFlayer.14, Frag_B_sdFlayer.19, Frag_B_sdFlayer.22, Frag_B_sdFlayer.25, Frag_B_sdFlayer.26, Frag_B_sdFlayer.28, Frag_B_sdFlayer.30, Frag_B_sdFlayer.31, Frag_B_sdFlayer.34, Frag_B_sdFlayer.35))
Frag_G_sdF_spatial <- as.data.frame(cbind(list, Frag_G_sdFlayer.1, Frag_G_sdFlayer.3, Frag_G_sdFlayer.4, Frag_G_sdFlayer.5, Frag_G_sdFlayer.6, Frag_G_sdFlayer.9,  Frag_G_sdFlayer.13, Frag_G_sdFlayer.14, Frag_G_sdFlayer.19, Frag_G_sdFlayer.22, Frag_G_sdFlayer.25, Frag_G_sdFlayer.26, Frag_G_sdFlayer.28, Frag_G_sdFlayer.30, Frag_G_sdFlayer.31, Frag_G_sdFlayer.34, Frag_G_sdFlayer.35))
Frag_H_sdF_spatial <- as.data.frame(cbind(list, Frag_H_sdFlayer.1, Frag_H_sdFlayer.3, Frag_H_sdFlayer.4, Frag_H_sdFlayer.5, Frag_H_sdFlayer.6, Frag_H_sdFlayer.9,  Frag_H_sdFlayer.13, Frag_H_sdFlayer.14, Frag_H_sdFlayer.19, Frag_H_sdFlayer.22, Frag_H_sdFlayer.25, Frag_H_sdFlayer.26, Frag_H_sdFlayer.28, Frag_H_sdFlayer.30, Frag_H_sdFlayer.31, Frag_H_sdFlayer.34, Frag_H_sdFlayer.35))

library(lubridate)
yrs <- c(1983, 1985, 1986, 1987, 1988, 1991, 1995, 1996, 2001,2004, 2007, 2008, 2010, 2012, 2013, 2017 ,2018)
Frag_A_sdF_temp$dates <- as.Date(as.character(yrs), format = "%Y")
Frag_B_sdF_temp$dates <- Frag_A_sdF_temp$dates
Frag_G_sdF_temp$dates <- Frag_A_sdF_temp$dates
Frag_H_sdF_temp$dates <- Frag_A_sdF_temp$dates

Frag_A_sdF_spatial$list <- Frag_A_sdF_spatial$list *3
Frag_B_sdF_spatial$list <- Frag_A_sdF_spatial$list *3
Frag_G_sdF_spatial$list <- Frag_A_sdF_spatial$list *3
Frag_H_sdF_spatial$list <- Frag_A_sdF_spatial$list *3

####

# standart deviation lregsummary
(summary(m1G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.1 ~ Frag_G_sdF_spatial$list)))
(summary(m3G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.3 ~ Frag_G_sdF_spatial$list)))
(summary(m4G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.4 ~ Frag_G_sdF_spatial$list)))
(summary(m5G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.5 ~ Frag_G_sdF_spatial$list)))
(summary(m6G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.6 ~ Frag_G_sdF_spatial$list)))
(summary(m9G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.9 ~ Frag_G_sdF_spatial$list)))
(summary(m13G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.13 ~ Frag_G_sdF_spatial$list)))
(summary(m14G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.14 ~ Frag_G_sdF_spatial$list)))
(summary(m19G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.19 ~ Frag_G_sdF_spatial$list)))
(summary(m22G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.22 ~ Frag_G_sdF_spatial$list)))
(summary(m25G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.25 ~ Frag_G_sdF_spatial$list)))
(summary(m26G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.26 ~ Frag_G_sdF_spatial$list)))
(summary(m28G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.28 ~ Frag_G_sdF_spatial$list)))
(summary(m30G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.30 ~ Frag_G_sdF_spatial$list)))
(summary(m31G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.31 ~ Frag_G_sdF_spatial$list)))
(summary(m34G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.34 ~ Frag_G_sdF_spatial$list)))
(summary(m35G <- lm(Frag_G_sdF_spatial$Frag_G_sdFlayer.35 ~ Frag_G_sdF_spatial$list)))

(summary(m1H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.1 ~ Frag_H_sdF_spatial$list)))
(summary(m3H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.3 ~ Frag_H_sdF_spatial$list)))
(summary(m4H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.4 ~ Frag_H_sdF_spatial$list)))
(summary(m5H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.5 ~ Frag_H_sdF_spatial$list)))
(summary(m6H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.6 ~ Frag_H_sdF_spatial$list)))
(summary(m9H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.9 ~ Frag_H_sdF_spatial$list)))
(summary(m13H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.13 ~ Frag_H_sdF_spatial$list)))
(summary(m14H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.14 ~ Frag_H_sdF_spatial$list)))
(summary(m19H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.19 ~ Frag_H_sdF_spatial$list)))
(summary(m22H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.22 ~ Frag_H_sdF_spatial$list)))
(summary(m25H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.25 ~ Frag_H_sdF_spatial$list)))
(summary(m26H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.26 ~ Frag_H_sdF_spatial$list)))
(summary(m28H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.28 ~ Frag_H_sdF_spatial$list)))
(summary(m30H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.30 ~ Frag_H_sdF_spatial$list)))
(summary(m31H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.31 ~ Frag_H_sdF_spatial$list)))
(summary(m34H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.34 ~ Frag_H_sdF_spatial$list)))
(summary(m35H <- lm(Frag_H_sdF_spatial$Frag_H_sdFlayer.35 ~ Frag_H_sdF_spatial$list)))


'plotting'
par(mfrow=c(1,2))
par(oma=c(0, 0, 0, 5))
plot(NULL, ylim = c(0,300), xlim = c(90, 300), type="p", pch=8, cex=0.4, col=1, main = "Fragment G", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
abline(m1G, lty=1, col=1)
abline(m3G, lty=2, col=2)
abline(m4G, lty=3, col=3)
abline(m5G, lty=4, col=4)
abline(m6G, lty=5, col=5)
abline(m9G, lty=6, col=6)
abline(m13G, lty=7, col=7)
abline(m14G, lty=8, col=8)
abline(m19G, lty=9, col=9)
abline(m22G, lty=10, col=10)
abline(m25G, lty=11, col=11)
abline(m28G, lty=12, col=12)
abline(m30G, lty=13, col=13)
abline(m31G, lty=14, col=14)
abline(m34G, lty=15, col=15)
abline(m35G, lty=16, col=16)

legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c("1983", "1985", "1986", "1987", "1988", "1991", "1995", "1996", "2001","2004", "2007", "2010", "2012", "2013", "2017" ,"2018"), horiz = FALSE, lty =  seq(1,16,1), col = seq(1,16,1), cex = 0.3)


plot(NULL, ylim = c(0,300), xlim = c(90, 300), type="p", pch=8, cex=0.4, col=1, main = "Fragment H", xlab = "Edge distance [m]", ylab = "Standard deviation of NDVI")
abline(m1H, lty=1, col=1)
abline(m3H, lty=2, col=2)
abline(m4H, lty=3, col=3)
abline(m5H, lty=4, col=4)
abline(m6H, lty=5, col=5)
abline(m9H, lty=6, col=6)
abline(m13H, lty=7, col=7)
abline(m14H, lty=8, col=8)
abline(m19H, lty=9, col=9)
abline(m22H, lty=10, col=10)
abline(m25H, lty=11, col=11)
abline(m28H, lty=12, col=12)
abline(m30H, lty=13, col=13)
abline(m31H, lty=14, col=14)
abline(m34H, lty=15, col=15)
abline(m35H, lty=16, col=16)


###########
# standart deviation lregsummary
(summary(m40G <- lm(Frag_G_sdF_temp$`40` ~ Frag_G_sdF_temp$dates)))
(summary(m50G <- lm(Frag_G_sdF_temp$`50` ~ Frag_G_sdF_temp$dates)))
(summary(m60G <- lm(Frag_G_sdF_temp$`60` ~ Frag_G_sdF_temp$dates)))
(summary(m70G <- lm(Frag_G_sdF_temp$`70` ~ Frag_G_sdF_temp$dates)))
(summary(m80G <- lm(Frag_G_sdF_temp$`80` ~ Frag_G_sdF_temp$dates)))
(summary(m90G <- lm(Frag_G_sdF_temp$`90` ~ Frag_G_sdF_temp$dates)))
(summary(m100G <- lm(Frag_G_sdF_temp$`100` ~ Frag_G_sdF_temp$dates)))

(summary(m40H <- lm(Frag_H_sdF_temp$`40` ~ Frag_H_sdF_temp$dates)))
(summary(m50H <- lm(Frag_H_sdF_temp$`50` ~ Frag_H_sdF_temp$dates)))
(summary(m60H <- lm(Frag_H_sdF_temp$`60` ~ Frag_H_sdF_temp$dates)))
(summary(m70H <- lm(Frag_H_sdF_temp$`70` ~ Frag_H_sdF_temp$dates)))
(summary(m80H <- lm(Frag_H_sdF_temp$`80` ~ Frag_H_sdF_temp$dates)))
(summary(m90H <- lm(Frag_H_sdF_temp$`90` ~ Frag_H_sdF_temp$dates)))
(summary(m100H <- lm(Frag_H_sdF_temp$`90` ~ Frag_H_sdF_temp$dates)))


plot(Frag_G_sdF_temp$`100` ~ Frag_G_sdF_temp$dates)

'plotting'
plot(NULL, ylim = c(50,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment G \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
abline(m40G, lty=1, col=1)
abline(m50G, lty=2, col=2)
abline(m60G, lty=3, col=3)
abline(m70G, lty=4, col=4)
abline(m80G, lty=5, col=5)
abline(m90G, lty=6, col=6)
abline(m100G, lty=7, col=7)
legend("topright", inset=0.001, legend = c("40", "50", "60", "70", "80", "90", "100"), horiz = FALSE, lty =  seq(1,7,1), col = seq(1,7,1), cex = 0.3)


plot(NULL, ylim = c(100,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment H \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI") 
abline(m40H, lty=1, col=1)
abline(m50H, lty=2, col=2)
abline(m60H, lty=3, col=3)
abline(m70H, lty=4, col=4)
abline(m80H, lty=5, col=5)
abline(m90H, lty=6, col=6)
abline(m100H, lty=7, col=7)


##
###########
# standart deviation lregsummary

(summary(m40G <- lm(Frag_G_sdF_temp$`40` ~ Frag_G_sdF_temp$dates)))
(summary(m50G <- lm(Frag_G_sdF_temp$`50` ~ Frag_G_sdF_temp$dates)))
(summary(m60G <- lm(Frag_G_sdF_temp$`60` ~ Frag_G_sdF_temp$dates)))
(summary(m70G <- lm(Frag_G_sdF_temp$`70` ~ Frag_G_sdF_temp$dates)))
(summary(m80G <- lm(Frag_G_sdF_temp$`80` ~ Frag_G_sdF_temp$dates)))
(summary(m90G <- lm(Frag_G_sdF_temp$`90` ~ Frag_G_sdF_temp$dates)))
(summary(m100G <- lm(Frag_G_sdF_temp$`100` ~ Frag_G_sdF_temp$dates)))

(summary(m40H <- lm(Frag_H_sdF_temp$`40` ~ Frag_H_sdF_temp$dates)))
(summary(m50H <- lm(Frag_H_sdF_temp$`50` ~ Frag_H_sdF_temp$dates)))
(summary(m60H <- lm(Frag_H_sdF_temp$`60` ~ Frag_H_sdF_temp$dates)))
(summary(m70H <- lm(Frag_H_sdF_temp$`70` ~ Frag_H_sdF_temp$dates)))
(summary(m80H <- lm(Frag_H_sdF_temp$`80` ~ Frag_H_sdF_temp$dates)))
(summary(m90H <- lm(Frag_H_sdF_temp$`90` ~ Frag_H_sdF_temp$dates)))
(summary(m100H <- lm(Frag_H_sdF_temp$`90` ~ Frag_H_sdF_temp$dates)))


plot(Frag_G_sdF_temp$`100` ~ Frag_G_sdF_temp$dates)

'plotting'
plot(NULL, ylim = c(50,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment G \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
abline(m40G, lty=1, col=1)
abline(m50G, lty=2, col=2)
abline(m60G, lty=3, col=3)
abline(m70G, lty=4, col=4)
abline(m80G, lty=5, col=5)
abline(m90G, lty=6, col=6)
abline(m100G, lty=7, col=7)
legend("topright", inset=0.001, legend = c("40", "50", "60", "70", "80", "90", "100"), horiz = FALSE, lty =  seq(1,7,1), col = seq(1,7,1), cex = 0.3)


plot(NULL, ylim = c(100,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment H \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI") 
abline(m40H, lty=1, col=1)
abline(m50H, lty=2, col=2)
abline(m60H, lty=3, col=3)
abline(m70H, lty=4, col=4)
abline(m80H, lty=5, col=5)
abline(m90H, lty=6, col=6)
abline(m100H, lty=7, col=7)

###############
#pixel-based analysis of plot G and H

for (i in 1:nlayers(annualmean_sub)){
  mask_buf_Fragment_G <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_G.tif")
  mask_buf_Fragment_H <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_H.tif")
  mask_buf_Fragment_A <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_A.tif")
  mask_buf_Fragment_B <- raster("C:/Geo/Thesis/Thesis_R/GIS_Data/Buffer_Raster/300m_Distance_Buffer/Fragment_B.tif")
  mask_buf_Fragment_A <- (crop(mask_buf_Fragment_A, annualmean_sub$layer.1))
  mask_buf_Fragment_B <- (crop(mask_buf_Fragment_B, annualmean_sub$layer.1))
  mask_buf_Fragment_G <- (crop(mask_buf_Fragment_G, annualmean_sub$layer.1))
  mask_buf_Fragment_H <- (crop(mask_buf_Fragment_H, annualmean_sub$layer.1))
  
  
  list <- seq(40,100,10)
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_G), mask_buf_Fragment_G, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_G_meanF",names(annualmean_sub[[i]])))){assign(paste0("Frag_G_meanF",names(annualmean_sub[[i]])), mean(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_G_meanF",names(annualmean_sub[[i]]),"[",k,"] <- mean(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_H), mask_buf_Fragment_H, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_H_meanF",names(annualmean_sub[[i]])))){assign(paste0("Frag_H_meanF",names(annualmean_sub[[i]])), mean(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_H_meanF",names(annualmean_sub[[i]]),"[",k,"] <- mean(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_A), mask_buf_Fragment_A, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_A_meanF",names(annualmean_sub[[i]])))){assign(paste0("Frag_A_meanF",names(annualmean_sub[[i]])), mean(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_A_meanF",names(annualmean_sub[[i]]),"[",k,"] <- mean(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
  for (k in 1:length(list)){
    assign(paste0(list[k],names(annualmean_sub[[i]])), raster::mask(raster::mask(annualmean_sub[[i]], mask_buf_Fragment_B), mask_buf_Fragment_B, maskvalue = list[k], inverse = TRUE))
    if(!exists(paste0("Frag_B_meanF",names(annualmean_sub[[i]])))){assign(paste0("Frag_B_meanF",names(annualmean_sub[[i]])), mean(getValues(get(paste0(list[k],names(annualmean_sub[[i]])))), na.rm=TRUE))}else{
      eval(parse(text=c(paste0("Frag_B_meanF",names(annualmean_sub[[i]]),"[",k,"] <- mean(getValues(get(paste0(list[",k,"],names(annualmean_sub[[",i,"]])))), na.rm=TRUE)"))))
    }}
  
}

Frag_A_meanF_temp <- as.data.frame(rbind(Frag_A_meanFlayer.1, Frag_A_meanFlayer.3, Frag_A_meanFlayer.4, Frag_A_meanFlayer.5, Frag_A_meanFlayer.6, Frag_A_meanFlayer.9,  Frag_A_meanFlayer.13, Frag_A_meanFlayer.14, Frag_A_meanFlayer.19, Frag_A_meanFlayer.22, Frag_A_meanFlayer.25, Frag_A_meanFlayer.26, Frag_A_meanFlayer.28, Frag_A_meanFlayer.30, Frag_A_meanFlayer.31, Frag_A_meanFlayer.34, Frag_A_meanFlayer.35))
Frag_B_meanF_temp <- as.data.frame(rbind(Frag_B_meanFlayer.1, Frag_B_meanFlayer.3, Frag_B_meanFlayer.4, Frag_B_meanFlayer.5, Frag_B_meanFlayer.6, Frag_B_meanFlayer.9,  Frag_B_meanFlayer.13, Frag_B_meanFlayer.14, Frag_B_meanFlayer.19, Frag_B_meanFlayer.22, Frag_B_meanFlayer.25, Frag_B_meanFlayer.26, Frag_B_meanFlayer.28, Frag_B_meanFlayer.30, Frag_B_meanFlayer.31, Frag_B_meanFlayer.34, Frag_B_meanFlayer.35))
Frag_G_meanF_temp <- as.data.frame(rbind(Frag_G_meanFlayer.1, Frag_G_meanFlayer.3, Frag_G_meanFlayer.4, Frag_G_meanFlayer.5, Frag_G_meanFlayer.6, Frag_G_meanFlayer.9,  Frag_G_meanFlayer.13, Frag_G_meanFlayer.14, Frag_G_meanFlayer.19, Frag_G_meanFlayer.22, Frag_G_meanFlayer.25, Frag_G_meanFlayer.26, Frag_G_meanFlayer.28, Frag_G_meanFlayer.30, Frag_G_meanFlayer.31, Frag_G_meanFlayer.34, Frag_G_meanFlayer.35))
Frag_H_meanF_temp <- as.data.frame(rbind(Frag_H_meanFlayer.1, Frag_H_meanFlayer.3, Frag_H_meanFlayer.4, Frag_H_meanFlayer.5, Frag_H_meanFlayer.6, Frag_H_meanFlayer.9,  Frag_H_meanFlayer.13, Frag_H_meanFlayer.14, Frag_H_meanFlayer.19, Frag_H_meanFlayer.22, Frag_H_meanFlayer.25, Frag_H_meanFlayer.26, Frag_H_meanFlayer.28, Frag_H_meanFlayer.30, Frag_H_meanFlayer.31, Frag_H_meanFlayer.34, Frag_H_meanFlayer.35))

names(Frag_A_meanF_temp) <- list
names(Frag_B_meanF_temp) <- list
names(Frag_G_meanF_temp) <- list
names(Frag_H_meanF_temp) <- list


Frag_A_meanF_spatial <- as.data.frame(cbind(list, Frag_A_meanFlayer.1, Frag_A_meanFlayer.3, Frag_A_meanFlayer.4, Frag_A_meanFlayer.5, Frag_A_meanFlayer.6, Frag_A_meanFlayer.9,  Frag_A_meanFlayer.13, Frag_A_meanFlayer.14, Frag_A_meanFlayer.19, Frag_A_meanFlayer.22, Frag_A_meanFlayer.25, Frag_A_meanFlayer.26, Frag_A_meanFlayer.28, Frag_A_meanFlayer.30, Frag_A_meanFlayer.31, Frag_A_meanFlayer.34, Frag_A_meanFlayer.35))
Frag_B_meanF_spatial <- as.data.frame(cbind(list, Frag_B_meanFlayer.1, Frag_B_meanFlayer.3, Frag_B_meanFlayer.4, Frag_B_meanFlayer.5, Frag_B_meanFlayer.6, Frag_B_meanFlayer.9,  Frag_B_meanFlayer.13, Frag_B_meanFlayer.14, Frag_B_meanFlayer.19, Frag_B_meanFlayer.22, Frag_B_meanFlayer.25, Frag_B_meanFlayer.26, Frag_B_meanFlayer.28, Frag_B_meanFlayer.30, Frag_B_meanFlayer.31, Frag_B_meanFlayer.34, Frag_B_meanFlayer.35))
Frag_G_meanF_spatial <- as.data.frame(cbind(list, Frag_G_meanFlayer.1, Frag_G_meanFlayer.3, Frag_G_meanFlayer.4, Frag_G_meanFlayer.5, Frag_G_meanFlayer.6, Frag_G_meanFlayer.9,  Frag_G_meanFlayer.13, Frag_G_meanFlayer.14, Frag_G_meanFlayer.19, Frag_G_meanFlayer.22, Frag_G_meanFlayer.25, Frag_G_meanFlayer.26, Frag_G_meanFlayer.28, Frag_G_meanFlayer.30, Frag_G_meanFlayer.31, Frag_G_meanFlayer.34, Frag_G_meanFlayer.35))
Frag_H_meanF_spatial <- as.data.frame(cbind(list, Frag_H_meanFlayer.1, Frag_H_meanFlayer.3, Frag_H_meanFlayer.4, Frag_H_meanFlayer.5, Frag_H_meanFlayer.6, Frag_H_meanFlayer.9,  Frag_H_meanFlayer.13, Frag_H_meanFlayer.14, Frag_H_meanFlayer.19, Frag_H_meanFlayer.22, Frag_H_meanFlayer.25, Frag_H_meanFlayer.26, Frag_H_meanFlayer.28, Frag_H_meanFlayer.30, Frag_H_meanFlayer.31, Frag_H_meanFlayer.34, Frag_H_meanFlayer.35))

library(lubridate)
yrs <- c(1983, 1985, 1986, 1987, 1988, 1991, 1995, 1996, 2001,2004, 2007, 2008, 2010, 2012, 2013, 2017 ,2018)
Frag_A_meanF_temp$dates <- as.Date(as.character(yrs), format = "%Y")
Frag_B_meanF_temp$dates <- Frag_A_meanF_temp$dates
Frag_G_meanF_temp$dates <- Frag_A_meanF_temp$dates
Frag_H_meanF_temp$dates <- Frag_A_meanF_temp$dates

Frag_A_meanF_spatial$list <- Frag_A_meanF_spatial$list *3
Frag_B_meanF_spatial$list <- Frag_A_meanF_spatial$list *3
Frag_G_meanF_spatial$list <- Frag_A_meanF_spatial$list *3
Frag_H_meanF_spatial$list <- Frag_A_meanF_spatial$list *3

####

# standart deviation lregsummary
(summary(m1G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.1 ~ Frag_G_meanF_spatial$list)))
(summary(m3G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.3 ~ Frag_G_meanF_spatial$list)))
(summary(m4G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.4 ~ Frag_G_meanF_spatial$list)))
(summary(m5G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.5 ~ Frag_G_meanF_spatial$list)))
(summary(m6G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.6 ~ Frag_G_meanF_spatial$list)))
(summary(m9G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.9 ~ Frag_G_meanF_spatial$list)))
(summary(m13G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.13 ~ Frag_G_meanF_spatial$list)))
(summary(m14G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.14 ~ Frag_G_meanF_spatial$list)))
(summary(m19G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.19 ~ Frag_G_meanF_spatial$list)))
(summary(m22G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.22 ~ Frag_G_meanF_spatial$list)))
(summary(m25G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.25 ~ Frag_G_meanF_spatial$list)))
(summary(m26G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.26 ~ Frag_G_meanF_spatial$list)))
(summary(m28G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.28 ~ Frag_G_meanF_spatial$list)))
(summary(m30G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.30 ~ Frag_G_meanF_spatial$list)))
(summary(m31G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.31 ~ Frag_G_meanF_spatial$list)))
(summary(m34G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.34 ~ Frag_G_meanF_spatial$list)))
(summary(m35G <- lm(Frag_G_meanF_spatial$Frag_G_meanFlayer.35 ~ Frag_G_meanF_spatial$list)))

(summary(m1H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.1 ~ Frag_H_meanF_spatial$list)))
(summary(m3H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.3 ~ Frag_H_meanF_spatial$list)))
(summary(m4H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.4 ~ Frag_H_meanF_spatial$list)))
(summary(m5H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.5 ~ Frag_H_meanF_spatial$list)))
(summary(m6H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.6 ~ Frag_H_meanF_spatial$list)))
(summary(m9H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.9 ~ Frag_H_meanF_spatial$list)))
(summary(m13H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.13 ~ Frag_H_meanF_spatial$list)))
(summary(m14H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.14 ~ Frag_H_meanF_spatial$list)))
(summary(m19H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.19 ~ Frag_H_meanF_spatial$list)))
(summary(m22H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.22 ~ Frag_H_meanF_spatial$list)))
(summary(m25H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.25 ~ Frag_H_meanF_spatial$list)))
(summary(m26H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.26 ~ Frag_H_meanF_spatial$list)))
(summary(m28H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.28 ~ Frag_H_meanF_spatial$list)))
(summary(m30H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.30 ~ Frag_H_meanF_spatial$list)))
(summary(m31H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.31 ~ Frag_H_meanF_spatial$list)))
(summary(m34H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.34 ~ Frag_H_meanF_spatial$list)))
(summary(m35H <- lm(Frag_H_meanF_spatial$Frag_H_meanFlayer.35 ~ Frag_H_meanF_spatial$list)))


'plotting'
par(mfrow=c(1,2))
par(oma=c(0, 0, 0, 5))
plot(NULL, ylim = c(0,300), xlim = c(90, 300), type="p", pch=8, cex=0.4, col=1, main = "Fragment G", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
abline(m1G, lty=1, col=1)
abline(m3G, lty=2, col=2)
abline(m4G, lty=3, col=3)
abline(m5G, lty=4, col=4)
abline(m6G, lty=5, col=5)
abline(m9G, lty=6, col=6)
abline(m13G, lty=7, col=7)
abline(m14G, lty=8, col=8)
abline(m19G, lty=9, col=9)
abline(m22G, lty=10, col=10)
abline(m25G, lty=11, col=11)
abline(m28G, lty=12, col=12)
abline(m30G, lty=13, col=13)
abline(m31G, lty=14, col=14)
abline(m34G, lty=15, col=15)
abline(m35G, lty=16, col=16)

legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA, c("1983", "1985", "1986", "1987", "1988", "1991", "1995", "1996", "2001","2004", "2007", "2010", "2012", "2013", "2017" ,"2018"), horiz = FALSE, lty =  seq(1,16,1), col = seq(1,16,1), cex = 0.3)


plot(NULL, ylim = c(0,300), xlim = c(90, 300), type="p", pch=8, cex=0.4, col=1, main = "Fragment H", xlab = "Edge distance [m]", ylab = "Standard deviation of NDVI")
abline(m1H, lty=1, col=1)
abline(m3H, lty=2, col=2)
abline(m4H, lty=3, col=3)
abline(m5H, lty=4, col=4)
abline(m6H, lty=5, col=5)
abline(m9H, lty=6, col=6)
abline(m13H, lty=7, col=7)
abline(m14H, lty=8, col=8)
abline(m19H, lty=9, col=9)
abline(m22H, lty=10, col=10)
abline(m25H, lty=11, col=11)
abline(m28H, lty=12, col=12)
abline(m30H, lty=13, col=13)
abline(m31H, lty=14, col=14)
abline(m34H, lty=15, col=15)
abline(m35H, lty=16, col=16)


###########
# standart deviation lregsummary
(summary(m40G <- lm(Frag_G_meanF_temp$`40` ~ Frag_G_meanF_temp$dates)))
(summary(m50G <- lm(Frag_G_meanF_temp$`50` ~ Frag_G_meanF_temp$dates)))
(summary(m60G <- lm(Frag_G_meanF_temp$`60` ~ Frag_G_meanF_temp$dates)))
(summary(m70G <- lm(Frag_G_meanF_temp$`70` ~ Frag_G_meanF_temp$dates)))
(summary(m80G <- lm(Frag_G_meanF_temp$`80` ~ Frag_G_meanF_temp$dates)))
(summary(m90G <- lm(Frag_G_meanF_temp$`90` ~ Frag_G_meanF_temp$dates)))
(summary(m100G <- lm(Frag_G_meanF_temp$`100` ~ Frag_G_meanF_temp$dates)))

(summary(m40H <- lm(Frag_H_meanF_temp$`40` ~ Frag_H_meanF_temp$dates)))
(summary(m50H <- lm(Frag_H_meanF_temp$`50` ~ Frag_H_meanF_temp$dates)))
(summary(m60H <- lm(Frag_H_meanF_temp$`60` ~ Frag_H_meanF_temp$dates)))
(summary(m70H <- lm(Frag_H_meanF_temp$`70` ~ Frag_H_meanF_temp$dates)))
(summary(m80H <- lm(Frag_H_meanF_temp$`80` ~ Frag_H_meanF_temp$dates)))
(summary(m90H <- lm(Frag_H_meanF_temp$`90` ~ Frag_H_meanF_temp$dates)))
(summary(m100H <- lm(Frag_H_meanF_temp$`90` ~ Frag_H_meanF_temp$dates)))


plot(Frag_G_meanF_temp$`100` ~ Frag_G_meanF_temp$dates)

'plotting'
plot(NULL, ylim = c(6000, 9000), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment G \n Mean of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
plot(Frag_G_meanF_temp$`40` ~ Frag_G_meanF_temp$dates, lty=1, col=1, type = "l")
lines.default(Frag_G_meanF_temp$`50` ~ Frag_G_meanF_temp$dates)
lines.default(Frag_G_meanF_temp$`60` ~ Frag_G_meanF_temp$dates)
lines.default(Frag_G_meanF_temp$`70` ~ Frag_G_meanF_temp$dates)
lines.default(Frag_G_meanF_temp$`80` ~ Frag_G_meanF_temp$dates)
legend("topright", inset=0.001, legend = c("40", "50", "60", "70", "80", "90", "100"), horiz = FALSE, lty =  seq(1,7,1), col = seq(1,7,1), cex = 0.3)

## mean
plot(Frag_H_meanF_temp$`40` ~ Frag_G_meanF_temp$dates, lty=1, type = "l", lwd = 1, col ="deepskyblue", ylim = c(6500, 8500), main = "Fragment H and G \n Mean of NDVI per edge Distance", xlab = "Time [year]", ylab = "Mean of NDVI")
lines.default(Frag_H_meanF_temp$`50` ~ Frag_G_meanF_temp$dates, lwd = 1.5, col ="deepskyblue3")
lines.default(Frag_H_meanF_temp$`60` ~ Frag_G_meanF_temp$dates, lwd = 2,  col ="dodgerblue2")
lines.default(Frag_H_meanF_temp$`70` ~ Frag_G_meanF_temp$dates, lwd = 2.5, col = "dodgerblue3")
lines.default(Frag_H_meanF_temp$`80` ~ Frag_G_meanF_temp$dates, lwd = 3, col = "dodgerblue4")
lines.default(Frag_H_meanF_temp$`90` ~ Frag_G_meanF_temp$dates, lwd = 3.5, col = "blue4")
lines.default(Frag_H_meanF_temp$`100` ~ Frag_G_meanF_temp$dates, lwd = 4, col = "darkblue")
lines.default(Frag_G_meanF_temp$`40` ~ Frag_G_meanF_temp$dates, col = "gold1", lwd = 1)
lines.default(Frag_G_meanF_temp$`50` ~ Frag_G_meanF_temp$dates, col = "gold3", lwd = 1.5)
lines.default(Frag_G_meanF_temp$`60` ~ Frag_G_meanF_temp$dates, col = "goldenrod1",lwd = 2)
lines.default(Frag_G_meanF_temp$`70` ~ Frag_G_meanF_temp$dates, col = "goldenrod3", lwd = 2.5)
lines.default(Frag_G_meanF_temp$`80` ~ Frag_G_meanF_temp$dates, col = "goldenrod4", lwd = 3)
lines.default(Frag_H_meanF_temp$`90` ~ Frag_G_meanF_temp$dates, lwd = 3.5, col = "darkgoldenrod3")
lines.default(Frag_H_meanF_temp$`100` ~ Frag_G_meanF_temp$dates, lwd = 4, col = "darkgoldenrod4")
abline(v=as.numeric(Frag_A_meanF_temp$dates[4]), col="red")
abline(v=as.numeric(Frag_A_meanF_temp$dates[8]), col="red")
text(x=as.numeric(Frag_A_meanF_temp$dates[6]), y=8250, 'Deforestation', col='red')
arrows(as.numeric(Frag_A_meanF_temp$dates[4]), 8150, as.numeric(Frag_A_meanF_temp$dates[8]), 8150, col = "red", length = 0.1)
arrows(as.numeric(Frag_A_meanF_temp$dates[8]), 8150, as.numeric(Frag_A_meanF_temp$dates[4]), 8150, col = "red", length = 0.1)
## (x,y) bezeichnet den Mittelpunkt des Textes

legend_order <- matrix(1:14,ncol=2,byrow = TRUE)
legend("bottomright", legend = c("","40", "50", "60", "70", "80", "90", "100","", "40", "50", "60", "70", "80", "90", "100"),lty = 1,  lwd = rep(seq(0,7,1),2),  col = c("white","deepskyblue", "deepskyblue3", "dodgerblue2", "dodgerblue3", "dodgerblue4", "blue4", "darkblue","white", "gold1", "gold3", "goldenrod1", "goldenrod3", "goldenrod4", "darkgoldenrod3", "darkgoldenrod4"), cex = 0.3,  ncol=2)
text(x=as.numeric(Frag_A_meanF_temp$dates[15]), y=7250, 'Fragment G                     ', col='black', font =2)
text(x=as.numeric(Frag_A_meanF_temp$dates[16]), y=7250, 'Fragment H      ', col='black', font =2)
#legend(x = as.numeric(Frag_A_meanF_temp$dates[14]), y =  7450, title = "Fragment G", bty = "n", legend = c("40", "50", "60", "70", "80", "90", "100"), lty =  seq(1,7,1), lwd = seq(1,4,0.5),  col=c("gold1", "gold3", "goldenrod1", "goldenrod3", "goldenrod4", "darkgoldenrod3", "darkgoldenrod4"), cex = 0.4)

par(mfrow=c(1,1))
#sd 
plot(Frag_H_sdF_temp$`40` ~ Frag_G_sdF_temp$dates, lty=1, type = "l", lwd = 1, col ="deepskyblue", ylim = c(0, 300), main = "Fragment H and G \n sd of NDVI per edge Distance", xlab = "Time [year]", ylab = "sd of NDVI")
lines.default(Frag_H_sdF_temp$`50` ~ Frag_G_sdF_temp$dates, lwd = 1.5, col ="deepskyblue3")
lines.default(Frag_H_sdF_temp$`60` ~ Frag_G_sdF_temp$dates, lwd = 2,  col ="dodgerblue2")
lines.default(Frag_H_sdF_temp$`70` ~ Frag_G_sdF_temp$dates, lwd = 2.5, col = "dodgerblue3")
lines.default(Frag_H_sdF_temp$`80` ~ Frag_G_sdF_temp$dates, lwd = 3, col = "dodgerblue4")
lines.default(Frag_H_sdF_temp$`90` ~ Frag_G_sdF_temp$dates, lwd = 3.5, col = "blue4")
lines.default(Frag_H_sdF_temp$`100` ~ Frag_G_sdF_temp$dates, lwd = 4, col = "darkblue")
lines.default(Frag_G_sdF_temp$`40` ~ Frag_G_sdF_temp$dates, col = "gold1", lwd = 1)
lines.default(Frag_G_sdF_temp$`50` ~ Frag_G_sdF_temp$dates, col = "gold3", lwd = 1.5)
lines.default(Frag_G_sdF_temp$`60` ~ Frag_G_sdF_temp$dates, col = "goldenrod1",lwd = 2)
lines.default(Frag_G_sdF_temp$`70` ~ Frag_G_sdF_temp$dates, col = "goldenrod3", lwd = 2.5)
lines.default(Frag_G_sdF_temp$`80` ~ Frag_G_sdF_temp$dates, col = "goldenrod4", lwd = 3)
lines.default(Frag_H_sdF_temp$`90` ~ Frag_G_sdF_temp$dates, lwd = 3.5, col = "darkgoldenrod3")
lines.default(Frag_H_sdF_temp$`100` ~ Frag_G_sdF_temp$dates, lwd = 4, col = "darkgoldenrod4")
abline(v=as.numeric(Frag_A_sdF_temp$dates[4]), col="red")
abline(v=as.numeric(Frag_A_sdF_temp$dates[8]), col="red")
text(x=as.numeric(Frag_A_sdF_temp$dates[6]), y=270, 'Deforestation', col='red')
arrows(as.numeric(Frag_A_sdF_temp$dates[4]), 250, as.numeric(Frag_A_sdF_temp$dates[8]), 250, col = "red", length = 0.1)
arrows(as.numeric(Frag_A_sdF_temp$dates[8]), 250, as.numeric(Frag_A_sdF_temp$dates[4]), 250, col = "red", length = 0.1)
## (x,y) bezeichnet den Mittelpunkt des Textes

legend("topright", legend = c("","40", "50", "60", "70", "80", "90", "100","", "40", "50", "60", "70", "80", "90", "100"),lty = 1,  lwd = rep(seq(0,7,1),2),  col = c("white","deepskyblue", "deepskyblue3", "dodgerblue2", "dodgerblue3", "dodgerblue4", "blue4", "darkblue","white", "gold1", "gold3", "goldenrod1", "goldenrod3", "goldenrod4", "darkgoldenrod3", "darkgoldenrod4"), cex = 0.21,  ncol=2)
text(x=as.numeric(Frag_A_sdF_temp$dates[13]), y=300, 'Fragment G              ', col='black', cex = 0.7 , font =2)
text(x=as.numeric(Frag_A_sdF_temp$dates[16]), y=300, 'Fragment H                ', col='black', cex = 0.7 , font =2)
#legend(x = as.numeric(Frag_A_sdF_temp$dates[14]), y =  7450, title = "Fragment G", bty = "n", legend = c("40", "50", "60", "70", "80", "90", "100"), lty =  seq(1,7,1), lwd = seq(1,4,0.5),  col=c("gold1", "gold3", "goldenrod1", "goldenrod3", "goldenrod4", "darkgoldenrod3", "darkgoldenrod4"), cex = 0.4)

Frag_H_sdF_temp$`40`

##
###########
# standart deviation lregsummary

(summary(m40G <- lm(Frag_G_meanF_temp$`40` ~ Frag_G_meanF_temp$dates)))
(summary(m50G <- lm(Frag_G_meanF_temp$`50` ~ Frag_G_meanF_temp$dates)))
(summary(m60G <- lm(Frag_G_meanF_temp$`60` ~ Frag_G_meanF_temp$dates)))
(summary(m70G <- lm(Frag_G_meanF_temp$`70` ~ Frag_G_meanF_temp$dates)))
(summary(m80G <- lm(Frag_G_meanF_temp$`80` ~ Frag_G_meanF_temp$dates)))
(summary(m90G <- lm(Frag_G_meanF_temp$`90` ~ Frag_G_meanF_temp$dates)))
(summary(m100G <- lm(Frag_G_meanF_temp$`100` ~ Frag_G_meanF_temp$dates)))

(summary(m40H <- lm(Frag_H_meanF_temp$`40` ~ Frag_H_meanF_temp$dates)))
(summary(m50H <- lm(Frag_H_meanF_temp$`50` ~ Frag_H_meanF_temp$dates)))
(summary(m60H <- lm(Frag_H_meanF_temp$`60` ~ Frag_H_meanF_temp$dates)))
(summary(m70H <- lm(Frag_H_meanF_temp$`70` ~ Frag_H_meanF_temp$dates)))
(summary(m80H <- lm(Frag_H_meanF_temp$`80` ~ Frag_H_meanF_temp$dates)))
(summary(m90H <- lm(Frag_H_meanF_temp$`90` ~ Frag_H_meanF_temp$dates)))
(summary(m100H <- lm(Frag_H_meanF_temp$`90` ~ Frag_H_meanF_temp$dates)))


plot(Frag_G_meanF_temp$`100` ~ Frag_G_meanF_temp$dates)

'plotting'
plot(NULL, ylim = c(50,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment G \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI")
abline(m40G, lty=1, col=1)
abline(m50G, lty=2, col=2)
abline(m60G, lty=3, col=3)
abline(m70G, lty=4, col=4)
abline(m80G, lty=5, col=5)
abline(m90G, lty=6, col=6)
abline(m100G, lty=7, col=7)
legend("topright", inset=0.001, legend = c("40", "50", "60", "70", "80", "90", "100"), horiz = FALSE, lty =  seq(1,7,1), col = seq(1,7,1), cex = 0.3)


plot(NULL, ylim = c(100,150), xlim = c(1983, 2018), type="p", pch=8, cex=0.4, col=1, main = "Fragment H \n Standard deviation of NDVI per edge Distance", xlab = "Edge distance [m]", ylab = "Standart deviation of NDVI") 
abline(m40H, lty=1, col=1)
abline(m50H, lty=2, col=2)
abline(m60H, lty=3, col=3)
abline(m70H, lty=4, col=4)
abline(m80H, lty=5, col=5)
abline(m90H, lty=6, col=6)
abline(m100H, lty=7, col=7)




