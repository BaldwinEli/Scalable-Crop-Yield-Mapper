install.packages("devtools")
library(devtools)
install.packages("dplyr")
library(dplyr)
install.packages("sp")
library(sp)
install.packages("raster")
library(raster)
install.packages("rgdal")
library(rgdal)
install.packages("Metrics")
library(Metrics)

#read in data in the form of raster images from a drone.
fs <- dir("F:/HonorsResearch/Drone imagery processed/drone/drone", pattern = "tif$", full.names = TRUE)
fieldsCI <- lapply(fs, raster)

fieldsCI <- as.list(up_1_ci1, up_2_ci1, up_3_ci1, up_4_ci1, mid_1_ci1, mid_2_ci1, mid_3_ci1, mid_4_ci1, dwn_1_ci1, dwn_2_ci1, dwn_3_ci1, dwn_4_ci1)

#read in a csv table containing yield predictor values
yield_predictors <- read.csv(file = "F:/HonorsResearch/yield_predictors.csv")

#specify the days of year to be used
DOYs <- c(196:258)

#initialize data frames for R-squared values and correlation information
DOYGCVIR2 <- data.frame()
DOYGCVICO <- data.frame()

#For every DOY in the range, creeate a model for yield (HWAH) estimated by DSSAT, based on max temp, precipitation and (red-edge)NDVI and green Chlorophyll Vegetation Index
for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, temp))
  DOYGCVIR2 <- rbind(DOYGCVIR2, tempS[["adj.r.squared"]])
}

for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, temp))
  DOYGCVICO <- rbind(DOYGCVICO, tempS[["coefficients"]])
}

for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~NDVI+T2M_MAX+Precip_cumsum, temp))
  DOYNDVIR2 <- rbind(DOYNDVIR2, tempS[["adj.r.squared"]])
}

for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~NDVI+T2M_MAX+Precip_cumsum, temp))
  DOYNDVICO <- rbind(DOYNDVICO, tempS[["coefficients"]])
}

DOYR2 <- cbind(DOYGCVIR2, DOYNDVIR2)

#initialize new data frame
DOYGCVIT <- data.frame()

for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~GCVI+T2M_MAX+Precip_cumsum, temp))
  DOYGCVIT <- rbind(DOYGCVIT, tempS[["coefficients"]])
}

DOYNDVIT <- data.frame()

for(x in DOYs){
  temp <- yield_predictors %>% subset(DOY==x)
  tempS <- summary(lm(HWAH~GCVI+T2M_MAX+Precip_cumsum, temp))
  DOYNDVIT <- rbind(DOYNDVIT, tempS[["coefficients"]])
}

#subset yield predictors to just 2018
yp2018 <- yield_predictors %>% subset(year==2018)

#calculate average yield for each of three fields based on values from DSSAT
upYP2018 <- yp2018 %>% subset(device=="A000671") 
upYield2018 <- unique(upYP2018$HWAH)
mean(upYield2018, trim=0, na.rm=TRUE)
#Average yield is 9753.229
midYP2018 <- yp2018 %>% subset(device=="A000680")
midYield2018 <- unique(midYP2018$HWAH)
mean(midYield2018, trim=0, na.rm=TRUE)
#Average yield is 9456.291
dwnYP2018 <- yp2018 %>% subset(device=="A000667")
dwnYield2018 <- unique(dwnYP2018$HWAH)
mean(dwnYield2018, trim=0, na.rm=TRUE)
#Average yield is 9775.593

#get weather variables for each date
#July19, DOY200
july19 <- yp2018 %>% subset(DOY==200)
mean(july19$Precip_cumsum, trim = 0, na.rm=TRUE) #650.22
mean(july19$T2M_MAX, trim = 0, na.rm=TRUE) #26.3

#Aug02, DOY214
aug02 <- yp2018 %>% subset(DOY==214)
mean(aug02$Precip_cumsum, trim = 0, na.rm=TRUE) #714.57
mean(aug02$T2M_MAX, trim = 0, na.rm=TRUE) #31.41

#Aug10, DOY222
aug10 <- yp2018 %>% subset(DOY==222)
mean(aug10$Precip_cumsum, trim = 0, na.rm=TRUE) #737.31
mean(aug10$T2M_MAX, trim = 0, na.rm=TRUE) #29.92

#Aug24, DOY236
aug24 <- yp2018 %>% subset(DOY==236)
mean(aug24$Precip_cumsum, trim = 0, na.rm=TRUE) #830.59
mean(aug24$T2M_MAX, trim = 0, na.rm=TRUE) #27.47

#models
#July19, DOY200
modJ19 <- yield_predictors %>% subset(DOY==200)
summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, modJ19))
#coefficients int = -10240, GCVI2 = 1437, temp = 383.2, precip = 2.089
#Aug02, DOY214
modA02 <- yield_predictors %>% subset(DOY==214)
summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, modA02))
#coefficients int = 10460, GCVI2 = 1082, temp = -318.9, precip = 1.540
#Aug10, DOY222
modA10 <- yield_predictors %>% subset(DOY==222)
summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, modA10))
#coefficients int = 11430, GCVI2 = 784.5, temp = -268.3, precip = 0.7118
#Aug24, DOY236
modA24 <- yield_predictors %>% subset(DOY==236)
summary(lm(HWAH~GCVI2+T2M_MAX+Precip_cumsum, modA24))
#coefficients int = 2361, GCVI2 = 892.9, temp = 26.54, precip = 0.5986

#calculate per pixel yield rasters for each field for each date
#July19, DOY200
up_1_yield <- calc(up_1_ci1, function(x){
  1196.46958+1437*x
})
mid_1_yield <- calc(mid_1_ci1, function(x){
  1196.46958+1437*x
})
dwn_1_yield <- calc(dwn_1_ci1, function(x){
  1196.46958+1437*x
})
#Aug02, DOY214
up_2_yield <- calc(up_2_ci1, function(x){
  1543.789+1082*x
})
mid_2_yield <- calc(mid_2_ci1, function(x){
  1543.789+1082*x
})
dwn_2_yield <- calc(dwn_2_ci1, function(x){
  1543.789+1082*x
})
#Aug10, DOY222
up_3_yield <- calc(up_3_ci1, function(x){
  3927.281258+784.5*x
})
mid_3_yield <- calc(mid_3_ci1, function(x){
  3927.281258+784.5*x
})
dwn_3_yield <- calc(dwn_3_ci1, function(x){
  3927.281258+784.5*x
})
#Aug24, DOY236
up_4_yield <- calc(up_4_ci1, function(x){
  3587.244974+892.9*x
})
mid_4_yield <- calc(mid_4_ci1, function(x){
  3587.244974+892.9*x
})
dwn_4_yield <- calc(dwn_4_ci1, function(x){
  3587.244974+892.9*x
})

#correlation and rmse for each field for each date
#July 19
up1yld_vct <- values(up_1_yield)
observed <- na.omit(up1yld_vct)
up_yield <- as.data.frame(observed)
up_yield <- mutate(up_yield, simYield = 9753.229)

mid1yld_vct <- values(mid_1_yield)
observed <- na.omit(mid1yld_vct)
mid_yield <- as.data.frame(observed)
mid_yield <- mutate(mid_yield, simYield = 9456.291)

dwn1yld_vct <- values(dwn_1_yield)
observed <- na.omit(dwn1yld_vct)
dwn_yield <- as.data.frame(observed)
dwn_yield <- mutate(dwn_yield, simYield = 9775.593)
rmse(up_yield$simYield, up_yield$observed)
rmse(mid_yield$simYield, mid_yield$observed)
rmse(dwn_yield$simYield, dwn_yield$observed)

yield_1 <- rbind(up_yield, mid_yield)
yield_1 <- rbind(yield_1, dwn_yield)

rmse(yield_1$simYield,yield_1$observed)
cor(yield_1)

#Aug 02
up2yld_vct <- values(up_2_yield)
observed <- na.omit(up2yld_vct)
up_yield <- as.data.frame(observed)
up_yield <- mutate(up_yield, simYield = 9753.229)

mid2yld_vct <- values(mid_2_yield)
observed <- na.omit(mid2yld_vct)
mid_yield <- as.data.frame(observed)
mid_yield <- mutate(mid_yield, simYield = 9456.291)

dwn2yld_vct <- values(dwn_2_yield)
observed <- na.omit(dwn2yld_vct)
dwn_yield <- as.data.frame(observed)
dwn_yield <- mutate(dwn_yield, simYield = 9775.593)
rmse(up_yield$simYield, up_yield$observed)
rmse(mid_yield$simYield, mid_yield$observed)
rmse(dwn_yield$simYield, dwn_yield$observed)

yield_2 <- rbind(up_yield, mid_yield)
yield_2 <- rbind(yield_2, dwn_yield)

rmse(yield_2$simYield,yield_2$observed)
cor(yield_2)

#Aug 10
up3yld_vct <- values(up_3_yield)
observed <- na.omit(up3yld_vct)
up_yield <- as.data.frame(observed)
up_yield <- mutate(up_yield, simYield = 9753.229)


mid3yld_vct <- values(mid_3_yield)
observed <- na.omit(mid3yld_vct)
mid_yield <- as.data.frame(observed)
mid_yield <- mutate(mid_yield, simYield = 9456.291)

dwn3yld_vct <- values(dwn_3_yield)
observed <- na.omit(dwn3yld_vct)
dwn_yield <- as.data.frame(observed)
dwn_yield <- mutate(dwn_yield, simYield = 9775.593)
rmse(up_yield$simYield, up_yield$observed)
rmse(mid_yield$simYield, mid_yield$observed)
rmse(dwn_yield$simYield, dwn_yield$observed)

yield_3 <- rbind(up_yield, mid_yield)
yield_3 <- rbind(yield_3, dwn_yield)

rmse(yield_3$simYield,yield_3$observed)
cor(yield_3)

#Aug 24
up4yld_vct <- values(up_4_yield)
observed <- na.omit(up4yld_vct)
up_yield <- as.data.frame(observed)
up_yield <- mutate(up_yield, simYield = 9753.229)


mid4yld_vct <- values(mid_4_yield)
observed <- na.omit(mid4yld_vct)
mid_yield <- as.data.frame(observed)
mid_yield <- mutate(mid_yield, simYield = 9456.291)

dwn4yld_vct <- values(dwn_4_yield)
observed <- na.omit(dwn4yld_vct)
dwn_yield <- as.data.frame(observed)
dwn_yield <- mutate(dwn_yield, simYield = 9775.593)
rmse(up_yield$simYield, up_yield$observed)
rmse(mid_yield$simYield, mid_yield$observed)
rmse(dwn_yield$simYield, dwn_yield$observed)

yield_4 <- rbind(up_yield, mid_yield)
yield_4 <- rbind(yield_4, dwn_yield)

rmse(yield_4$simYield,yield_4$observed)
cor(yield_4)
