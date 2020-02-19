
DOYs <- c(196:258)

DOYGCVIR2 <- data.frame()
DOYGCVICO <- data.frame()

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

DOYNDVIR2 <- data.frame()
DOYNDVICO <- data.frame()

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
install.packages("ape")
library(ape)
install.packages("spdep")
library(spdep)

#fieldNames <- c("up_1_ci1.tif","up_2_ci1.tif","up_3_ci1.tif","up_4_ci1.tif","mid_1_ci1.tif","mid_2_ci1.tif","mid_3_ci1.tif","mid_4_ci1.tif","dwn_1_ci1.tif","dwn_2_ci1.tif","dwn_3_ci1.tif","dwn_4_ci1.tif")
#dwn_4_ci1 <- raster("F:/HonorsResearch/Drone imagery processed/drone/drone/dwn_4_ci1.tif")

dwn_4_ci1 <- raster("E:/HonorsResearch/Drone imagery processed/drone/drone/dwn_4_ci1.tif")
dwn_4_CI_table <- as.data.frame(dwn_4_ci1, xy=TRUE, na.rm=TRUE)
coords <- matrix()
coords <- cbind(dwn_4_CI_table$x, dwn_4_CI_table$y)
ci_nb <- dnearneigh(coords, 0, 0.010, longlat=TRUE)
#calculate dnearestneighto get nb then nb2listw



# calculate k-nearest neigbors, translate to nb object, use nb2listw

#CI.dists <- as.matrix(dist(cbind(dwn_4_CI_table$x, dwn_4_CI_table$y)))


fs <- dir("F:/HonorsResearch/Drone imagery processed/drone/drone", pattern = "tif$", full.names = TRUE)
fieldsCI <- lapply(fs, raster)

lapply(fieldsCI, FUN(x){
  temp <- as.vector(x)
  
})
       
#for (var in fieldNames){
#  var <- raster("F:/HonorsResearch/Drone imagery processed/drone/drone/var")
#}

Moran(up_3_ci1, w=matrix(c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1), 5,5))

fieldsCI <- as.list(up_1_ci1, up_2_ci1, up_3_ci1, up_4_ci1, mid_1_ci1, mid_2_ci1, mid_3_ci1, mid_4_ci1, dwn_1_ci1, dwn_2_ci1, dwn_3_ci1, dwn_4_ci1)

MoranCI <- lapply(fieldsCI, function(x) Moran(x, w=matrix(c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1), 5,5)))
View(MoranCI)

yield_predictors <- read.csv(file = "F:/HonorsResearch/yield_predictors.csv")
yp2018 <- yield_predictors %>% subset(year==2018)

yield2018 <- unique(yp2018$HWAH)
mean(yield2018, trim=0, na.rm=TRUE)
#Average yield is 9739.737

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

#weather
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

#per pixel yield rasters
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

#Moran's I for yield values
fieldsYield <- as.list(up_1_yield, up_2_yield, up_3_yield, up_4_yield, mid_1_yield, mid_2_yield, mid_3_yield, mid_4_yield, dwn_1_yield, dwn_2_yield, dwn_3_yield, dwn_4_yield)
MoranYield <- lapply(fieldsYield, function(x) Moran(x, w=matrix(c(1,1,1,1,1,1,1,0,1,1,1,1,1,1,1), 5,5)))
View(MoranYield)

#correlation and rmse
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
