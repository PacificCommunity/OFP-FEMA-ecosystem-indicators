######################################################################
## Length and fish condition indicators for SC17 Indicators paper
######################################################################

## Created on: 31 May 2021, 
## Latest update: 3 June 2021
## Created by: Jed Macdonald

## Extract length (L) and weight (W) data from LL caught YFT and BET and 
## develop indicators for use in the 'Trophic' ecosystem indicators Report Card.

library(RODBC) 

## Subset function
substrLeft <- function(x, n){
  substr(x, 1, n)
}

substrRight<- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("P:/OFPEMA/WCPFC/SC17/Indicators/Fish condition")

################################################
## Extract all L & W records for LL catch
################################################

## From BioDaSys 
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=BioDaSys; Trusted_Connection=yes;") 

bio_data <- sqlQuery(myConn," 
SELECT      BIO.sample_base.sample_base_id, BIO.trip.country_code_id, REF.trip_type.trip_type_id, BIO.trip.gear_id,
            REF.species.asfis_code, BIO.fish.sampling_date, BIO.set_base.latitude_dec, BIO.set_base.longitude_dec,
            BIO.fish.length_mm, BIO.fish.length_code_id, BIO.fish.weight_gr, BIO.fish.weight_code_id
FROM        BIO.trip INNER JOIN
            BIO.set_base ON BIO.trip.trip_id = BIO.set_base.trip_id INNER JOIN
            BIO.fish ON BIO.set_base.set_base_id = BIO.fish.set_base_id INNER JOIN
				    REF.trip_type ON BIO.trip.trip_type_id = REF.trip_type.trip_type_id INNER JOIN
				    BIO.sample_base ON BIO.fish.fish_id = BIO.sample_base.fish_id INNER JOIN
				    REF.species ON BIO.fish.species_id = REF.species.species_id
WHERE       ((BIO.fish.species_id)=1 Or (BIO.fish.species_id)=2 Or (BIO.fish.species_id)=3)", max=0, stringsAsFactors=FALSE)

close(myConn)

## From OBSV_MASTER
myConn2 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=OBSV_MASTER; Trusted_Connection=yes;")

obsv_data <- sqlQuery(myConn2, " 
SELECT      obsv.l_setcatch.catch_date, obsv.l_setcatch.catch_time, obsv.l_setcatch.sp_code, obsv.l_setcatch.len,
            obsv.l_setcatch.len_code, obsv.l_setcatch.wt, obsv.l_setcatch.wt_code, obsv.l_sethaullog.lond, obsv.l_sethaullog.latd
FROM        obsv.l_setcatch
INNER JOIN  obsv.l_sethaullog ON obsv.l_setcatch.l_set_id = obsv.l_sethaullog.l_set_id and stend_id=83
WHERE       ((obsv.l_setcatch.sp_code)='YFT' Or (obsv.l_setcatch.sp_code)='BET')", stringsAsFactors = FALSE)

close(myConn2)
odbcCloseAll()

## From 'FISH_MASTER_work' and 'tufman2' (previously extracted from BET L-W work in Feb. 2021)
port1_LW <- read.csv("LW_CF_Data_03.csv") # from 'FISH_MASTER_work' (note: port1_LW set was pre-filtered applying Peter W.'s criteria for outlier removal - see details under 'INDICATOR 2' below)
port2_LW <- read.csv("LW_CF_Data_04.csv") # from 'tufman2'


################################################
## Collate data for YFT, BET
################################################

## BioDaSys
bio_data$yy<-as.numeric(substrLeft(bio_data$sampling_date, 4)) # create year field
bio_data$weight_kg<-(bio_data$weight_gr)/1000 # convert to kg
bio_data$length_cm<-(bio_data$length_mm)/10 # convert to cm
bio_data<-bio_data[bio_data$gear_id=="L",] # get LL records only
bio_data<-bio_data[bio_data$yy%in%2000:2019,] # subset for 2000 to 2019 inclusive

# -- BET
bio_BET<-bio_data[bio_data$asfis_code=="BET",]
unique(bio_BET$length_code_id) # check length codes
unique(bio_BET$weight_code_id) # check weight codes
bio_BET$WW_kg<-ifelse(bio_BET$weight_code_id=="GG", 1.27496*(bio_BET$weight_kg+(runif(1,0,1)-0.5))^0.960613, bio_BET$weight_kg) # use P90 CFs for GG-WW, from Langley et al. 2006
bio_BET<-bio_BET[bio_BET$length_code_id=="UF",] # robust conversion factors to UF unavailable, so use only UF records
bio_BET_LW<-bio_BET[!is.na(bio_BET$length_cm) & !is.na(bio_BET$WW_kg),] # retain only complete L and W dataset

# Use bio_BET for Indicator 1, bio_BET_LW for Indicator 2

# -- YFT
bio_YFT<-bio_data[bio_data$asfis_code=="YFT",]
unique(bio_YFT$length_code_id) 
unique(bio_YFT$weight_code_id) 
bio_YFT$WW_kg<-ifelse(bio_YFT$weight_code_id=="GG", 1.1893*(bio_YFT$weight_kg+(runif(1,0,1)-0.5))^0.972, 
                      ifelse(bio_YFT$weight_code_id=="WW", bio_YFT$weight_kg, NA)) 
bio_YFT<-bio_YFT[bio_YFT$length_code_id=="UF",]
bio_YFT_LW<-bio_YFT[!is.na(bio_YFT$length_cm) & !is.na(bio_YFT$WW_kg),]

# Use bio_YFT for Indicator 1, bio_YFT_LW for Indicator 2

## OBSV_MASTER
str(obsv_data)
obsv_data$yy<-as.numeric(substrLeft(obsv_data$catch_date, 4)) # create year field
obsv_data<-obsv_data[obsv_data$yy%in%2000:2019,] # subset for 2000 to 2019 inclusive

# -- BET
obsv_BET<-obsv_data[obsv_data$sp_code=="BET",]
unique(obsv_BET$len_code) 
unique(obsv_BET$wt_code) 
obsv_BET$WW_kg<-ifelse(obsv_BET$wt_code=="GG", 1.27496*(obsv_BET$wt+(runif(1,0,1)-0.5))^0.960613, 
                     ifelse(obsv_BET$wt_code=="GT", 1.3264*(obsv_BET$wt+(runif(1,0,1)-0.5))^0.969,
                            ifelse(obsv_BET$wt_code=="GH", 1.25*obsv_BET$wt,
                                   ifelse(obsv_BET$wt_code=="GO", 1.06*obsv_BET$wt,
                                          ifelse(obsv_BET$wt_code=="GX", 1.25*obsv_BET$wt,
                                                 ifelse(obsv_BET$wt_code=="WW", obsv_BET$wt, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

obsv_BET<-obsv_BET[obsv_BET$len_code=="UF",]
obsv_BET$length_cm<-obsv_BET$len

obsv_BET_LW<-obsv_BET[!is.na(obsv_BET$length_cm) & !is.na(obsv_BET$WW_kg),] # remove NAs

# Use obsv_BET for Indicator 1, obsv_BET_LW for Indicator 2

# -- YFT
obsv_YFT<-obsv_data[obsv_data$sp_code=="YFT",]
unique(obsv_YFT$len_code) 
unique(obsv_YFT$wt_code) 
obsv_YFT$WW_kg<-ifelse(obsv_YFT$wt_code=="GG", 1.1893*(obsv_YFT$wt+(runif(1,0,1)-0.5))^0.972, 
                       ifelse(obsv_YFT$wt_code=="GT", 1.2988*(obsv_YFT$wt+(runif(1,0,1)-0.5))^0.968,
                              ifelse(obsv_YFT$wt_code=="GH", 1.22*obsv_YFT$wt,
                                     ifelse(obsv_YFT$wt_code=="GO", 1.06*obsv_YFT$wt,
                                            ifelse(obsv_YFT$wt_code=="GX", 1.23*obsv_YFT$wt,
                                                   ifelse(obsv_YFT$wt_code=="WW", obsv_YFT$wt, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

obsv_YFT<-obsv_YFT[obsv_YFT$len_code=="UF",]
obsv_YFT$length_cm<-obsv_YFT$len

obsv_YFT_LW<-obsv_YFT[!is.na(obsv_YFT$length_cm) & !is.na(obsv_YFT$WW_kg),] # remove NAs

# Use obsv_YFT for Indicator 1, obsv_YFT_LW for Indicator 2

## FISH_MASTER_work 

str(port1_LW)
colnames(port1_LW)[1:5]<-c("yy", "mm", "gear_code", "latd", "lond") # change colnames for port1_LW
colnames(port1_LW)[6]<-"ASTRAT" # change colnames for port1_LW
colnames(port1_LW)[7:12]<-c("sp_code", "len", "wt_kg", "wt_code", "wt_decimal", "source_cod") # change colnames for port1_LW
port1_LW<-port1_LW[port1_LW$yy%in%2000:2019,]

# -- BET
port1.BET_LW<-port1_LW[port1_LW$sp_code=="BET",]
port1.BET_LW$lat.d<-as.numeric(substrLeft(as.character(port1.BET_LW$latd), 2)) # clean up lat and lon fields
port1.BET_LW$lon.d<-as.numeric(substrLeft(as.character(port1.BET_LW$lond), 3))
port1.BET_LW$lat.d<-ifelse(grepl("S", port1.BET_LW$latd, fixed=T), (port1.BET_LW$lat.d)*-1, port1.BET_LW$lat.d)
port1.BET_LW$lon.d<-ifelse(grepl("W", port1.BET_LW$lond, fixed=T), 360-port1.BET_LW$lon.d, port1.BET_LW$lon.d)
port1.BET_LW$lat.d<-ifelse(port1.BET_LW$ASTRAT=="T", port1.BET_LW$lat.d+5,
                           ifelse(port1.BET_LW$ASTRAT=="0", port1.BET_LW$lat.d+5,
                                  ifelse(port1.BET_LW$ASTRAT=="5", port1.BET_LW$lat.d+2.5, NA))) # add correction to present data at grid centroid
port1.BET_LW$lon.d<-ifelse(port1.BET_LW$ASTRAT=="T", port1.BET_LW$lon.d+10,
                           ifelse(port1.BET_LW$ASTRAT=="0", port1.BET_LW$lon.d+5,
                                  ifelse(port1.BET_LW$ASTRAT=="5", port1.BET_LW$lon.d+2.5, NA)))
summary(port1.BET_LW$wt_code)
port1.BET_LW$WW_kg<-ifelse(port1.BET_LW$wt_code=="GG", 1.27496*(port1.BET_LW$wt_kg+(runif(1,0,1)-0.5))^0.960613, 
                           ifelse(port1.BET_LW$wt_code=="GT", 1.3264*(port1.BET_LW$wt_kg+(runif(1,0,1)-0.5))^0.969,
                                  ifelse(port1.BET_LW$wt_code=="GH", 1.25*port1.BET_LW$wt_kg,
                                         ifelse(port1.BET_LW$wt_code=="GO", 1.06*port1.BET_LW$wt_kg,
                                                ifelse(port1.BET_LW$wt_code=="GX", 1.25*port1.BET_LW$wt_kg,
                                                       ifelse(port1.BET_LW$wt_code=="WW", port1.BET_LW$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 
port1.BET_LW$length_cm<-port1.BET_LW$len
port1.BET_LW<-port1.BET_LW[!is.na(port1.BET_LW$WW_kg),] # remove NAs

# Use port1.BET_LW for Indicator 1 and Indicator 2

# -- YFT
port1.YFT_LW<-port1_LW[port1_LW$sp_code=="YFT",]
port1.YFT_LW$lat.d<-as.numeric(substrLeft(as.character(port1.YFT_LW$latd), 2)) # clean up lat and lon fields
port1.YFT_LW$lon.d<-as.numeric(substrLeft(as.character(port1.YFT_LW$lond), 3))
port1.YFT_LW$lat.d<-ifelse(grepl("S", port1.YFT_LW$latd, fixed=T), (port1.YFT_LW$lat.d)*-1, port1.YFT_LW$lat.d)
port1.YFT_LW$lon.d<-ifelse(grepl("W", port1.YFT_LW$lond, fixed=T), 360-port1.YFT_LW$lon.d, port1.YFT_LW$lon.d)
port1.YFT_LW$lat.d<-ifelse(port1.YFT_LW$ASTRAT=="T", port1.YFT_LW$lat.d+5,
                           ifelse(port1.YFT_LW$ASTRAT=="0", port1.YFT_LW$lat.d+5,
                                  ifelse(port1.YFT_LW$ASTRAT=="5", port1.YFT_LW$lat.d+2.5, NA))) # add correction to present data at grid centroid
port1.YFT_LW$lon.d<-ifelse(port1.YFT_LW$ASTRAT=="T", port1.YFT_LW$lon.d+10,
                           ifelse(port1.YFT_LW$ASTRAT=="0", port1.YFT_LW$lon.d+5,
                                  ifelse(port1.YFT_LW$ASTRAT=="5", port1.YFT_LW$lon.d+2.5, NA)))
summary(port1.YFT_LW$wt_code)
port1.YFT_LW$WW_kg<-ifelse(port1.YFT_LW$wt_code=="GG", 1.1893*(port1.YFT_LW$wt_kg+(runif(1,0,1)-0.5))^0.972, 
                           ifelse(port1.YFT_LW$wt_code=="GT", 1.2988*(port1.YFT_LW$wt_kg+(runif(1,0,1)-0.5))^0.968,
                                  ifelse(port1.YFT_LW$wt_code=="GH", 1.22*port1.YFT_LW$wt_kg,
                                         ifelse(port1.YFT_LW$wt_code=="GO", 1.06*port1.YFT_LW$wt_kg,
                                                ifelse(port1.YFT_LW$wt_code=="GX", 1.23*port1.YFT_LW$wt_kg,
                                                       ifelse(port1.YFT_LW$wt_code=="WW", port1.YFT_LW$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 
port1.YFT_LW$length_cm<-port1.YFT_LW$len
port1.YFT_LW<-port1.YFT_LW[!is.na(port1.YFT_LW$WW_kg),] # remove NAs

# Use port1.YFT_LW for Indicator 1 and Indicator 2

## tufman2
str(port2_LW)
colnames(port2_LW)<-colnames(port1_LW)[c(1:5,7:12)] # change colnames
port2_LW<-port2_LW[port2_LW$yy%in%2000:2019,]

# -- BET
port2.BET_LW<-port2_LW[port2_LW$sp_code=="BET",]
port2.BET_LW<-port2.BET_LW[port2.BET_LW$len<300 & port2.BET_LW$wt_kg<300,] # Remove clear outliers
port2.BET_LW$lat.d<-as.numeric(substrLeft(as.character(port2.BET_LW$latd), 2)) # make lat and lon numeric
port2.BET_LW$lon.d<-as.numeric(substrLeft(as.character(port2.BET_LW$lond), 3))
port2.BET_LW$lat.d<-ifelse(grepl("S", port2.BET_LW$latd, fixed=T), (port2.BET_LW$lat.d)*-1, port2.BET_LW$lat.d)
port2.BET_LW$lon.d<-ifelse(grepl("W", port2.BET_LW$lond, fixed=T), 360-port2.BET_LW$lon.d, port2.BET_LW$lon.d)
port2.BET_LW$lat.d<-port2.BET_LW$lat.d+2.5 # correction to present data at grid centroid
port2.BET_LW$lon.d<-port2.BET_LW$lon.d+2.5
summary(port2.BET_LW$wt_code)
port2.BET_LW$WW_kg<-ifelse(port2.BET_LW$wt_code=="GG", 1.27496*(port2.BET_LW$wt_kg+(runif(1,0,1)-0.5))^0.960613, # Note, we have 211 records for GF, but no conversion factor for WW and GF
                           ifelse(port2.BET_LW$wt_code=="GT", 1.3264*(port2.BET_LW$wt_kg+(runif(1,0,1)-0.5))^0.969,
                                  ifelse(port2.BET_LW$wt_code=="GH", 1.25*port2.BET_LW$wt_kg,
                                         ifelse(port2.BET_LW$wt_code=="GO", 1.06*port2.BET_LW$wt_kg,
                                                ifelse(port2.BET_LW$wt_code=="GX", 1.25*port2.BET_LW$wt_kg,
                                                       ifelse(port2.BET_LW$wt_code=="WW", port2.BET_LW$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 
port2.BET_LW$length_cm<-port2.BET_LW$len
port2.BET_LW<-port2.BET_LW[!is.na(port2.BET_LW$WW_kg),] # remove NAs
port2.BET_LW<-port2.BET_LW[!is.na(port2.BET_LW$lat.d) & !is.na(port2.BET_LW$lon.d),] # remove NAs

# Use port2.BET_LW for Indicator 1 and Indicator 2

# -- YFT
port2.YFT_LW<-port2_LW[port2_LW$sp_code=="YFT",]
port2.YFT_LW<-port2.YFT_LW[port2.YFT_LW$len<300 & port2.YFT_LW$wt_kg<300,] # Remove clear outliers
port2.YFT_LW$lat.d<-as.numeric(substrLeft(as.character(port2.YFT_LW$latd), 2)) # make lat and lon numeric
port2.YFT_LW$lon.d<-as.numeric(substrLeft(as.character(port2.YFT_LW$lond), 3))
port2.YFT_LW$lat.d<-ifelse(grepl("S", port2.YFT_LW$latd, fixed=T), (port2.YFT_LW$lat.d)*-1, port2.YFT_LW$lat.d)
port2.YFT_LW$lon.d<-ifelse(grepl("W", port2.YFT_LW$lond, fixed=T), 360-port2.YFT_LW$lon.d, port2.YFT_LW$lon.d)
port2.YFT_LW$lat.d<-port2.YFT_LW$lat.d+2.5 # correction to present data at grid centroid
port2.YFT_LW$lon.d<-port2.YFT_LW$lon.d+2.5
summary(port2.YFT_LW$wt_code)
port2.YFT_LW$WW_kg<-ifelse(port2.YFT_LW$wt_code=="GG", 1.1893*(port2.YFT_LW$wt_kg+(runif(1,0,1)-0.5))^0.972, 
                           ifelse(port2.YFT_LW$wt_code=="GT", 1.2988*(port2.YFT_LW$wt_kg+(runif(1,0,1)-0.5))^0.968,
                                  ifelse(port2.YFT_LW$wt_code=="GH", 1.22*port2.YFT_LW$wt_kg,
                                         ifelse(port2.YFT_LW$wt_code=="GO", 1.06*port2.YFT_LW$wt_kg,
                                                ifelse(port2.YFT_LW$wt_code=="GX", 1.23*port2.YFT_LW$wt_kg,
                                                       ifelse(port2.YFT_LW$wt_code=="WW", port2.YFT_LW$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

port2.YFT_LW$length_cm<-port2.YFT_LW$len
port2.YFT_LW<-port2.YFT_LW[!is.na(port2.YFT_LW$WW_kg),] # remove NAs
port2.YFT_LW<-port2.YFT_LW[!is.na(port2.YFT_LW$lat.d) & !is.na(port2.YFT_LW$lon.d),] # remove NAs

# Use port2.YFT_LW for Indicator 1 and Indicator 2


################################################
## INDICATOR 1: Annual Meam length 
################################################

# -- BET

# Summary plots
par(mfrow=c(2,2))
plot(bio_BET$length_cm~as.factor(bio_BET$yy))
plot(obsv_BET$length_cm~as.factor(obsv_BET$yy))
plot(port1.BET_LW$length_cm~as.factor(port1.BET_LW$yy))
plot(port2.BET_LW$length_cm~as.factor(port2.BET_LW$yy))

# Combine datsets
yy<-c(bio_BET$yy, obsv_BET$yy, port1.BET_LW$yy, port2.BET_LW$yy)
length_cm<-c(bio_BET$length_cm, obsv_BET$length_cm, port1.BET_LW$length_cm, port2.BET_LW$length_cm)
BET_length<-data.frame(yy,length_cm)

# Outlier checks
par(mfrow=c(1,1))
plot(BET_length$length_cm ~ as.factor(BET_length$yy))
BET_length<-BET_length[!BET_length$length_cm>250 & !BET_length$length_cm<10,] # remove clear outliers

# Re-boxplot, extract annual means and plot
plot(BET_length$length_cm ~ as.factor(BET_length$yy)) # replot
L.means_BET <- aggregate(length_cm ~ yy, BET_length, mean)
plot(L.means_BET$length_cm~L.means_BET$yy, type="l", col="blue", lwd=2, 
     ylim=c(100,150), las=2)

# -- YFT

# Summary plots
par(mfrow=c(2,2))
plot(bio_YFT$length_cm~as.factor(bio_YFT$yy))
plot(obsv_YFT$length_cm~as.factor(obsv_YFT$yy))
plot(port1.YFT_LW$length_cm~as.factor(port1.YFT_LW$yy))
plot(port2.YFT_LW$length_cm~as.factor(port2.YFT_LW$yy))

# Combine datsets
yy<-c(bio_YFT$yy, obsv_YFT$yy, port1.YFT_LW$yy, port2.YFT_LW$yy)
length_cm<-c(bio_YFT$length_cm, obsv_YFT$length_cm, port1.YFT_LW$length_cm, port2.YFT_LW$length_cm)
YFT_length<-data.frame(yy,length_cm)

# Outlier checks
par(mfrow=c(1,1))
plot(YFT_length$length_cm ~ as.factor(YFT_length$yy))
YFT_length<-YFT_length[!YFT_length$length_cm>250 & !YFT_length$length_cm<10,] # remove clear outliers

# Re-boxplot, extract annual means and plot
plot(YFT_length$length_cm ~ as.factor(YFT_length$yy)) # replot
L.means_YFT <- aggregate(length_cm ~ yy, YFT_length, mean)
plot(L.means_YFT$length_cm~L.means_YFT$yy, type="l", col="goldenrod1", lwd=2, 
     ylim=c(100,150), las=2)

# BET and YFT combined
plot(L.means_BET$length_cm~L.means_BET$yy, type="l", col="blue", lwd=2, 
     ylim=c(100,150), las=2)
lines(L.means_YFT$length_cm~L.means_YFT$yy, type="l", col="goldenrod1", lwd=2)


################################################
## INDICATOR 2: Annual condition index (K_rel) 
################################################

# -- BET

# Summary L-W plots
par(mfrow=c(2,2))
plot(bio_BET_LW$WW_kg~bio_BET_LW$length_cm)
plot(obsv_BET_LW$WW_kg~obsv_BET_LW$length_cm)
plot(port1.BET_LW$WW_kg~port1.BET_LW$length_cm)
plot(port2.BET_LW$WW_kg~port2.BET_LW$length_cm)

# Combine datsets
yy<-c(bio_BET_LW$yy, obsv_BET_LW$yy, port1.BET_LW$yy, port2.BET_LW$yy)
length_cm<-c(bio_BET_LW$length_cm, obsv_BET_LW$length_cm, port1.BET_LW$length_cm, port2.BET_LW$length_cm)
WW_kg<-c(bio_BET_LW$WW_kg, obsv_BET_LW$WW_kg, port1.BET_LW$WW_kg, port2.BET_LW$WW_kg)
BET_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
par(mfrow=c(1,1))
plot(BET_LW$WW_kg ~ BET_LW$length_cm)
BET_LW<-BET_LW[!BET_LW$WW_kg>500 & !BET_LW$WW_kg<1,] # remove clear outliers
BET_LW<-BET_LW[!BET_LW$length_cm>500 & !BET_LW$length_cm<10,] # remove clear outliers

# Replot
plot(BET_LW$WW_kg ~ BET_LW$length_cm)

# Fit a linear model
plot(log(BET_LW$WW_kg)~log(BET_LW$length_cm)) 
BET_model<-lm(log(BET_LW$WW_kg)~log(BET_LW$length_cm)) 
summary(BET_model)
para.all <- coef(BET_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(BET_LW$WW_kg~BET_LW$length_cm)
lines(sort(BET_LW$length_cm), para.all[1]*sort(BET_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Do further filtering steps - Following Peter W.'s approach for the 'port1.BET.LW' data, 
                             #  involves removing fish +/- 15 kg from predicted weight at length drawn from model.
BET_LW$pred.WW_kg<-para.all[1]*BET_LW$length_cm^para.all[2] 
BET_LW$WW_kg<-ifelse(abs(BET_LW$WW_kg-BET_LW$pred.WW_kg)>15,NA,BET_LW$WW_kg) 

# Refit linear model
plot(log(BET_LW$WW_kg)~log(BET_LW$length_cm)) 
BET_model<-lm(log(BET_LW$WW_kg)~log(BET_LW$length_cm)) 
summary(BET_model)
para.all <- coef(BET_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(BET_LW$WW_kg~BET_LW$length_cm)
lines(sort(BET_LW$length_cm), para.all[1]*sort(BET_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Get predictions
BET_LW$pred.WW_kg<-para.all[1]*BET_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
BET_LW$K_rel<-BET_LW$WW_kg/BET_LW$pred.WW_kg

# Extract annual means and plot
K_rel.means_BET <- aggregate(K_rel ~ yy, BET_LW, mean)
plot(K_rel.means_BET$K_rel~K_rel.means_BET$yy, type="l", col="blue", lwd=2, las=2)
abline(a=1, b=0, lty=2)

# -- YFT

# Summary L-W plots
par(mfrow=c(2,2))
plot(bio_YFT_LW$WW_kg~bio_YFT_LW$length_cm)
plot(obsv_YFT_LW$WW_kg~obsv_YFT_LW$length_cm)
plot(port1.YFT_LW$WW_kg~port1.YFT_LW$length_cm)
plot(port2.YFT_LW$WW_kg~port2.YFT_LW$length_cm)

# Combine datsets
yy<-c(bio_YFT_LW$yy, obsv_YFT_LW$yy, port1.YFT_LW$yy, port2.YFT_LW$yy)
length_cm<-c(bio_YFT_LW$length_cm, obsv_YFT_LW$length_cm, port1.YFT_LW$length_cm, port2.YFT_LW$length_cm)
WW_kg<-c(bio_YFT_LW$WW_kg, obsv_YFT_LW$WW_kg, port1.YFT_LW$WW_kg, port2.YFT_LW$WW_kg)
YFT_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
par(mfrow=c(1,1))
plot(YFT_LW$WW_kg ~ YFT_LW$length_cm)
YFT_LW<-YFT_LW[!YFT_LW$WW_kg>500 & !YFT_LW$WW_kg<1,] # remove clear outliers
YFT_LW<-YFT_LW[!YFT_LW$length_cm>500 & !YFT_LW$length_cm<10,] # remove clear outliers

# Replot
plot(YFT_LW$WW_kg ~ YFT_LW$length_cm)

# Fit a linear model
plot(log(YFT_LW$WW_kg)~log(YFT_LW$length_cm)) 
YFT_model<-lm(log(YFT_LW$WW_kg)~log(YFT_LW$length_cm)) 
summary(YFT_model)
para.all <- coef(YFT_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(YFT_LW$WW_kg~YFT_LW$length_cm)
lines(sort(YFT_LW$length_cm), para.all[1]*sort(YFT_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Do further filtering steps - Following Peter W.'s approach
YFT_LW$pred.WW_kg<-para.all[1]*YFT_LW$length_cm^para.all[2] 
YFT_LW$WW_kg<-ifelse(abs(YFT_LW$WW_kg-YFT_LW$pred.WW_kg)>15,NA,YFT_LW$WW_kg) 

# Refit linear model
plot(log(YFT_LW$WW_kg)~log(YFT_LW$length_cm)) 
YFT_model<-lm(log(YFT_LW$WW_kg)~log(YFT_LW$length_cm)) 
summary(YFT_model)
para.all <- coef(YFT_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(YFT_LW$WW_kg~YFT_LW$length_cm)
lines(sort(YFT_LW$length_cm), para.all[1]*sort(YFT_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Get predictions
YFT_LW$pred.WW_kg<-para.all[1]*YFT_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
YFT_LW$K_rel<-YFT_LW$WW_kg/YFT_LW$pred.WW_kg

# Extract annual means and plot
K_rel.means_YFT <- aggregate(K_rel ~ yy, YFT_LW, mean)
plot(K_rel.means_YFT$K_rel~K_rel.means_YFT$yy, type="l", col="goldenrod1", lwd=2, las=2)
abline(a=1, b=0, lty=2)

# BET and YFT combined
plot(K_rel.means_BET$K_rel~K_rel.means_BET$yy, type="l", col="blue", lwd=2, 
     ylim=c(0.93,1.07), las=2)
lines(K_rel.means_YFT$K_rel~K_rel.means_YFT$yy, type="l", col="goldenrod1", lwd=2)
abline(a=1, b=0, lty=2)

################################################
## Final plots and save data
################################################

# Plot INDICATOR 1 and 2 by species
par(mfrow=c(1,1))
plot(L.means_BET$length_cm~L.means_BET$yy, type="l", col="red", lwd=2, 
     ylim=c(105,135), las=2, xlab="Year", ylab="Mean fork length (cm) from LL catch",
     xaxt="n")
axis(side=1, at=seq(2000,2018,2), labels=(as.character(seq(2000, 2018, by=2))), las=2)
lines(L.means_YFT$length_cm~L.means_YFT$yy, type="l", col="goldenrod1", lwd=2)
legend(x=2015, y=134, legend=c("BET", "YFT"), col=c("red", "goldenrod1"), lwd=2,bty="n")

plot(K_rel.means_BET$K_rel~K_rel.means_BET$yy, type="l", col="red", lwd=2, 
     ylim=c(0.93,1.07), las=2, xlab="Year", ylab=expression(paste("Mean ", italic("K")[rel], " from LL catch")), 
     xaxt="n")
axis(side=1, at=seq(2000,2018,2), labels=(as.character(seq(2000, 2018, by=2))), las=2)
lines(K_rel.means_YFT$K_rel~K_rel.means_YFT$yy, type="l", col="goldenrod1", lwd=2)
abline(a=1, b=0, lty=2)
legend(x=2015, y=0.97, legend=c("BET", "YFT"), col=c("red", "goldenrod1"), lwd=2,bty="n")

# Save to .csv's
write.csv(L.means_BET, "L.means_BET.csv", row.names=F)
write.csv(L.means_YFT, "L.means_YFT.csv", row.names=F)
write.csv(K_rel.means_BET, "K_rel.means_BET.csv", row.names=F)
write.csv(K_rel.means_YFT, "K_rel.means_YFT.csv", row.names=F)


##########################################################
## Extract all L & W records for PS and LL catch for SKJ
##########################################################

## From BioDaSys 
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=BioDaSys; Trusted_Connection=yes;") 

bio_SKJ <- sqlQuery(myConn," 
SELECT      BIO.sample_base.sample_base_id, BIO.trip.country_code_id, REF.trip_type.trip_type_id, BIO.trip.gear_id,
            REF.species.asfis_code, BIO.fish.sampling_date, BIO.set_base.latitude_dec, BIO.set_base.longitude_dec,
            BIO.fish.length_mm, BIO.fish.length_code_id, BIO.fish.weight_gr, BIO.fish.weight_code_id
FROM        BIO.trip INNER JOIN
            BIO.set_base ON BIO.trip.trip_id = BIO.set_base.trip_id INNER JOIN
            BIO.fish ON BIO.set_base.set_base_id = BIO.fish.set_base_id INNER JOIN
				    REF.trip_type ON BIO.trip.trip_type_id = REF.trip_type.trip_type_id INNER JOIN
				    BIO.sample_base ON BIO.fish.fish_id = BIO.sample_base.fish_id INNER JOIN
				    REF.species ON BIO.fish.species_id = REF.species.species_id
WHERE       BIO.fish.species_id=4", max=0, stringsAsFactors=FALSE)

close(myConn)

## From OBSV_MASTER
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=OBSV_MASTER; Trusted_Connection=yes;") 

# Purse seine data
PS.obsv_SKJ <- sqlQuery(myConn,"
SELECT      d.act_date, act_time, utc_act_time, utc_act_date, latd, lond, len_code, sp_code, len
FROM        obsv.s_daylog d INNER JOIN 
            obsv.s_set s on s.s_daylog_id=d.s_daylog_id
            join obsv.s_lf lf on lf.s_set_id = s.s_set_id
            join obsv.s_lfmeas lfm ON lfm.s_lf_id = lf.s_lf_id
WHERE       (sp_code = 'SKJ')", max=0, stringsAsFactors=FALSE)

# Longline data
LL.obsv_SKJ <- sqlQuery(myConn, " 
SELECT      obsv.l_setcatch.catch_date, obsv.l_setcatch.catch_time, obsv.l_setcatch.sp_code, obsv.l_setcatch.len,
            obsv.l_setcatch.len_code, obsv.l_setcatch.wt, obsv.l_setcatch.wt_code, obsv.l_sethaullog.lond, obsv.l_sethaullog.latd
FROM        obsv.l_setcatch
INNER JOIN  obsv.l_sethaullog ON obsv.l_setcatch.l_set_id = obsv.l_sethaullog.l_set_id and stend_id=83
WHERE       obsv.l_setcatch.sp_code='SKJ'", max=0, stringsAsFactors = FALSE)

close(myConn)

## From Tufman2
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=tufman2; Trusted_Connection=yes;") 

port_SKJ <- sqlQuery(myConn,"
SELECT      ref.ports.port_name, ref.ports.country_code, port.samples.sample_date, port.samples.lon_from, port.samples.lat_from, port.samples.lon_to, port.samples.lat_to, port.samples.gear_code, port.sample_catch.len, port.sample_catch.sp_code, 
            port.sample_catch.len_code, port.sample_catch.sp_kg_code, port.sample_catch.sp_kg
FROM        ref.ports INNER JOIN
            port.samples ON ref.ports.port_id = port.samples.port_id INNER JOIN
            port.sample_catch ON port.samples.sample_id = port.sample_catch.sample_id
WHERE       (port.sample_catch.sp_code = N'SKJ')", max=0, stringsAsFactors=FALSE)

close(myConn)
odbcCloseAll()


################################################
## Collate data for SKJ
################################################

## BioDaSys
bio_SKJ$yy<-as.numeric(substrLeft(bio_SKJ$sampling_date, 4)) # create year field
bio_SKJ$length_cm<-(bio_SKJ$length_mm)/10 # convert to cm
bio_SKJ$weight_kg<-(bio_SKJ$weight_gr)/1000 # convert to kg
unique(bio_SKJ$length_code_id) # check length codes
unique(bio_SKJ$weight_code_id) # check weight codes
bio_SKJ$WW_kg<-bio_SKJ$weight_kg # add whole weight (WW) field for consistency with YFT and BET analysis
bio_SKJ<-bio_SKJ[bio_SKJ$length_code_id=="UF",] # robust conversion factors to UF unavailable, so use only UF records
bio_SKJ_LW<-bio_SKJ[!is.na(bio_SKJ$length_cm) & !is.na(bio_SKJ$WW_kg),] # retain only complete L and W dataset, all gears combined

# Split by gear: LL, PS, PL
PS.bio_SKJ<-bio_SKJ[bio_SKJ$gear_id=="S" & bio_SKJ$yy%in%2000:2019,] # get PS records only
PS.bio_SKJ_LW<-bio_SKJ_LW[bio_SKJ_LW$gear_id=="S" & bio_SKJ_LW$yy%in%2000:2019,]
LL.bio_SKJ<- bio_SKJ[bio_SKJ$gear_id=="L" & bio_SKJ$yy%in%2000:2019,] # get LL records only
LL.bio_SKJ_LW<-bio_SKJ_LW[bio_SKJ_LW$gear_id=="L" & bio_SKJ_LW$yy%in%2000:2019,] # no records
PL.bio_SKJ<-bio_SKJ[bio_SKJ$gear_id=="P" & bio_SKJ$yy%in%2000:2019,] # get PL records only
PL.bio_SKJ_LW<-bio_SKJ_LW[bio_SKJ_LW$gear_id=="P" & bio_SKJ_LW$yy%in%2000:2019,] # only 37 records

# Use bio_SKJ datasets for Indicator 1, bio_SKJ_LW datasets for Indicator 2


## OBSV_MASTER
# PS 
str(PS.obsv_SKJ)
PS.obsv_SKJ$yy<-as.numeric(substrLeft(PS.obsv_SKJ$utc_act_date, 4)) # create year field
PS.obsv_SKJ<-PS.obsv_SKJ[PS.obsv_SKJ$yy%in%2000:2019,] # subset for 2000 to 2019 inclusive
unique(PS.obsv_SKJ$len_code) 
PS.obsv_SKJ<-PS.obsv_SKJ[PS.obsv_SKJ$len_code=="UF",]
PS.obsv_SKJ$length_cm<-PS.obsv_SKJ$len

# Use PS.obsv_SKJ for Indicator 1, no weight data, so no calculation of K_rel possible 

## LL
str(LL.obsv_SKJ)
LL.obsv_SKJ$yy<-as.numeric(substrLeft(LL.obsv_SKJ$catch_date, 4)) # create year field
LL.obsv_SKJ<-LL.obsv_SKJ[LL.obsv_SKJ$yy%in%2000:2019,] # subset for 2000 to 2019 inclusive
unique(LL.obsv_SKJ$len_code)
unique(LL.obsv_SKJ$wt_code) 
LL.obsv_SKJ<-LL.obsv_SKJ[LL.obsv_SKJ$len_code=="UF",]
LL.obsv_SKJ$length_cm<-LL.obsv_SKJ$len
LL.obsv_SKJ$WW_kg<-ifelse(LL.obsv_SKJ$wt_code=="GG", 1.14*LL.obsv_SKJ$wt, 
                              ifelse(LL.obsv_SKJ$wt_code=="GH", 1.33*LL.obsv_SKJ$wt,
                                     ifelse(LL.obsv_SKJ$wt_code=="GX", 1.35*LL.obsv_SKJ$wt,
                                            ifelse(LL.obsv_SKJ$wt_code=="WW", LL.obsv_SKJ$wt, NA)))) # If not any of these known weight codes, set WW_kg to NA 

LL.obsv_SKJ_LW<-LL.obsv_SKJ[!is.na(LL.obsv_SKJ$length_cm) & !is.na(LL.obsv_SKJ$WW_kg),] # remove NAs

# Use LL.obsv_SKJ for Indicator 1, LL.obsv_SKJ_LW for Indicator 2


## tufman2
str(port_SKJ)
port_SKJ$yy<-as.numeric(substrLeft(port_SKJ$sample_date, 4)) # create year field
port_SKJ<-port_SKJ[port_SKJ$yy%in%2000:2019,] # subset for 2000 to 2019 inclusive
port_SKJ$lond<-as.numeric(substrLeft(as.character(port_SKJ$lon_from), 3)) # estimate lon and lat
port_SKJ$lond<-with(port_SKJ, ifelse(grepl("W", port_SKJ$lon_from, fixed=T)|port_SKJ$country_code=="CK", 360-port_SKJ$lond, port_SKJ$lond))
port_SKJ$latd<-as.numeric(substrLeft(as.character(port_SKJ$lat_from), 2))
port_SKJ$latd<-with(port_SKJ, ifelse(grepl("S", port_SKJ$lat_from, fixed=T)|port_SKJ$country_code=="CK", (port_SKJ$latd)*-1, port_SKJ$latd))

unique(port_SKJ$len_code)
unique(port_SKJ$sp_kg_code)
port_SKJ<-port_SKJ[port_SKJ$len_code=="UF",]
port_SKJ$length_cm<-port_SKJ$len
port_SKJ$WW_kg<-ifelse(port_SKJ$sp_kg_code=="GG", 1.14*port_SKJ$sp_kg, 
                          ifelse(port_SKJ$sp_kg_code=="GH", 1.33*port_SKJ$sp_kg,
                                 ifelse(port_SKJ$sp_kg_code=="GX", 1.35*port_SKJ$sp_kg,
                                        ifelse(port_SKJ$sp_kg_code=="GO", 1.09*port_SKJ$sp_kg,
                                               ifelse(port_SKJ$sp_kg_code=="WW", port_SKJ$sp_kg, NA))))) # If not any of these known weight codes, set WW_kg to NA 

port_SKJ_LW<-port_SKJ[!is.na(port_SKJ$length_cm) & !is.na(port_SKJ$WW_kg),] # remove NAs

# Split by gear: LL, PS, PL
PS.port_SKJ<-port_SKJ[port_SKJ$gear_code=="S",] # one PS record only!
PS.port_SKJ_LW<-port_SKJ_LW[port_SKJ_LW$gear_code=="S",]
LL.port_SKJ<- port_SKJ[port_SKJ$gear_code=="L",] # get LL records only
LL.port_SKJ_LW<-port_SKJ_LW[port_SKJ_LW$gear_code=="L",]

# Longline has >99% of all records for length and length & weight..
# Use LL.port_SKJ datset only for Indicator 1, LL.port_SKJ_LW dataset for Indicator 2


################################################
## INDICATOR 1: Annual Meam length 
################################################

# Calculate for PS (all set types), LL, PL 
# Present PS and LL only for SC 17 paper.

# -- SKJ - PS

# Summary plots 
par(mfrow=c(2,1))
plot(PS.bio_SKJ$length_cm~as.factor(PS.bio_SKJ$yy))
plot(PS.obsv_SKJ$length_cm~as.factor(PS.obsv_SKJ$yy))

# Combine datsets
yy<-c(PS.bio_SKJ$yy, PS.obsv_SKJ$yy)
length_cm<-c(PS.bio_SKJ$length_cm, PS.obsv_SKJ$length_cm)
PS.SKJ_length<-data.frame(yy,length_cm)

# Outlier checks
par(mfrow=c(1,1))
plot(PS.SKJ_length$length_cm ~ as.factor(PS.SKJ_length$yy))
PS.SKJ_length<-PS.SKJ_length[!PS.SKJ_length$length_cm>110 & !PS.SKJ_length$length_cm<10,] # remove clear outliers

# Re-boxplot, extract annual means and plot
plot(PS.SKJ_length$length_cm ~ as.factor(PS.SKJ_length$yy)) # replot
PS.means_SKJ <- aggregate(length_cm ~ yy, PS.SKJ_length, mean)
plot(PS.means_SKJ$length_cm~PS.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, 
     ylim=c(20,100), las=2)


# -- SKJ - LL

# Summary plots 
par(mfrow=c(3,1))
plot(LL.bio_SKJ$length_cm~as.factor(LL.bio_SKJ$yy))
plot(LL.obsv_SKJ$length_cm~as.factor(LL.obsv_SKJ$yy))
plot(LL.port_SKJ$length_cm~as.factor(LL.port_SKJ$yy))

# Combine datsets
yy<-c(LL.bio_SKJ$yy, LL.obsv_SKJ$yy, LL.port_SKJ$yy)
length_cm<-c(LL.bio_SKJ$length_cm, LL.obsv_SKJ$length_cm, LL.port_SKJ$length_cm)
LL.SKJ_length<-data.frame(yy,length_cm)

# Outlier checks
par(mfrow=c(1,1))
plot(LL.SKJ_length$length_cm ~ as.factor(LL.SKJ_length$yy))
LL.SKJ_length<-LL.SKJ_length[!LL.SKJ_length$length_cm>110 & !LL.SKJ_length$length_cm<10,] # remove clear outliers

# Re-boxplot, extract annual means and plot
plot(LL.SKJ_length$length_cm ~ as.factor(LL.SKJ_length$yy)) # replot
LL.means_SKJ <- aggregate(length_cm ~ yy, LL.SKJ_length, mean)
plot(LL.means_SKJ$length_cm~LL.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, 
     ylim=c(20,100), lty=2, las=2)


# -- SKJ - PL

# Summary plots 
par(mfrow=c(1,1))
plot(PL.bio_SKJ$length_cm~as.factor(PL.bio_SKJ$yy))

# Combine datsets
PL.SKJ_length<-PL.bio_SKJ

# Outlier checks
plot(PL.SKJ_length$length_cm ~ as.factor(PL.SKJ_length$yy))
PL.SKJ_length<-PL.SKJ_length[!PL.SKJ_length$length_cm>110 & !PL.SKJ_length$length_cm<10,] # remove clear outliers

# Re-boxplot, extract annual means and plot
plot(PL.SKJ_length$length_cm ~ as.factor(PL.SKJ_length$yy)) # replot
PL.means_SKJ <- aggregate(length_cm ~ yy, PL.SKJ_length, mean)
plot(PL.means_SKJ$length_cm~PL.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, 
      ylim=c(20,100), lty=3, las=2)


################################################
## INDICATOR 2: Annual condition index (K_rel) 
################################################

# Only 38 records in total for PS, so not useful for this analysis.
# Dedicated sampling plans are now underway to collect further 
# weight data on PS vessels across the WCPO.
# For PL only two years of data were avaiable (2009, 2019) in 'PL.bio_SKJ_LW',
# so not useful for this analysis.
# Present K_rel for LL only for SC17 paper.


# -- SKJ - LL

# Summary L-W plots
plot(LL.obsv_SKJ_LW$WW_kg~LL.obsv_SKJ_LW$length_cm)
plot(LL.port_SKJ_LW$WW_kg~LL.port_SKJ_LW$length_cm)

# Combine datsets
yy<-c(LL.obsv_SKJ_LW$yy, LL.port_SKJ_LW$yy)
length_cm<-c(LL.obsv_SKJ_LW$length_cm, LL.port_SKJ_LW$length_cm)
WW_kg<-c(LL.obsv_SKJ_LW$WW_kg, LL.port_SKJ_LW$WW_kg)
LL.SKJ_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
par(mfrow=c(1,1))
plot(LL.SKJ_LW$WW_kg ~ LL.SKJ_LW$length_cm)
LL.SKJ_LW<-LL.SKJ_LW[!LL.SKJ_LW$WW_kg>35 & !LL.SKJ_LW$WW_kg<0.01,] # remove clear outliers
LL.SKJ_LW<-LL.SKJ_LW[!LL.SKJ_LW$length_cm>110 & !LL.SKJ_LW$length_cm<10,] # remove clear outliers

# Replot
plot(LL.SKJ_LW$WW_kg ~ LL.SKJ_LW$length_cm)

# Fit a linear model
plot(log(LL.SKJ_LW$WW_kg)~log(LL.SKJ_LW$length_cm)) 
SKJ_model<-lm(log(LL.SKJ_LW$WW_kg)~log(LL.SKJ_LW$length_cm)) 
summary(SKJ_model)
para.all <- coef(SKJ_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(LL.SKJ_LW$WW_kg~LL.SKJ_LW$length_cm)
lines(sort(LL.SKJ_LW$length_cm), para.all[1]*sort(LL.SKJ_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Do further filtering steps - Following Peter W.'s approach
#  involves removing fish +/- 10 kg from predicted weight at length drawn from model.
LL.SKJ_LW$pred.WW_kg<-para.all[1]*LL.SKJ_LW$length_cm^para.all[2] 
LL.SKJ_LW$WW_kg<-ifelse(abs(LL.SKJ_LW$WW_kg-LL.SKJ_LW$pred.WW_kg)>10,NA,LL.SKJ_LW$WW_kg) 

# Refit linear model
plot(log(LL.SKJ_LW$WW_kg)~log(LL.SKJ_LW$length_cm)) 
SKJ_model<-lm(log(LL.SKJ_LW$WW_kg)~log(LL.SKJ_LW$length_cm)) 
summary(SKJ_model)
para.all <- coef(SKJ_model) 
para.all[1] <- exp(para.all[1])  
para.all 
plot(LL.SKJ_LW$WW_kg~LL.SKJ_LW$length_cm)
lines(sort(LL.SKJ_LW$length_cm), para.all[1]*sort(LL.SKJ_LW$length_cm)^para.all[2], col="red", lwd=2) 

# Get predictions
LL.SKJ_LW$pred.WW_kg<-para.all[1]*LL.SKJ_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
LL.SKJ_LW$K_rel<-LL.SKJ_LW$WW_kg/LL.SKJ_LW$pred.WW_kg

# Extract annual means and plot
LL.K_rel.means_SKJ <- aggregate(K_rel ~ yy, LL.SKJ_LW, mean)
plot(LL.K_rel.means_SKJ$K_rel~LL.K_rel.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, las=2)
abline(a=1, b=0, lty=2)


################################################
## Final plots and save data
################################################

# Plot INDICATOR 1 and 2 by species
par(mfrow=c(1,1))
plot(PS.means_SKJ$length_cm~PS.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, 
     ylim=c(20,100), las=2, xlab="", ylab="Mean SKJ fork length (cm)",
     xaxt="n")
axis(side=1, at=seq(2000,2018,2), labels=(as.character(seq(2000, 2018, by=2))), las=2)
lines(LL.means_SKJ$length_cm~LL.means_SKJ$yy, col="dodgerblue", lwd=2, lty=2)
lines(PL.means_SKJ$length_cm~PL.means_SKJ$yy, col="dodgerblue", lwd=2, lty=3)
legend(x=2015, y=105, legend=c("PS", "LL", "PL"), col="dodgerblue", lwd=2, lty=c(1,2,3), bty="n")

plot(LL.K_rel.means_SKJ$K_rel~LL.K_rel.means_SKJ$yy, type="l", col="dodgerblue", lwd=2, 
     ylim=c(0.80,1.30), las=2, xlab="", ylab=expression(paste("Mean SKJ ", italic("K")[rel], " from LL catch")), 
     xaxt="n")
axis(side=1, at=seq(2000,2018,2), labels=(as.character(seq(2000, 2018, by=2))), las=2)
abline(a=1, b=0, lty=2)

# Save to .csv's
write.csv(PS.means_SKJ, "PS.means_SKJ.csv", row.names=F)
write.csv(LL.means_SKJ, "LL.means_SKJ.csv", row.names=F)
write.csv(PL.means_SKJ, "PL.means_SKJ.csv", row.names=F)
write.csv(LL.K_rel.means_SKJ, "LL.K_rel.means_SKJ.csv", row.names=F)

SKJ_condition<-list(PS.means_SKJ, LL.means_SKJ, PL.means_SKJ, LL.K_rel.means_SKJ)
names(SKJ_condition)<-c("PS.length.means", "LL.length.means", "PL.length.means", "LL.K_rel.means")
save(SKJ_condition, file="SKJ_condition.Rdata")

## END ##