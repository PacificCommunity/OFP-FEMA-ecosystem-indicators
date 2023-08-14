######################################################################
## Target Species Condition indicators - for SC19 paper 2023
######################################################################

## Created on: 07/07/23
## Latest update: 11/07/23
## Created by: Jed Macdonald

## This code extracts all available length (L) and weight (W) data for YFT, BET and SKJ from SPC's databases.
## This data is then used to calculate two indicators that sit within the 'Target Species Condition'
## section of the Report Cards. These are:

## Indicator 1: Mean relative condition factor (K_rel) from LL catch
## Indicator 2: Mean length across all gears

## Key changes from 2021: 
## 1) Addition of new data from 1990 to 2022 inclusive.
## 2) Use new SQL queries for L-W data adapted from the 2023 BET & YFT L-W work (in 'P:\OFPEMA\Project 90\Data\2023 data\BET.YFT_assessment_LW), with SKJ data added. 
## 3) Use new SQL queries for length data adapted from the SKJ length frquency/warm pool size analysis (in 'P:\OFPEMA\WCPFC\SC19\Ecosystem indicators\SKJ_lengthfreq').
## 2) Begin SKJ K_rel time series at 1998, and then continue from 2010 to 2022, as data are few in other years.

library(RODBC) 
library(grid)
library(reshape2)
library(tools)
library(stringi)
library(tidyverse)
library(ggplot2)
library(gdata)
library(magrittr)
library(sp)
library(maps)
library(mapdata)
library(RGeostats)


## Simple subset function
substrLeft <- function(x, n){
  substr(x, 1, n)
}

substrRight<- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem Indicators/Fish condition")

#######################################################
## Indicator 1: Relative condition factor (K_rel)
#######################################################

################################################
## Data extractions
################################################

## BioDaSys data (housed in BioDaSys)
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=BioDaSys; Trusted_Connection=yes;") 
bio_LW<- sqlQuery(myConn,"
SELECT  YEAR(SAMPLING_DATE) AS YY, 
        MONTH(SAMPLING_DATE) AS MM,
        'L' AS GEAR_CODE, 
        latitude_dec as latd, 
        longitude_dec as lond, 
        [asfis_code] as sp_code,
        [length_mm]/10.0 as len,
        [weight_gr]/1000.000 as wt_kg,
        [weight_code_id] as wt_code,
        case when weight_gr/1000.000 <> floor(weight_gr/1000.000) then 'Y' else 'N' end as wt_decimal,
        '4' AS SOURCE_code 
FROM    [BioDaSys].[BIO].[fish] f 
		    left outer join [BioDaSys].[REF].[species] sp on f.species_id = sp.species_id
		    left outer join [BioDaSys].[BIO].[set_base] sb on f.set_base_id = sb.set_base_id
WHERE   coalesce(length_mm,0) > 0 
        and coalesce(weight_gr,0) > 0 
        and f.length_code_id = 'UF' 
        and weight_code_id in ('WW', 'GG', 'GO') 
        and asfis_code in ('YFT','BET', 'SKJ')", max=0, stringsAsFactors=FALSE)
close(myConn)

## Observer data (housed in OBSV_MASTER)
myConn2 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=OBSV_MASTER; Trusted_Connection=yes;")
obsv_LW <- sqlQuery(myConn2, "
SELECT 	year(set_date) as YY,
		    month(set_date) as MM,
		    'L' AS GEAR_CODE, 
        latd,
        lond, 
        [sp_code],
        [len_us] as len,	--  should be len_uf when the LEN4 database table has been fixed .. len_uf to len_us ... 
        [wt_whole] as wt_kg,
        'WW' as wt_code,
	      case when wt_whole <> floor(wt_whole) then 'Y' else 'N' end as wt_decimal,
	      '2' AS SOURCE_code 	  
FROM	  obsv.trip ot 
			  inner join	obsv.l_set ls on ot.obstrip_id = ls.obstrip_id 
			  inner join	obsv.l_gen4 l4 on ls.l_set_id = l4.l_set_id
			  left outer join	obsv.l_sethaullog lsh on ls.l_set_id = lsh.l_set_id and stend_id = 83
WHERE   len_us > 0 and wt_whole > 0 
        and sp_code in ('YFT','BET', 'SKJ')
        order by 1,2,4,5,7", max=0, stringsAsFactors = FALSE)
close(myConn2)

## Port sampling data from prior to 2015 (housed in FISH_MASTER_work)
myConn3 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=FISH_MASTER_work; Trusted_Connection=yes;") 
port1_LW<-sqlQuery(myConn3, "
SELECT  YEAR(case when TRIP_DATE is null then DATEADD(day,-7,T.START_DATE) else T.TRIP_DATE end) AS YY, 
		MONTH(case when TRIP_DATE is null then DATEADD(day,-7,T.START_DATE) else T.TRIP_DATE end) AS MM, 
		'L' AS GEAR_CODE, 
		case when not( T.LAT5X5 is null		or ltrim(rtrim(T.LAT5X5))='0'	or ltrim(rtrim(T.LON5X5))='000E')		then upper(T.LAT5X5)
			 when not (T.LAT10X20 is null	or ltrim(rtrim(T.LAT10X20))='0'	or ltrim(rtrim(T.LON10X20))='000E')		then upper(T.LAT10X20)	else FISH_MASTER_agg.REF.GET_LAT_FROM_AREA_CODE(CO_ID) end as lat_short, 
		case when not( T.LON5X5 is null		or ltrim(rtrim(T.LON5X5))='0'	or ltrim(rtrim(T.LON5X5))='000E')		then upper(T.LON5X5)
			 when not (T.LON10X20 is null	or ltrim(rtrim(T.LON10X20))='0'	or ltrim(rtrim(T.LON10X20))='000E')		then upper(T.LON10X20)	else FISH_MASTER_agg.REF.GET_LON_FROM_AREA_CODE(CO_ID) end as lon_short, 
		case when not( T.LON5X5 is null		or ltrim(rtrim(T.LON5X5))='0'	or ltrim(rtrim(T.LON5X5))='000E')		then '5'
			 when not (T.LON10X20 is null	or ltrim(rtrim(T.LON10X20))='0'	or ltrim(rtrim(T.LON10X20))='000E')		then 'T'	else FISH_MASTER_agg.REF.GET_astrat_FROM_AREA_CODE(CO_ID) end as astrat, 
		c.SP_ID AS sp_code, 
		len,
		wtkg as wt_kg,
		wt_id as wt_code,
		case when wtkg <> floor(wtkg) then 'Y' else 'N' end as wt_decimal,
		co_id AS SOURCE_code 
FROM port_hist.l_lf_h t 
		inner join port_hist.l_lf_d c on t.samp_id = c.samp_id
WHERE   c.sp_id in ('YFT','BET', 'SKJ') 
		AND len_id = 'UF'  
		AND LEN > 10 
		AND len < 300
		AND Q_Reject <> 'Y' 
		AND YEAR(start_date) < 2015 
		and coalesce(wtkg,0) > 0", max=0, stringsAsFactors=FALSE)
close(myConn3)

## Port sampling data from 2015 to 2023 (housed in FISH_MASTER_agg)
myConn4 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=FISH_MASTER_agg; Trusted_Connection=yes;") 
port2_LW<-sqlQuery(myConn4,"
SELECT  YEAR(SAMPLE_DATE) AS YY, 
		MONTH(SAMPLE_DATE) AS MM, 
		'L' AS GEAR_CODE, 
		left(ref.GetLatfromLatd(log.LatD5(ref.GetLatdfromLat(upper(lat_from)))-2.5),2) + upper(right(lat_from,1)) as lat_short, 
		left(ref.GetLonfromLond(log.LonD5(ref.GetLondfromLon(upper(lon_from)))-2.5),3) + upper(right(lon_from,1)) as lon_short, 
	  [sp_code],
      [len],
      [sp_kg] as wt_kg,
      [sp_kg_code] as wt_code,
	  case when sp_kg <> floor(sp_kg) then 'Y' else 'N' end as wt_decimal,
	 '4' AS SOURCE_code 
FROM [tufman2].[port].[samples] h
		inner join [tufman2].[port].[sample_catch] d on h.sample_id = d.sample_id
where  coalesce(len,0) > 0 
 and coalesce(sp_kg,0) > 0 
 and len_code = 'UF' 
 and sp_code in ('YFT','BET', 'SKJ')
 and YEAR(SAMPLE_DATE) >= 2015", max=0, stringsAsFactors=FALSE)
close(myConn4) 

## SFFAII port sampling data (housed in tufman2)
myConn5 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=tufman2; Trusted_Connection=yes;") 
SFFAII_LW<-sqlQuery(myConn5,"
SELECT 
		year(sample_date) as YY, 
		month(sample_date) as MM, 
		sp_code, 
		len_uf,
		len_us,
		len_lf,
		len_pf,
		len_ps,
		len_tl,
		sp_kg_processed,
		sp_kg_processed_code,
		sp_kg_landed,
		sp_kg_landed_code
FROM [port].[conversion_factor] c
		inner join [port].[conversion_factor_catch] cc on c.conversion_factor_id = cc.conversion_factor_id
order by 1,2", max=0, stringsAsFactors=FALSE)
close(myConn5)
odbcCloseAll()

# Read in WCPFC-CA boundary info
wcp_ca = read.csv("wcpfc_ca_stat_area.csv") 
wcp = Polygon(wcp_ca) # Convert to spatial polygon
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords = wcp@polygons[[1]]@Polygons[[1]]@coords 


################################################
## Filter bio_LW
################################################
str(bio_LW)
bio_LW<-bio_LW[bio_LW$GEAR_CODE=="L",] # get LL records only
bio_LW<-bio_LW[bio_LW$YY%in%1990:2022,] # subset for 1990 to 2023 inclusive
bio_LW$WCP_CA = point.in.polygon(bio_LW$lond, bio_LW$latd, coords[,1], coords[,2]) # Check that points are in WCPFC-CA
bio_LW %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) # retain records within WCPFC-CA only


# -- BET
set.seed(31500) # set seed to make analysis reproducible. Note that new seeds are set for each species within each dataset.
bio_LW_BET<-bio_LW[bio_LW$sp_code=="BET",]
summary(bio_LW_BET)

# Convert processed weights to whole weight (WW)
unique(bio_LW_BET$wt_code)
bio_LW_BET$WW_kg<-ifelse(bio_LW_BET$wt_code=="GG", 1.1616911*(bio_LW_BET$wt_kg+(runif(1,0,1)-0.5))^0.9817353, bio_LW_BET$wt_kg) # use GG:WW relationship derived from the SFFAII data collected between 2020 and 2023.
bio_LW_BET$WW_kg<-ifelse(bio_LW_BET$wt_code=="GO", 1.06*bio_LW_BET$wt_kg, bio_LW_BET$WW_kg) # use main P90 CF for GO:WW. Based on processed catch from the Hawaii longline fishery (see Table 2b Langley et al. 2006) 

# Remove (or convert) extreme outliers
bio_LW_BET$WW_kg<- ifelse(bio_LW_BET$WW_kg<0.02, 1000*bio_LW_BET$WW_kg, bio_LW_BET$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len<25 & bio_LW_BET$WW_kg>0.75),]
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len>=50 & bio_LW_BET$WW_kg<0.5),] 
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len>=100 & bio_LW_BET$WW_kg<7),] 
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len>=140 & bio_LW_BET$WW_kg<20),]
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len<150 & bio_LW_BET$WW_kg>95),]
bio_LW_BET<- bio_LW_BET[!(bio_LW_BET$len>180 & bio_LW_BET$WW_kg<40),]

# Plot L-W relationship
plot(y=bio_LW_BET$WW_kg, x=bio_LW_BET$len) 

# Fit a power expl (WW=a*UF^b)
bio.BET_expl<-lm(log(WW_kg)~log(len), data=bio_LW_BET) # fit expl

# Get mean predictions and add to dataset
bio.BET_expl_RSE <- summary(bio.BET_expl)$sigma # get residual standard error of expl
bio.BET_expl_preds_log<-predict(bio.BET_expl) # mean prediction on log scale
bio.BET_expl_preds_orig<-exp(bio.BET_expl_preds_log) # take exponent to put back on original scale
bio.BET_expl_preds_orig<- exp(bio.BET_expl_preds_log + bio.BET_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
bio_LW_BET$pred_WW_kg<-bio.BET_expl_preds_orig # add predictions to dataset
bio_LW_BET$pred_plus70<-1.7*bio_LW_BET$pred_WW_kg # add limit for 70% higher WW than predicted  
bio_LW_BET$pred_minus70<-0.3*bio_LW_BET$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=bio_LW_BET, col="firebrick4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="BET")
lines(x=sort(bio_LW_BET$len), y=sort(bio_LW_BET$pred_WW_kg), lwd=2)
lines(x=sort(bio_LW_BET$len), y=sort(bio_LW_BET$pred_plus70), lwd=2, lty=2)
lines(x=sort(bio_LW_BET$len), y=sort(bio_LW_BET$pred_minus70), lwd=2, lty=2)

bio_LW_BET$outlier70<-ifelse(bio_LW_BET$WW_kg < bio_LW_BET$pred_minus70 | bio_LW_BET$WW_kg > bio_LW_BET$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
bio_LW_outlier70<-bio_LW_BET[bio_LW_BET$outlier70=="Y",] # compile these records
bio_LW_BET<-bio_LW_BET[!bio_LW_BET$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(bio_LW_BET$WW_kg~bio_LW_BET$len)

# -- YFT
set.seed(47829)
bio_LW_YFT<-bio_LW[bio_LW$sp_code=="YFT",]
summary(bio_LW_YFT)

# Convert processed weights to whole weight (WW)
unique(bio_LW_YFT$wt_code)
bio_LW_YFT$WW_kg<-ifelse(bio_LW_YFT$wt_code=="GG", 1.1821032*(bio_LW_YFT$wt_kg+(runif(1,0,1)-0.5))^0.9754946, bio_LW_YFT$wt_kg) # use GG:WW relationship derived from the SFFAII data collected YFTween 2020 and 2023.

# Remove (or convert) extreme outliers
bio_LW_YFT$WW_kg<- ifelse(bio_LW_YFT$WW_kg<0.02, 1000*bio_LW_YFT$WW_kg, bio_LW_YFT$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len<25 & bio_LW_YFT$WW_kg>0.75),]
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len>=50 & bio_LW_YFT$WW_kg<0.5),] 
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len>=100 & bio_LW_YFT$WW_kg<7),] 
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len>=140 & bio_LW_YFT$WW_kg<20),]
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len<150 & bio_LW_YFT$WW_kg>95),]
bio_LW_YFT<- bio_LW_YFT[!(bio_LW_YFT$len>180 & bio_LW_YFT$WW_kg<40),]

# Plot L-W relationship
plot(y=bio_LW_YFT$WW_kg, x=bio_LW_YFT$len)

# Fit a power expl (WW=a*UF^b)
bio.YFT_expl<-lm(log(WW_kg)~log(len), data=bio_LW_YFT) # fit expl

# Get mean predictions and add to dataset
bio.YFT_expl_RSE <- summary(bio.YFT_expl)$sigma # get residual standard error of expl
bio.YFT_expl_preds_log<-predict(bio.YFT_expl) # mean prediction on log scale
bio.YFT_expl_preds_orig<-exp(bio.YFT_expl_preds_log) # take exponent to put back on original scale
bio.YFT_expl_preds_orig<- exp(bio.YFT_expl_preds_log + bio.YFT_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
bio_LW_YFT$pred_WW_kg<-bio.YFT_expl_preds_orig # add predictions to dataset
bio_LW_YFT$pred_plus70<-1.7*bio_LW_YFT$pred_WW_kg # add limit for 70% higher WW than predicted  
bio_LW_YFT$pred_minus70<-0.3*bio_LW_YFT$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=bio_LW_YFT, col="goldenrod", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="YFT")
lines(x=sort(bio_LW_YFT$len), y=sort(bio_LW_YFT$pred_WW_kg), lwd=2)
lines(x=sort(bio_LW_YFT$len), y=sort(bio_LW_YFT$pred_plus70), lwd=2, lty=2)
lines(x=sort(bio_LW_YFT$len), y=sort(bio_LW_YFT$pred_minus70), lwd=2, lty=2)

bio_LW_YFT$outlier70<-ifelse(bio_LW_YFT$WW_kg < bio_LW_YFT$pred_minus70 | bio_LW_YFT$WW_kg > bio_LW_YFT$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
bio_LW_outlier70<-bio_LW_YFT[bio_LW_YFT$outlier70=="Y",] # compile these records
bio_LW_YFT<-bio_LW_YFT[!bio_LW_YFT$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(bio_LW_YFT$WW_kg~bio_LW_YFT$len)


# -- SKJ
set.seed(25542)
bio_LW_SKJ<-bio_LW[bio_LW$sp_code=="SKJ",]
summary(bio_LW_SKJ)
unique(bio_LW_SKJ$wt_code) # All weights in WW so no conversions needed. 
bio_LW_SKJ$WW_kg<-bio_LW_SKJ$wt_kg # Make new variable for WW_kg to match other species.

# Remove (or convert) extreme outliers
bio_LW_SKJ$WW_kg<- ifelse(bio_LW_SKJ$WW_kg<0.02, 1000*bio_LW_SKJ$WW_kg, bio_LW_SKJ$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
bio_LW_SKJ<-bio_LW_SKJ[!(bio_LW_SKJ$WW_kg>30),]
bio_LW_SKJ<- bio_LW_SKJ[!(bio_LW_SKJ$len>30 & bio_LW_SKJ$WW_kg<0.1),]

# Plot L-W relationship
plot(y=bio_LW_SKJ$WW_kg, x=bio_LW_SKJ$len)

# Fit a power expl (WW=a*UF^b)
bio.SKJ_expl<-lm(log(WW_kg)~log(len), data=bio_LW_SKJ) # fit expl

# Get mean predictions and add to dataset
bio.SKJ_expl_RSE <- summary(bio.SKJ_expl)$sigma # get residual standard error of expl
bio.SKJ_expl_preds_log<-predict(bio.SKJ_expl) # mean prediction on log scale
bio.SKJ_expl_preds_orig<-exp(bio.SKJ_expl_preds_log) # take exponent to put back on original scale
bio.SKJ_expl_preds_orig<- exp(bio.SKJ_expl_preds_log + bio.SKJ_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
bio_LW_SKJ$pred_WW_kg<-bio.SKJ_expl_preds_orig # add predictions to dataset
bio_LW_SKJ$pred_plus70<-1.7*bio_LW_SKJ$pred_WW_kg # add limit for 70% higher WW than predicted  
bio_LW_SKJ$pred_minus70<-0.3*bio_LW_SKJ$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=bio_LW_SKJ, col="dodgerblue4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="SKJ")
lines(x=sort(bio_LW_SKJ$len), y=sort(bio_LW_SKJ$pred_WW_kg), lwd=2)
lines(x=sort(bio_LW_SKJ$len), y=sort(bio_LW_SKJ$pred_plus70), lwd=2, lty=2)
lines(x=sort(bio_LW_SKJ$len), y=sort(bio_LW_SKJ$pred_minus70), lwd=2, lty=2)

bio_LW_SKJ$outlier70<-ifelse(bio_LW_SKJ$WW_kg < bio_LW_SKJ$pred_minus70 | bio_LW_SKJ$WW_kg > bio_LW_SKJ$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
bio_LW_outlier70<-bio_LW_SKJ[bio_LW_SKJ$outlier70=="Y",] # compile these records
bio_LW_SKJ<-bio_LW_SKJ[!bio_LW_SKJ$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(bio_LW_SKJ$WW_kg~bio_LW_SKJ$len)



################################################
## Filter obsv_LW
################################################
str(obsv_LW)
obsv_LW<-obsv_LW[obsv_LW$GEAR_CODE=="L",] # get LL records only
obsv_LW<-obsv_LW[obsv_LW$YY%in%1990:2022,] # subset for 1990 to 2022 inclusive
obsv_LW$WCP_CA = point.in.polygon(obsv_LW$lond, obsv_LW$latd, coords[,1], coords[,2]) # Check that points are in WCPFC-CA
obsv_LW %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) # retain records within WCPFC-CA only


# -- BET
set.seed(38810) 
obsv_LW_BET<-obsv_LW[obsv_LW$sp_code=="BET",]
summary(obsv_LW_BET)
unique(obsv_LW_BET$wt_code) # All weights in WW so no conversions needed. 
obsv_LW_BET$WW_kg<-obsv_LW_BET$wt_kg # Make new variable for WW_kg to match other species.

# Remove (or convert) extreme outliers
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len<30 & obsv_LW_BET$WW_kg>0.75),]
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len>=50 & obsv_LW_BET$WW_kg<0.5),] 
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len>=100 & obsv_LW_BET$WW_kg<7),] 
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len>=140 & obsv_LW_BET$WW_kg<20),] 
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len<150 & obsv_LW_BET$WW_kg>95),]
obsv_LW_BET<- obsv_LW_BET[!(obsv_LW_BET$len>180 & obsv_LW_BET$WW_kg<40),]

# Plot L-W relationship
plot(obsv_LW_BET$WW_kg~obsv_LW_BET$len)

# Fit a power expl (WW=a*UF^b)
obsv.BET_expl<-lm(log(WW_kg)~log(len), data=obsv_LW_BET) # fit expl

# Get mean predictions and add to dataset
obsv.BET_expl_RSE <- summary(obsv.BET_expl)$sigma # get residual standard error of expl
obsv.BET_expl_preds_log<-predict(obsv.BET_expl) # mean prediction on log scale
obsv.BET_expl_preds_orig<-exp(obsv.BET_expl_preds_log) # take exponent to put back on original scale
obsv.BET_expl_preds_orig<- exp(obsv.BET_expl_preds_log + obsv.BET_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
obsv_LW_BET$pred_WW_kg<-obsv.BET_expl_preds_orig # add predictions to dataset
obsv_LW_BET$pred_plus70<-1.7*obsv_LW_BET$pred_WW_kg # add limit for 70% higher WW than predicted  
obsv_LW_BET$pred_minus70<-0.3*obsv_LW_BET$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=obsv_LW_BET, col="firebrick4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="BET")
lines(x=sort(obsv_LW_BET$len), y=sort(obsv_LW_BET$pred_WW_kg), lwd=2)
lines(x=sort(obsv_LW_BET$len), y=sort(obsv_LW_BET$pred_plus70), lwd=2, lty=2)
lines(x=sort(obsv_LW_BET$len), y=sort(obsv_LW_BET$pred_minus70), lwd=2, lty=2)

obsv_LW_BET$outlier70<-ifelse(obsv_LW_BET$WW_kg < obsv_LW_BET$pred_minus70 | obsv_LW_BET$WW_kg > obsv_LW_BET$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
obsv_LW_outlier70<-obsv_LW_BET[obsv_LW_BET$outlier70=="Y",] # compile these records
obsv_LW_BET<-obsv_LW_BET[!obsv_LW_BET$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(obsv_LW_BET$WW_kg~obsv_LW_BET$len)


# -- YFT
set.seed(29929) 
obsv_LW_YFT<-obsv_LW[obsv_LW$sp_code=="YFT",]
summary(obsv_LW_YFT)
unique(obsv_LW_YFT$wt_code) # All weights in WW so no conversions needed. 
obsv_LW_YFT$WW_kg<-obsv_LW_YFT$wt_kg # Make new variable for WW_kg to match other species.

# Remove (or convert) extreme outliers
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len<30 & obsv_LW_YFT$WW_kg>0.75),]
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len>=50 & obsv_LW_YFT$WW_kg<0.5),] 
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len>=100 & obsv_LW_YFT$WW_kg<7),] 
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len>=140 & obsv_LW_YFT$WW_kg<20),] 
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len<150 & obsv_LW_YFT$WW_kg>95),]
obsv_LW_YFT<- obsv_LW_YFT[!(obsv_LW_YFT$len>180 & obsv_LW_YFT$WW_kg<40),]

# Plot L-W relationship
plot(obsv_LW_YFT$WW_kg~obsv_LW_YFT$len)

# Fit a power expl (WW=a*UF^b)
obsv.YFT_expl<-lm(log(WW_kg)~log(len), data=obsv_LW_YFT) # fit expl

# Get mean predictions and add to dataset
obsv.YFT_expl_RSE <- summary(obsv.YFT_expl)$sigma # get residual standard error of expl
obsv.YFT_expl_preds_log<-predict(obsv.YFT_expl) # mean prediction on log scale
obsv.YFT_expl_preds_orig<-exp(obsv.YFT_expl_preds_log) # take exponent to put back on original scale
obsv.YFT_expl_preds_orig<- exp(obsv.YFT_expl_preds_log + obsv.YFT_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
obsv_LW_YFT$pred_WW_kg<-obsv.YFT_expl_preds_orig # add predictions to dataset
obsv_LW_YFT$pred_plus70<-1.7*obsv_LW_YFT$pred_WW_kg # add limit for 70% higher WW than predicted  
obsv_LW_YFT$pred_minus70<-0.3*obsv_LW_YFT$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=obsv_LW_YFT, col="goldenrod", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="YFT")
lines(x=sort(obsv_LW_YFT$len), y=sort(obsv_LW_YFT$pred_WW_kg), lwd=2)
lines(x=sort(obsv_LW_YFT$len), y=sort(obsv_LW_YFT$pred_plus70), lwd=2, lty=2)
lines(x=sort(obsv_LW_YFT$len), y=sort(obsv_LW_YFT$pred_minus70), lwd=2, lty=2)

obsv_LW_YFT$outlier70<-ifelse(obsv_LW_YFT$WW_kg < obsv_LW_YFT$pred_minus70 | obsv_LW_YFT$WW_kg > obsv_LW_YFT$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
obsv_LW_outlier70<-obsv_LW_YFT[obsv_LW_YFT$outlier70=="Y",] # compile these records
obsv_LW_YFT<-obsv_LW_YFT[!obsv_LW_YFT$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(obsv_LW_YFT$WW_kg~obsv_LW_YFT$len)


# -- SKJ
set.seed(19646)
obsv_LW_SKJ<-obsv_LW[obsv_LW$sp_code=="SKJ",]
summary(obsv_LW_SKJ)
unique(obsv_LW_SKJ$wt_code) # All weights in WW so no conversions needed. 
obsv_LW_SKJ$WW_kg<-obsv_LW_SKJ$wt_kg # Make new variable for WW_kg to match other species.

# Remove (or convert) extreme outliers
obsv_LW_SKJ$WW_kg<- ifelse(obsv_LW_SKJ$WW_kg<0.02, 1000*obsv_LW_SKJ$WW_kg, obsv_LW_SKJ$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
obsv_LW_SKJ<-obsv_LW_SKJ[!(obsv_LW_SKJ$WW_kg>30),]
obsv_LW_SKJ<- obsv_LW_SKJ[!(obsv_LW_SKJ$len>30 & obsv_LW_SKJ$WW_kg<0.1),]

# Plot L-W relationship
plot(obsv_LW_SKJ$WW_kg~obsv_LW_SKJ$len)

# Fit a power expl (WW=a*UF^b)
obsv.SKJ_expl<-lm(log(WW_kg)~log(len), data=obsv_LW_SKJ) # fit expl

# Get mean predictions and add to dataset
obsv.SKJ_expl_RSE <- summary(obsv.SKJ_expl)$sigma # get residual standard error of expl
obsv.SKJ_expl_preds_log<-predict(obsv.SKJ_expl) # mean prediction on log scale
obsv.SKJ_expl_preds_orig<-exp(obsv.SKJ_expl_preds_log) # take exponent to put back on original scale
obsv.SKJ_expl_preds_orig<- exp(obsv.SKJ_expl_preds_log + obsv.SKJ_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
obsv_LW_SKJ$pred_WW_kg<-obsv.SKJ_expl_preds_orig # add predictions to dataset
obsv_LW_SKJ$pred_plus70<-1.7*obsv_LW_SKJ$pred_WW_kg # add limit for 70% higher WW than predicted  
obsv_LW_SKJ$pred_minus70<-0.3*obsv_LW_SKJ$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=obsv_LW_SKJ, col="dodgerblue4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="SKJ")
lines(x=sort(obsv_LW_SKJ$len), y=sort(obsv_LW_SKJ$pred_WW_kg), lwd=2)
lines(x=sort(obsv_LW_SKJ$len), y=sort(obsv_LW_SKJ$pred_plus70), lwd=2, lty=2)
lines(x=sort(obsv_LW_SKJ$len), y=sort(obsv_LW_SKJ$pred_minus70), lwd=2, lty=2)

obsv_LW_SKJ$outlier70<-ifelse(obsv_LW_SKJ$WW_kg < obsv_LW_SKJ$pred_minus70 | obsv_LW_SKJ$WW_kg > obsv_LW_SKJ$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
obsv_LW_outlier70<-obsv_LW_SKJ[obsv_LW_SKJ$outlier70=="Y",] # compile these records
obsv_LW_SKJ<-obsv_LW_SKJ[!obsv_LW_SKJ$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(obsv_LW_SKJ$WW_kg~obsv_LW_SKJ$len)


################################################
## Filter port1_LW
################################################
str(port1_LW)
port1_LW<-port1_LW[port1_LW$GEAR_CODE=="L",] # get LL records only
port1_LW<-port1_LW[port1_LW$YY%in%1991:2014,] # dataset runs from 1991 to 2014, so this retains all data

# Clean up lat and lon fields
port1_LW$latd<-as.numeric(substrLeft(as.character(port1_LW$lat_short), 2))
port1_LW$lond<-as.numeric(substrLeft(as.character(port1_LW$lon_short), 3))
port1_LW$latd<-ifelse(grepl("S", port1_LW$lat_short, fixed=T), (port1_LW$latd)*-1, port1_LW$latd)
port1_LW$lond<-ifelse(grepl("W", port1_LW$lon_short, fixed=T), 360-port1_LW$lond, port1_LW$lond)
port1_LW$latd<-ifelse(port1_LW$astrat=="T", port1_LW$latd+5,
                      ifelse(port1_LW$astrat=="0", port1_LW$latd+5,
                             ifelse(port1_LW$astrat=="5", port1_LW$latd+2.5, NA))) # add correction to present data at grid centroid
port1_LW$lond<-ifelse(port1_LW$astrat=="T", port1_LW$lond+10,
                      ifelse(port1_LW$astrat=="0", port1_LW$lond+5,
                             ifelse(port1_LW$astrat=="5", port1_LW$lond+2.5, NA)))

port1_LW$WCP_CA = point.in.polygon(port1_LW$lond, port1_LW$latd, coords[,1], coords[,2]) # Check that points are in WCPFC-CA
port1_LW %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) # retain records within WCPFC-CA only


# -- BET
set.seed(27289)
port1_LW_BET<-port1_LW[port1_LW$sp_code=="BET",]

# Convert processed weights to whole weight (WW)
unique(port1_LW_BET$wt_code)
port1_LW_BET$WW_kg<-ifelse(port1_LW_BET$wt_code=="GG", 1.1616911*(port1_LW_BET$wt_kg+(runif(1,0,1)-0.5))^0.9817353, 
                           ifelse(port1_LW_BET$wt_code=="GT", 1.3264*(port1_LW_BET$wt_kg+(runif(1,0,1)-0.5))^0.969,
                                  ifelse(port1_LW_BET$wt_code=="GH", 1.25*port1_LW_BET$wt_kg,
                                         ifelse(port1_LW_BET$wt_code=="GO", 1.06*port1_LW_BET$wt_kg,
                                                ifelse(port1_LW_BET$wt_code=="GX", 1.25*port1_LW_BET$wt_kg,
                                                       ifelse(port1_LW_BET$wt_code=="WW", port1_LW_BET$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

port1_LW_BET<-port1_LW_BET[!is.na(port1_LW_BET$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port1_LW_BET$WW_kg~port1_LW_BET$len)

# Remove (or convert) extreme outliers
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len<30 & port1_LW_BET$WW_kg>0.75),]
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len>=50 & port1_LW_BET$WW_kg<0.5),] 
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len>=100 & port1_LW_BET$WW_kg<7),] 
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len>=140 & port1_LW_BET$WW_kg<20),] 
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len<150 & port1_LW_BET$WW_kg>125),]
port1_LW_BET<- port1_LW_BET[!(port1_LW_BET$len>180 & port1_LW_BET$WW_kg<40),]

# Replot L-W relationship
#plot(port1_LW_BET$WW_kg~port1_LW_BET$len)

# Fit a power expl (WW=a*UF^b)
port1.BET_expl<-lm(log(WW_kg)~log(len), data=port1_LW_BET) # fit expl

# Get mean predictions and add to dataset
port1.BET_expl_RSE <- summary(port1.BET_expl)$sigma # get residual standard error of expl
port1.BET_expl_preds_log<-predict(port1.BET_expl) # mean prediction on log scale
port1.BET_expl_preds_orig<-exp(port1.BET_expl_preds_log) # take exponent to put back on original scale
port1.BET_expl_preds_orig<- exp(port1.BET_expl_preds_log + port1.BET_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port1_LW_BET$pred_WW_kg<-port1.BET_expl_preds_orig # add predictions to dataset
port1_LW_BET$pred_plus70<-1.7*port1_LW_BET$pred_WW_kg # add limit for 70% higher WW than predicted  
port1_LW_BET$pred_minus70<-0.3*port1_LW_BET$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port1_LW_BET, col="firebrick4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="BET")
#lines(x=sort(port1_LW_BET$len), y=sort(port1_LW_BET$pred_WW_kg), lwd=2)
#lines(x=sort(port1_LW_BET$len), y=sort(port1_LW_BET$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port1_LW_BET$len), y=sort(port1_LW_BET$pred_minus70), lwd=2, lty=2)

port1_LW_BET$outlier70<-ifelse(port1_LW_BET$WW_kg < port1_LW_BET$pred_minus70 | port1_LW_BET$WW_kg > port1_LW_BET$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port1_LW_outlier70<-port1_LW_BET[port1_LW_BET$outlier70=="Y",] # compile these records
port1_LW_BET<-port1_LW_BET[!port1_LW_BET$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port1_LW_BET$WW_kg~port1_LW_BET$len)


# -- YFT
set.seed(45271)
port1_LW_YFT<-port1_LW[port1_LW$sp_code=="YFT",]

# Convert processed weights to whole weight (WW)
unique(port1_LW_YFT$wt_code)
port1_LW_YFT$WW_kg<-ifelse(port1_LW_YFT$wt_code=="GG", 1.1821032*(port1_LW_YFT$wt_kg+(runif(1,0,1)-0.5))^0.9754946,
                           ifelse(port1_LW_YFT$wt_code=="GT", 1.2988*(port1_LW_YFT$wt_kg+(runif(1,0,1)-0.5))^0.968,
                                  ifelse(port1_LW_YFT$wt_code=="GH", 1.22*port1_LW_YFT$wt_kg,
                                         ifelse(port1_LW_YFT$wt_code=="GO", 1.06*port1_LW_YFT$wt_kg,
                                                ifelse(port1_LW_YFT$wt_code=="GX", 1.23*port1_LW_YFT$wt_kg,
                                                       ifelse(port1_LW_YFT$wt_code=="WW", port1_LW_YFT$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

port1_LW_YFT<-port1_LW_YFT[!is.na(port1_LW_YFT$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port1_LW_YFT$WW_kg~port1_LW_YFT$len)

# Remove (or convert) extreme outliers
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len<30 & port1_LW_YFT$WW_kg>0.75),]
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len>=50 & port1_LW_YFT$WW_kg<0.5),] 
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len>=100 & port1_LW_YFT$WW_kg<7),] 
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len>=140 & port1_LW_YFT$WW_kg<20),] 
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len<150 & port1_LW_YFT$WW_kg>100),]
port1_LW_YFT<- port1_LW_YFT[!(port1_LW_YFT$len>180 & port1_LW_YFT$WW_kg<40),]

# Replot L-W relationship
#plot(port1_LW_YFT$WW_kg~port1_LW_YFT$len)

# Fit a power expl (WW=a*UF^b)
port1.YFT_expl<-lm(log(WW_kg)~log(len), data=port1_LW_YFT) # fit expl

# Get mean predictions and add to dataset
port1.YFT_expl_RSE <- summary(port1.YFT_expl)$sigma # get residual standard error of expl
port1.YFT_expl_preds_log<-predict(port1.YFT_expl) # mean prediction on log scale
port1.YFT_expl_preds_orig<-exp(port1.YFT_expl_preds_log) # take exponent to put back on original scale
port1.YFT_expl_preds_orig<- exp(port1.YFT_expl_preds_log + port1.YFT_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port1_LW_YFT$pred_WW_kg<-port1.YFT_expl_preds_orig # add predictions to dataset
port1_LW_YFT$pred_plus70<-1.7*port1_LW_YFT$pred_WW_kg # add limit for 70% higher WW than predicted  
port1_LW_YFT$pred_minus70<-0.3*port1_LW_YFT$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port1_LW_YFT, col="goldenrod", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="YFT")
#lines(x=sort(port1_LW_YFT$len), y=sort(port1_LW_YFT$pred_WW_kg), lwd=2)
#lines(x=sort(port1_LW_YFT$len), y=sort(port1_LW_YFT$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port1_LW_YFT$len), y=sort(port1_LW_YFT$pred_minus70), lwd=2, lty=2)

port1_LW_YFT$outlier70<-ifelse(port1_LW_YFT$WW_kg < port1_LW_YFT$pred_minus70 | port1_LW_YFT$WW_kg > port1_LW_YFT$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port1_LW_outlier70<-port1_LW_YFT[port1_LW_YFT$outlier70=="Y",] # compile these records
port1_LW_YFT<-port1_LW_YFT[!port1_LW_YFT$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port1_LW_YFT$WW_kg~port1_LW_YFT$len)


# -- SKJ
set.seed(42231)
port1_LW_SKJ<-port1_LW[port1_LW$sp_code=="SKJ",]

# Convert processed weights to whole weight (WW)
unique(port1_LW_SKJ$wt_code)
port1_LW_SKJ$WW_kg<-ifelse(port1_LW_SKJ$wt_code=="GG", 1.14*port1_LW_SKJ$wt_kg, # processed to WW taken from P90 conversion factor reference tables in Tufman2: https://www.spc.int/ofp/tufman2/data/SpeciesConversionFactor   
                           ifelse(port1_LW_SKJ$wt_code=="GX", 1.35*port1_LW_SKJ$wt_kg,
                                 ifelse(port1_LW_SKJ$wt_code=="WW", port1_LW_SKJ$wt_kg, NA))) # If not any of these known weight codes, set WW_kg to NA 

port1_LW_SKJ<-port1_LW_SKJ[!is.na(port1_LW_SKJ$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port1_LW_SKJ$WW_kg~port1_LW_SKJ$len)

# Remove (or convert) extreme outliers
port1_LW_SKJ$WW_kg<- ifelse(port1_LW_SKJ$WW_kg<0.02, 1000*port1_LW_SKJ$WW_kg, port1_LW_SKJ$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
port1_LW_SKJ<-port1_LW_SKJ[!(port1_LW_SKJ$WW_kg>30),]
port1_LW_SKJ<- port1_LW_SKJ[!(port1_LW_SKJ$len>30 & port1_LW_SKJ$WW_kg<0.1),]

# Replot L-W relationship
#plot(port1_LW_SKJ$WW_kg~port1_LW_SKJ$len)

# Fit a power expl (WW=a*UF^b)
port1.SKJ_expl<-lm(log(WW_kg)~log(len), data=port1_LW_SKJ) # fit expl

# Get mean predictions and add to dataset
port1.SKJ_expl_RSE <- summary(port1.SKJ_expl)$sigma # get residual standard error of expl
port1.SKJ_expl_preds_log<-predict(port1.SKJ_expl) # mean prediction on log scale
port1.SKJ_expl_preds_orig<-exp(port1.SKJ_expl_preds_log) # take exponent to put back on original scale
port1.SKJ_expl_preds_orig<- exp(port1.SKJ_expl_preds_log + port1.SKJ_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port1_LW_SKJ$pred_WW_kg<-port1.SKJ_expl_preds_orig # add predictions to dataset
port1_LW_SKJ$pred_plus70<-1.7*port1_LW_SKJ$pred_WW_kg # add limit for 70% higher WW than predicted  
port1_LW_SKJ$pred_minus70<-0.3*port1_LW_SKJ$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port1_LW_SKJ, col="dodgerblue4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="SKJ")
#lines(x=sort(port1_LW_SKJ$len), y=sort(port1_LW_SKJ$pred_WW_kg), lwd=2)
#lines(x=sort(port1_LW_SKJ$len), y=sort(port1_LW_SKJ$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port1_LW_SKJ$len), y=sort(port1_LW_SKJ$pred_minus70), lwd=2, lty=2)

port1_LW_SKJ$outlier70<-ifelse(port1_LW_SKJ$WW_kg < port1_LW_SKJ$pred_minus70 | port1_LW_SKJ$WW_kg > port1_LW_SKJ$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port1_LW_outlier70<-port1_LW_SKJ[port1_LW_SKJ$outlier70=="Y",] # compile these records
port1_LW_SKJ<-port1_LW_SKJ[!port1_LW_SKJ$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port1_LW_SKJ$WW_kg~port1_LW_SKJ$len)


################################################
## Filter port2_LW
################################################
str(port2_LW)
port2_LW<-port2_LW[port2_LW$GEAR_CODE=="L",] # get LL records only
port2_LW<-port2_LW[port2_LW$YY%in%2015:2022,] # subset for records up to and including 2022.

# Clean up lat and lon fields
port2_LW$latd<-as.numeric(substrLeft(as.character(port2_LW$lat_short), 2))
port2_LW$lond<-as.numeric(substrLeft(as.character(port2_LW$lon_short), 3))
port2_LW$latd<-ifelse(grepl("S", port2_LW$lat_short, fixed=T), (port2_LW$latd)*-1, port2_LW$latd)
port2_LW$lond<-ifelse(grepl("W", port2_LW$lon_short, fixed=T), 360-port2_LW$lond, port2_LW$lond)

port2_LW$WCP_CA = point.in.polygon(port2_LW$lond, port2_LW$latd, coords[,1], coords[,2]) # Check that points are in WCPFC-CA
port2_LW %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) # retain records within WCPFC-CA only


# -- BET
set.seed(76209)
port2_LW_BET<-port2_LW[port2_LW$sp_code=="BET",]

# Convert processed weights to whole weight (WW)
unique(port2_LW_BET$wt_code)
port2_LW_BET$WW_kg<-ifelse(port2_LW_BET$wt_code=="GG", 1.1616911*(port2_LW_BET$wt_kg+(runif(1,0,1)-0.5))^0.9817353, 
                           ifelse(port2_LW_BET$wt_code=="GT", 1.3264*(port2_LW_BET$wt_kg+(runif(1,0,1)-0.5))^0.969,
                                  ifelse(port2_LW_BET$wt_code=="GH", 1.25*port2_LW_BET$wt_kg,
                                         ifelse(port2_LW_BET$wt_code=="GO", 1.06*port2_LW_BET$wt_kg,
                                                ifelse(port2_LW_BET$wt_code=="GX", 1.25*port2_LW_BET$wt_kg,
                                                       ifelse(port2_LW_BET$wt_code=="WW", port2_LW_BET$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

port2_LW_BET<-port2_LW_BET[!is.na(port2_LW_BET$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port2_LW_BET$WW_kg~port2_LW_BET$len)

# Remove (or convert) extreme outliers
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len<30 & port2_LW_BET$WW_kg>0.75),]
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len>=50 & port2_LW_BET$WW_kg<0.5),] 
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len>=100 & port2_LW_BET$WW_kg<7),] 
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len>=140 & port2_LW_BET$WW_kg<20),] 
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len<150 & port2_LW_BET$WW_kg>125),]
port2_LW_BET<- port2_LW_BET[!(port2_LW_BET$len>180 & port2_LW_BET$WW_kg<40),]
port2_LW_BET<- port2_LW_BET[!port2_LW_BET$len>500,]

# Replot L-W relationship
#plot(port2_LW_BET$WW_kg~port2_LW_BET$len)

# Fit a power expl (WW=a*UF^b)
port2.BET_expl<-lm(log(WW_kg)~log(len), data=port2_LW_BET) # fit expl

# Get mean predictions and add to dataset
port2.BET_expl_RSE <- summary(port2.BET_expl)$sigma # get residual standard error of expl
port2.BET_expl_preds_log<-predict(port2.BET_expl) # mean prediction on log scale
port2.BET_expl_preds_orig<-exp(port2.BET_expl_preds_log) # take exponent to put back on original scale
port2.BET_expl_preds_orig<- exp(port2.BET_expl_preds_log + port2.BET_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port2_LW_BET$pred_WW_kg<-port2.BET_expl_preds_orig # add predictions to dataset
port2_LW_BET$pred_plus70<-1.7*port2_LW_BET$pred_WW_kg # add limit for 70% higher WW than predicted  
port2_LW_BET$pred_minus70<-0.3*port2_LW_BET$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port2_LW_BET, col="firebrick4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="BET")
#lines(x=sort(port2_LW_BET$len), y=sort(port2_LW_BET$pred_WW_kg), lwd=2)
#lines(x=sort(port2_LW_BET$len), y=sort(port2_LW_BET$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port2_LW_BET$len), y=sort(port2_LW_BET$pred_minus70), lwd=2, lty=2)

port2_LW_BET$outlier70<-ifelse(port2_LW_BET$WW_kg < port2_LW_BET$pred_minus70 | port2_LW_BET$WW_kg > port2_LW_BET$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port2_LW_outlier70<-port2_LW_BET[port2_LW_BET$outlier70=="Y",] # compile these records
port2_LW_BET<-port2_LW_BET[!port2_LW_BET$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port2_LW_BET$WW_kg~port2_LW_BET$len)


# -- YFT
set.seed(34987)
port2_LW_YFT<-port2_LW[port2_LW$sp_code=="YFT",]

# Convert processed weights to whole weight (WW)
unique(port2_LW_YFT$wt_code)
port2_LW_YFT$WW_kg<-ifelse(port2_LW_YFT$wt_code=="GG", 1.1821032*(port2_LW_YFT$wt_kg+(runif(1,0,1)-0.5))^0.9754946,
                           ifelse(port2_LW_YFT$wt_code=="GT", 1.2988*(port2_LW_YFT$wt_kg+(runif(1,0,1)-0.5))^0.968,
                                  ifelse(port2_LW_YFT$wt_code=="GH", 1.22*port2_LW_YFT$wt_kg,
                                         ifelse(port2_LW_YFT$wt_code=="GO", 1.06*port2_LW_YFT$wt_kg,
                                                ifelse(port2_LW_YFT$wt_code=="GX", 1.23*port2_LW_YFT$wt_kg,
                                                       ifelse(port2_LW_YFT$wt_code=="WW", port2_LW_YFT$wt_kg, NA)))))) # If not any of these known weight codes, set WW_kg to NA 

port2_LW_YFT<-port2_LW_YFT[!is.na(port2_LW_YFT$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port2_LW_YFT$WW_kg~port2_LW_YFT$len)

# Remove (or convert) extreme outliers
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len<30 & port2_LW_YFT$WW_kg>0.75),]
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len>=50 & port2_LW_YFT$WW_kg<0.5),] 
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len>=100 & port2_LW_YFT$WW_kg<7),] 
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len>=140 & port2_LW_YFT$WW_kg<20),] 
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len<150 & port2_LW_YFT$WW_kg>125),]
port2_LW_YFT<- port2_LW_YFT[!(port2_LW_YFT$len>180 & port2_LW_YFT$WW_kg<40),]
port2_LW_YFT<- port2_LW_YFT[!port2_LW_YFT$len>500,]

# Replot L-W relationship
#plot(port2_LW_YFT$WW_kg~port2_LW_YFT$len)

# Fit a power expl (WW=a*UF^b)
port2.YFT_expl<-lm(log(WW_kg)~log(len), data=port2_LW_YFT) # fit expl

# Get mean predictions and add to dataset
port2.YFT_expl_RSE <- summary(port2.YFT_expl)$sigma # get residual standard error of expl
port2.YFT_expl_preds_log<-predict(port2.YFT_expl) # mean prediction on log scale
port2.YFT_expl_preds_orig<-exp(port2.YFT_expl_preds_log) # take exponent to put back on original scale
port2.YFT_expl_preds_orig<- exp(port2.YFT_expl_preds_log + port2.YFT_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port2_LW_YFT$pred_WW_kg<-port2.YFT_expl_preds_orig # add predictions to dataset
port2_LW_YFT$pred_plus70<-1.7*port2_LW_YFT$pred_WW_kg # add limit for 70% higher WW than predicted  
port2_LW_YFT$pred_minus70<-0.3*port2_LW_YFT$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port2_LW_YFT, col="goldenrod", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="YFT")
#lines(x=sort(port2_LW_YFT$len), y=sort(port2_LW_YFT$pred_WW_kg), lwd=2)
#lines(x=sort(port2_LW_YFT$len), y=sort(port2_LW_YFT$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port2_LW_YFT$len), y=sort(port2_LW_YFT$pred_minus70), lwd=2, lty=2)

port2_LW_YFT$outlier70<-ifelse(port2_LW_YFT$WW_kg < port2_LW_YFT$pred_minus70 | port2_LW_YFT$WW_kg > port2_LW_YFT$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port2_LW_outlier70<-port2_LW_YFT[port2_LW_YFT$outlier70=="Y",] # compile these records
port2_LW_YFT<-port2_LW_YFT[!port2_LW_YFT$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port2_LW_YFT$WW_kg~port2_LW_YFT$len)


# -- SKJ
set.seed(35334)
port2_LW_SKJ<-port2_LW[port2_LW$sp_code=="SKJ",]

# Convert processed weights to whole weight (WW)
unique(port2_LW_SKJ$wt_code)
port2_LW_SKJ$WW_kg<-ifelse(port2_LW_SKJ$wt_code=="GG", 1.14*port2_LW_SKJ$wt_kg, # processed to WW taken from P90 conversion factor reference tables in Tufman2: https://www.spc.int/ofp/tufman2/data/SpeciesConversionFactor   
                           ifelse(port2_LW_SKJ$wt_code=="GH", 1.33*port2_LW_SKJ$wt_kg,
                                  ifelse(port2_LW_SKJ$wt_code=="GX", 1.35*port2_LW_SKJ$wt_kg,
                                         ifelse(port2_LW_SKJ$wt_code=="WW", port2_LW_SKJ$wt_kg, NA)))) # If not any of these known weight codes, set WW_kg to NA 

port2_LW_SKJ<-port2_LW_SKJ[!is.na(port2_LW_SKJ$WW_kg),] # remove NAs

# Plot L-W relationship
#plot(port2_LW_SKJ$WW_kg~port2_LW_SKJ$len)

# Remove (or convert) extreme outliers
port2_LW_SKJ$WW_kg<- ifelse(port2_LW_SKJ$WW_kg<0.02, 1000*port2_LW_SKJ$WW_kg, port2_LW_SKJ$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
port2_LW_SKJ<-port2_LW_SKJ[!(port2_LW_SKJ$WW_kg>30),]
port2_LW_SKJ<- port2_LW_SKJ[!(port2_LW_SKJ$len>30 & port2_LW_SKJ$WW_kg<0.1),]

# Replot L-W relationship
#plot(port2_LW_SKJ$WW_kg~port2_LW_SKJ$len)

# Fit a power expl (WW=a*UF^b)
port2.SKJ_expl<-lm(log(WW_kg)~log(len), data=port2_LW_SKJ) # fit expl

# Get mean predictions and add to dataset
port2.SKJ_expl_RSE <- summary(port2.SKJ_expl)$sigma # get residual standard error of expl
port2.SKJ_expl_preds_log<-predict(port2.SKJ_expl) # mean prediction on log scale
port2.SKJ_expl_preds_orig<-exp(port2.SKJ_expl_preds_log) # take exponent to put back on original scale
port2.SKJ_expl_preds_orig<- exp(port2.SKJ_expl_preds_log + port2.SKJ_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
port2_LW_SKJ$pred_WW_kg<-port2.SKJ_expl_preds_orig # add predictions to dataset
port2_LW_SKJ$pred_plus70<-1.7*port2_LW_SKJ$pred_WW_kg # add limit for 70% higher WW than predicted  
port2_LW_SKJ$pred_minus70<-0.3*port2_LW_SKJ$pred_WW_kg # add limit for 70% lower WW than predicted  

#plot(WW_kg~len, data=port2_LW_SKJ, col="dodgerblue4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
#title(main="SKJ")
#lines(x=sort(port2_LW_SKJ$len), y=sort(port2_LW_SKJ$pred_WW_kg), lwd=2)
#lines(x=sort(port2_LW_SKJ$len), y=sort(port2_LW_SKJ$pred_plus70), lwd=2, lty=2)
#lines(x=sort(port2_LW_SKJ$len), y=sort(port2_LW_SKJ$pred_minus70), lwd=2, lty=2)

port2_LW_SKJ$outlier70<-ifelse(port2_LW_SKJ$WW_kg < port2_LW_SKJ$pred_minus70 | port2_LW_SKJ$WW_kg > port2_LW_SKJ$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
port2_LW_outlier70<-port2_LW_SKJ[port2_LW_SKJ$outlier70=="Y",] # compile these records
port2_LW_SKJ<-port2_LW_SKJ[!port2_LW_SKJ$outlier70=="Y",] # remove these records

# Replot L-W relationship
#plot(port2_LW_SKJ$WW_kg~port2_LW_SKJ$len)


################################################
## Filter SFFAII_LW
################################################
str(SFFAII_LW)
summary(SFFAII_LW)
SFFAII_LW<-SFFAII_LW[SFFAII_LW$YY<2023,] # subset for records up to and including 2022.

# -- BET
set.seed(54221)
SFFAII_LW_BET<-SFFAII_LW[SFFAII_LW$sp_code=="BET",]
summary(SFFAII_LW_BET)

# Convert processed weights to whole weight (WW)
unique(SFFAII_LW_BET$sp_kg_landed_code)
SFFAII_LW_BET$WW_kg<-SFFAII_LW_BET$sp_kg_landed
SFFAII_LW_BET$len<-SFFAII_LW_BET$len_uf
SFFAII_LW_BET<-SFFAII_LW_BET[!is.na(SFFAII_LW_BET$WW_kg),] # remove NAs

# Plot L-W relationship
plot(SFFAII_LW_BET$WW_kg~SFFAII_LW_BET$len)

# Remove (or convert) extreme outliers
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len<30 & SFFAII_LW_BET$WW_kg>0.75),]
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len>=50 & SFFAII_LW_BET$WW_kg<0.5),] 
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len>=100 & SFFAII_LW_BET$WW_kg<7),] 
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len>=140 & SFFAII_LW_BET$WW_kg<20),] 
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len<150 & SFFAII_LW_BET$WW_kg>125),]
SFFAII_LW_BET<- SFFAII_LW_BET[!(SFFAII_LW_BET$len>180 & SFFAII_LW_BET$WW_kg<40),]
SFFAII_LW_BET<- SFFAII_LW_BET[!SFFAII_LW_BET$len>500,]

# Replot L-W relationship
plot(SFFAII_LW_BET$WW_kg~SFFAII_LW_BET$len)

# Fit a power expl (WW=a*UF^b)
SFFAII.BET_expl<-lm(log(WW_kg)~log(len), data=SFFAII_LW_BET) # fit expl

# Get mean predictions and add to dataset
SFFAII.BET_expl_RSE <- summary(SFFAII.BET_expl)$sigma # get residual standard error of expl
SFFAII.BET_expl_preds_log<-predict(SFFAII.BET_expl) # mean prediction on log scale
SFFAII.BET_expl_preds_orig<-exp(SFFAII.BET_expl_preds_log) # take exponent to put back on original scale
SFFAII.BET_expl_preds_orig<- exp(SFFAII.BET_expl_preds_log + SFFAII.BET_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
SFFAII_LW_BET$pred_WW_kg<-SFFAII.BET_expl_preds_orig # add predictions to dataset
SFFAII_LW_BET$pred_plus70<-1.7*SFFAII_LW_BET$pred_WW_kg # add limit for 70% higher WW than predicted  
SFFAII_LW_BET$pred_minus70<-0.3*SFFAII_LW_BET$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=SFFAII_LW_BET, col="firebrick4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="BET")
lines(x=sort(SFFAII_LW_BET$len), y=sort(SFFAII_LW_BET$pred_WW_kg), lwd=2)
lines(x=sort(SFFAII_LW_BET$len), y=sort(SFFAII_LW_BET$pred_plus70), lwd=2, lty=2)
lines(x=sort(SFFAII_LW_BET$len), y=sort(SFFAII_LW_BET$pred_minus70), lwd=2, lty=2)

SFFAII_LW_BET$outlier70<-ifelse(SFFAII_LW_BET$WW_kg < SFFAII_LW_BET$pred_minus70 | SFFAII_LW_BET$WW_kg > SFFAII_LW_BET$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
SFFAII_LW_outlier70<-SFFAII_LW_BET[SFFAII_LW_BET$outlier70=="Y",] # compile these records
SFFAII_LW_BET<-SFFAII_LW_BET[!SFFAII_LW_BET$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(SFFAII_LW_BET$WW_kg~SFFAII_LW_BET$len)


# -- YFT
set.seed(39787)
SFFAII_LW_YFT<-SFFAII_LW[SFFAII_LW$sp_code=="YFT",]
summary(SFFAII_LW_YFT)

# Convert processed weights to whole weight (WW)
unique(SFFAII_LW_YFT$sp_kg_landed_code)
SFFAII_LW_YFT$WW_kg<-SFFAII_LW_YFT$sp_kg_landed
SFFAII_LW_YFT$len<-SFFAII_LW_YFT$len_uf
SFFAII_LW_YFT<-SFFAII_LW_YFT[!is.na(SFFAII_LW_YFT$WW_kg),] # remove NAs

# Plot L-W relationship
plot(SFFAII_LW_YFT$WW_kg~SFFAII_LW_YFT$len)

# Remove (or convert) extreme outliers
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len<30 & SFFAII_LW_YFT$WW_kg>0.75),]
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len<50 & SFFAII_LW_YFT$WW_kg>5),]
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len<60 & SFFAII_LW_YFT$WW_kg>20),]
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len>=50 & SFFAII_LW_YFT$WW_kg<0.5),] 
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len>=100 & SFFAII_LW_YFT$WW_kg<7),] 
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len>=140 & SFFAII_LW_YFT$WW_kg<20),] 
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len<150 & SFFAII_LW_YFT$WW_kg>125),]
SFFAII_LW_YFT<- SFFAII_LW_YFT[!(SFFAII_LW_YFT$len>180 & SFFAII_LW_YFT$WW_kg<40),]

# Replot L-W relationship
plot(SFFAII_LW_YFT$WW_kg~SFFAII_LW_YFT$len) 

# Fit a power expl (WW=a*UF^b)
SFFAII.YFT_expl<-lm(log(WW_kg)~log(len), data=SFFAII_LW_YFT) # fit expl

# Get mean predictions and add to dataset
SFFAII.YFT_expl_RSE <- summary(SFFAII.YFT_expl)$sigma # get residual standard error of expl
SFFAII.YFT_expl_preds_log<-predict(SFFAII.YFT_expl) # mean prediction on log scale
SFFAII.YFT_expl_preds_orig<-exp(SFFAII.YFT_expl_preds_log) # take exponent to put back on original scale
SFFAII.YFT_expl_preds_orig<- exp(SFFAII.YFT_expl_preds_log + SFFAII.YFT_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
SFFAII_LW_YFT$pred_WW_kg<-SFFAII.YFT_expl_preds_orig # add predictions to dataset
SFFAII_LW_YFT$pred_plus70<-1.7*SFFAII_LW_YFT$pred_WW_kg # add limit for 70% higher WW than predicted  
SFFAII_LW_YFT$pred_minus70<-0.3*SFFAII_LW_YFT$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=SFFAII_LW_YFT, col="goldenrod", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="YFT")
lines(x=sort(SFFAII_LW_YFT$len), y=sort(SFFAII_LW_YFT$pred_WW_kg), lwd=2)
lines(x=sort(SFFAII_LW_YFT$len), y=sort(SFFAII_LW_YFT$pred_plus70), lwd=2, lty=2)
lines(x=sort(SFFAII_LW_YFT$len), y=sort(SFFAII_LW_YFT$pred_minus70), lwd=2, lty=2)

SFFAII_LW_YFT$outlier70<-ifelse(SFFAII_LW_YFT$WW_kg < SFFAII_LW_YFT$pred_minus70 | SFFAII_LW_YFT$WW_kg > SFFAII_LW_YFT$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
SFFAII_LW_outlier70<-SFFAII_LW_YFT[SFFAII_LW_YFT$outlier70=="Y",] # compile these records
SFFAII_LW_YFT<-SFFAII_LW_YFT[!SFFAII_LW_YFT$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(SFFAII_LW_YFT$WW_kg~SFFAII_LW_YFT$len)

# -- SKJ
set.seed(58934)
SFFAII_LW_SKJ<-SFFAII_LW[SFFAII_LW$sp_code=="SKJ",]
summary(SFFAII_LW_SKJ)

# Convert processed weights to whole weight (WW)
unique(SFFAII_LW_SKJ$sp_kg_landed_code)
SFFAII_LW_SKJ$WW_kg<-SFFAII_LW_SKJ$sp_kg_landed
SFFAII_LW_SKJ$len<-SFFAII_LW_SKJ$len_uf
SFFAII_LW_SKJ<-SFFAII_LW_SKJ[!is.na(SFFAII_LW_SKJ$WW_kg),] # remove NAs

# Plot L-W relationship
plot(SFFAII_LW_SKJ$WW_kg~SFFAII_LW_SKJ$len)

# Remove (or convert) extreme outliers
SFFAII_LW_SKJ$WW_kg<- ifelse(SFFAII_LW_SKJ$WW_kg<0.02, 1000*SFFAII_LW_SKJ$WW_kg, SFFAII_LW_SKJ$WW_kg) # no fish expected less than 20g WW (from 2020 analysis of Philippines' ringnet catch data in Gen Santos - see P:\OFPEMA\Project 90\Data\2020 data).
# Inspection of these values reveals they are mis-reported and need to be corrected by multiplying by 1000 to get back to the kg scale.
SFFAII_LW_SKJ<-SFFAII_LW_SKJ[!(SFFAII_LW_SKJ$WW_kg>30),]
SFFAII_LW_SKJ<- SFFAII_LW_SKJ[!(SFFAII_LW_SKJ$len>30 & SFFAII_LW_SKJ$WW_kg<0.1),]

# Replot L-W relationship
plot(SFFAII_LW_SKJ$WW_kg~SFFAII_LW_SKJ$len) 

# Fit a power expl (WW=a*UF^b)
SFFAII.SKJ_expl<-lm(log(WW_kg)~log(len), data=SFFAII_LW_SKJ) # fit expl

# Get mean predictions and add to dataset
SFFAII.SKJ_expl_RSE <- summary(SFFAII.SKJ_expl)$sigma # get residual standard error of expl
SFFAII.SKJ_expl_preds_log<-predict(SFFAII.SKJ_expl) # mean prediction on log scale
SFFAII.SKJ_expl_preds_orig<-exp(SFFAII.SKJ_expl_preds_log) # take exponent to put back on original scale
SFFAII.SKJ_expl_preds_orig<- exp(SFFAII.SKJ_expl_preds_log + SFFAII.SKJ_expl_RSE^2/2) # bias-correct mean predictions on original scale, assumes normal errors.
SFFAII_LW_SKJ$pred_WW_kg<-SFFAII.SKJ_expl_preds_orig # add predictions to dataset
SFFAII_LW_SKJ$pred_plus70<-1.7*SFFAII_LW_SKJ$pred_WW_kg # add limit for 70% higher WW than predicted  
SFFAII_LW_SKJ$pred_minus70<-0.3*SFFAII_LW_SKJ$pred_WW_kg # add limit for 70% lower WW than predicted  

plot(WW_kg~len, data=SFFAII_LW_SKJ, col="dodgerblue4", cex=1.5, ylab="WW (kg)", xlab="UF (cm)")
title(main="SKJ")
lines(x=sort(SFFAII_LW_SKJ$len), y=sort(SFFAII_LW_SKJ$pred_WW_kg), lwd=2)
lines(x=sort(SFFAII_LW_SKJ$len), y=sort(SFFAII_LW_SKJ$pred_plus70), lwd=2, lty=2)
lines(x=sort(SFFAII_LW_SKJ$len), y=sort(SFFAII_LW_SKJ$pred_minus70), lwd=2, lty=2)

SFFAII_LW_SKJ$outlier70<-ifelse(SFFAII_LW_SKJ$WW_kg < SFFAII_LW_SKJ$pred_minus70 | SFFAII_LW_SKJ$WW_kg > SFFAII_LW_SKJ$pred_plus70, "Y", "N") # label records outside the 70% difference range. 
SFFAII_LW_outlier70<-SFFAII_LW_SKJ[SFFAII_LW_SKJ$outlier70=="Y",] # compile these records
SFFAII_LW_SKJ<-SFFAII_LW_SKJ[!SFFAII_LW_SKJ$outlier70=="Y",] # remove these records

# Replot L-W relationship
plot(SFFAII_LW_SKJ$WW_kg~SFFAII_LW_SKJ$len)


################################################
## Compile all datasets
################################################

# -- BET
# Combine datasets
yy<-c(bio_LW_BET$YY, obsv_LW_BET$YY, port1_LW_BET$YY, port2_LW_BET$YY, SFFAII_LW_BET$YY)
length_cm<-c(bio_LW_BET$len, obsv_LW_BET$len, port1_LW_BET$len, port2_LW_BET$len, SFFAII_LW_BET$len)
WW_kg<-c(bio_LW_BET$WW_kg, obsv_LW_BET$WW_kg, port1_LW_BET$WW_kg, port2_LW_BET$WW_kg, SFFAII_LW_BET$WW_kg)
BET_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
#par(mfrow=c(1,1))
#plot(BET_LW$WW_kg ~ BET_LW$length_cm) # no clear outliers

# Fit a linear model
BET_LW_model<-lm(log(BET_LW$WW_kg)~log(BET_LW$length_cm)) 
summary(BET_LW_model)
para.all <- coef(BET_LW_model) 
para.all[1] <- exp(para.all[1])  
para.all 
#plot(BET_LW$WW_kg~BET_LW$length_cm)
#lines(sort(BET_LW$length_cm), para.all[1]*sort(BET_LW$length_cm)^para.all[2], col="firebrick4", lwd=2) 

# Get predictions
BET_LW$pred.WW_kg<-para.all[1]*BET_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
BET_LW$K_rel<-BET_LW$WW_kg/BET_LW$pred.WW_kg

# Extract annual means and plot
K_rel.means_BET <- aggregate(K_rel ~ yy, BET_LW, mean)
plot(K_rel.means_BET$K_rel~K_rel.means_BET$yy, type="l", col="firebrick4", lwd=2, las=2)
abline(a=1, b=0, lty=2)

# -- YFT
# Combine datasets
yy<-c(bio_LW_YFT$YY, obsv_LW_YFT$YY, port1_LW_YFT$YY, port2_LW_YFT$YY, SFFAII_LW_YFT$YY)
length_cm<-c(bio_LW_YFT$len, obsv_LW_YFT$len, port1_LW_YFT$len, port2_LW_YFT$len, SFFAII_LW_YFT$len)
WW_kg<-c(bio_LW_YFT$WW_kg, obsv_LW_YFT$WW_kg, port1_LW_YFT$WW_kg, port2_LW_YFT$WW_kg, SFFAII_LW_YFT$WW_kg)
YFT_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
#par(mfrow=c(1,1))
#plot(YFT_LW$WW_kg ~ YFT_LW$length_cm) # no clear outliers

# Fit a linear model
YFT_LW_model<-lm(log(YFT_LW$WW_kg)~log(YFT_LW$length_cm)) 
summary(YFT_LW_model)
para.all <- coef(YFT_LW_model) 
para.all[1] <- exp(para.all[1])  
para.all 
#plot(YFT_LW$WW_kg~YFT_LW$length_cm)
#lines(sort(YFT_LW$length_cm), para.all[1]*sort(YFT_LW$length_cm)^para.all[2], col="goldenrod", lwd=2) 

# Get predictions
YFT_LW$pred.WW_kg<-para.all[1]*YFT_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
YFT_LW$K_rel<-YFT_LW$WW_kg/YFT_LW$pred.WW_kg

# Extract annual means and plot
K_rel.means_YFT <- aggregate(K_rel ~ yy, YFT_LW, mean)
plot(K_rel.means_YFT$K_rel~K_rel.means_YFT$yy, type="l", col="goldenrod", lwd=2, las=2)
abline(a=1, b=0, lty=2)

# -- SKJ
# Combine datasets
yy<-c(bio_LW_SKJ$YY, obsv_LW_SKJ$YY, port1_LW_SKJ$YY, port2_LW_SKJ$YY, SFFAII_LW_SKJ$YY)
length_cm<-c(bio_LW_SKJ$len, obsv_LW_SKJ$len, port1_LW_SKJ$len, port2_LW_SKJ$len, SFFAII_LW_SKJ$len)
WW_kg<-c(bio_LW_SKJ$WW_kg, obsv_LW_SKJ$WW_kg, port1_LW_SKJ$WW_kg, port2_LW_SKJ$WW_kg, SFFAII_LW_SKJ$WW_kg)
SKJ_LW<-data.frame(yy,length_cm,WW_kg)

# Outlier checks
#par(mfrow=c(1,1))
#plot(SKJ_LW$WW_kg ~ SKJ_LW$length_cm) # no clear outliers

# Fit a linear model
SKJ_LW_model<-lm(log(SKJ_LW$WW_kg)~log(SKJ_LW$length_cm)) 
summary(SKJ_LW_model)
para.all <- coef(SKJ_LW_model) 
para.all[1] <- exp(para.all[1])  
para.all 
#plot(SKJ_LW$WW_kg~SKJ_LW$length_cm)
#lines(sort(SKJ_LW$length_cm), para.all[1]*sort(SKJ_LW$length_cm)^para.all[2], col="dodgerblue4", lwd=2) 

# Get predictions
SKJ_LW$pred.WW_kg<-para.all[1]*SKJ_LW$length_cm^para.all[2]

# Calculate K_rel for each fish
SKJ_LW$K_rel<-SKJ_LW$WW_kg/SKJ_LW$pred.WW_kg

# Extract annual means and plot
K_rel.means_SKJ <- aggregate(K_rel ~ yy, SKJ_LW, mean)
plot(K_rel.means_SKJ$K_rel~K_rel.means_SKJ$yy, type="l", col="dodgerblue4", lwd=2, las=2)
abline(a=1, b=0, lty=2)

# BET, YFT, SKJ combined
plot(K_rel.means_BET$K_rel~K_rel.means_BET$yy, type="l", col="firebrick4", lwd=2, 
     ylim=c(0.93,1.15), las=2, xlab="Year", ylab="Mean K_rel")
lines(K_rel.means_YFT$K_rel~K_rel.means_YFT$yy, type="l", col="goldenrod1", lwd=2)
lines(K_rel.means_SKJ$K_rel[19:31]~K_rel.means_SKJ$yy[19:31], type="l", col="dodgerblue4", lwd=2) # note, we plot SKJ K_rel from 2010 onwards to match scale in previous SC papers and TFAR reports.
abline(a=1, b=0, lty=2)

################################################
## Save data
################################################

write.csv(K_rel.means_BET, "K_rel.means_BET.csv", row.names=F)
write.csv(K_rel.means_YFT, "K_rel.means_YFT.csv", row.names=F)
write.csv(K_rel.means_SKJ, "K_rel.means_SKJ.csv", row.names=F)




################################################
## Indicator 2: Mean length across all gears
################################################

################################################
## Data extractions
################################################

# Get all length records available for BET, YFT and SKJ
db1 <- "driver=SQL Server;server=nouSQL03;database=LOG_master"
lmchannel <- odbcDriverConnect(db1)

LF_Detail<-sqlQuery(lmchannel,"
SELECT   lf.ORIGIN_ID,
         ORIGINDESC,
         YR,
         QTR,
         MON,
         GR, 
         FLAG_ID,
         FLEET_ID,
         LAT_SHORT,
         CAST(LON_SHORT as CHAR(3)) as lon_c,
         RIGHT(LON_SHORT,1) as Lon_h,
         AREA_ID,
         TSTRAT,
         ASTRAT,
         SCHTYPE_ID,
         SP_ID,
         LEN,
         LSTRAT,
         FREQ
FROM  [len].LF_MASTER lf inner join len.origin o on lf.origin_id = o.origin_id
WHERE sp_id in ('BET', 'YFT', 'SKJ')
order by 1,2,3,4,5,6,7,8,9,10,14,16", max=0, stringsAsFactors=FALSE) 
odbcCloseAll()

# Collate and filter
LF_Detail %<>% mutate(LAT = ifelse(substr(LAT_SHORT, 3, 3) == "N",
                      as.numeric(substr(LAT_SHORT, 1, 2)),
                      ifelse(substr(LAT_SHORT, 3, 3) == "S",
                             as.numeric(substr(LAT_SHORT, 1, 2)) * -1, NA)),
         LON = ifelse(Lon_h == "W", 360 - lon_c, lon_c)) %>%
  select(YR, QTR, LAT, LON, SP_ID, LENGTH = LEN, FREQ, GR) %>%
  filter(!is.na(LAT), !is.na(LON)) %>%
  mutate(LAT = floor(LAT / 5) * 5 + 2.5, LON = floor(LON / 5) * 5 + 2.5)
if(!exists("LF_Detail.final")){
  LF_Detail.final <- LF_Detail
} else {
  LF_Detail.final <- rbind(LF_Detail.final, LF_Detail)
}
rm(LF_Detail)

# Re-read in WCPFC-CA boundary info
wcp_ca = read.csv("wcpfc_ca_stat_area.csv") 
wcp = Polygon(wcp_ca) # Convert to spatial polygon
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords = wcp@polygons[[1]]@Polygons[[1]]@coords 

# Subset for records inside the WCPFC-CA
LF_Detail.final$WCP_CA = point.in.polygon(LF_Detail.final$LON, LF_Detail.final$LAT, coords[,1], coords[,2]) # Check that points are in WCPFC-CA
LF_Detail.final %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) # retain records within WCPFC-CA only

###############################################
## Species breakdown
###############################################

# -- BET (present LL records only for SC19 paper)
BET_LF <- LF_Detail.final[LF_Detail.final$SP_ID=="BET",] # subset for BET
BET_LF <- BET_LF[rep(row.names(BET_LF), BET_LF$FREQ), 1:8] # transpose data to long form
BET_LF <- BET_LF[BET_LF$YR%in%1990:2022,] # subset for 1990 to 2022 inclusive
BET_LF <- BET_LF[BET_LF$GR=="L",] # subset for LL

# Outlier checks
plot(BET_LF$LENGTH ~ as.factor(BET_LF$YR)) 

# Extract annual means and plot
L.means_BET <- aggregate(LENGTH ~ YR, BET_LF, mean)
plot(L.means_BET$LENGTH~L.means_BET$YR, type="l", col="firebrick4", lwd=2, 
     ylim=c(100,130), las=2)


# -- YFT (present LL records only for SC19 paper)
YFT_LF <- LF_Detail.final[LF_Detail.final$SP_ID=="YFT",] # subset for YFT
YFT_LF <- YFT_LF[rep(row.names(YFT_LF), YFT_LF$FREQ), 1:8] # transpose data to long form
YFT_LF <- YFT_LF[YFT_LF$YR%in%1990:2022,] # subset for 1990 to 2022 inclusive
YFT_LF <- YFT_LF[YFT_LF$GR=="L",] # subset for LL

# Outlier checks
plot(YFT_LF$LENGTH ~ as.factor(YFT_LF$YR)) 

# Extract annual means and plot
L.means_YFT <- aggregate(LENGTH ~ YR, YFT_LF, mean)
plot(L.means_YFT$LENGTH~L.means_YFT$YR, type="l", col="goldenrod", lwd=2, 
     ylim=c(100,130), las=2)


# -- SKJ  (LL and PS presented for SC19 paper)
SKJ_LF <- LF_Detail.final[LF_Detail.final$SP_ID=="SKJ",] # subset for SKJ
SKJ_LF <- SKJ_LF[rep(row.names(SKJ_LF), SKJ_LF$FREQ), 1:8] # transpose data to long form
SKJ_LF <- SKJ_LF[SKJ_LF$YR%in%1990:2022,] # subset for 1990 to 2022 inclusive
SKJ_LF_LL <- SKJ_LF[SKJ_LF$GR=="L",] # subset for LL
SKJ_LF_PS <- SKJ_LF[SKJ_LF$GR=="S",] # subset for PS

# LL
# Outlier checks
plot(SKJ_LF_LL$LENGTH ~ as.factor(SKJ_LF_LL$YR)) 

# Extract annual means and plot
L.means_SKJ_LL <- aggregate(LENGTH ~ YR, SKJ_LF_LL, mean)
plot(L.means_SKJ_LL$LENGTH~L.means_SKJ_LL$YR, type="l", col="dodgerblue4", lwd=2, 
     ylim=c(60,80), las=2)

# PS
# Outlier checks
plot(SKJ_LF_PS$LENGTH ~ as.factor(SKJ_LF_PS$YR)) 

# Extract annual means and plot
L.means_SKJ_PS <- aggregate(LENGTH ~ YR, SKJ_LF_PS, mean)
plot(L.means_SKJ_PS$LENGTH~L.means_SKJ_PS$YR, type="l", col="dodgerblue4", lwd=2, lty=2, 
     ylim=c(40,60), las=2)

# Plot BET, YFT and SKJ combined
plot(L.means_BET$LENGTH~L.means_BET$YR, type="l", col="firebrick4", lwd=2, 
     ylim=c(40,130), las=2)
lines(L.means_YFT$LENGTH~L.means_YFT$YR, type="l", col="goldenrod", lwd=2)
lines(L.means_SKJ_LL$LENGTH~L.means_SKJ_LL$YR, type="l", col="dodgerblue4", lwd=2)
lines(L.means_SKJ_PS$LENGTH~L.means_SKJ_PS$YR, type="l", col="dodgerblue4", lwd=2, lty=2)

################################################
## Save data
################################################

write.csv(L.means_BET, "L.means_BET.csv", row.names=F)
write.csv(L.means_YFT, "L.means_YFT.csv", row.names=F)
write.csv(L.means_SKJ_LL, "L.means_SKJ_LL.csv", row.names=F)
write.csv(L.means_SKJ_PS, "L.means_SKJ_PS.csv", row.names=F)

## END ##