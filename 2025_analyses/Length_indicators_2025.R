######################################################################
## Target Species Condition indicators - for SC19 paper 2023
######################################################################

## Created on: 07/07/23
## Latest update: 15/1/2025
## Created by: Jed Macdonald
## Modified by:: Nick Hill

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

## Key changes for 2025:
## Reduction in number of indicators and the type for new 'state of climate' report

# 1. Preamble ----
#library(RODBC) 
# library(grid)
# library(reshape2)
# library(tools)
# library(stringi)
library(tidyverse)
# library(gdata)
# library(magrittr)
library(sp)
# library(maps)
# library(mapdata)
# library(RGeostats)
library(RColorBrewer)

#setwd('P:/OFPEMA/WCPFC/SC19/Ecosystem Indicators/Catch and distribution')
# data_wd <- './2024_analyses/data/'
# results_wd <- "./2024_analyses/results/"
data_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/data/'
results_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/results/'

# Set up mapping theme
#newmap = fortify(maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE))
map_bg <- map_data("world") #|>
#coord_map(projection = 'mercator') 
map_bg2 <- map_bg |>
  dplyr::mutate(long = long + 360, group = group + max(group) + 1) |>
  rbind(map_bg)

# Plot theme
gg.theme <- theme_bw() + 
  theme(axis.line = element_line(color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(size=14),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA, color = "black")) 

# WCPFC area
# Read in WCPFC-CA boundary info
wcp_ca = read.csv(paste0(data_wd, "wcpfc_ca_stat_area.csv"))

# Convert to spatial polygon
wcp = Polygon(wcp_ca)
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# Check that points are in WCPFC-CA
coords = wcp@polygons[[1]]@Polygons[[1]]@coords

## Simple subset function
# substrLeft <- function(x, n){
#   substr(x, 1, n)
# }
# 
# substrRight<- function(x, n){
#   substr(x, nchar(x)-n+1, nchar(x))
# }

#setwd("P:/OFPEMA/WCPFC/SC19/Ecosystem Indicators/Fish condition")

# 2. Extract data from database ----

## BioDaSys data (housed in BioDaSys)
myConn <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=BioDaSys; Trusted_Connection=yes;") 
bio_LW <- sqlQuery(myConn,"
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
write.csv(bio_LW, paste0(data_wd, "biology/bio_biodasys_lenwt_raw.csv"), row.names=F) # write .csv to folder


## Observer data (housed in OBSV_MASTER)
myConn2 <- odbcDriverConnect(connection="Driver=SQL Server; Server=NOUSQL03; Database=OBSV_MASTER; Trusted_Connection=yes;")
obsv_LW <- sqlQuery(myConn2, "
SELECT 	year(set_date) as YY,
		    month(set_date) as MM,
		    'L' AS GEAR_CODE, 
        latd,
        lond, 
        [sp_code],
        [len_uf] as len,	--  should be len_uf when the LEN4 database table has been fixed .. len_uf to len_us ... 
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
write.csv(obsv_LW, paste0(data_wd, "biology/bio_obsv_lenwt_raw.csv"), row.names=F) # write .csv to folder


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
write.csv(port1_LW, paste0(data_wd, "biology/bio_port1_lenwt_raw.csv"), row.names=F) # write .csv to folder


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

write.csv(port2_LW, paste0(data_wd, "biology/bio_port2_lenwt_raw.csv"), row.names=F) # write .csv to folder


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

write.csv(SFFAII_LW, paste0(data_wd, "biology/bio_sfaii_lenwt_raw.csv"), row.names=F) # write .csv to folder

#  3. Clean datasets ----

#bio_LW <- read.csv(paste0(data_wd, "biology/bio_biodasys_lenwt_raw.csv"))
#obsv_LW <- read.csv(paste0(data_wd, "biology/bio_obsv_lenwt_raw.csv"))
#port1_LW <- read.csv(paste0(data_wd, "biology/bio_port1_lenwt_raw.csv"))
#port2_LW <- read.csv(paste0(data_wd, "biology/bio_port2_lenwt_raw.csv"))
#SFFAII_LW <- read.csv(paste0(data_wd, "biology/bio_sfaii_lenwt_raw.csv"))

# Cleaning functions 

# Vectorized version of lw_outliers to handle single length and weight values
# This function takes code from J. Mcdonald and removes clear outliers based on lenwt
lw_outliers <- function(len, wt) {
  # Create a logical flag for each pair of length and weight
  outlier_flag <- case_when(
    (len < 25 & wt > 0.75) ~ TRUE,   # Flag small length, large wt
    (len >= 50 & wt < 0.5) ~ TRUE,    # Flag larger length, small wt
    (len >= 100 & wt < 7) ~ TRUE,     # Flag larger length, small wt
    (len >= 140 & wt < 20) ~ TRUE,    # Flag larger length, small wt
    (len < 150 & wt > 95) ~ TRUE,     # Flag small length, large wt
    (len > 180 & wt < 40) ~ TRUE,     # Flag large length, small wt
    TRUE ~ FALSE                      # Otherwise, not an outlier
  )
  
  return(outlier_flag)
}

# Makes various length to wt conversion for YFT, BET and SKJ
# gutted_2_whole_wt <- function(wt, spp, wt_code, seed = 31500) {
#   # what is NM weight code?
#   out <- NA
#   set.seed(seed)
#   
#   if (is.na(wt) | is.na(spp) | is.na(wt_code)) {
#     return(NA)
#   }
#   
#   # BET
#   if (spp == 'BET') {
#     if (wt_code == "WW") { out <- wt }
#     else if(wt_code == "WW") {out <- wt}
#     else if(wt_code == "GG") {out <- 1.1616911*(wt+(runif(1,0,1)-0.5))^0.9817353}
#     else if(wt_code == "GO") {out <- 1.06*wt}
#     else if(wt_code == "GT") {out <- 1.3264*(wt+(runif(1,0,1)-0.5))^0.969}
#     else if(wt_code %in% c("GH", "GX")) {out <- 1.25*wt}
#   }
#   
#   # YFT
#   if (spp == 'YFT') {
#     if (wt_code == "WW") { out <- wt }
#     else if (wt_code == "GG") { out <- 1.1821032 * (wt + (runif(1, 0, 1) - 0.5))^0.9754946 }
#     else if (wt_code == "GT") { out <- 1.2988 * (wt + (runif(1, 0, 1) - 0.5))^0.968 }
#     else if (wt_code == "GH") { out <- 1.22 * wt }
#     else if (wt_code == "GO") { out <- 1.06 * wt }
#     else if (wt_code == "GX") { out <- 1.23 * wt }
#   }
#   
#   # SKJ
#   if (spp == 'SKJ') {
#     if (wt_code == "WW") { out <- wt }
#     else if (wt_code == "GG") { out <- 1.14 * wt }
#     else if (wt_code == "GX") { out <- 1.35 * wt }
#     else if (wt_code == "GH") { out <- 1.33 * wt }
#   }
#   
#   if (is.na(out)) {
#     return(NA)
#   }
#   
#   return(out)
# }

# NH outlier removal based on plotting raw lenwt data and removing points
# that are overly heavy for their lengh using lenwt ratio
lw_outliers2 <- function(len, wt) {
  # Calculate ratio
  ratio <- wt / len
  
  # Apply conditions based on len and the calculated ratio
  case_when(
    len < 60 & ratio > 0.25 ~ 1,
    len >= 60 & len < 100 & ratio > 0.4 ~ 1,
    len >= 100 & len < 125 & ratio > 0.6 ~ 1,
    len >= 125 & len < 150 & ratio > 0.8 ~ 1,
    TRUE ~ 0  # Default case
  )
}

# Makes various length to wt conversion for YFT, BET and SKJ
gutted_2_whole_wt <- function(wt, spp, wt_code, seed = 31500) {
  set.seed(seed)
  
  # Initialize output vector with NAs
  out <- rep(NA, length(wt))  
  
  # Ensure no assignment for NA values in wt or wt_code
  valid_data <- !is.na(wt) & !is.na(spp) & !is.na(wt_code)
  
  # BET species
  bet_condition <- spp == 'BET' & valid_data
  out[bet_condition & wt_code == "WW"] <- wt[bet_condition & wt_code == "WW"]
  out[bet_condition & wt_code == "GG"] <- 1.1616911 * (wt[bet_condition & wt_code == "GG"] + (runif(sum(bet_condition & wt_code == "GG"), 0, 1) - 0.5))^0.9817353
  out[bet_condition & wt_code == "GO"] <- 1.06 * wt[bet_condition & wt_code == "GO"]
  out[bet_condition & wt_code == "GT"] <- 1.3264 * (wt[bet_condition & wt_code == "GT"] + (runif(sum(bet_condition & wt_code == "GT"), 0, 1) - 0.5))^0.969
  out[bet_condition & wt_code %in% c("GH", "GX")] <- 1.25 * wt[bet_condition & wt_code %in% c("GH", "GX")]
  
  # YFT species
  yft_condition <- spp == 'YFT' & valid_data
  out[yft_condition & wt_code == "WW"] <- wt[yft_condition & wt_code == "WW"]
  out[yft_condition & wt_code == "GG"] <- 1.1821032 * (wt[yft_condition & wt_code == "GG"] + (runif(sum(yft_condition & wt_code == "GG"), 0, 1) - 0.5))^0.9754946
  out[yft_condition & wt_code == "GT"] <- 1.2988 * (wt[yft_condition & wt_code == "GT"] + (runif(sum(yft_condition & wt_code == "GT"), 0, 1) - 0.5))^0.968
  out[yft_condition & wt_code == "GH"] <- 1.22 * wt[yft_condition & wt_code == "GH"]
  out[yft_condition & wt_code == "GO"] <- 1.06 * wt[yft_condition & wt_code == "GO"]
  out[yft_condition & wt_code == "GX"] <- 1.23 * wt[yft_condition & wt_code == "GX"]
  
  # SKJ species
  skj_condition <- spp == 'SKJ' & valid_data
  out[skj_condition & wt_code == "WW"] <- wt[skj_condition & wt_code == "WW"]
  out[skj_condition & wt_code == "GG"] <- 1.14 * wt[skj_condition & wt_code == "GG"]
  out[skj_condition & wt_code == "GX"] <- 1.35 * wt[skj_condition & wt_code == "GX"]
  out[skj_condition & wt_code == "GH"] <- 1.33 * wt[skj_condition & wt_code == "GH"]
  
  return(out)
}

# 3.1 Biodasys dataset ----
bio_LW <- read.csv(paste0(data_wd, "biology/bio_biodasys_lenwt_raw.csv"))

bio_LW2 <- bio_LW |>
  dplyr::filter(GEAR_CODE == 'L' & YY %in% c(1990:2023)) |>
  mutate(WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  select(-WCP_CA) |>
  #rowwise() |>
  mutate(WW_kg = gutted_2_whole_wt(wt = wt_kg, spp = sp_code, wt_code = wt_code),
         WW_kg = ifelse(WW_kg<0.02, 1000*WW_kg, WW_kg),
         outlier = lw_outliers(len = len, wt = WW_kg)) |>
  dplyr::filter(outlier == FALSE)

bio_LW2 |>
  #filter(!(sp_code == 'SKJ' & WW_kg > 200)) |>
  ggplot() +
  aes(len, WW_kg, col = outlier) +
  geom_point() +
  facet_wrap(~sp_code, scales = 'free') +
  gg.theme

#save(bio_LW2, file = paste0(data_wd, "biology/bio_biodasys_lenwt_clean.Rdata"))

# 3.2 Observer gen4 dataset ----
obsv_LW <- read.csv(paste0(data_wd, "biology/bio_obsv_lenwt_raw.csv"))

obsv_LW2 <-
  obsv_LW |>
  dplyr::filter(GEAR_CODE == 'L' & YY%in% 1990:2023) |>
  mutate(WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  mutate(#WW_kg = gutted_2_whole_wt(wt = wt_kg, spp = sp_code, wt_code = wt_code),
         #WW_kg = ifelse(WW_kg<0.02, 1000*WW_kg, WW_kg),
         outlier = lw_outliers(len = len, wt = wt_kg),
         outlier2 = lw_outliers2(len = len, wt = wt_kg)) |>
  dplyr::filter(outlier != TRUE)

# obsv_LW2 |>
#   ggplot() +
#   aes(len, wt_kg, col = as.factor(outlier)) +
#   geom_point() +
#   facet_wrap(~sp_code, scales = 'free')

#save(obsv_LW2, file = paste0(data_wd, "biology/bio_obsv_lenwt_clean.Rdata"))

# 3.3 Port sampling dataset 1 ----
port1_LW <- read.csv(paste0(data_wd, "biology/bio_port1_lenwt_raw.csv"))

# there were a number of outliers in the raw data, that almost looked like a 2nd LW curve for BET and YFT
# A function (lw_outlier2) used the ratio of len to wt to ID and remove these outliers.

port1_LW2 <-
  port1_LW |>
  dplyr::filter(GEAR_CODE == "L" & YY %in% c(1991:2014)) |>
  mutate(lat = ifelse(str_detect(lat_short, "S"), 
                       as.numeric(str_extract(lat_short, "\\d+"))*-1, as.numeric(str_extract(lat_short, "\\d+"))),
         lon_numeric = as.numeric(str_extract(lon_short, "\\d+")),  # Extract the numeric part of longitude
         lon = if_else(str_detect(lon_short, "W"),                            # If direction is "W"
           360 - lon_numeric, lon_numeric),
         latd = case_when(astrat %in% c("T", "0") ~ lat + 5,
                           astrat=="5" ~ lat + 2.5,
                           .default = NA),
        lond = case_when(astrat =="T" ~ lon + 10,
                          astrat =="0" ~ lon + 5,
                          astrat=="5" ~ lon + 2.5,
                          .default = NA),
        WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  dplyr::select(-c(lat, lon, lon_numeric, WCP_CA)) |>
  #rowwise() |>
  mutate(WW_kg = gutted_2_whole_wt(wt = wt_kg, spp = sp_code, wt_code = wt_code),
         WW_kg = ifelse(WW_kg<0.02, 1000*WW_kg, WW_kg),
         outlier = lw_outliers(len = len, wt = WW_kg),
         skj_outlier = ifelse(sp_code == 'SKJ' & len >30 & WW_kg<0.1, 1, 0),
         outlier2 = lw_outliers2(len, WW_kg)) |>
  dplyr::filter(!is.na(WW_kg) & outlier2 == 0)

# LW outlier removal check
# table(port1_LW2$outlier2)
# port1_LW2 |>
#   dplyr::filter(!is.na(wt_code)) |>
#   sample_n(100000) |>
#   ggplot() +
#   aes(len, WW_kg, col = as.factor(outlier2)) +
#   geom_point() +
#   facet_wrap(~sp_code, scales = 'free')

#save(port1_LW2, file = paste0(data_wd, "biology/bio_port1_lenwt_clean.Rdata"))

# 3.4 Port 2 data ----
port2_LW <- read.csv(paste0(data_wd, "biology/bio_port2_lenwt_raw.csv"))

port2_LW2 <- port2_LW |>
  mutate(latd = ifelse(str_detect(lat_short, "S"), 
                      as.numeric(str_extract(lat_short, "\\d+"))*-1, as.numeric(str_extract(lat_short, "\\d+"))),
         lon_numeric = as.numeric(str_extract(lon_short, "\\d+")),  # Extract the numeric part of longitude
         lond = if_else(str_detect(lon_short, "W"),                            # If direction is "W"
                       360 - lon_numeric, lon_numeric),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  dplyr::select(-c(WCP_CA, lon_numeric)) |>
  mutate(WW_kg = gutted_2_whole_wt(wt = wt_kg, spp = sp_code, wt_code = wt_code),
       WW_kg = case_when(WW_kg>200 ~ WW_kg/1000,
                         len < 50 & WW_kg > 50 ~ WW_kg/1000,
                         .default = WW_kg),
       len = ifelse(len > 300, len/10, len),
    outlier = lw_outliers(len, WW_kg),
    outlier2 = lw_outliers2(len, WW_kg)) |>
  dplyr::filter(!is.na(WW_kg) & outlier2 != 1) 

#save(port2_LW2, file = paste0(data_wd, "biology/bio_port2_lenwt_clean.Rdata"))

# 3.5 SFFAII dataset ----

SFFAII_LW <- read.csv(paste0(data_wd, "biology/bio_sfaii_lenwt_raw.csv"))

SFFAII_LW2 <- SFFAII_LW |>
  dplyr::rename(WW_kg = sp_kg_landed, len = len_uf) |>
  mutate(outlier = lw_outliers(WW_kg, len),
         outlier2 = lw_outliers2(WW_kg, len),
         WW_kg = ifelse(WW_kg > 300, WW_kg/1000, WW_kg)) |>
  filter(!is.na(WW_kg) & outlier == T)

SFFAII_LW2 |>
  #dplyr::filter(outlier == 'TRUE') |>
  ggplot() +
  aes(len, WW_kg, col = as.factor(outlier2)) +
  geom_point() +
  facet_wrap(~sp_code, scales = 'free')

#save(SFFAII_LW2, file = paste0(data_wd, "biology/bio_sffaii_lenwt_clean.Rdata"))

# 3.5 combine datasets for analysis ----

load(file = paste0(data_wd, "biology/bio_biodasys_lenwt_clean.Rdata"))
load(file = paste0(data_wd, "biology/bio_obsv_lenwt_clean.Rdata"))
load(file = paste0(data_wd, "biology/bio_port1_lenwt_clean.Rdata"))
load(file = paste0(data_wd, "biology/bio_port2_lenwt_clean.Rdata"))
load(file = paste0(data_wd, "biology/bio_sffaii_lenwt_clean.Rdata"))

tmp1 <- bio_LW2 |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(dataset = 'biodasys')
tmp2 <-obsv_LW2 |>
  dplyr::select(YY, sp_code, WW_kg = wt_kg, len) |>
  mutate(dataset = 'observer')
tmp3 <- port1_LW2 |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(dataset = 'port1')
tmp4 <- port2_LW2 |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(dataset = 'port2')
tmp5 <- SFFAII_LW2 |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(dataset = 'sffaii')
LW_data <- bind_rows(tmp1,tmp2,tmp3,tmp4,tmp5)
rm(tmp1,tmp2,tmp3,tmp4,tmp5)

LW_data <-
  LW_data |>
  dplyr::filter(!(sp_code == 'SKJ' & dataset == 'observer'))

save(LW_data, file = paste0(data_wd, "biology/bio_combined_lenwt_clean.Rdata"))

# 4. Length-weight models ----
#lms_list <- setNames(lms$lm, lms$Variable)

# Load in cleaned LW data
load(file = paste0(data_wd, "biology/bio_combined_lenwt_clean.Rdata"))

# 4.1 Apply initial model to each spp and dataset ----
# Apply model to each dataset and species
LW_mod1 <-
LW_data |>
  #dplyr::filter(dataset == 'biodasys') |>
  group_by(sp_code, dataset) |>
  nest() |>
  mutate(lm = map(data, ~ lm(log(.$WW_kg) ~ log(.$len), data = .)),
         lm_RSE = map(lm, ~ summary(.x)$sigma),
         preds_log = map(lm, ~ predict(.x, newdata = .x$model)))

# Compile predictions from model to plot/explore and extract outliers
LW_out1 <-
LW_mod1 |>
  dplyr::select(sp_code, dataset, data, lm_RSE, preds_log) |>
  unnest(c(data, lm_RSE, preds_log)) |>
  mutate(preds1 = exp(preds_log + lm_RSE^2/2),
         outlier = ifelse(WW_kg > preds1 * 1.7 | WW_kg < preds1 * 0.3, 1, 0)) |>
  dplyr::select(-c(lm_RSE, preds_log)) 

# Plot model and flag outliers
LW_out1 |>
  filter(dataset == 'biodasys') |>
  ggplot() +
  aes(len, WW_kg, col = as.factor(outlier)) +
  geom_point() +
  geom_line(data = . %>%
              group_by(sp_code) %>%
              arrange(len) %>% # Ensure sorting within each species
              ungroup(),
            aes(x = len, y = preds1), col = 'black', linewidth = 1.5) +
  facet_wrap(~sp_code, scales = 'free') +
  labs(x = 'Length (cm)', y = 'Whole weight (kg)', col = 'outlier') +
  scale_color_brewer(palette = 'Set1') +
  gg.theme +
  theme(aspect.ratio = 2)

# 4.2 Remove outliers and refit models ----
# Now that we have id'ed the outliers, we can drop them and rerun model
# Could use residuals or IQR to id and drop outliers instead of current approach
LW_data2 <-
  LW_out1 |>
  dplyr::filter(outlier == 0) |>
  dplyr::select(YY, sp_code, dataset, WW_kg, len)
save(LW_data2, file = paste0(data_wd, "biology/bio_combined_lenwt_clean2.Rdata"))
rm(LW_data, LW_mod1)

# refit lw models
LW_mod2 <-
  LW_out1 |>
  dplyr::filter(outlier == 0) |>
  mutate(WW_kg_log = log(WW_kg), len_log = log(len)) |>
  group_by(sp_code, dataset) |>
  nest() |>
  mutate(lm = map(data, ~ lm(WW_kg_log ~ len_log, data = .)),
         lm_RSE = map(lm, ~ summary(.x)$sigma),
         preds_log = map(lm, ~ predict(.x, newdata = .x$model)))

# Make df from 0-max len for each sp to predict to
lens <- 
LW_data2 |> 
  group_by(sp_code) |>
  summarise(max_len = max(len)) |>
  mutate(lens = purrr::map(max_len, ~ tibble(len_log = log(1:.x)))) |>
  dplyr::select(-max_len)

# Produce final model outputs for plotting and exploration
LW_out2 <-
LW_mod2 |>
  left_join(lens, by = c('sp_code')) |>
  dplyr::select(sp_code, dataset, lm, lens) |>
  group_by(sp_code, dataset, lm, lens) |>
  nest() |>
  #head(n=3) |>
  mutate(preds_log = map2(lm, lens, ~ predict(.x, newdata = .y)),
         lm_RSE = map(lm, ~ summary(.x)$sigma)) |>
  unnest(cols = c(sp_code, dataset, lens, preds_log, lm_RSE)) |>
  mutate(preds = exp(preds_log + lm_RSE^2/2),
         len = exp(len_log)) |>
  ungroup() |>
  dplyr::select(sp_code, dataset, len, preds)

#save(LW_mod2, file = paste0(data_wd, "biology/bio_LWmodel2_mods.Rdata"))
#save(LW_out2, file = paste0(data_wd, "biology/bio_LWmodel2_outs.Rdata"))

# Plot lw curves for each species and dataset to compare
p <- 
LW_out2 |>
  ggplot() +
  aes(len, preds, col = dataset) +
  geom_line() +
  facet_wrap(~sp_code, scales = 'free') +
  labs(x= 'Length(cm)', y = 'Weight (kg)', col = 'Dataset',
       title = 'Length-weight relationships by species and dataset') +
  scale_color_brewer(palette = 'Set1') +
  gg.theme +
  theme(aspect.ratio = 1, legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_curve.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 4.3 Combine data and extract Kmeans indicator -----
# This indicator looks at the observed vs predicted lenwt relationship
# Here, we refit LW model with cleaned data combining all datasets

# Load in len-wt data that has all outliers removed
load(file = paste0(data_wd, "biology/bio_combined_lenwt_clean2.Rdata"))

# Fit lenwt models by species, combining all datasets
Krels_mod <-
  LW_data2 |>
  mutate(WW_kg_log = log(WW_kg), len_log = log(len)) |>
  group_by(sp_code) |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  group_by(sp_code) |>
  nest() |>
  mutate(lm = map(data, ~ lm(log(WW_kg) ~ log(len), data = .)),
          coef = map(lm, ~coef(.)))

# Determine obs vs expected len wt ratio to see if fish condition is changing over time
Krels_out <- 
  Krels_mod |>
  dplyr::select(sp_code, data, coef) |>
  unnest(cols = c(data)) |>
  mutate(WW_kg_pred = map2(len, coef, ~ exp(.y[1]) * .x^.y[2])) |>
  unnest(cols = c(WW_kg_pred)) |>
  mutate(K_rel = WW_kg/WW_kg_pred)

#save(Krels_mod, file = paste0(data_wd, "biology/bio_LWKrels_mods.Rdata"))
#save(Krels_out, file = paste0(data_wd, "biology/bio_LWKrels_outs.Rdata"))

# Plot condition indicator
p <- 
Krels_out |> 
  group_by(sp_code, YY) |>
  summarise(K_rel = mean(K_rel)) |>
  # group_by(sp_code) |>
  # mutate(Krel_mn90 = mean(K_rel[YY %in% c(1990:2000)]),
  #        Krel_norm = K_rel/Krel_mn90) |>
  ggplot() +
  aes(YY, K_rel, col = sp_code) +
  geom_line() +
  geom_point(size=0.5) +
  ylim(0,1.5) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Obs vs Pred weight-at-length', col = 'Species',
       title = 'Obs vs Pred weight-at-length ratio indicator') +
  gg.theme
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_indicator.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# lenwt by species plot
p <- 
Krels_out |>
  #dplyr::filter(sp_code == 'SKJ') |>
  ggplot() +
  aes(x = len, y = WW_kg, col = sp_code) +  # Plot WW_kg vs len for 'SKJ'
  #geom_point() +  # Uncomment this to plot the points
  geom_line(data = . %>%
              group_by(sp_code) %>%  # Group by species
              arrange(len) %>%       # Ensure sorting within each species
              ungroup(),
            aes(x = len, y = WW_kg_pred, col = sp_code), linewidth = 1.5) +  # Overlay predicted line
  facet_wrap(~sp_code, scales= 'free') +  # Facet by species
  labs(x = 'Length (cm)', y = 'Weight (kg)', col = 'Species',
       title = 'Length-weight relationship') +
  scale_color_brewer(palette = 'Set1') +
  gg.theme +
  theme(legend.position = 'none', aspect.ratio = 1)
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_species.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 5. Length weight over time exploration ----
# Here, we fit LW relationships by year and see if they have a directional change over time
# This may be more sensitive to the indicator above.

# Load in len-wt data that has all outliers removed
load(file = paste0(data_wd, "biology/bio_combined_lenwt_clean2.Rdata"))

# Fit lenwt models by species and year across all datasets
LW_modYY <- 
LW_data2 |>
  #dplyr::filter(sp_code == 'YFT') |>
  group_by(YY, sp_code) |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(len_log = log(len), WW_kg_log = log(WW_kg)) |>
  group_by(YY, sp_code) |>
  nest() |>
  mutate(lm = map(data, ~ lm(WW_kg_log ~ len_log, data = .)),
         coef = map(lm, ~coef(.)))

#exp(predict(LW_modYY$lm[[30]], newdata = data.frame(len_log = log(200))))

# Make df from 0-max len for each sp to predict to
lens <- 
  LW_data2 |> 
  group_by(sp_code) |>
  summarise(max_len = max(len)) |>
  mutate(lens = purrr::map(max_len, ~ tibble(len_log = log(1:.x)))) |>
  dplyr::select(-max_len)

# Produce final model outputs for plotting and exploration
LW_outYY <-
  LW_modYY |>
  left_join(lens, by = c('sp_code')) |>
  dplyr::select(YY, sp_code, lm, lens) |>
  group_by(YY, sp_code, lm, lens) |>
  nest() |>
  #head(n=3) |>
  mutate(preds_log = map2(lm, lens, ~ predict(.x, newdata = .y)),
         lm_RSE = map(lm, ~ summary(.x)$sigma)) |>
  unnest(cols = c(lens, preds_log, lm_RSE)) |>
  mutate(preds = exp(preds_log + lm_RSE^2/2),
         len = exp(len_log)) |>
  ungroup() |>
  dplyr::select(YY, sp_code, len, preds)

#save(LW_modYY, file = paste0(data_wd, "biology/bio_LWYY_mods.Rdata"))
#save(LW_outYY, file = paste0(data_wd, "biology/bio_LWYY_outs.Rdata"))
#load(file = paste0(data_wd, "biology/bio_LWYY_outs.Rdata"))

# Plot annual lenwt curves for each species - are there any clear trends?
library(cowplot)
pal <- rev(colorRampPalette(brewer.pal(9, 'Spectral'))(35))
p<- 
  LW_outYY |>
  #dplyr::filter(YY %in% c(1990, 2000, 2010, 2020, 2023)) |>
  group_by(YY, sp_code) |>  # Group by species
  arrange(len) |>
  ggplot() +
  aes(len, preds, col = as.numeric(YY)) +
  geom_line() +
  facet_wrap(~sp_code, scales = 'free') +
  scale_color_distiller(palette = 'Spectral') +
  #scale_color_manual(values = pal) +
  labs(x = 'Length (cm', y = 'Weight (kg)', col = 'Year') +
  gg.theme +
  theme(aspect.ratio=1, panel.background = element_rect(fill = "white")) 
leg <- get_legend(p)

p<- 
LW_outYY |>
  #dplyr::filter(YY %in% c(1990, 2000, 2010, 2020, 2023)) |>
  group_by(YY, sp_code) |>  # Group by species
  arrange(len) |>
  ggplot() +
  aes(len, preds, col = as.factor(YY)) +
  geom_line() +
  facet_wrap(~sp_code, scales = 'free') +
  #scale_color_distiller(palette = 'Spectral') +
  scale_color_manual(values = pal) +
  labs(x = 'Length (cm)', y = 'Weight (kg)', col = 'Year') +
  gg.theme +
  theme(legend.position = 'none') 
p

combined_plot <- plot_grid(p, leg, rel_widths = c(1, 0.2)) + 
  theme(plot.background = element_rect(fill = "white", colour = NA))
ggsave(combined_plot, file = paste0(results_wd, 'biology/biology_lenwt_speciesYY.png'),height = 8, width = 11, units = "in", dpi = 200)

# 5.2 Historical vs current LW indicator ----
ribbon <- 
LW_outYY |>
  dplyr::filter(YY %in% 1990:2000) |>
  dplyr::filter(YY != 1998) |>
  group_by(sp_code, len) |>
  summarise(min_pred = min(preds), max_pred = max(preds), mean_pred = mean(preds), .groups = 'drop') 

p <- 
ggplot() +
  # Ribbon plot for min/max boundaries
  geom_ribbon(data = ribbon, aes(x = len, ymin = min_pred, ymax = max_pred), alpha = 0.3) +
  # Line plot for years 2019-2023
  geom_line(data = LW_outYY %>% dplyr::filter(YY %in% 2019:2023), 
            aes(x = len, y = preds, col = as.factor(YY)), linewidth = 1.05) +
  facet_wrap(~sp_code, scales = 'free') +
  scale_color_brewer(palette = 'Spectral') +
  labs(x = 'Length (cm)', y = 'Weight (kg)', col = 'Year',
       title = 'Historical vs recent length-weight relationship') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_hist_recent_indicator.png'),height = 8, width = 11, units = "in", dpi = 200)

# historical vs recent boxplot
#tmp <- 
LW_outYY |>
  dplyr::filter(near(len, 50) & sp_code != 'SKJ') |>
  ggplot() +
  aes(x = YY, y = preds, group = 1) +
  geom_line() +
  coord_cartesian(ylim = c(0, NA)) +  # Set ymin to 0 while keeping ymax free
  #geom_violin() +
  #geom_jitter(alpha = 0.2, width = .02) +
  facet_wrap(~sp_code)

p<- 
LW_outYY |>
  #dplyr::filter((near(len, 150) | near(len, 100) | near(len, 50)) & sp_code != 'SKJ') |>
  mutate(bins = case_when(sp_code != 'SKJ' & near(len, 50) ~ 'small (50/40cm)',
                          sp_code != 'SKJ' & near(len, 100) ~ 'medium (100/60cm)',
                          sp_code != 'SKJ' & near(len, 150) ~ 'large (150/80cm)',
                          sp_code == 'SKJ' & near(len, 40) ~ 'small (50/40cm)',
                          sp_code == 'SKJ' & near(len, 60) ~ 'medium (100/60cm)',
                          sp_code == 'SKJ' & near(len, 80) ~ 'large (150/80cm)',
                          .default = NA)) |>
  dplyr::filter(!is.na(bins)) |>
  ggplot() +
  aes(x = YY, y = preds, col = bins) +
  geom_line() +
  geom_point() +
  coord_cartesian(ylim = c(0, NA)) + 
  #geom_violin() +
  #geom_jitter(alpha = 0.2, width = .02) +
  facet_wrap(~sp_code, scales = 'free') +
  scale_color_brewer(palette = 'Set1') +
  gg.theme +
  labs(x = 'Year', y = 'Weight (kg)', col = 'Fish size', 
       title = 'Changing weight at length indicator') + 
  theme(aspect.ratio = 1.5, legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_species_lenquants2.png'),height = 8, width = 11, units = "in", dpi = 200)


# Plotting different length classes by year to see if the prdicted weight hass changed
p <- 
LW_outYY %>%
  group_by(sp_code) %>% 
  #nest()# Group by species and year
  mutate(len20 = quantile(len, 0.2),len40 = quantile(len, 0.4),
    len50 = quantile(len, 0.5),len60 = quantile(len, 0.6),
    len80 = quantile(len, 0.8)) %>%
  ungroup() %>% 
  pivot_longer(cols = c(len20, len40, len50, len60, len80),
               names_to = "len_bin", 
               values_to = "quantile_value") %>%
  mutate(quantile_value = round(quantile_value,0)) |>
  dplyr::filter(dplyr::near(len,quantile_value)) |>
  ggplot(aes(x = YY, y = preds, col = len_bin)) +  # Color lines by quantile
  geom_line() +  # Plot lines
  scale_color_brewer(palette = "Set1") +  # Color palette for the quantiles
  facet_wrap(~sp_code, scales = 'free') +  # Facet by species
  labs(title = "Predicted weight by length quantiles",
       x = "Year",y = "Weight (kg)",color = "Length quantile") +  # Label for the color legend
  gg.theme
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenwt_species_lenquants.png'),height = 11, width = 8, units = "in", dpi = 200)

LW_outYY %>%
  group_by(sp_code) %>% 
  #nest()# Group by species and year
  mutate(len20 = quantile(len, 0.2),len40 = quantile(len, 0.4),
         len50 = quantile(len, 0.5),len60 = quantile(len, 0.6),
         len80 = quantile(len, 0.8)) %>%
  ungroup() %>% 
  pivot_longer(cols = c(len20, len40, len50, len60, len80),
               names_to = "len_bin", 
               values_to = "quantile_value") %>%
  mutate(quantile_value = round(quantile_value,0)) |>
  dplyr::filter(dplyr::near(len,quantile_value)) |>
  ggplot(aes(x = YY, y = preds, col = len_bin)) +  # Color lines by quantile
  geom_line() +  # Plot lines
  scale_color_brewer(palette = "Set1") +  # Color palette for the quantiles
  facet_wrap(~sp_code, scales = 'free') +  # Facet by species
  labs(title = "Predicted weight by length quantiles",
       x = "Year",y = "Weight (kg)",color = "Length quantile") +  # Label for the color legend
  gg.theme

#5.1 Historical vs recent LWs
# Load in len-wt data that has all outliers removed
load(file = paste0(data_wd, "biology/bio_combined_lenwt_clean2.Rdata"))

# Fit lenwt models by species and year across all datasets
LW_modYYbin <- 
  LW_data2 |>
  dplyr::filter(YY %in% 1990:2000 | YY >= 2019) |>
  dplyr::filter(YY <= 2023) |>
  #group_by(YY, sp_code) |>
  dplyr::select(YY, sp_code, WW_kg, len) |>
  mutate(len_log = log(len), WW_kg_log = log(WW_kg),
         YY_bin = ifelse(YY%in% 1990:2000, '1990-2000', as.character(YY))) |>
  group_by(YY_bin, sp_code) |>
  nest() |>
  mutate(lm = map(data, ~ lm(WW_kg_log ~ len_log, data = .)),
         coef = map(lm, ~coef(.)))

#exp(predict(LW_modYY$lm[[30]], newdata = data.frame(len_log = log(200))))

# Make df from 0-max len for each sp to predict to
lens <- 
  LW_data2 |> 
  group_by(sp_code) |>
  summarise(max_len = max(len)) |>
  mutate(lens = purrr::map(max_len, ~ tibble(len_log = log(1:.x)))) |>
  dplyr::select(-max_len)

# Produce final model outputs for plotting and exploration
LW_outYYbin <-
  LW_modYYbin |>
  left_join(lens, by = c('sp_code')) |>
  dplyr::select(YY_bin, sp_code, lm, lens) |>
  group_by(YY_bin, sp_code, lm, lens) |>
  nest() |>
  #head(n=3) |>
  mutate(preds_log = map2(lm, lens, ~ predict(.x, newdata = .y)),
         lm_RSE = map(lm, ~ summary(.x)$sigma)) |>
  unnest(cols = c(lens, preds_log, lm_RSE)) |>
  mutate(preds = exp(preds_log + lm_RSE^2/2),
         len = exp(len_log)) |>
  ungroup() |>
  dplyr::select(YY_bin, sp_code, len, preds)

pal <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(5), 'grey50'))
LW_outYYbin |>
  ggplot() +
  aes(len, preds, col = YY_bin) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~sp_code, scales = 'free') +
  scale_color_manual(values = pal) +
  theme_classic()

# 6. Mean length indicator ----

# 6.1 Database call to extract raw length data ----
# Get all length records available for BET, YFT and SKJ
# db1 <- "driver=SQL Server;server=nouSQL03;database=LOG_master"
# lmchannel <- odbcDriverConnect(db1)
db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
channel <- odbcDriverConnect(db1)

LF_raw <-sqlQuery(lmchannel,"
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

#save(LF_raw, file = paste0(data_wd, "biology/bio_len_freqs_raw.Rdata"))

# 6.2 Clean raw length data ----
# Re-read in WCPFC-CA boundary info
load(file = paste0(data_wd, "biology/bio_len_freqs_raw.Rdata"))

wcp_ca = read.csv("wcpfc_ca_stat_area.csv") 
wcp = Polygon(wcp_ca) # Convert to spatial polygon
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords = wcp@polygons[[1]]@Polygons[[1]]@coords 

# Clean LF data - tidy lat and lon, filter for inside convention area etc
LFs <-
  LF_raw |>
  mutate(lat = case_when(str_detect(LAT_SHORT, "S") ~ as.numeric(substr(LAT_SHORT,1,2))*-1,
                         str_detect(LAT_SHORT, "^[0-9]{2}N$") ~ as.numeric(substr(LAT_SHORT,1,2)),
                         .default = NA),
         lon = ifelse(Lon_h == "W", 360 - lon_c, lon_c)) |>
  select(yy = YR, qtr = QTR, lat, lon, sp_code = SP_ID, len = LEN, freq = FREQ, gr = GR) %>%
  filter(!is.na(lat), !is.na(lon) & yy %in% c(1990:2023)) %>%
  mutate(lat5 = floor(lat / 5) * 5 + 2.5, lon5 = floor(lon / 5) * 5 + 2.5) |>
  mutate(WCP_CA = point.in.polygon(lon, lat, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  select(-WCP_CA)

# Load in len-wt data that has all outliers removed
#save(LFs, file = paste0(data_wd, "biology/bio_len_freqs_clean.Rdata"))

# 6.3 Mean length indicator ----
load(file = paste0(data_wd, "biology/bio_len_freqs_clean.Rdata"))

# Calculate some simple stats for plotting
summ_stats <- LFs %>%
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | (sp_code %in% c('BET', 'YFT') & gr %in% c('L')))  |>
  group_by(yy, sp_code) %>%
  summarise(min_val = min(len),
            q25 = quantile(len, 0.25),
            median_val = median(len),
            mean_val = mean(len),
            q75 = quantile(len, 0.75),
            max_val = max(len),
            n = n())

# Number of length samples taken by sp and year
p <- 
summ_stats |>
  ggplot() +
  aes(yy, n/1000, fill = sp_code) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Length samples (x1000)', fill = 'Species',
       title = 'Longline samples taken by year for tunas') +
  gg.theme
p
#ggsave(p, file = paste0(results_wd, 'biology/biology_lens_sample_sizes.png'),
#       height = 4, width = 5.5, units = "in", dpi = 200)

# Fit lms to mean length data and extract slopes
slopes <- LFs |>
  dplyr::filter(gr == 'L') |>
  uncount(freq) |> 
  group_by(yy, sp_code) |>
  summarise(mn_len = mean(len, na.rm=T)) |>
  group_by(sp_code) |>  # Group by species
  do(model = lm(mn_len ~ yy, data = .)) |>
  mutate(slope = coef(model)[2]) |>
  select(sp_code, slope)

mns <- LFs |>
  dplyr::filter(gr == 'L') |>
  uncount(freq) |> 
  mutate(yy_bin = ifelse(yy %in% 1990:2000, '1990-2000', yy)) |>
  dplyr::filter(yy %in% 1990:2000 | yy == max(yy)) |>
  group_by(yy_bin, sp_code) |>
  summarise(mn_len = mean(len, na.rm=T)) 

# Violin plots 
mns <- LFs |>
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | 
                  (sp_code %in% c('BET', 'YFT') & gr %in% c('L'))) |>
  dplyr::filter(yy %in% c(1990:2000)) |>
  uncount(freq) |>
  group_by(sp_code) |>
  summarise(mn_len = round(mean(len, na.rm = T), 1)) |>
  mutate(y = c(200, 95, 200)*0.92)

mns23 <- LFs |>
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | 
                  (sp_code %in% c('BET', 'YFT') & gr %in% c('L'))) |>
  dplyr::filter(yy %in% c(2023)) |>
  uncount(freq) |>
  group_by(sp_code) |>
  summarise(mn_len = round(mean(len, na.rm = T), 1)) |>
  mutate(y = c(200, 95, 200)*0.84)

L50s <-
  data.frame(sp_code = c('BET', 'SKJ', 'YFT'), L50 = c(103,55,105), y = c(200, 95, 200)) 

p <- 
LFs |>
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | (sp_code %in% c('BET', 'YFT') & gr %in% c('L'))) |>
  uncount(freq) |> 
  #ggplot(aes(x = interaction(yy, sp_code), y = len, fill = sp_code)) +
  ggplot() +
  aes(yy, len, fill = sp_code, group = yy) +
  geom_violin() +
  geom_hline(data = mns, aes(yintercept = mns$mn_len, group = sp_code), linetype = 'dashed', size = 1.2) +
  geom_hline(data = L50s, aes(yintercept = L50s$L50, group = sp_code), linetype = 'dotted', size = 1.2) +
  geom_point(data = summ_stats, aes(x = yy, y = mean_val), 
             color = 'black', shape = 21, fill = 'black', size = 3, inherit.aes = FALSE) +
  geom_errorbar(data = summ_stats, aes(x = yy, ymin = q25, ymax = q75), 
                width = 0.3, size = 1.2, color = 'black', inherit.aes = FALSE) +
  geom_text(data = L50s, aes(x = 1988, y = y, label = paste0('L50=',L50, 'cm')), inherit.aes = FALSE,size = 4, hjust = 0) +
  geom_text(data = mns, aes(x = 1988, y = y-5, label = paste0('mean=',mn_len, 'cm')), inherit.aes = FALSE,size = 4, hjust = 0) +
  facet_wrap(~sp_code, scales = 'free_y', nrow = 3) +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Length (cm)', fill = 'Species') +
  gg.theme +
  theme(legend.position = 'none') # 
p 
ggsave(p, file = paste0(results_wd, 'biology/biology_lens_violin_plot.png'),
       width = 8, height = 11, units = "in", dpi = 100)

summ_stats2 <- LFs %>%
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | (sp_code %in% c('BET', 'YFT') & gr %in% c('L')))  |>
  mutate(yy_grouped = ifelse(yy %in% c(1990:2000), "1990–2000", as.character(yy)),
         yy_grouped = factor(yy_grouped, levels = c("1990–2000", as.character(sort(unique(LFs$yy[LFs$yy > 2000])))))) |>
  uncount(freq) |> 
  group_by(yy_grouped, sp_code) %>%
  summarise(min_val = min(len),
            q25 = quantile(len, 0.25),
            median_val = median(len),
            mean_val = mean(len),
            q75 = quantile(len, 0.75),
            max_val = max(len),
            n = n())

p <- 
  LFs |>
  dplyr::filter((sp_code == 'SKJ' & gr %in% c('L', 'S')) | (sp_code %in% c('BET', 'YFT') & gr %in% c('L'))) |>
  uncount(freq) |> 
  mutate(yy_grouped = ifelse(yy <= 2000, "1990–2000", as.character(yy)),
         yy_grouped = factor(yy_grouped, levels = c("1990–2000", as.character(sort(unique(LFs$yy[LFs$yy > 2000])))))) |>
  ggplot() +
  aes(yy_grouped, len, fill = sp_code) +
  geom_violin(alpha = 0.7) +
  geom_hline(data = mns, aes(yintercept = mn_len, group = sp_code), linetype = 'dashed', col = 'grey30') +
  geom_hline(data = L50s, aes(yintercept = L50, group = sp_code), linetype = 'dotted', col = 'grey30') +
  geom_point(data = summ_stats2, aes(x = yy_grouped, y = mean_val), 
             color = 'black', shape = 21, fill = 'black', size = 2, inherit.aes = FALSE) +
  geom_errorbar(data = summ_stats2, aes(x = yy_grouped, ymin = q25, ymax = q75), 
                 width = 0.3, size = 1, color = 'black', inherit.aes = FALSE) +
  geom_text(data = L50s, aes(x = "1990–2000", y = y, label = paste0('L50=',L50, 'cm')), fontface = "bold", inherit.aes = FALSE,size = 4, hjust = 0) +
  geom_text(data = mns, aes(x = "1990–2000", y = y, label = paste0('1990-2000=',mn_len, 'cm')), fontface = "bold", inherit.aes = FALSE,size = 4, hjust = 0) +
  geom_text(data = mns23, aes(x = "1990–2000", y = y, label = paste0('2023=',mn_len, 'cm')), fontface = "bold", inherit.aes = FALSE,size = 4, hjust = 0) +
  facet_wrap(~sp_code, scales = 'free_y', nrow = 3) +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Length (cm)', fill = 'Species') +
  gg.theme +
  theme(legend.position = 'none', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) # 
p 
ggsave(p, file = paste0(results_wd, 'biology/biology_lens_violin_plot2.png'),
       width = 8, height = 11, units = "in", dpi = 100)


# Plot mean length by species and year
p <- 
LFs |>
  dplyr::filter(gr == 'L') |>
  uncount(freq) |> 
  group_by(yy, sp_code) |>
  summarise(mn_len = mean(len, na.rm=T)) |>
  ggplot() +
  aes(x = yy, y = mn_len, col = sp_code) +
  geom_line(linewidth=0.8) +
  geom_point(sizew = 0.5) +
  geom_smooth(method = "lm",se = FALSE, linetype = "dashed", linewidth = 0.5) +
  scale_color_brewer(palette = 'Set1') +
  facet_wrap(~sp_code, scales = 'free', nrow=3) +
  geom_text(data = slopes, aes(x = 2015, y = 25, label = paste0("Slope: ", round(slope, 2))), 
            inherit.aes = FALSE, col = 'black', size = 3, hjust = 0) +
  geom_text(data = mns[mns$yy_bin == '1990-2000',], aes(x = 2015, y = 55, label = paste0(yy_bin, ': ', round(mn_len, 1))), 
            inherit.aes = FALSE, col = 'black', size = 3, hjust = 0) +
  geom_text(data = mns[mns$yy_bin != '1990-2000',], aes(x = 2015, y = 40, label = paste0(yy_bin, ': ', round(mn_len, 1))), 
            inherit.aes = FALSE, col = 'black', size = 3, hjust = 0) +
  coord_cartesian(ylim = c(0, NA)) +  # Set ymin to 0 while keeping ymax free
  gg.theme +
  labs(x = 'Year', y = 'Length (cm)', col = 'Species',
       title = 'Mean length (longline)') +
  theme(aspect.ratio = 0.3)
p
ggsave(p, file = paste0(results_wd, 'biology/biology_mnlen_indicator2.png'),
       height = 11, width = 8, units = "in", dpi = 200)

# 6.4  Mean and median length of species by year indicator ----
p <- 
ggplot(summ_stats, aes(x = yy)) +
  # Quartile box (from q25 to q75)
  geom_segment(aes(xend = yy, y = q25, yend = q75), linewidth = 0.5, color = "black") +
  # Whiskers (from min to q25, and q75 to max)
  #geom_segment(aes(xend = yy, y = min_val, yend = q25), linewidth = 1, color = "black") +
  #geom_segment(aes(xend = yy, y = q75, yend = max_val), linewidth = 1, color = "black") +
  # Dot for the median
  geom_point(aes(y = median_val), color = "red", size = 0.8) +
  geom_line(aes(y = median_val), color = "red", linewidth = 0.8) +
  geom_point(aes(y = mean_val), color = "blue", size = 0.5) +
  geom_line(aes(y = mean_val), color = "blue", linewidth = 0.5) +
  facet_wrap(~sp_code, scales = 'free') +
  # Optionally add dot for the mean
  #stat_summary(aes(y = value), fun = "mean", geom = "point", color = "blue", size = 3) +
  gg.theme +
  coord_cartesian(ylim = c(0, NA)) +  # Set ymin to 0 while keeping ymax free
  #theme(aspect.ratio = 0.5) +
  labs(title = "Length distribution",
       y = "Length (cm)", x = "Year") +
  theme(aspect.ratio = 1.5)
p
ggsave(p, file = paste0(results_wd, 'biology/biology_mnlen_indicator3.png'),
       height = 4, width = 5.5, units = "in", dpi = 200)

# 6.5 Length proportions indicator ----
# Here we plot proportional length indicators;
# the proportion of big fish >80th% from 1990-2000
# the proportion of fish > mean from 1990-2000
# the proportion of small fish < 20th % from 1990-2000

library(zoo)
pal4 <- brewer.pal(3, 'Set1')

p <- 
LFs |>
  dplyr::filter(gr == 'L') |>
  group_by(sp_code) |>
  mutate(len20 = quantile(len[yy %in% c(1990:2000)], 0.2), mn_len = mean(len[yy %in% c(1990:2000)], na.rm=T),
                     len80 = quantile(len[yy %in% c(1990:2000)], 0.8)) |>
  group_by(sp_code, yy, len20, mn_len, len80) |>
  summarise(n = n(), prop20 = sum(len < len20)/n(),
            propmn = sum(len > mn_len)/n(),
            prop80 = sum(len > len80)/n()) |>
  ungroup() |>
  #dplyr::select(yy, sp_code, prop20, propmn, prop80) |>
  mutate(prop20_roll = rollmean(prop20, k = 3, fill = NA,align = "left"),
         mn_len_roll = rollmean(propmn, k = 3, fill = NA,align = "left"),
         prop80_roll = rollmean(prop80, k = 3, fill = NA, align = "left")) |>
  pivot_longer(-c(sp_code, yy, len20, mn_len, len80, n, prop20, propmn, prop80), names_to = 'indicator', values_to = 'val') |>
  #left_join(labs, by = "sp_code") |>
  ggplot() +
  aes(x = yy, y = val, col = indicator) +
  geom_line() +
  geom_point(size = 0.5) +
  facet_wrap(~sp_code, nrow = 3) +
  geom_smooth(method = "lm",se = FALSE, linetype = "dashed", linewidth = 0.5) +
  #scale_color_brewer(palette = 'Set1') + 
  scale_color_manual(values = pal4, labels = c(">mean", "small fish", "big fish")) +
  geom_text(aes(x = 2020, y = 0.1, label = paste0("small=", round(len20, 0), 'cm')), 
            inherit.aes = FALSE, size = 3, color = "black", hjust = 1) +
  geom_text(aes(x = 2020, y = 0.05, label = paste0("mean=", round(mn_len, 0), 'cm')), 
            inherit.aes = FALSE, size = 3, color = "black", hjust = 1) +
  geom_text(aes(x = 2020, y = 0.01, label = paste0("large=", round(len80, 0), 'cm')), 
            inherit.aes = FALSE, size = 3, color = "black", hjust = 1) +
  coord_cartesian(ylim = c(0, NA)) +  # Set ymin to 0 while keeping ymax free
  labs(x = 'Year', y = 'Proportion', col = 'Indicator', 
       title = 'Length proportion indicators') +
  gg.theme +
  theme(aspect.ratio = 0.3)
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenprop_indicator2.png'),
       height = 11, width = 8, units = "in", dpi = 200)

# 6.6 GGridges length frequency distribution indicator----
# Length frequency distribution over time

library(ggridges)
mns <- 
  LFs |>
  dplyr::filter(gr == 'L') |>
  group_by(sp_code) |>
  summarise(mn_len = median(len[yy %in% c(1990:2000)], na.rm=T))

# Ridgeplot by year and species with species mean length (1990:2000) plotted
p <-
LFs |>
  dplyr::filter(gr == 'L' ) |>
  ggplot() +
  aes(x = len, y = yy, group = yy, fill = yy) + #after_stat(x)
  geom_density_ridges(rel_min_height = 0.01, scale  = 5) +
  facet_wrap(~sp_code, scales = 'free') +
  #stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.5)) +
  #geom_density_ridges_gradient() +
  #scale_fill_viridis_c(name = "Length (cm)", option = "C")
  scale_fill_distiller(palette = 'Blues') +
  geom_vline(data = mns, aes(xintercept = mn_len), linetype = 'dashed') +
  labs(x = 'Length (cm)', y = 'Year', 
       title = 'Length frequency distribution') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenridges_indicator1.png'),
       height = 8, width = 11, units = "in", dpi = 200)


# plot B - ridge plot with historical mean and most recent 5 yrs
pal3 <- rev(c(brewer.pal(5, 'Blues'), 'grey50'))

p <- 
LFs |>
  dplyr::filter(gr == 'L' & (yy >= 1990 & yy <= 2000 | yy >= max(yy) - 4)) |>
  mutate(yr_bin = ifelse(yy %in% c(1990:2000), '1990-2000', as.character(yy))) |>
  ggplot() +
  aes(x = len, y = yr_bin, group = yr_bin, fill = yr_bin) + #after_stat(x)
  geom_density_ridges(rel_min_height = 0.01) +
  facet_wrap(~sp_code, scales = 'free') +
  #stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.5)) +
  #geom_density_ridges_gradient() +
  #scale_fill_viridis_c(name = "Length (cm)", option = "C")
  #scale_fill_brewer(palette = 'Blues') +
  scale_fill_manual(values = pal3) +
  geom_vline(data = mns, aes(xintercept = mn_len), linetype = 'dashed') +
  labs(x = 'Length (cm)', y = 'Year', 
       title = 'Length frequency distribution in recent years vs historical') +
  gg.theme +
  theme(legend.position = 'none')
p 
ggsave(p, file = paste0(results_wd, 'biology/biology_lenridges_indicator2.png'),
       height = 8, width = 11, units = "in", dpi = 200)

# Plot c - 1990-2000 vs most recent yr
pal5 <- brewer.pal(length(unique(LFs$sp_code)), "Set1")

labs <- 
  LFs |>
  dplyr::filter(gr == 'L' & (yy >= 1990 & yy <= 2000 | yy == max(yy))) |>
  mutate(yr_bin = ifelse(yy %in% c(1990:2000), '1990-2000', as.character(yy))) |>
  group_by(sp_code, yr_bin) |>
  summarise(n = n(), .groups = 'drop')
  
p<- 
LFs |>
  dplyr::filter(gr == 'L' & (yy >= 1990 & yy <= 2000 | yy == max(yy))) |>
  mutate(
    yr_bin = ifelse(yy %in% c(1990:2000), '1990-2000', as.character(yy)),
    fill_group = ifelse(yr_bin == '1990-2000', '1990-2000', sp_code)) |>
  ggplot(aes(x = len, fill = fill_group)) + 
  geom_density(aes(y = after_stat(density) * 100), alpha = 0.5) +
  facet_wrap(~sp_code, scales = 'free', strip.position = 'right',nrow=3) +
  scale_fill_manual(
    values = c('1990-2000' = 'gray50', 
               setNames(pal5, unique(LFs$sp_code)))) +
  geom_vline(data = mns, aes(xintercept = mn_len), linetype = 'dashed') +
  geom_text(data = labs[labs$yr_bin == '1990-2000',],  aes(x = -Inf, y = Inf, label = paste("1990-2000:", n)), 
            inherit.aes = FALSE, color = 'black', size = 3, hjust = -0, vjust = 2) +
  geom_text(data = labs[labs$yr_bin == '2023',],  aes(x = -Inf, y = Inf, label = paste("2023:", n)), 
            inherit.aes = FALSE, color = 'black', size = 3, hjust = -0, vjust = 1) +
  labs(x = 'Length (cm)', y = 'Proportion of fish (%)', fill = '',
       title = 'Historical vs 2023') +
  gg.theme +
  theme(aspect.ratio = 0.3, legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'biology/biology_lenridges_indicator3.png'),
       height = 4, width = 5.5, units = "in", dpi = 200)
