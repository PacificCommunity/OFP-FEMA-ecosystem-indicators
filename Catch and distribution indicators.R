######################################################
# Fishery catch indicators for SC17
######################################################

# Created on: 03/07/21
# Updated on: 14/07/21
# Created by: J. Macdonald
# Modified from: 'Effort indicators' code of Tiffany Vidal

# Target species: SKJ, YFT, BET, ALB (North and South Pacific ALB stocks separately)
# Area: WCPFC Convention Area
# Fishing gears: PS (associated), PS (unassociated), LL, PL, Other
# Time series: annual, 2000-2020
# Spatial resolution: COG and inertia 5° x 5°; Area occupied 1° x 1° (PS), 5° x 5° (LL)

# The following three indicators were considered:
# 1) Annual longitudinal centre of catch          This is the annual mean zonal centre of gravity of catch for each of SKJ, YFT, BET, ALB between 2000 and 2020, for each gear type. 
# 2) Annual latitudinal centre of catch           This is the annual mean meridional centre of gravity of catch for each of SKJ, YFT, BET, ALB, between 2000 and 2020, for each gear type.
# 3) Annual area of catch ('Area occupied')       This is the annual number of 1° x 1° grid cells (PS), 5° x 5° grid cells (LL) and area (in km^2) in which each of SKJ, YFT, BET, ALB were captured in the WCPFC-CA between 2000 and 2020, for each gear type. 

# Load packages
library(tidyverse)
library(magrittr)
library(maps)
library(zoo)
library(RODBC)
library(sp)
library(mapdata)
library(RGeostats)

# Set dir.
setwd('P:/OFPEMA/WCPFC/SC17/Indicators/Catch and distribution')

# Set up mapping theme
newmap = fortify(maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE))

gg.theme <- theme_bw() + 
  theme(axis.line = element_line(color="black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=14),
        axis.title = element_text(size=16),
        strip.background =element_rect(fill="white"),
        strip.text = element_text(size=14),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA, color = "black"))  


######################################################
## Read in and organise data
######################################################

# Full aggregated catch data (from Steven Hare's extract @ 02/07/21)
catch_dat<-read.csv("agg.data.jed.csv")

# Format geographic coordinates  
# Use SW corner of 5° x 5° grid cells as catch location (mirroring fishery effort indicators)
catch_dat %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                      lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4), 
                      lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
                      select(-c(lon_short, lon.dir, lat_short, lat.dir))

# Read in WCPFC-CA boundary info
wcp_ca = read.csv("wcpfc_ca_stat_area.csv")

# Convert to spatial polygon
wcp = Polygon(wcp_ca)
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Check that points are in WCPFC-CA
coords = wcp@polygons[[1]]@Polygons[[1]]@coords
catch_dat$WCP_CA = point.in.polygon(catch_dat$lon, catch_dat$lat, coords[,1], coords[,2])
catch_dat %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)

# Get data summaries
summary(catch_dat)
unique(catch_dat$school) # get school association codes  

# Define school association codes (for discussion, but kept consistent here with Fishing Effort indictaors)
# LL, PL and Z = '0'; for PS:
#ASS = c(3,4,5,6,7) # all associated sets
#FAD = c(3,4,5) # all FAD sets
#AFAD = c(5) # anchored FAD sets
#DFAD = c(3,4) # dFAD sets
#WHA = c(6,7) # whale and whale shark sets
#UNA <- c(1,-9) # unassociated sets (check on '-9' code)

ASS <- c(4,3) # DFAD only
AFAD = c(5)
ASSWH = c(6,7) # whale and whale shark sets
UNA <- c(1,2,-9)

# Subset catch_dat by species
SKJ_catch<-catch_dat[catch_dat$skj_mt>0,]
SKJ_catch<-SKJ_catch[,-c(6,7,9)]
SKJ_catch$sp_code<-"SKJ"
colnames(SKJ_catch)[6]<-"sp_mt"

YFT_catch<-catch_dat[catch_dat$yft_mt>0,]
YFT_catch<-YFT_catch[,-(6:8)]
YFT_catch$sp_code<-"YFT"
colnames(YFT_catch)[6]<-"sp_mt"

BET_catch<-catch_dat[catch_dat$bet_mt>0,]
BET_catch<-BET_catch[,-c(6,8,9)]
BET_catch$sp_code<-"BET"
colnames(BET_catch)[6]<-"sp_mt"

ALB.S_catch<-catch_dat[catch_dat$alb_mt>0,] # South Pacific stock
ALB.S_catch<-ALB.S_catch[,-(7:9)]
ALB.S_catch<-ALB.S_catch[ALB.S_catch$lat<0,]
ALB.S_catch$sp_code<-"ALB.S"
colnames(ALB.S_catch)[6]<-"sp_mt"

ALB.N_catch<-catch_dat[catch_dat$alb_mt>0,] # North Pacific stock
ALB.N_catch<-ALB.N_catch[,-(7:9)]
ALB.N_catch<-ALB.N_catch[ALB.N_catch$lat>=0,]
ALB.N_catch$sp_code<-"ALB.N"
colnames(ALB.N_catch)[6]<-"sp_mt"

# Recombine data for all 4 species in new format
tuna_catch<-rbind(SKJ_catch, YFT_catch, BET_catch, ALB.S_catch, ALB.N_catch) 


#################################################
## Indicators for all 4 species combined 
#################################################

setwd("P:/OFPEMA/WCPFC/SC17/Indicators/Catch/All species")

# Use 'tuna_catch' dataset

# Sort data by gear type
PS_catch_all = tuna_catch %>% filter(school %in% c(1,-9,3,4,5,6,7)) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # all PS catch
PS_catch_ass = tuna_catch %>% filter(school %in% ASS) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # catch for PS associated sets
PS_catch_una = tuna_catch %>%  filter(school %in% UNA) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # catch for PS unassociated sets
LL_catch = tuna_catch %>% filter(gear == "L") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # LL catch
PL_catch = tuna_catch %>% filter(gear == "P") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # PL catch
OTH_catch = tuna_catch %>% filter(gear == "Z") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # Other gear catch

# Maps of catch by gear type
pdf("Catch distributions.pdf")
# PS all
ggplot()+
  # geom_contour_filled(data=PS_catch_all %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_all %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle('Distribution of purse seine tuna catch')

# PS ass
ggplot()+
  # geom_contour_filled(data=PS_catch_ass %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_ass %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle('Distribution of purse seine associated tuna catch')

# PS una
ggplot()+
  # geom_contour_filled(data=PS_catch_una %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_una %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle('Distribution of purse seine unassociated tuna catch')

# LL
ggplot()+
  #geom_contour_filled(data=LL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=LL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle('Distribution of longline tuna catch')

# PL
ggplot()+
  #geom_contour_filled(data=PL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=PL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle('Distribution of pole-and-line tuna catch')

# Other gear
ggplot()+
  #geom_contour_filled(data=OTH_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=OTH_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle('Distribution of other tuna catch')
dev.off()

### --- Calculate annual centers of gravity (COG) and inertia 

# Function for annual values
ann.cg.inert = function(dat, catch, lab){
  cgi = c()
  
  for(i in 1:length(unique(dat$yy))){
    CG = dat %>% filter(yy==unique(yy)[i]) %>% select(lon,lat,catch,yy) 
    names(CG)[3] = 'catch'
    CG %<>%  group_by(lon,lat) %>% summarize(z=sum(as.numeric(catch),na.rm=T))
    db = db.create(x1=CG$lon, x2=CG$lat, z1=CG$z)
    projec.define(projection="mean",db=db)
    
    projec.toggle(0)
    
    png(paste0('./',lab,'_',unique(dat$yy)[i],'.png'),width=10,height=8, units='in',res=300)
    plot(db,title="",asp=1,inches=5, col='DodgerBlue', alpha=0.4, cex.axis=2, cex.main=3, ylim=c(-50,50), xlim=c(110,235)) 
    map("worldHires",add=T,fill=T,col='gray90')
    CG = SI.cgi(db, flag.plot=T,flag.inertia=TRUE,flag.ellipse=T,col=1,lwd=2, density=25)
    dev.off()
    
    cgi  = rbind(cgi, c("yy"=unique(dat$yy)[i], "CG"=CG$center, "inertia"=CG$inertia, "iso"=CG$iso))
  }    
  cgi %<>% data.frame() %>% rename(lon=CG1, lat=CG2)
  return(cgi)
}

# Get COG and inertia, by gear type
PS_all.cg = ann.cg.inert(PS_catch_all, catch='mt', lab='PS_all_annual')
PS_ass.cg = ann.cg.inert(PS_catch_ass, catch='mt', lab='PS_ass_annual')
PS_una.cg = ann.cg.inert(PS_catch_una, catch='mt', lab='PS_una_annual')
LL.cg = ann.cg.inert(LL_catch, catch='mt', lab='LL_annual')
PL.cg = ann.cg.inert(PL_catch, catch='mt', lab='PL_annual')
OTH.cg = ann.cg.inert(OTH_catch, catch='mt', lab='OTH_annual')

# Plot COG
pdf("COGs.pdf")
ggplot(PS_all.cg, aes(yy,lon))+ geom_line(aes(col='PS_all')) + geom_point() + xlab('Year') + 
  geom_line(data=PS_ass.cg, aes(yy,lon, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,lon, col='PS_una'))+
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Purse seine catch (annual)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')

ggplot(PS_all.cg, aes(yy,lat))+ geom_line(aes(col='PS_all')) + geom_point() + xlab('Year') + 
  geom_line(data=PS_ass.cg, aes(yy,lat, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,lat, col='PS_una'))+
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Purse seine catch (annual)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')

ggplot(LL.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Longline catch (annual)')

ggplot(LL.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Longline catch (annual)')

ggplot(PL.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Pole-and-line catch (annual)')

ggplot(PL.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Pole-and-line catch (annual)')

ggplot(OTH.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Other catch (annual)')

ggplot(OTH.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Other catch (annual)')
dev.off()

# Plot inertia
pdf("Inertia.pdf")
ggplot(PS_all.cg, aes(yy, inertia))+geom_line(aes(col='PS_all')) + geom_point() + gg.theme + ylab('Inertia') + 
  geom_line(data=PS_ass.cg, aes(yy,inertia, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,inertia, col='PS_una'))+
  xlab('Year') + ggtitle('Purse seine catch (annual)') + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
ggplot(LL.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle('Longline catch (annual)')
ggplot(PL.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle('Pole-and-line catch (annual)')
ggplot(OTH.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle('Other catch (annual)')
dev.off()

# Save to file
write.csv(PS_all.cg, "PS_all.cg.csv", row.names = F)
write.csv(PS_ass.cg, "PS_ass.cg.csv", row.names = F)
write.csv(PS_una.cg, "PS_una.cg.csv", row.names = F)
write.csv(LL.cg, "LL.cg.csv", row.names = F)
write.csv(PL.cg, "PL.cg.csv", row.names = F)
write.csv(OTH.cg, "OTH.cg.csv", row.names = F)


### --- Area occupied (AO)

# Calculate for PS (all sets types combined) and LL.
# Use 1° x 1° data from PS ('s_best') and 5° x 5° data from LL (l_best).

# Extract and organise data 
db1 <- "driver=SQL Server;server=nouSQL03;database=LOG_MASTER"
channel <- odbcDriverConnect(db1)

s_catch_query <- "
SELECT      s.S_BEST_ID, YY, MM, lat_short, lon_short, ez_aprx_code, days,
            Schass_code, sp_code, sp_mt
FROM        best.S_BEST_AGG s
LEFT OUTER JOIN  best.S_BEST_AGG_CATCH c on s.S_BEST_ID = c.S_BEST_ID
WHERE       s.GEAR_CODE like 'S' and YY>1999 and YY<=2020
"
s_catch <- sqlQuery(channel, s_catch_query, as.is=TRUE)

l_catch_query <- "
SELECT      l.L_BEST_ID, YY, MM, lat_short, lon_short, sp_code, sp_n, sp_mt
FROM        best.L_BEST_AGG l
LEFT OUTER JOIN  best.L_BEST_AGG_CATCH c on l.L_BEST_ID = c.L_BEST_ID
WHERE       l.GEAR_CODE like 'L' and YY>1999 and YY<=2020
"
l_catch <- sqlQuery(channel, l_catch_query, as.is=TRUE)

odbcCloseAll()

# Format geographic coordinates  
# Use SW corner of grid cells (1° x 1° for PS, 5° x 5° for LL) as catch location (mirroring fishery effort indicators)
s_catch %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                    lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                    lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
                    select(-c(lon_short, lon.dir, lat_short, lat.dir))

l_catch %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                    lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                    lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
                    select(-c(lon_short, lon.dir, lat_short, lat.dir))

# Check that points are in WCPFC-CA
s_catch$WCP_CA = point.in.polygon(s_catch$lon, s_catch$lat, coords[,1], coords[,2])
s_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
l_catch$WCP_CA = point.in.polygon(l_catch$lon, l_catch$lat, coords[,1], coords[,2])
l_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)

# Filter for tunas
s_catch %<>% filter(sp_code %in% c("SKJ", "YFT", "BET"))
l_catch %<>% filter(sp_code %in% c("SKJ", "YFT", "BET", "ALB"))

# Split ALB into North and South Pacific stocks 
s_catch$sp.stk_code<-s_catch$sp_code
l_catch$sp.stk_code<-ifelse(l_catch$sp_code=="ALB" & l_catch$lat>0, "ALB.N", ifelse(l_catch$sp_code=="ALB" & l_catch$lat<=0, "ALB.S", l_catch$sp_code))

# Split PS data by set type
s_catch_ass = s_catch %>% filter(Schass_code %in% ASS)
s_catch_una = s_catch %>%  filter(Schass_code %in% UNA)

# Function for AO annual values
area.occ = function(dat, catch, res){
  library(raster)
  # get area of each cell
  grid.df <- expand.grid('Lat'=seq(min(dat$lat),max(dat$lat),res),
                         'Lon'=seq(min(dat$lon),max(dat$lon),res))
  r <- raster(ext = extent(min(dat$lon), max(dat$lon)+res, min(dat$lat), 
                           max(dat$lat)+res), res=c(res,res))
  grid.df$Area_km2 =  area(r)@data@values
  detach('package:raster')
  
  ao = dat %>% filter(catch>0) %>% 
    left_join(grid.df %>%  rename(lat=Lat, lon=Lon)) %>% 
    mutate(cell = paste(lon, lat, sep=':')) %>%      
    select(YY, cell, Area_km2) %>% distinct() %>% 
    group_by(YY) %>% 
    summarise(cells = n_distinct(cell), area = sum(Area_km2))
  return(ao)
  
}

# Calculate by gear type
PS_all.ao = area.occ(s_catch, catch='sp_mt', res=1) # 1° x 1° resolution
PS_ass.ao = area.occ(s_catch_ass, catch='sp_mt', res=1) # 1° x 1° resolution
PS_una.ao = area.occ(s_catch_una, catch='sp_mt', res=1) # 1° x 1° resolution
LL.ao = area.occ(l_catch, catch='sp_mt', res=5) # 5° x 5° resolution

# Plot by gear type
pdf("Area occupied.pdf")
ggplot(PS_all.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine all sets area occupied')
ggplot(PS_ass.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine associated area occupied')
ggplot(PS_una.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine unassociated area occupied')
ggplot(LL.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle('Longline area occupied')
ggplot(PS_all.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine all sets area occupied')
ggplot(PS_ass.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine associated area occupied')
ggplot(PS_una.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle('Purse seine unassociated area occupied')
ggplot(LL.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle('Longline area occupied')
dev.off()

# Save to file
write.csv(PS_all.ao, "PS_all.ao.csv", row.names = F)
write.csv(PS_ass.ao, "PS_ass.ao.csv", row.names = F)
write.csv(PS_una.ao, "PS_una.ao.csv", row.names = F)
write.csv(LL.ao, "LL.ao.csv", row.names = F)


#################################################
## Indicators by species or stock
#################################################

sp.stk = c('SKJ', 'YFT', 'BET', 'ALB.S', 'ALB.N')

for (i in unique(sp.stk)){
  setwd(paste("P:/OFPEMA/WCPFC/SC17/Indicators/Catch/",i, sep=""))
  sp.stk_catch<-tuna_catch[tuna_catch$sp_code==i,]

# Sort data by gear type
PS_catch_all = sp.stk_catch %>% filter(school %in% c(1,-9,3,4,5,6,7)) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # all PS catch
PS_catch_ass = sp.stk_catch %>% filter(school %in% ASS) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # catch for PS associated sets
PS_catch_una = sp.stk_catch %>%  filter(school %in% UNA) %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # catch for PS unassociated sets
LL_catch = sp.stk_catch %>% filter(gear == "L") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # LL catch
PL_catch = sp.stk_catch %>% filter(gear == "P") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # PL catch
OTH_catch = sp.stk_catch %>% filter(gear == "Z") %>% group_by(yy, lat, lon) %>% summarise(mt = sum(sp_mt) ) %>% ungroup() # Other gear catch

# Maps of catch by gear type
# PS all
pdf(paste("Catch distributions_",i,".pdf", sep=""))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
ggplot()+
  # geom_contour_filled(data=PS_catch_all %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_all %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle(paste('Distribution of purse seine', i, 'catch'))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

# PS ass
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
ggplot()+
  # geom_contour_filled(data=PS_catch_ass %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_ass %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle(paste('Distribution of purse seine associated', i, 'catch'))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

# PS una
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
ggplot()+
  # geom_contour_filled(data=PS_catch_una %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=PS_catch_una %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40')+
  ggtitle(paste('Distribution of purse seine unassociated', i, 'catch'))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

# LL
print(ggplot()+
  #geom_contour_filled(data=LL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=LL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle(paste('Distribution of longline', i, 'catch')))

# PL
print(ggplot()+
  #geom_contour_filled(data=PL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=PL_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle(paste('Distribution of pole-and-line', i, 'catch')))

# Other gear
print(ggplot()+
  #geom_contour_filled(data=OTH_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=OTH_catch %>% group_by(lon,lat,yy) %>% summarise(mt=sum(mt)), aes(x=lon, y=lat, fill=mt)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~yy) + coord_equal()+theme_bw()+labs(fill='Catch (mt)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8))+
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40') + 
  ggtitle(paste('Distribution of other', i, 'catch')))
dev.off()


### --- Calculate annual centers of gravity (COG) and inertia, by species/stock 

# Get COG and inertia, by gear type
tryCatch({
if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  PS_all.cg = ann.cg.inert(PS_catch_all, catch='mt', lab='PS_all_annual')
  PS_ass.cg = ann.cg.inert(PS_catch_ass, catch='mt', lab='PS_ass_annual')
  PS_una.cg = ann.cg.inert(PS_catch_una, catch='mt', lab='PS_una_annual')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

LL.cg = ann.cg.inert(LL_catch, catch='mt', lab='LL_annual')
PL.cg = ann.cg.inert(PL_catch, catch='mt', lab='PL_annual')
OTH.cg = ann.cg.inert(OTH_catch, catch='mt', lab='OTH_annual')

# Plot COG
pdf(paste("COGs_",i,".pdf", sep=""))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  ggplot(PS_all.cg, aes(yy,lon))+ geom_line(aes(col='PS_all')) + geom_point() + xlab('Year') + 
  geom_line(data=PS_ass.cg, aes(yy,lon, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,lon, col='PS_una'))+
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Purse seine', i, 'catch (annual)'))  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
ggplot(PS_all.cg, aes(yy,lat))+ geom_line(aes(col='PS_all')) + geom_point() + xlab('Year') + 
  geom_line(data=PS_ass.cg, aes(yy,lat, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,lat, col='PS_una'))+
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Purse seine', i, 'catch (annual)'))  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))

print(ggplot(LL.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Longline', i, 'catch (annual)')))

print(ggplot(LL.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Longline', i, 'catch (annual)')))

print(ggplot(PL.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Pole-and-line', i, 'catch (annual)')))

print(ggplot(PL.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Pole-and-line', i, 'catch (annual)')))

print(ggplot(OTH.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Other', i, 'catch (annual)')))

print(ggplot(OTH.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle(paste('Other', i, 'catch (annual)')))
dev.off()

# Plot inertia
pdf(paste("Inertia_",i,".pdf", sep=""))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
ggplot(PS_all.cg, aes(yy, inertia))+geom_line(aes(col='PS_all')) + geom_point() + gg.theme + ylab('Inertia') + 
  geom_line(data=PS_ass.cg, aes(yy,inertia, col='PS_ass'))+geom_line(data=PS_una.cg, aes(yy,inertia, col='PS_una'))+
  xlab('Year') + ggtitle(paste('Purse seine', i, 'catch (annual)')) + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(ggplot(LL.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle(paste('Longline', i, 'catch (annual)')))
print(ggplot(PL.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle(paste('Pole-and-line', i, 'catch (annual)')))
print(ggplot(OTH.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle(paste('Other', i, 'catch (annual)')))
dev.off()

# Save to file
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
write.csv(PS_all.cg, "PS_all.cg.csv", row.names = F)
write.csv(PS_ass.cg, "PS_ass.cg.csv", row.names = F)
write.csv(PS_una.cg, "PS_una.cg.csv", row.names = F)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
write.csv(LL.cg, "LL.cg.csv", row.names = F)
write.csv(PL.cg, "PL.cg.csv", row.names = F)
write.csv(OTH.cg, "OTH.cg.csv", row.names = F)

}


### --- Area occupied (AO), by species/stock

for (i in unique(sp.stk)){
  setwd(paste("P:/OFPEMA/WCPFC/SC17/Indicators/Catch/",i, sep=""))
  s_catch_sp.stk<-s_catch[s_catch$sp.stk_code==i,]
  s_catch_ass_sp.stk<-s_catch_ass[s_catch_ass$sp.stk_code==i,]
  s_catch_una_sp.stk<-s_catch_una[s_catch_una$sp.stk_code==i,]
  l_catch_sp.stk<-l_catch[l_catch$sp.stk_code==i,]
  
# Calculate by gear type
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
PS_all.ao = area.occ(s_catch_sp.stk, catch='sp_mt', res=1)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_ass has zero rows")
PS_ass.ao = area.occ(s_catch_ass_sp.stk, catch='sp_mt', res=1)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_ass has zero rows")
PS_una.ao = area.occ(s_catch_una_sp.stk, catch='sp_mt', res=1)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
LL.ao = area.occ(l_catch_sp.stk, catch='sp_mt', res=5)

# Plot by gear type
pdf(paste("Area occupied_",i,".pdf", sep=""))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  ggplot(PS_all.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine all sets area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_ass has zero rows")
  ggplot(PS_ass.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine associated area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_una has zero rows")
  ggplot(PS_una.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine unassociated area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(ggplot(LL.ao, aes(YY,cells))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Longline area occupied_', i)))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  ggplot(PS_all.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine all sets area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_ass has zero rows")
  ggplot(PS_ass.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine associated area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_una has zero rows")
  ggplot(PS_una.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Purse seine unassociated area occupied_', i))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}))
print(ggplot(LL.ao, aes(YY,area))+geom_line()+ geom_point() +gg.theme + ggtitle(paste('Longline area occupied_', i)))
dev.off()

# Save to file
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  write.csv(PS_all.ao, "PS_all.ao.csv", row.names = F)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  write.csv(PS_ass.ao, "PS_ass.ao.csv", row.names = F)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
tryCatch({
  if (i %in% c('ALB.S','ALB.N')) stop("PS_catch_all has zero rows")
  write.csv(PS_una.ao, "PS_una.ao.csv", row.names = F)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
write.csv(LL.ao, "LL.ao.csv", row.names = F)

}


#################################################
## Package data for SC17 paper
#################################################

## --- All species, and by species/stock - SKJ, YFT, BET

# COG longitude
species<-c('All species', 'SKJ', 'YFT', 'BET')
for (i in unique(species)){

  dir<-paste("P:/OFPEMA/WCPFC/SC17/Indicators/Catch/", i, sep="")
  setwd(dir)
  myfiles = list.files(path=dir, pattern="*cg.csv", full.names=F)
  myfiles
  dat<-do.call(cbind, lapply(myfiles, read.csv))
  sp_lon<-dat[,c(1,seq(2,27,5))]
  colnames(sp_lon)<-c("yy", unlist(strsplit(myfiles, "[.]"))[seq(1,16,3)])
  write.csv(sp_lon, paste(i,"_lon.csv", sep=""), row.names = F)
  
  # COG latitude
  sp_lat<-dat[,c(1,seq(3,28,5))]
  colnames(sp_lat)<-c("yy", unlist(strsplit(myfiles, "[.]"))[seq(1,16,3)])
  write.csv(sp_lat, paste(i,"_lat.csv", sep=""), row.names = F)
  
  # Area occupied - cells and km^2
  myfiles = list.files(path=dir, pattern="*ao.csv", full.names=F)
  myfiles
  dat<-do.call(cbind, lapply(myfiles, read.csv))
  sp_ao<-dat[,c(1,2,3,5,6,8,9,11,12)]
  colnames(sp_ao)<-c("yy", paste(unlist(strsplit(myfiles, "[.]"))[1], "cells", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[1], "area", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[4], "cells", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[4], "area", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[7], "cells", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[7], "area", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[10], "cells", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[10], "area", sep="."))
  write.csv(sp_ao, paste(i,"_ao.csv", sep=""), row.names = F)
}


## --- ALB.S and ALB.N

# COG longitude
species<-c('ALB.S', 'ALB.N')

for (i in unique(species)){
  
  dir<-paste("P:/OFPEMA/WCPFC/SC17/Indicators/Catch/", i, sep="")
  setwd(dir)
  myfiles = list.files(path=dir, pattern="*cg.csv", full.names=F)
  myfiles
  file_list<-lapply(myfiles, read.csv)
  dat<-merge(file_list[[1]], file_list[[2]], by="yy", all=T)
  dat<-merge(dat, file_list[[3]], by="yy", all=T)
  sp_lon<-dat[,c(1,seq(2,10,4))]
  colnames(sp_lon)<-c("yy", unlist(strsplit(myfiles, "[.]"))[seq(1,7,3)])
  write.csv(sp_lon, paste(i,"_lon.csv", sep=""), row.names = F)
  
  # COG latitude
  sp_lat<-dat[,c(1,seq(3,11,4))]
  colnames(sp_lat)<-c("yy", unlist(strsplit(myfiles, "[.]"))[seq(1,7,3)])
  write.csv(sp_lat, paste(i,"_lat.csv", sep=""), row.names = F)
  
  # Area occupied - cells and km^2
  myfiles = list.files(path=dir, pattern="*ao.csv", full.names=F)
  myfiles
  file_list<-lapply(myfiles, read.csv)
  sp_ao<-file_list[[1]]
  colnames(sp_ao)<-c("yy", paste(unlist(strsplit(myfiles, "[.]"))[1], "cells", sep="."),
                     paste(unlist(strsplit(myfiles, "[.]"))[1], "area", sep="."))
  write.csv(sp_ao, paste(i,"_ao.csv", sep=""), row.names = F)
}

# Save as .Rdata files
# COG_longitude
dir<-"P:/OFPEMA/WCPFC/SC17/Indicators/Catch/"
setwd(dir)
myfiles = list.files(path=dir, pattern="*lon.csv", full.names=F, recursive = T)
myfiles
lon_list<-lapply(myfiles, read.csv)
names(lon_list)<- paste(unlist(strsplit(myfiles, "[/]"))[seq(1,11,2)], "_lon", sep="")
save(lon_list, file="COG_longitude.RData")

# COG_latitude
dir<-"P:/OFPEMA/WCPFC/SC17/Indicators/Catch/"
setwd(dir)
myfiles = list.files(path=dir, pattern="*lat.csv", full.names=F, recursive = T)
myfiles
lat_list<-lapply(myfiles, read.csv)
names(lat_list)<- paste(unlist(strsplit(myfiles, "[/]"))[seq(1,11,2)], "_lat", sep="")
save(lat_list, file="COG_latitude.RData")

# Area occupied
dir<-"P:/OFPEMA/WCPFC/SC17/Indicators/Catch/"
setwd(dir)
myfiles = list.files(path=dir, pattern="*_ao.csv", full.names=F, recursive = T)
myfiles
ao_list<-lapply(myfiles, read.csv)
names(ao_list)<- paste(unlist(strsplit(myfiles, "[/]"))[seq(1,11,2)], "_ao", sep="")
save(ao_list, file="Area occupied.RData")

## END ##





     