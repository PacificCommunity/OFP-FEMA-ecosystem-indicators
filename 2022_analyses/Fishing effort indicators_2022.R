#########################################################
# Fishing Effort Indicators - for SC18 paper 2022
#########################################################

# Created on: 28/06/22
# Latest update: 30/06/22
# Created by: Jed Macdonald and Tiffany Vidal

# Code to update the Fishing Effort Indicators first produced in 2021 for SC17.
# Key changes from 2021:
# 1) Changed from RMarkdown to standard R script.
# 2) Addition of new data from 1990 to 2020 inclusive.
# 3) Addition of a new indicator capturing the temporal trend in the proportion of purse seine effort 
#    inside high seas areas (HS) between 1990 & 2020.      


library(tidyverse)
library(magrittr)
library(maps)
library(zoo)
library(RODBC)
library(sp)
library(mapdata)
library(formatR)
library(RGeostats)

setwd('P:/OFPEMA/WCPFC/SC18/Ecosystem indicators/Effort/')
newmap = fortify(maps::map(wrap=c(0,360), plot=FALSE, fill=TRUE))


gg.theme <- theme_bw() + 
            theme(axis.line = element_line(color="black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  text = element_text(size=10),
                  axis.title = element_text(size=10),
                  strip.background =element_rect(fill="white"),
                  strip.text = element_text(size=6),
                  legend.box.background = element_blank(),
                  legend.background = element_blank(),
                  panel.background = element_rect(fill = NA, color = "black"))  


db1 <- "driver=SQL Server;server=nouSQL03;database=LOG_MASTER"
channel <- odbcDriverConnect(db1)

## S_BEST purse seine catch and effort
s_eff_query <- "
SELECT      s.S_BEST_ID, YY, MM, lat_short, lon_short, ez_aprx_code, days,
            sh.Schass_code, days_sch, sets_sch
FROM        best.S_BEST_AGG s
LEFT OUTER JOIN  best.S_BEST_AGG_SCHEFF sh on s.S_BEST_ID = sh.S_BEST_ID
WHERE       s.GEAR_CODE like 'S' and YY>1989 and YY<2021
"
s_eff <- sqlQuery(channel, s_eff_query, as.is=TRUE)

s_catch_query <- "
SELECT      s.S_BEST_ID, YY, MM, lat_short, lon_short, ez_aprx_code, days,
            Schass_code, sp_code, sp_mt
FROM        best.S_BEST_AGG s
LEFT OUTER JOIN  best.S_BEST_AGG_CATCH c on s.S_BEST_ID = c.S_BEST_ID
WHERE       s.GEAR_CODE like 'S' and YY>1989 and YY<2021
"
s_catch <- sqlQuery(channel, s_catch_query, as.is=TRUE)


## L_BEST longline catch and effort
l_eff_query <- "
SELECT      L_BEST_ID, YY, MM, lat_short, lon_short, hhooks, std_effort
FROM        best.L_BEST_AGG
WHERE       GEAR_CODE like 'L' and YY>1989 and YY<2021
"
l_eff <- sqlQuery(channel, l_eff_query, as.is=TRUE)

l_catch_query <- "
SELECT      l.L_BEST_ID, YY, MM, lat_short, lon_short, sp_code, sp_n, sp_mt
FROM        best.L_BEST_AGG l
LEFT OUTER JOIN  best.L_BEST_AGG_CATCH c on l.L_BEST_ID = c.L_BEST_ID
WHERE       l.GEAR_CODE like 'L' and YY>1989 and YY<2021
"
l_catch <- sqlQuery(channel, l_catch_query, as.is=TRUE)

# Close the database connection
odbcCloseAll()


########################################################
# Spatial domain
########################################################
wcp_ca = read.csv("wcpfc_ca_stat_area.csv") # read in WCPFC-CA boundary info

# Convert to spatial polygon
wcp = Polygon(wcp_ca)
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Check that points are in WCPFC-CA
coords = wcp@polygons[[1]]@Polygons[[1]]@coords

# Format data and filter out observations outside WCPFC-CA
s_eff %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                  lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                  lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
  select(-c(lon_short, lon.dir, lat_short, lat.dir)) %>%  
  mutate(days_sch = as.numeric(days_sch), sets_sch = as.numeric(sets_sch))

s_catch %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                  lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                  lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
  select(-c(lon_short, lon.dir, lat_short, lat.dir))

l_eff %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                  lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                  lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
  select(-c(lon_short, lon.dir, lat_short, lat.dir)) %>% 
  mutate(hhoks = as.numeric(hhooks), std_effort = as.numeric(std_effort))

l_catch %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                  lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                  lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
  select(-c(lon_short, lon.dir, lat_short, lat.dir))

# Filter out s_eff records outside WCPFC-CA
s_eff$WCP_CA = point.in.polygon(s_eff$lon, s_eff$lat, coords[,1], coords[,2])
  s_eff %<>% filter(WCP_CA %in% c(1,2)) %>% select(-WCP_CA) 

# Set school association codes
ASS <- c(4,3) # DFAD only
AFAD = c(5) # anchored FAD
ASSWH = c(6,7) # whale and whale shark sets
UNA <- c(1,2) # free school
  
# Aggregate all set types together, and disaggregate by DFAD and free school sets
s_eff_all = s_eff %>% filter(Schass_code %in% c(1,2,3,4,5,6,7)) %>% 
  group_by(YY,MM, lat, lon, ez_aprx_code) %>%
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup()
s_eff_fad = s_eff %>% filter(Schass_code %in% ASS) %>%
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  group_by(YY,MM, lat, lon, ez_aprx_code) %>% 
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup()
s_eff_free = s_eff %>%  filter(Schass_code %in% UNA) %>% 
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  group_by(YY,MM, lat, lon, ez_aprx_code) %>% 
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup()

# Filter out s_catch records outside WCPFC-CA 
s_catch$WCP_CA = point.in.polygon(s_catch$lon, s_catch$lat, coords[,1], coords[,2])
  s_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
# Filter out l_eff records outside WCPFC-CA 
l_eff$WCP_CA = point.in.polygon(l_eff$lon, l_eff$lat, coords[,1], coords[,2])
  l_eff %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA) 
  l_eff %<>% mutate(hhooks = as.numeric(hhooks))
# Filter out l_catch records outside WCPFC-CA 
l_catch$WCP_CA = point.in.polygon(l_catch$lon, l_catch$lat, coords[,1], coords[,2])
  l_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
  

# Plots of longline and purse seine effort distribution by year
pdf("./Figures/LL effort distribution.pdf")
  ggplot()+
  #geom_contour_filled(data=l_eff %>% group_by(lon,lat,YY) %>% summarise(hhooks=log(sum(hhooks))), aes(x=lon, y=lat, z=hhooks), bins=12) + 
  geom_tile(data=l_eff %>% group_by(lon,lat,YY) %>% summarise(hhooks=log(sum(hhooks))), aes(x=lon, y=lat, fill=hhooks)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~YY) + coord_equal()+theme_bw()+labs(fill='Effort (log hundred hooks)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_blank(), panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(), axis.text = element_text(size=4)) +
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40', size=0.2) + 
  ggtitle('Distribution of longline effort')
dev.off()

pdf("./Figures/PS effort distribution_all.pdf")
ggplot()+
 # geom_contour_filled(data=s_eff_all %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=s_eff_all %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, fill=sets)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~YY) + coord_equal()+theme_bw()+labs(fill='Effort (log sets)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_blank(), panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(), axis.text = element_text(size=4)) +
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40', size=0.2)+
  ggtitle('Distribution of all purse seine effort')
dev.off()

pdf("./Figures/PS effort distribution_dFAD.pdf")
ggplot()+
  # geom_contour_filled(data=s_eff_all %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=s_eff_fad %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, fill=sets)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~YY) + coord_equal()+theme_bw()+labs(fill='Effort (log sets)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_blank(), panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(), axis.text = element_text(size=4)) +
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40', size=0.2)+
  ggtitle('Distribution of purse seine effort on dFADs')
dev.off()

pdf("./Figures/PS effort distribution_free.pdf")
ggplot()+
  # geom_contour_filled(data=s_eff_all %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, z=sets), bins=12) + 
  geom_tile(data=s_eff_free %>% group_by(lon,lat,YY) %>% summarise(sets=log(sum(sets,na.rm=T))), aes(x=lon, y=lat, fill=sets)) + 
  scale_fill_distiller(palette='Spectral') + facet_wrap(~YY) + coord_equal()+theme_bw()+labs(fill='Effort (log sets)')+
  xlim(c(110,210))+ylim(c(-50,50))+
  xlab('Longitude')+ylab('Latitude')+theme(strip.background = element_blank(), panel.grid.major = element_blank(), 
                                           panel.grid.minor = element_blank(), axis.text = element_text(size=4)) +
  geom_map(data=newmap, map=newmap, aes(map_id=region), fill='gray80', col='gray40', size=0.2)+
  ggtitle('Distribution of purse seine effort on free schools')
dev.off()



####################################################################################
# Calculate centre of gravity and inertia
####################################################################################

# Calculate annual values
ann.cg.inert = function(dat, effort, lab){
cgi = c()

for(i in 1:length(unique(dat$YY))){
  CG = dat %>% filter(YY==unique(YY)[i]) %>% select(lon,lat,effort,YY)
  names(CG)[3] = 'effort'
  CG %<>% group_by(lon,lat) %>% summarize(z=sum(as.numeric(effort),na.rm=T))
  db = db.create(x1=CG$lon, x2=CG$lat, z1=CG$z)
  projec.define(projection="mean",db=db)
  
  projec.toggle(0)
  CG = SI.cgi(db, flag.plot=F)
  cgi = rbind(cgi, c("yy"=unique(dat$YY)[i], "CG"=CG$center,
  "inertia"=CG$inertia, "iso"=CG$iso))
}
cgi %<>% data.frame() %>% rename(lon=CG1, lat=CG2)
return(cgi)
}

ll.cg = ann.cg.inert(l_eff, effort='hhooks', lab='ll_annual')
ps.cg = ann.cg.inert(s_eff_all, effort='sets', lab='ps_annual')
fad.cg = ann.cg.inert(s_eff_fad, effort='sets', lab='fad_annual')
free.cg = ann.cg.inert(s_eff_free, effort='sets', lab='free_annual')

# Plot central tendency
pdf("./Figures/Annual centre of gravity.pdf", height=4, width=6)
ggplot(ll.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Longline effort (annual)')
ggplot(ll.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + 
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Longline effort (annual)')
ggplot(ps.cg, aes(yy,lon))+ geom_line(aes(col='All')) + geom_point() + xlab('Year') + 
  geom_line(data=fad.cg, aes(yy,lon, col='dFAD'))+geom_line(data=free.cg, aes(yy,lon, col='Free'))+
  ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Purse seine sets (annual)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
ggplot(ps.cg, aes(yy,lat))+ geom_line(aes(col='All')) + geom_point() + xlab('Year') + 
    geom_line(data=fad.cg, aes(yy,lat, col='dFAD'))+geom_line(data=free.cg, aes(yy,lat, col='Free'))+
  ylab('Latitude \n (central tendency)') + gg.theme + 
  ggtitle('Purse seine sets (annual)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()

# Plot inertia
pdf("./Figures/Annual inertia.pdf", height=4, width=6)
ggplot(ll.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + 
  xlab('Year') + ggtitle('Longline effort (annual)')
ggplot(ps.cg, aes(yy, inertia))+geom_line(aes(col='All')) + geom_point() + gg.theme + ylab('Inertia') + 
    geom_line(data=fad.cg, aes(yy,inertia, col='dFAD'))+geom_line(data=free.cg, aes(yy,inertia, col='Free'))+
  xlab('Year') + ggtitle('Purse seine effort (annual)') + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()


# Calculate seasonal values
seas.cg.inert = function(dat, effort){
cgi = c()
  for(i in 1:length(unique(dat$YY))){
  for(s in unique(dat$qtr)){
  CG = dat %>% filter(YY==unique(YY)[i], qtr==s) %>%
               select(lon,lat,effort,YY,qtr)
  names(CG)[3] = 'effort'
  CG = db.create(x1=CG$lon, x2=CG$lat, z1=CG$effort)
  projec.toggle(0)
  CG = SI.cgi(CG, flag.plot=F)
  cgi = rbind(cgi, c("yy"=unique(dat$YY)[i], "qtr" = s, "CG"=CG$center, "inertia"=CG$inertia, "iso"=CG$iso))
  }
}
cgi %<>% data.frame() %>% rename(lon=CG1, lat=CG2)
return(cgi)
}

ll.seas.cg = seas.cg.inert(l_eff %>% mutate(qtr = ceiling(MM/3)), effort='hhooks')
ps.seas.cg = seas.cg.inert(s_eff_all %>% mutate(qtr = ceiling(MM/3)), effort='sets')
fad.seas.cg = seas.cg.inert(s_eff_fad %>% mutate(qtr = ceiling(MM/3)), effort='sets')
free.seas.cg = seas.cg.inert(s_eff_free %>% mutate(qtr = ceiling(MM/3)), effort='sets')

# Plot central tendency
pdf("./Figures/Seasonal centre of gravity.pdf", height=4, width=6)
ggplot(ll.seas.cg, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') +  ylab('Longitude \n (central tendency)')  + gg.theme + facet_wrap(~qtr) + 
  ggtitle('Longline effort (seasonal)')
ggplot(ll.seas.cg, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') +  ylab('Latitude  \n (central tendency)') + gg.theme  + facet_wrap(~qtr) +
  ggtitle('Longline effort (seasonal)')
ggplot(ps.seas.cg, aes(yy,lon))+ geom_line(aes(col='All')) + geom_point() + 
  geom_line(data=fad.seas.cg, aes(yy,lon, col='dFAD'))+geom_line(data=free.seas.cg, aes(yy,lon, col='Free'))+
  xlab('Year') +  ylab('Longitude \n (central tendency)')  + gg.theme  + facet_wrap(~qtr)+
  ggtitle('Purse seine effort (seasonal)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
ggplot(ps.seas.cg, aes(yy,lat))+ geom_line(aes(col='All')) + geom_point() + 
  geom_line(data=fad.seas.cg, aes(yy,lat, col='dFAD'))+geom_line(data=free.seas.cg, aes(yy,lat, col='Free'))+
  xlab('Year') +  ylab('Latitude  \n (central tendency)') + gg.theme  + facet_wrap(~qtr)+
  ggtitle('Purse seine effort (seasonal)')  + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()

# Plot inertia
pdf("./Figures/Seasonal inertia.pdf", height=4, width=6)
ggplot(ll.seas.cg, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + xlab('Year') + facet_wrap(~qtr) + ggtitle('Longline effort (seasonal)')
ggplot(ps.seas.cg, aes(yy, inertia)) + geom_line(aes(col='All')) + geom_point() + geom_line(data=fad.seas.cg, aes(yy,inertia, col='dFAD'))+geom_line(data=free.seas.cg, aes(yy,inertia, col='Free'))+
   gg.theme + ylab('Inertia') + xlab('Year') + facet_wrap(~qtr) + ggtitle('Purse seine effort (seasonal)')  + 
  scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()


# Calculate 5-yr rolling average values - with mid-point year as reference
roll5.cg.inert = function(dat, effort){
cgi  = c()

  for(i in 1:(length(unique(dat$YY))-4)){
  CG = dat %>% filter(YY  %in%  c(unique(YY)[c(i:(i+4))]))  %>% 
               select(lon,lat,effort,YY)
  names(CG)[3] = 'effort'
  CG = db.create(x1=CG$lon,  x2=CG$lat,  z1=CG$effort) 
  projec.toggle(0)
  CG = SI.cgi(CG, flag.plot=F)
  cgi = rbind(cgi,  c("yy"=unique(dat$YY)[i+2],  "CG"=CG$center,
                      "inertia"=CG$inertia,  "iso"=CG$iso))
}
cgi %<>% data.frame() %>% rename(lon=CG1, lat=CG2) 
return(cgi)
}

ll.cg5 = roll5.cg.inert(l_eff, effort='hhooks')
ps.cg5 = roll5.cg.inert(s_eff_all, effort='sets')
fad.cg5 = roll5.cg.inert(s_eff_fad, effort='sets')
free.cg5 = roll5.cg.inert(s_eff_free, effort='sets')

# Plot central tendency
pdf("./Figures/Annual 5-yr centre of gravity.pdf", height=4, width=6)
ggplot(ll.cg5, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + ylab('Longitude \n (central tendency)') + gg.theme + 
  ggtitle('Longline effort (5-yr mean)')
ggplot(ps.cg5 , aes(yy,lon))+ geom_line(aes(col='All')) + geom_point() + 
  geom_line(data=fad.cg5, aes(yy,lon, col='dFAD'))+geom_line(data=free.cg5, aes(yy,lon, col='Free'))+
  xlab('Year') + ylab('Longitude  \n (central tendency)') + gg.theme + ggtitle('Purse seine effort (5-yr mean)') + 
  scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
ggplot(ll.cg5, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + ylab('Latitude \n (central tendency)') + gg.theme +
  ggtitle('Longline effort (5-yr mean)')
ggplot(ps.cg5, aes(yy,lat))+ geom_line(aes(col='All')) + geom_point() + 
  geom_line(data=fad.cg5, aes(yy,lat, col='dFAD')) +geom_line(data=free.cg5, aes(yy,lat, col='Free'))+
  xlab('Year') + ylab('Latitude \n (central tendency)') + gg.theme + ggtitle('Purse seine effort (5-yr mean)') + 
  scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()

# Plot inertia
pdf("./Figures/Annual 5-yr inertia.pdf", height=4, width=6)
ggplot(ll.cg5, aes(yy, inertia))+geom_line()+ geom_point() + gg.theme + ylab('Inertia') + xlab('Year')
ggplot(ps.cg5, aes(yy, inertia))+geom_line(aes(col='All')) + geom_point() +   geom_line(data=fad.cg5, aes(yy,inertia, col='dFAD'))+
  geom_line(data=free.cg5, aes(yy,inertia, col='Free'))+
  gg.theme + ylab('Inertia') + xlab('Year') + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()


# Calculate 5-yr rolling average values, seasonally
roll5.seas.cg.inert = function(dat,  effort){
cgi  = c()
  for(i in 1:(length(unique(dat$YY))-4)){ 
    for(s in unique(dat$qtr)){
    CG  =  dat  %>%  filter(YY  %in%  c(unique(YY)[c(i:(i+4))]), qtr==s)  %>% 
                     select(lon,lat,effort,YY, qtr)
    names(CG)[3] = 'effort'
    CG = db.create(x1=CG$lon,  x2=CG$lat,  z1=CG$effort) 
    projec.toggle(0)
    CG = SI.cgi(CG, flag.plot=F)
    cgi = rbind(cgi, c("yy"=unique(dat$YY)[i+2],'qtr'=s, "CG"=CG$center, 
                       "inertia"=CG$inertia, "iso"=CG$iso))
    }
  }
  cgi %<>%  data.frame() %>%  rename(lon=CG1,  lat=CG2)
  return(cgi)
}

ll.seas.cg5 = roll5.seas.cg.inert(l_eff %>% mutate(qtr = ceiling(MM/3)), effort='hhooks')
ps.seas.cg5 = roll5.seas.cg.inert(s_eff_all %>%  mutate(qtr = ceiling(MM/3)), effort='sets')
fad.seas.cg5 = roll5.seas.cg.inert(s_eff_fad %>%  mutate(qtr = ceiling(MM/3)), effort='sets')
free.seas.cg5 = roll5.seas.cg.inert(s_eff_free %>%  mutate(qtr = ceiling(MM/3)), effort='sets')

# Plot central tendency
pdf("./Figures/Seasonal 5-yr centre of gravity.pdf", height=4, width=6)
ggplot(ll.seas.cg5, aes(yy,lon))+ geom_line() + geom_point() + xlab('Year') + ylab('Longitude \n (central tendency)') + facet_wrap(~qtr) + 
  gg.theme + ggtitle('Longline effort (5-yr mean)')
ggplot(ps.seas.cg5 , aes(yy,lon))+ geom_line(aes(col='All')) + geom_point() + 
     geom_line(data=fad.seas.cg5, aes(yy,lon, col='dFAD'))+geom_line(data=free.seas.cg5, aes(yy,lon, col='Free'))+
  xlab('Year') + ylab('Longitude  \n (central tendency)') + facet_wrap(~qtr) +gg.theme + ggtitle('Purse seine effort (5-yr mean)') + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
ggplot(ll.seas.cg5, aes(yy,lat))+ geom_line() + geom_point() + xlab('Year') + ylab('Latitide \n (central tendency)') + facet_wrap(~qtr) +
  gg.theme  +ggtitle('Longline effort (5-yr mean)')
ggplot(ps.seas.cg5, aes(yy,lat))+ geom_line(aes(col='All')) + geom_point() + 
   geom_line(data=fad.seas.cg5, aes(yy,lat, col='dFAD'))+geom_line(data=free.seas.cg5, aes(yy,lat, col='Free'))+
  xlab('Year') + ylab('Latitude \n (central tendency)') + facet_wrap(~qtr) +gg.theme  +ggtitle('Purse seine effort (5-yr mean)') + scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()

# Plot inertia
pdf("./Figures/Seasonal 5-yr inertia.pdf", height=4, width=6)
ggplot(ll.seas.cg5, aes(yy, inertia))+geom_line() + geom_point() + gg.theme + ylab('Inertia') + xlab('Year')+ facet_wrap(~qtr) 
ggplot(ps.seas.cg5, aes(yy, inertia))+geom_line(aes(col='All')) + geom_point() + gg.theme + ylab('Inertia') + xlab('Year') + facet_wrap(~qtr)  +
   geom_line(data=fad.seas.cg5, aes(yy,inertia, col='dFAD'))+geom_line(data=free.seas.cg5, aes(yy,inertia, col='Free'))+scale_color_manual(values=c('black', 'blue', 'firebrick3')) + labs(col='')
dev.off()


#############################################################################
# Area occupied - 1x1 (PS) cells or 5x5 cells (LL) converted to km^2 fished
#############################################################################

area.occ = function(dat, effort, res){
    library(raster)
    # get area of each cell
    grid.df <- expand.grid('Lat'=seq(min(dat$lat),max(dat$lat),res),'Lon'=seq(min(dat$lon),max(dat$lon),res))
    r <- raster(ext = extent(min(dat$lon), max(dat$lon)+res, min(dat$lat), max(dat$lat)+res), res=c(res,res))
    grid.df$Area_km2 =  area(r)@data@values
    detach('package:raster')
    
    ao = dat %>% filter(effort>0) %>% 
          left_join(grid.df %>%  rename(lat=Lat, lon=Lon)) %>% 
          mutate(cell = paste(lon, lat, sep=':')) %>%      
          select(YY, cell, Area_km2) %>% distinct() %>% 
          group_by(YY) %>% 
          summarise(cells = n_distinct(cell), area = sum(Area_km2)) %>%  
          mutate(cells5 = rollapply(cells,5,mean, align='center',fill=NA),
                 area5 = rollapply(area,5,mean, align='center',fill=NA))
    return(ao)
    }

ll.ao = area.occ(l_eff, effort='hhooks', res=5)
ps.ao = area.occ(s_eff_all, effort='sets', res=1)
fad.ao = area.occ(s_eff_fad, effort='sets', res=1) 
free.ao = area.occ(s_eff_free, effort='sets', res=1) 

pdf("./Figures/Area occupied.pdf", height=4, width=6)
ggplot(ll.ao, aes(YY,area))+geom_line()+ geom_point() + gg.theme + labs(y= "Area occupied (km^2)") + ggtitle('Longline area occupied')
ggplot(ps.ao, aes(YY,area))+geom_line()+ geom_point() + gg.theme + labs(y= "Area occupied (km^2)") + ggtitle('All purse seine area occupied')
ggplot(fad.ao, aes(YY,area))+geom_line()+ geom_point() + gg.theme + labs(y= "Area occupied (km^2)") + ggtitle('DFAD purse seine area occupied')
ggplot(free.ao, aes(YY,area))+geom_line()+ geom_point() + gg.theme + labs(y= "Area occupied (km^2)") + ggtitle('Free-school purse seine area occupied')
dev.off()

####################################################################
# Proportion of purse seine effort inside high seas areas (HS)
####################################################################

## DFAD sets
# Get total no. of sets per year
eff.props<-data.frame(YY=numeric(), tot.effort_fad=numeric(), HS.effort_fad=numeric(),
                      prop.HS_fad=numeric(), tot.effort_free=numeric(), HS.effort_free=numeric(),
                      prop.HS_free=numeric())
for (i in unique(s_eff_fad$YY)){
  dat<-s_eff_fad[s_eff_fad$YY==i,]
  eff.props[i-(min(s_eff_fad$YY)-1),1]<-i
  eff.props[i-(min(s_eff_fad$YY)-1),2]<-sum(dat$sets)
}

# Subset for sets in HS 
unique(s_eff_fad$ez_aprx_code)
s_eff_fad_HS<-s_eff_fad[s_eff_fad$ez_aprx_code%in%c("I1","I2","I3","I4","I5","I6","I7","I8","I9","H4","H5"),]
for (i in unique(s_eff_fad_HS$YY)){
  dat<-s_eff_fad_HS[s_eff_fad_HS$YY==i,]
  eff.props[i-(min(s_eff_fad_HS$YY)-1),3]<-sum(dat$sets)
}

# Calculate and plot proportions
eff.props[,4]<-eff.props$HS.effort_fad/eff.props$tot.effort_fad
plot(eff.props$prop.HS_fad~eff.props$YY, type="l")


## Free school sets
# Get total no. of sets per year
for (i in unique(s_eff_free$YY)){
  dat<-s_eff_free[s_eff_free$YY==i,]
  eff.props[i-(min(s_eff_free$YY)-1),5]<-sum(dat$sets)
}

# Subset for sets in HSs  
unique(s_eff_free$ez_aprx_code)
s_eff_free_HS<-s_eff_free[s_eff_free$ez_aprx_code%in%c("I1","I2","I3","I4","I5","I6","I7","I8","I9","H4","H5"),]
for (i in unique(s_eff_free_HS$YY)){
  dat<-s_eff_free_HS[s_eff_free_HS$YY==i,]
  eff.props[i-(min(s_eff_free_HS$YY)-1),6]<-sum(dat$sets)
}

# Calculate and plot proportions
eff.props[,7]<-eff.props$HS.effort_free/eff.props$tot.effort_free
plot(eff.props$prop.HS_free~eff.props$YY, type="l")

# Combine DFAD and free school on one plot
pdf("./Figures/Effort in HS.pdf", height=4, width=6)
plot(eff.props$prop.HS_fad~eff.props$YY, ylim=c(0.05,0.5), type="l", col="black", lwd=2, xlab="Year", ylab="Proportion of PS effort in HS"); 
lines(eff.props$prop.HS_free~eff.props$YY, col="red", lwd=2);
legend(x=2013, y=0.45, legend=c("DFAD", "Free"), col=c("black", "red"), lwd=2, bty="n")
dev.off()

####################################################################
# Package data 
####################################################################

cg=        list( "ll"=ll.cg, "ps"=ps.cg, "fad"=fad.cg, "free"=free.cg )
seas.cg =  list( "ll"=ll.seas.cg, "ps"=ps.seas.cg, "fad"=fad.seas.cg, "free"=free.seas.cg ) 
cg5 =      list( "ll"= ll.cg5, "ps"=ps.cg5, "fad"=fad.cg5, "free"=free.cg5 )
seas.cg5 = list( "ll"=ll.seas.cg5, "ps"=ps.seas.cg5, "fad"=fad.seas.cg5, "free"=free.seas.cg5 )
ao =       list( "ll"=ll.ao, "ps"=ps.ao, "fad"=fad.ao, "free"=free.ao)
HS.effort =list("eff.props"=eff.props)

# Save all indicators
all.ind = list("cg"=cg, "seas.cg"=seas.cg, "cg5"=cg5, "seas.cg5"=seas.cg5, "ao"=ao, "HS.effort"=HS.effort)
save(all.ind, file="all_effort_indicators_2022.Rdata")

# END #


