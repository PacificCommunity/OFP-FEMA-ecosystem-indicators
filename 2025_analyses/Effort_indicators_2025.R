#########################################################
# Fishing Effort Indicators - for SC19 paper 2023
#########################################################

# Created on: 06/07/23
# Latest update: 14/01/2025
# Created by: Jed Macdonald and Tiffany Vidal
# Updated by Nick Hill 2025

# Code to update the Fishing Effort Indicators first produced in 2021 for SC17.
# Key changes from 2021:

# 1) Changed from RMarkdown to standard R script.
# 2) Addition of new data from 1990 to 2022 inclusive.
# 3) Addition of a new indicator capturing the temporal trend in the proportion of purse seine effort 
#    inside high seas areas (HS) between 1990 and 2022.

# Nick Hill now updating these indicators based on updated needs.

# 1. Preamble ----
library(tidyverse)
#library(magrittr)
#library(maps)
#library(zoo)
library(RODBC)
library(sp)
#library(mapdata)
#library(formatR)
library(RGeostats)
library(RColorBrewer)

# Set dir.
#setwd('P:/OFPEMA/WCPFC/SC19/Ecosystem Indicators/Catch and distribution')
#data_wd <- './2025_analyses/data/'
#results_wd <- "./2025_analyses/results/"
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

# 2. Download effort data from DB ----

# db1 <- "driver=SQL Server;server=nouSQL03;database=LOG_MASTER"
# channel <- odbcDriverConnect(db1)
#NOUFAMESQL04 - new db - replace OFP_DBS with this
db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
channel <- odbcDriverConnect(db1)

## S_BEST purse seine catch and effort
s_eff_query <- "
SELECT      s.S_BEST_ID, YY, MM, FLAG_CODE, FLEET_CODE, lat_short, lon_short, ez_aprx_code, days,
            sh.Schass_code, days_sch, sets_sch
FROM        best.S_BEST_AGG s
LEFT OUTER JOIN  best.S_BEST_AGG_SCHEFF sh on s.S_BEST_ID = sh.S_BEST_ID
WHERE       s.GEAR_CODE like 'S' and YY>1989 and YY<=2023
"
s_eff <- sqlQuery(channel, s_eff_query, as.is=TRUE)

# s_catch_query <- "
# SELECT      s.S_BEST_ID, YY, MM, lat_short, lon_short, ez_aprx_code, days,
#             Schass_code, sp_code, sp_mt
# FROM        best.S_BEST_AGG s
# LEFT OUTER JOIN  best.S_BEST_AGG_CATCH c on s.S_BEST_ID = c.S_BEST_ID
# WHERE       s.GEAR_CODE like 'S' and YY>1989 and YY<=2022
# "
# s_catch <- sqlQuery(channel, s_catch_query, as.is=TRUE)


## L_BEST longline catch and effort
l_eff_query <- "
SELECT      L_BEST_ID, YY, MM, lat_short, lon_short, hhooks, std_effort
FROM        best.L_BEST_AGG
WHERE       GEAR_CODE like 'L' and YY>1989 and YY<=2023
"
l_eff <- sqlQuery(channel, l_eff_query, as.is=TRUE)

# l_catch_query <- "
# SELECT      l.L_BEST_ID, YY, MM, lat_short, lon_short, sp_code, sp_n, sp_mt
# FROM        best.L_BEST_AGG l
# LEFT OUTER JOIN  best.L_BEST_AGG_CATCH c on l.L_BEST_ID = c.L_BEST_ID
# WHERE       l.GEAR_CODE like 'L' and YY>1989 and YY<=2022
# "
# l_catch <- sqlQuery(channel, l_catch_query, as.is=TRUE)

# Close the database connection
odbcCloseAll()
write.csv(s_eff, paste0(data_wd, "effort/effort_PS_agg_data_1990-2023b.csv"), row.names=F) # write .csv to folder
# write.csv(l_eff, paste0(data_wd, "effort/effort_LL_agg_data_1990-2023.csv"), row.names=F) # write .csv to folder

# 2.1 Clean effort data ----

# Format data and filter out observations outside WCPFC-CA - PS
s_eff <-read.csv(paste0(data_wd, "effort/effort_PS_agg_data_1990-2023b.csv")) 
s_eff2 <- s_eff |>
  mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                  lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
                  lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
  select(-c(lon_short, lon.dir, lat_short, lat.dir)) %>%  
  mutate(days_sch = as.numeric(days_sch), sets_sch = as.numeric(sets_sch),
         WCP_CA = point.in.polygon(lon, lat, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2))  |>
  select(-WCP_CA)

# Also filter for regions 6-8 and remove domestic SE asian fleet effort
# and edge effects from EP
s_eff2 <- s_eff2 |> dplyr::filter(lat >= -20 & lat <= 10 & lon >= 140 & lon <=210) |>
  filter(!(FLAG_CODE %in% c('VN', 'EP') | (FLAG_CODE == 'PH' & FLEET_CODE == 'PH') |
                           (FLAG_CODE == 'ID' & FLEET_CODE == 'ID')))

# Format data and filter out observations outside WCPFC-CA - LL
#l_eff <-read.csv(paste0(data_wd, "effort/effort_LL_agg_data_1990-2023.csv")) 

# l_eff2 <- l_eff |>
#   mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
#                   lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
#                   lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
#   select(-c(lon_short, lon.dir, lat_short, lat.dir)) %>% 
#   mutate(hhoks = as.numeric(hhooks), std_effort = as.numeric(std_effort),
#          WCP_CA = point.in.polygon(lon, lat, coords[,1], coords[,2])) |>
#   dplyr::filter(WCP_CA %in% c(1, 2))  |>
#   select(-WCP_CA)

# 
# l_eff2 |>
#   group_by(lon, lat, YY) |>
#   summarise(hhooks=log(sum(hhooks))) |> 
#   ggplot() +
#   aes(x = lon, y = lat, fill = hhooks) +
#   geom_tile() +
#   scale_fill_distiller(palette='Spectral') + 
#   facet_wrap(~YY) + 
#   geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
#   coord_sf(xlim = c(110, 220), ylim = c(-50, 50)) +
#   gg.theme +
#   labs(x = 'Longitude', y = 'Latitude', 
#        fill='Effort (log hundred hooks)', title = 'Distribution of longline effort')

# Set school association codes
ASS <- c(4,3) # DFAD only
AFAD = c(5) # anchored FAD
ASSWH = c(6,7) # whale and whale shark sets
UNA <- c(1,2) # free school

# Aggregate all set types together, and disaggregate by DFAD and free school sets
tmp1 = s_eff2 %>% filter(Schass_code %in% c(1,2,3,4,5,6,7)) %>% 
  group_by(YY,MM, lat, lon, ez_aprx_code) %>%
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup() |>
  mutate(set_type = 'all')
tmp2 = s_eff2 %>% filter(Schass_code %in% ASS) %>%
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  group_by(YY,MM, lat, lon, ez_aprx_code) %>% 
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup() |>
  mutate(set_type = 'ass')
tmp3 = s_eff2 %>%  filter(Schass_code %in% UNA) %>% 
  mutate(sets_sch  =  ifelse(sets_sch<0,0,sets_sch))  %>%
  group_by(YY,MM, lat, lon, ez_aprx_code) %>% 
  summarise(days = sum(days_sch), sets = sum(sets_sch) ) %>% ungroup() |>
  mutate(set_type = 'una')

s_eff3 <- bind_rows(tmp1,tmp2,tmp3) 

rm(tmp1,tmp2,tmp3)
save(s_eff3, file = paste0(data_wd, "effort/effort_seine_clean_data_upd.Rdata")) 

load(file = paste0(data_wd, "effort/effort_seine_clean_data_upd.Rdata"))
# 
s_eff3 |>
  dplyr::filter(set_type == 'all' & YY %in% c(2017:2023)) |>
  group_by(lon, lat, YY) |>
  summarise(sets=log(sum(sets,na.rm=T))) |>
  ggplot() +
  aes(x = lon, y = lat, fill = sets) +
  geom_tile() +
  scale_fill_distiller(palette='Spectral') +
  facet_wrap(~YY) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(110, 220), ylim = c(-50, 50)) +
  gg.theme +
  labs(x = 'Longitude', y = 'Latitude',
       fill='Effort (log sets)', title = 'Distribution of purse seine effort (all)')

# 3. Centre of gravity indicator ----

# COG function
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

# Dropping SE Asia effort
s_eff3 |>
  ggplot() +
  aes(lon, lat, fill = ez_aprx_code) +
  geom_tile() +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 200), ylim = c(-50, 50)) +
  #scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  gg.theme

# 3.1 Purse seine effort COG ----
# Calculate COG, intertia
# Need to filter PS data? remove japane/indo fleet???
s_eff_COGs <- 
  s_eff3 |>
  #dplyr::filter(ez_aprx_code != 'ID' & lat >= -20 & lat <= 20) |>
  group_by(set_type) |>
  nest() |>
  mutate(cgi = purrr:::map(data, ~ann.cg.inert(.x, effort = 'sets', lab = 'PS'))) |>
  dplyr::select(-data) |>
  unnest(cols = c('cgi'))
save(s_eff_COGs, file = paste0(data_wd, "effort/effort_seine_COGs.Rdata"))
load(file = paste0(data_wd, "effort/effort_seine_COGs.Rdata"))

# Extract COG means for plotting
COG_mns <- 
  s_eff_COGs |>
  dplyr::filter(yy %in% c(1990:2000)) |>
  group_by(set_type) |>
  dplyr::summarise(mn_lon = mean(lon[set_type == 'all']),
                   mn_inertia = mean(inertia[set_type == 'all']),
                   mn_lat = mean(lat[set_type == 'all']))

library(zoo)
p <- 
  s_eff_COGs |>
  dplyr::arrange(yy) |>
  dplyr::group_by(set_type) |>
  dplyr::mutate(rolling_lon = zoo::rollapply(lon, width = 3, FUN = mean, align = "center", fill = NA)) |> # Centered 3-year rolling mean
  ggplot() +
  aes(reorder(yy, desc(yy)), rolling_lon, group = set_type, col = set_type) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  #facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lon), color='black') +
  labs(x = 'Year', y = 'Longitude', col = 'Set type') +
  scale_x_discrete(breaks = seq(min(s_eff_COGs$yy), max(s_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Longitudinal centre of gravity of PS effort") +
  ylim(140,180) +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'effort/effort_COGlong_PS_region68.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
  s_eff_COGs |>
  #unnest(cols = c('cgi_ass')) |>
  #dplyr::filter(set_type == 'all') |>
  ggplot() +
  aes(yy, lat, group = set_type, col = set_type) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  #coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  #facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lat), color='black') +
  labs(x = 'Year', y = 'Latitude', col = 'Set type') +
  scale_x_discrete(breaks = seq(min(s_eff_COGs$yy), max(s_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Latitudinal centre of gravity of PS effort") +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'effort/effort_COGlat_PS_region68.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
  s_eff_COGs |>
  #unnest(cols = c('cgi_ass')) |>
  #dplyr::filter(set_type == 'all') |>
  ggplot() +
  aes(yy, inertia, group = set_type, col = set_type) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  #coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  #facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_inertia), color='black') +
  labs(x = 'Year', y = 'Inertia', col = 'Set type') +
  scale_x_discrete(breaks = seq(min(s_eff_COGs$yy), max(s_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Centre of gravity inertia of PS effort") +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'effort/effort_COGinert_PSs_region68.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# COGs
tmp <- 
s_eff_COGs |>
  group_by(set_type) |>
  mutate(mn = mean(lon, na.rm=T), sd = sd(lon, na.rm=T)) |>
  group_by(set_type, yy) |>
  summarise(zscore = (lon-mn)/sd) |>
  group_by(set_type) |>
  mutate(zscore_3yr = rollapply(zscore, width = 3, FUN = mean, align = "center", fill = NA))

tmp |>
  ggplot() +
  aes(yy, zscore, col = set_type) +
  geom_line() +
  geom_point() +
  gg.theme +
  scale_color_brewer(palette = 'Set1') +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -1.96, linetype = 'dashed') +
  geom_hline(yintercept = 1.96, linetype = 'dashed') +
  geom_smooth(method = "lm", se = FALSE, aes(group = set_type, col = set_type), linetype = "dashed")


# 3.2 Longline effort COG ----
# Calculate COG, intertia

l_eff_COGs <- ann.cg.inert(l_eff2, effort='hhooks', lab='ll_annual')
#save(l_eff_COGs, file = paste0(data_wd, "effort/effort_LL_COGs.Rdata"))

COG_mns <- 
  l_eff_COGs |>
  dplyr::filter(yy %in% c(1990:2000)) |>
  group_by(1) |>
  dplyr::summarise(mn_lon = mean(lon),
                   mn_inertia = mean(inertia),
                   mn_lat = mean(lat))

p <- 
  l_eff_COGs |>
  ggplot() +
  aes(reorder(yy, desc(yy)), lon, group = 1) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lon), color='black') +
  labs(x = 'Year', y = 'Longitude') +
  scale_x_discrete(breaks = seq(min(l_eff_COGs$yy), max(l_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Longitudinal centre of gravity of LL effort") +
  gg.theme 
ggsave(p, file = paste0(results_wd, 'effort/effort_COGlon_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Longline latitudinal COG
p <- 
  l_eff_COGs |>
  ggplot() +
  aes(yy, lat, group = 1) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lat), color='black') +
  labs(x = 'Year', y = 'Latitude') +
  #scale_x_discrete(breaks = seq(min(l_eff_COGs$yy), max(l_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Latitudinal centre of gravity of LL effort") +
  gg.theme 
ggsave(p, file = paste0(results_wd, 'effort/effort_COGlat_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Longline COG inertia
p <- 
  l_eff_COGs |>
  ggplot() +
  aes(yy, inertia, group = 1) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_inertia), color='black') +
  labs(x = 'Year', y = 'Inertia') +
  #scale_x_discrete(breaks = seq(min(l_eff_COGs$yy), max(l_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Centre of gravity inertia of LL effort") +
  gg.theme 
ggsave(p, file = paste0(results_wd, 'effort/effort_COGinert_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 3.3 COG maps ----

p <- 
  s_eff_COGs |>
  #dplyr::filter(set_type == 'all') |>
  dplyr::arrange(yy) |>
  #dplyr::mutate(id = 1:n()) |>
  ggplot() +
  aes(lon, lat, fill = yy, col = yy) +
  geom_point(stroke = 1, shape = 21, size = 3, col = 'black') +
  geom_path(aes(col = yy)) +
  facet_wrap(~set_type, nrow = 3) +
  geom_text(data = s_eff_COGs %>% filter(yy %in% c(1990, 2000, 2010, 2020, 2023)),
           aes(label = yy), color = "black", size = 3, vjust = -0.5) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 180), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  labs(x = 'Longitude', y = 'Latitude', col = 'Year', fill = 'Year',
       title = 'Centre of gravity of PS effort') +
  gg.theme +
  theme(legend.position = 'bottom',legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'effort/effort_COGPS_map_region68_set_type.png'),height = 11, width = 8, units = "in", dpi = 200)

p <- 
  l_eff_COGs |>
  #dplyr::filter(set_type == 'all') |>
  dplyr::arrange(yy) |>
  #dplyr::mutate(id = 1:n()) |>
  ggplot() +
  aes(lon, lat, fill = yy, col = yy) +
  geom_point(stroke = 1, shape = 21, size = 3, col = 'black') +
  geom_path(aes(col = yy)) +
  geom_text(data = l_eff_COGs %>% filter(yy %in% c(1990, 2000, 2010, 2020, 2023)),
            aes(label = yy), color = "black", size = 3, vjust = -0.5) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 200), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  labs(x = 'Longitude', y = 'Latitude', col = 'Year', fill = 'Year',
       title = 'Centre of gravity of LL effort') +
  gg.theme +
  theme(legend.position = 'bottom',legend.text = element_text(angle = 90))

ggsave(p, file = paste0(results_wd, 'effort/effort_COGLL_map.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 4.Area occupied  effort indicator ----

# Area occupied function
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

# Calculate area occupied
s_eff_area<- 
  s_eff3 |>
  #dplyr::filter(lat >= -20 & lat <= 20) |>
  group_by(set_type) |>
  nest() |>
  mutate(area_occ = purrr:::map(data, ~area.occ(.x, effort = 'sets', res=1))) |>
  dplyr::select(-data) |>
  unnest(area_occ)

l_eff_area <- area.occ(l_eff2, effort='hhooks', res=5)

area_mns <-
  s_eff_area |>
  dplyr::filter(set_type == 'all') |>
  group_by(1) |>
  summarise(mn_area = mean(area[YY %in% c(1990:2000)]))

p <- 
  s_eff_area |>
  ggplot() +
  aes(YY, area/1000, col = set_type) +
  geom_line() +
  geom_point(size=0.8) +
  ylim(0,21000) +
  #facet_wrap(~sp_code, nrow = 3) +
  geom_hline(data = area_mns, aes(yintercept = mn_area/1000)) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Effort area (X1000 km2)', col = 'Set type',
       title = 'Area of PS effort (sets)') +
  gg.theme +
  theme(legend.position = 'bottom', aspect.ratio = 0.5) 
p
ggsave(p, file = paste0(results_wd, 'effort/effort_area_PS_region68.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

area_mns <- mean(l_eff_area$area[l_eff_area$YY %in% c(1990:2000)])

p <- 
  l_eff_area |>
  ggplot() +
  aes(YY, area/1000) +
  geom_line() +
  geom_point(size=0.8) +
  ylim(0,100000) +
  geom_hline(yintercept = area_mns/1000) +
  labs(x = 'Year', y = 'Effort area (X1000 km2)', col = 'Set type',
       title = 'Area of LL effort (hooks)') +
  gg.theme
p
ggsave(p, file = paste0(results_wd, 'effort/effort_area_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 5. Effort density maps indicator ----

#pal <- rev(c(brewer.pal(n = 9, name = "RdYlBu"), 'white'))
pal2 <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(11), 'white'))

p <- 
  s_eff3 |>
  dplyr::filter(!(YY %in% c(2001:2018)) & set_type == 'una' ) |>
  group_by(lat, lon, YY, set_type) |>
  summarise(sets = sum(sets, na.rm=T)) |>
  mutate(YY2 = case_when(YY %in% c(1990:2000) ~ '1990:2000',
                         YY %in% c(2019:2023) ~ as.character(YY))) |>
  group_by(lat, lon, YY2) |>
  summarise(sets2 = mean(sets, na.rm=T)) |>
  mutate(sets2 = round(sets2,0)) |>
  uncount(sets2) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_density_2d_filled(bins=12, h=10) +
  geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  #geom_point(size=0.2, alpha=0.2, col = 'grey50') +
  #geom_contour_filled(aes(z = sets2, fill = ..level..), bins = 10) +  # Transparent fill
  #geom_contour(aes(z = sets2), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-15, 15)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~YY2) +
  scale_fill_manual(values=pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "Purse seine sets density (una)") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'effort/effort_density_PSuna_region68.png'),height = 8, width = 11, units = "in", dpi = 200)

tmp <-
  l_eff2 |>
  dplyr::filter(YY %in% c(1990:2000)) |>
  group_by(lat, lon, YY) |>
  summarise(hhooks = round(sum(hhoks, na.rm=T),0)) |>
  group_by(lat,lon) |>
  summarise(hhooks = mean(hhooks)) |>
  mutate(YY2 = '1990-2000')

p <- 
  l_eff2 |>
  dplyr::filter(YY %in% c(2019:2023)) |>
  group_by(lat, lon, YY) |>
  summarise(hhooks = round(sum(hhoks, na.rm=T),0)) |>
  mutate(YY2 = as.character(YY)) |>
  dplyr::select(-YY) |>
  bind_rows(tmp) |>
  #uncount(hhooks) |>
  ggplot() +
  aes(x = lon, y = lat) +
  #geom_point()
  #geom_density_2d_filled(bins=12, h=10) +
  #geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  #geom_point(size=0.2, alpha=0.2, col = 'grey50') +
  geom_contour_filled(aes(z = hhooks, fill = ..level..), bins = 12,) +  # Transparent fill
  geom_contour(aes(z = hhooks), color = "grey50", linewidth = 0.2, bins = 11) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 250), ylim = c(-50, 50)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~YY2) +
  scale_fill_manual(values=pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "Longline hooks density") +
  gg.theme +
  theme(legend.position = 'none')
ggsave(p, file = paste0(results_wd, 'effort/effort_density_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)



# 6. Proportion of sets >180 degrees indicator ----

# Purse seine effort > 180 degrees by set type
p <- 
  s_eff3 |>
  group_by(YY, set_type) |>
  summarise(setsT = sum(sets), sets180 = sum(sets[lon>=180])) |>
  mutate(prop = sets180/setsT) |>
  group_by(set_type) |>
  mutate(mn_9020 = mean(prop[YY %in% c(1990:2000)]),
         prop_norm = zoo::rollapply(prop-mn_9020, width = 3, FUN = mean, align = "center", fill = NA)) |>
  ggplot() +
  aes(YY, prop_norm*100, col = set_type) +
  geom_line() +
  geom_point(size=0.8) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size=0.1) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Sets (%)', col = 'Set type',
       title = 'Proportion of PS sets >= 180 degrees (normalised by 1990-2000 mean)') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'effort/effort_180prop_PS_region68.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Longline effort outside 20 degrees 
p <- 
  l_eff2 |>
  group_by(YY) |>
  summarise(hhooksT = sum(hhoks), hhooks20 = sum(hhoks[lat>= 20 | lat <= -20])) |>
  mutate(prop = hhooks20/hhooksT,
         mn_9020 = mean(prop[YY %in% c(1990:2000)]),
         prop_norm = prop-mn_9020) |>
  ggplot() +
  aes(YY, prop*100) +
  geom_line() +
  geom_point(size=0.8) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size=0.1) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Sets (%)', col = 'Set type',
       title = 'Proportion of longline sets outside 20 degrees latitude') +
  gg.theme +
  theme(legend.position = 'bottom')
ggsave(p, file = paste0(results_wd, 'effort/effort_20prop_LL2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)


