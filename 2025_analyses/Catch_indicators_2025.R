#########################################################################
# Target Species Catch and Distribution indicators - for SC21 climate indicators report
#########################################################################

# Latest update : 13/01/2025
# Created on: 06/07/22
# Created by: Jed McDonald and JSP
# Modified by: Nick Hill

# Target species and stocks: SKJ, YFT, BET, North Pacific ALB (N_ALB), South Pacific ALB (S_ALB)  
# Area: WCPFC Convention Area
# Gears: PS (associated), PS (un-associated), LL, PL, Other
# Time series: annual, 1990-2023 inclusive
# Spatial resolution: COG and inertia 5? x 5?; Area occupied 1? x 1? (PS), 5? x 5? (LL)

# 2025 update NH updates code first developed by Jed McDonald and Joe-Scutt-Philips and 
# revises indicators based on updated report structure/needs

# The following indicators are computed:
# 1) Annual Tuna Catch                            This is the total annual tuna catch (by species) for SKJ, YFT, BET and all ALB (N_ALB and S_ALB catches combined) across the entire WCPFC-CA between 1990 and 2021 inclusive.
# 2) Annual longitudinal centre of catch          This is the annual mean zonal centre of gravity of catch for each of SKJ, YFT, BET, ALB between 2000 and 2020, for each gear type. 
# 3) Annual latitudinal centre of catch           This is the annual mean meridional centre of gravity of catch for each of SKJ, YFT, BET, ALB, between 2000 and 2020, for each gear type.
# 4) Annual area of catch ('Area occupied')       This is the annual number of 1? x 1? grid cells (PS), 5? x 5? grid cells (LL) and area (in km^2) in which each of SKJ, YFT, BET, ALB were captured in the WCPFC-CA between 1990 and 2021 inclusive, for each gear type. 

# Key changes from 2021:
# 1) Decision was made to only report Annual Tuna Catch for the 2022 and 2023 SC papers.
# 2) Addition of new data from 1990 to 2022 inclusive.
# 3) Code updated with minor edits.

# Key changes for 2025:
# 1) Reduce number of total indicators used
# 2) Modify ....
# 3) Addition of new data up to 2024

# 1. Preamble ----

# Libraries
library(tidyverse)
#library(magrittr)
#library(maps)
library(zoo)
#library(RODBC)
library(sp)
#library(mapdata)
library(RGeostats)
library(RColorBrewer)

# directories
data_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/data/'
results_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/results/'

# base map
map_bg <- map_data("world") #|>
#coord_map(projection = 'mercator') 
map_bg2 <- map_bg |>
  dplyr::mutate(long = long + 360, group = group + max(group) + 1) |>
  rbind(map_bg)

# Plotting theme
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

# Read in WCPFC-CA boundary info
wcp_ca = read.csv(paste0(data_wd, "wcpfc_ca_stat_area.csv"))

# Convert to spatial polygon
wcp = Polygon(wcp_ca)
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Check that points are in WCPFC-CA
coords = wcp@polygons[[1]]@Polygons[[1]]@coords

# 2. Download catch data from DB ----

# 2025 update - need to pull data from new fishmaster DB
db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
channel <- odbcDriverConnect(db1)

agg_data_query<-"
SELECT  yy,
        case when gear_code in ('S','L','P') then gear_code else 'Z'end as gear,
        ocean_code as area,
        flag_code as flag,
        fleet_code as fleet,
        lat_short,
        lon_short,
        coalesce(schass_code,0) as school,
        sum(case when sp_code = 'ALB' then sp_mt else 0000.000 end) as alb_mt,
        sum(case when sp_code = 'BET' then sp_mt else 0000.000 end) as bet_mt,
        sum(case when sp_code = 'SKJ' then sp_mt else 0000.000 end) as skj_mt,
        sum(case when sp_code = 'YFT' then sp_mt else 0000.000 end) as yft_mt,
        ref.GetLatD5fromLatD(ref.GetLatDfromLat(lat_short)) as lat5,
        case when ref.GetLonDfromLon(lon_short) < 0 then 360 else 0 end + ref.GetLonD5fromLonD(ref.GetLonDfromLon(lon_short)) as lon5
FROM    best.a_best_agg a
LEFT JOIN best.a_best_agg_CATCH c  on a.A_BEST_GUID = c.A_BEST_GUID
LEFT JOIN best.a_best_agg_SCHEFF s on c.A_SET_GUID = s.A_SET_GUID
WHERE   yy >= 1990 AND yy <= 2024
AND     flag_code <> '  '
GROUP BY yy,
        case when gear_code in ('S','L','P') then gear_code else 'Z'end,
        ocean_code,
        flag_code,
        fleet_code,
        lat_short,
        lon_short,
        coalesce(schass_code,0),
        ref.GetLatD5fromLatD(ref.GetLatDfromLat(lat_short)),
        case when ref.GetLonDfromLon(lon_short) < 0 then 360 else 0 end + ref.GetLonD5fromLonD(ref.GetLonDfromLon(lon_short))
"
agg_data <- sqlQuery(channel, agg_data_query, as.is=TRUE)
odbcCloseAll()

write.csv(agg_data, paste0(data_wd, "catch/catch_agg_data_1990-2024.csv"), row.names=F) # write .csv to folder

# 2.1 Clean and summarise catch data ----

# Read back in aggregated catch data
agg_data <- read.csv(paste0(data_wd, "catch/catch_agg_data_1990-2024.csv"))

# Format geographic coordinates  
# Use SW corner of 5? x 5? grid cells as catch location (mirroring fishery effort indicators)
catch_dat <-
  agg_data |> mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
                      lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4), 
                      lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon),
                      WCP_CA = point.in.polygon(lon, lat, coords[,1], coords[,2])) %>% 
  dplyr::filter(WCP_CA %in% c(1, 2)) |>
  dplyr::select(-c(lon_short, lon.dir, lat_short, lat.dir, WCP_CA)) |>
  mutate(yy = as.numeric(yy), gear = as.factor(gear), area = as.factor(area), flag = as.factor(flag),
         fleet = as.factor(fleet), school = as.factor(school), alb_mt = as.numeric(alb_mt),
         bet_mt = as.numeric(bet_mt), skj_mt = as.numeric(skj_mt), yft_mt = as.numeric(yft_mt),
         lat5 = as.numeric(lat5), lon5 = as.numeric(lon5))

# Get data summaries
summary(catch_dat)
unique(catch_dat$school) # get school association codes  

# Here we create a summary dataset of catch
tuna_catch <-
  catch_dat |>
  pivot_longer(-c(yy, gear, area, flag, school, lat5, lon5, lat, lon, fleet), 
               names_to = 'sp_code', values_to = 'sp_mt') |>
  mutate(sp_code = toupper(str_remove(sp_code, '_mt')),
         sp_code = case_when(sp_code == 'ALB' & lat >= 0 ~ 'ALB_N',
                             sp_code == 'ALB' & lat < 0 ~ 'ALB_S',
                             .default = sp_code)) |>
  dplyr::filter(sp_mt > 0)
save(tuna_catch, file = paste0(data_wd, "catch/catch_agg_data_clean_1990-2024.csv"))

tuna_catch |>
  group_by(sp_code, yy, gear) |>
  summarise(catch = sum(sp_mt)) |>
  ggplot() +
  aes(yy, catch/1000, fill = gear) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_classic() +
  facet_wrap(~sp_code, scales = 'free_y')

# 3 Catch COG indicator ----
# here, we calc catch COG by species, year by set type for PS fisheries

# set types
ASS <- c(4,3) # DFAD only
AFAD = c(5) # anchored FAD
ASSWH = c(6,7) # whale and whale shark sets
UNA <- c(1,2,-9) # free school

# Extract relevant PS catch data for region 6-8
PS_catch <-
  tuna_catch |>
  dplyr::filter(!str_detect(sp_code, 'ALB') & gear == 'S' & 
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <=210) |>
  filter(!(flag %in% c('VN') | (flag == 'PH' & fleet == 'PH') |
             (flag == 'ID' & fleet == 'ID'))) |>
  group_by(yy, sp_code, school, lat, lon) |> 
  summarise(catch = sum(sp_mt)) |> 
  ungroup() 

# Define function to pull COG and inertia annually
# Updated COG function that also pulls ellipse data
ann.cg.inert = function(dat, catch, lab){
  cgi = list()
  
  unique_years <- unique(dat$yy)
  for(i in seq_along(unique_years)){
    year_i <- unique_years[i]
    
    CG = dat %>% dplyr::filter(yy == year_i) %>% dplyr::select(lon, lat, catch, yy)
    names(CG)[3] = 'catch'
    CG %<>% group_by(lon, lat) %>% summarize(z = sum(as.numeric(catch), na.rm = TRUE), .groups = 'drop')
    
    db = db.create(x1 = CG$lon, x2 = CG$lat, z1 = CG$z)
    projec.define(projection = "mean", db = db)
    projec.toggle(0)
    CG_result = SI.cgi(db, flag.plot = FALSE)
    
    # Extract ellipse parameters
    eig_values <- CG_result$mvalue
    eig_vectors <- CG_result$mvector
    
    width <- 2 * sqrt(eig_values[1])
    height <- 2 * sqrt(eig_values[2])
    angle <- atan2(eig_vectors[2, 1], eig_vectors[1, 1]) * 180 / pi
    
    # Collect results in a named vector (for rbind)
    cgi[[i]] <- c(yy = year_i,
                  lon = CG_result$center[1], lat = CG_result$center[2],
                  inertia = CG_result$inertia, width = width,
                  height = height, angle = angle)
  }
  
  # Combine into data.frame, convert types
  cgi_df <- do.call(rbind, cgi) %>% as.data.frame(stringsAsFactors = FALSE)
  cgi_df %<>% mutate(yy = as.integer(yy),lon = as.numeric(lon),
                     lat = as.numeric(lat), inertia = as.numeric(inertia),
                     width = as.numeric(width),height = as.numeric(height),
                     angle = as.numeric(angle))
  
  return(cgi_df)
}

# Function to extract ellipse area
ellipse_area <- function(width, height) {
  # width and height are full axes length
  a <- width / 2
  b <- height / 2
  pi * a * b
}

# Recollate data by set type - will be duplication in this
tmp1 <-PS_catch |> dplyr:: filter(school %in% c(1,-9,3,4,5,6,7)) |>
  mutate(set_type = 'ALL')
tmp2 <-PS_catch |> dplyr:: filter(school  %in% ASS) |>
  mutate(set_type = 'DFAD')
tmp3 <-PS_catch |> dplyr:: filter(school %in% UNA) |>
  mutate(set_type = 'UNA')

PS_catch2 <- bind_rows(tmp1,tmp2,tmp3)
rm(tmp1,tmp2,tmp3)
#save(PS_catch2, file = paste0(data_wd, 'catch/catch_PS_clean_data.Rdata'))

# Extract COG information by set type
PS_catch_COGs <- 
  PS_catch2 %>%
  group_by(sp_code, set_type) |>
  nest() |>
  mutate(cgi = map(data, ~ ann.cg.inert(.x, catch = 'mt', lab = 'PS'))) |>
  select(-data) |>
  unnest(cols = c(cgi)) |>
  mutate(area_m2 = ellipse_area(width, height), area_km2 = area_m2 / 1e6)
save(PS_catch_COGs, file = paste0(data_wd, 'catch/catch_seine_COGs.Rdata'))

# Extract COG means for plotting
COG_mns <- 
  PS_catch_COGs |>
  dplyr::filter(yy %in% c(1990:2000)) |>
  group_by(sp_code) |>
  dplyr::summarise(mn_lon = mean(lon[set_type=='ALL']),
                   mn_inertia = mean(inertia[set_type=='ALL']),
                   mn_lat = mean(lat[set_type=='ALL']),)

# 3.1 COG plots ----

# Longitudinal COG by species and set type of PS catch
p <- 
PS_catch_COGs |>
  #unnest(cols = c('cgi_ass')) |>
  #dplyr::filter(set_type == 'all') |>
  ggplot() +
  aes(reorder(yy, desc(yy)), lon, group = set_type, col = set_type) +
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lon), color='black') +
  labs(x = 'Year', y = 'Longitude', col = 'Set type') +
  scale_x_discrete(breaks = seq(min(PS_catch_COGs$yy), max(PS_catch_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Longitudinal centre of gravity of PS catch") +
  gg.theme +
  theme(legend.position = 'bottom')
p
#ggsave(p, file = paste0(results_wd, 'catch/catch_PS_longCOG.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Latitudinal COG by species and set type of PS catch
p<- 
PS_catch_COGs |>
  #unnest(cols = c('cgi_ass')) |>
  #dplyr::filter(set_type == 'all') |>
  ggplot() +
  aes(yy, lat, group = set_type, col = set_type) +
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  #coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lat), color='black') +
  labs(x = 'Year', y = 'Latitude', col = 'Set type',
       title = "Latitudinal centre of gravity of PS catch") +
  gg.theme +
  theme(legend.position = 'bottom')
p
#ggsave(p, file = paste0(results_wd, 'catch/catch_PS_latCOG.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Intertia by species and set type of PS catch
p <- 
PS_catch_COGs |>
  #unnest(cols = c('cgi_ass')) |>
  #dplyr::filter(set_type == 'all') |>
  ggplot() +
  aes(reorder(yy, desc(yy)), inertia, group = set_type, col = set_type) +
  geom_line() +
  geom_point() +
  #scale_y_discrete(limits=rev) +
  coord_flip() +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size=0.1) +
  facet_wrap(~sp_code) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(data = COG_mns, aes(slope=0, intercept = mn_inertia), color='black') +
  labs(x = 'Year', y = 'Inertia', col = 'Set type',
       title = "Centre of gravity inertia of PS catch") +
  scale_x_discrete(breaks = seq(min(PS_catch_COGs$yy), max(PS_catch_COGs$yy), by = 5)) +  # Show every 5th year
  gg.theme +
  theme(legend.position = 'bottom')
p
#ggsave(p, file = paste0(results_wd, 'catch/catch_PS_inertiaCOG.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# PS COG catch map without ellipses by species (all sets)
library(ggforce)
p <- 
  PS_catch_COGs |>
  dplyr::filter(set_type == 'ALL') |>
  ggplot() +
  geom_ellipse(aes(x0 = lon, y0 = lat, a = width / 2,  b = height / 2,        
                   angle = angle * pi / 180,fill = yy), alpha = 0.2, col = NA) + 
  geom_point(aes(x = lon, y = lat, fill = yy), size = 3, shape = 21, col = 'black') +
  geom_path(aes(x = lon, y = lat, color = yy)) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 190), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  geom_text(data = PS_catch_COGs %>% filter(yy %in% c(1990, 2024) & set_type == 'ALL'),
            aes(x = lon, y = lat, label = yy), color = "black", size = 3, vjust = -0.5) +
  facet_wrap(~sp_code, ncol = 1) +
  labs(x = "Longitude", y = "Latitude", col = 'Year', fill = 'Year') +
  gg.theme +
  theme(legend.position = 'bottom', legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'catch/catchPS_COG_map_ellipses_allsp.png'),height = 10, width = 8, units = "in", dpi = 200)

# PS COG catch map without ellipses by species (all sets)
p <- 
  PS_catch_COGs |>
  dplyr::filter(set_type == 'ALL') |>
  ggplot() +
  geom_point(aes(x = lon, y = lat, fill = yy), size = 3, shape = 21, col = 'black') +
  geom_path(aes(x = lon, y = lat, color = yy)) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 190), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  geom_text(data = PS_catch_COGs %>% filter(yy %in% c(1990, 2024) & set_type == 'ALL'),
            aes(x = lon, y = lat, label = yy), color = "black", size = 3, vjust = -0.5) +
  facet_wrap(~sp_code, ncol = 1) +
  labs(x = "Longitude", y = "Latitude", col = 'Year', fill = 'Year') +
  gg.theme +
  theme(legend.position = 'bottom', legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'catch/catchPS_COG_map_allsp.png'),height = 10, width = 8, units = "in", dpi = 200)


# PS catch COG by set type - BET
p <- 
  PS_catch_COGs |>
  dplyr::filter(sp_code == 'BET') |>
  ggplot() +
  geom_ellipse(aes(x0 = lon, y0 = lat, a = width / 2,  b = height / 2,        
                   angle = angle * pi / 180,fill = yy), alpha = 0.2, col = NA) + 
  geom_point(aes(x = lon, y = lat, fill = yy), size = 3, shape = 21, col = 'black') +
  geom_path(aes(x = lon, y = lat, color = yy)) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 190), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  geom_text(data = PS_catch_COGs %>% filter(yy %in% c(1990, 2024) & sp_code == 'BET'),
            aes(x = lon, y = lat, label = yy), color = "black", size = 3, vjust = -0.5) +
  facet_wrap(~set_type, ncol = 1) +
  labs(x = "Longitude", y = "Latitude", col = 'Year', fill = 'Year') +
  gg.theme +
  theme(legend.position = 'bottom', legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'catch/catchPS_COG_map_allsets_BET.png'),height = 10, width = 8, units = "in", dpi = 200)

# PS catch COG by set type - SKJ
p <- 
  PS_catch_COGs |>
  dplyr::filter(sp_code == 'SKJ') |>
  ggplot() +
  geom_ellipse(aes(x0 = lon, y0 = lat, a = width / 2,  b = height / 2,        
                   angle = angle * pi / 180,fill = yy), alpha = 0.2, col = NA) + 
  geom_point(aes(x = lon, y = lat, fill = yy), size = 3, shape = 21, col = 'black') +
  geom_path(aes(x = lon, y = lat, color = yy)) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 190), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  geom_text(data = PS_catch_COGs %>% filter(yy %in% c(1990, 2024) & sp_code == 'SKJ'),
            aes(x = lon, y = lat, label = yy), color = "black", size = 3, vjust = -0.5) +
  facet_wrap(~set_type, ncol = 1) +
  labs(x = "Longitude", y = "Latitude", col = 'Year', fill = 'Year') +
  gg.theme +
  theme(legend.position = 'bottom', legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'catch/catchPS_COG_map_allsets_SKJ.png'),height = 10, width = 8, units = "in", dpi = 200)

# PS catch COG by set type - YFT
p <- 
  PS_catch_COGs |>
  dplyr::filter(sp_code == 'YFT') |>
  ggplot() +
  geom_ellipse(aes(x0 = lon, y0 = lat, a = width / 2,  b = height / 2,        
                   angle = angle * pi / 180,fill = yy), alpha = 0.2, col = NA) + 
  geom_point(aes(x = lon, y = lat, fill = yy), size = 3, shape = 21, col = 'black') +
  geom_path(aes(x = lon, y = lat, color = yy)) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 190), ylim = c(-10, 10)) +
  scale_color_distiller(palette = 'RdYlBu', direction = -1) +
  scale_fill_distiller(palette = 'RdYlBu', direction = -1) +
  geom_text(data = PS_catch_COGs %>% filter(yy %in% c(1990, 2024) & sp_code == 'YFT'),
            aes(x = lon, y = lat, label = yy), color = "black", size = 3, vjust = -0.5) +
  facet_wrap(~set_type, ncol = 1) +
  labs(x = "Longitude", y = "Latitude", col = 'Year', fill = 'Year') +
  gg.theme +
  theme(legend.position = 'bottom', legend.text = element_text(angle = 90))
p
ggsave(p, file = paste0(results_wd, 'catch/catchPS_COG_map_allsets_YFT.png'),height = 10, width = 8, units = "in", dpi = 200)



# 4. Catch area indicator ----

# 4.1 DB query ----
#Extract and organise data
# db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
# channel <- odbcDriverConnect(db1)
# 
# s_catch_query <- "
# SELECT      s.S_BEST_GUID, YY, MM, lat_short, lon_short, ez_aprx_code, days,
#             Schass_code, sp_code, sp_mt
# FROM        best.S_BEST_AGG s
# LEFT OUTER JOIN  best.S_BEST_AGG_CATCH c on s.S_BEST_GUID = c.S_BEST_GUID
# WHERE       s.GEAR_CODE like 'S' and YY>1989 and YY<=2024
# "
# s_catch <- sqlQuery(channel, s_catch_query, as.is=TRUE)
# 
# l_catch_query <- "
# SELECT      l.L_BEST_GUID, YY, MM, lat_short, lon_short, sp_code, sp_n, sp_mt
# FROM        best.L_BEST_AGG l
# LEFT OUTER JOIN  best.L_BEST_AGG_CATCH c on l.L_BEST_GUID = c.L_BEST_GUID
# WHERE       l.GEAR_CODE like 'L' and YY>1989 and YY<=2024
# "
# l_catch <- sqlQuery(channel, l_catch_query, as.is=TRUE)
# 
# odbcCloseAll()

# 4.2 Clean data ----
# Can use PS_catch from above for PS, only need to clean LL data

# Format geographic coordinates  
# Use SW corner of grid cells (1? x 1? for PS, 5? x 5? for LL) as catch location (mirroring fishery effort indicators)
# s_catch %<>% mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3), 
#                     lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
#                     lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>% 
#   select(-c(lon_short, lon.dir, lat_short, lat.dir)) |>
#   filter(sp_code %in% c("SKJ", "YFT", "BET"))
# 
# l_catch <- 
#   l_catch |> mutate(lat = as.numeric(substr(lat_short,1,2)), lat.dir = substr(lat_short,3,3),
#                     lon = as.numeric(substr(lon_short,1,3)), lon.dir = substr(lon_short,4,4),
#                     lat = ifelse(lat.dir=='S', lat*-1,lat), lon = ifelse(lon.dir=='W', 360-lon, lon)) %>%
#   select(-c(lon_short, lon.dir, lat_short, lat.dir)) |>
#   filter(sp_code %in% c("SKJ", "YFT", "BET", "ALB")) |>
#   mutate(sp_code = case_when(sp_code == 'ALB' & lat>=0 ~ 'ALB_N',
#                              sp_code == 'ALB' & lat<0 ~ 'ALB_S',
#                              .default = sp_code)) |>
#   dplyr::rename(yy=YY)
# 
# l_catch$WCP_CA = point.in.polygon(l_catch$lon, l_catch$lat, coords[,1], coords[,2])
# l_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
# 
# #Check that points are in WCPFC-CA
# s_catch$WCP_CA = point.in.polygon(s_catch$lon, s_catch$lat, coords[,1], coords[,2])
# s_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
# l_catch$WCP_CA = point.in.polygon(l_catch$lon, l_catch$lat, coords[,1], coords[,2])
# l_catch %<>% filter(WCP_CA %in% c(1,2)) %>%  select(-WCP_CA)
# 
# PS_catch
# 
# #Split PS data by set type
# s_catch_ass = s_catch %>% filter(Schass_code %in% ASS)
# s_catch_una = s_catch %>%  filter(Schass_code %in% UNA)

# 4.3 Calculate and plot area of catch indicator ----

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
    select(yy, cell, Area_km2) %>% distinct() %>% 
    group_by(yy) %>% 
    summarise(cells = n_distinct(cell), area = sum(Area_km2))
  return(ao)
}

# Calculate area of catch for PS data
# Drop sets outside 20 degree area? Done for now. This is a change from old reports. NH

PS_catch_area <- 
PS_catch2 |>
  dplyr::filter(lat >= -20 & lat <= 20) |>
  group_by(sp_code, set_type) |>
  nest() |>
  mutate(area_occ = purrr:::map(data, ~area.occ(.x, catch = 'catch', res=1))) |>
  dplyr::select(-data) |>
  unnest(area_occ)

area_mns <-
  PS_catch_area |>
  dplyr::filter(set_type == 'ALL') |>
  group_by(sp_code) |>
  summarise(mn_area = mean(area[yy %in% c(1990:2000)]))

p <- 
PS_catch_area |>
  ggplot() +
  aes(yy, area/1000, col = set_type) +
  geom_line() +
  geom_point(size=0.8) +
  facet_wrap(~sp_code, nrow = 3) +
  geom_hline(data = area_mns, aes(yintercept = mn_area/1000)) +
  scale_color_brewer(palette = 'Set1') +
  theme_classic() +
  labs(x = 'Year', y = 'Catch area (X1000 km2)', col = 'Set type',
       title = 'Area of PS catch') +
  gg.theme +
  theme(legend.position = 'bottom', aspect.ratio = 0.5) 
p
ggsave(p, file = paste0(results_wd, 'catch/catch_area_PS.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Longline catch area
l_catch_area <-
  tuna_catch |> 
  filter(gear == 'L') |>
  dplyr::select(yy, gear, sp_code, lat, lon, catch = sp_mt) |>
  group_by(sp_code) |>
  nest() |>
  mutate(area_occ = purrr:::map(data, ~area.occ(.x, catch = 'sp_mt', res=1))) |>
  dplyr::select(-data) |>
  unnest(area_occ)

p <- 
l_catch_area |>
  ggplot() +
  aes(yy, area/1000, col = sp_code) +
  geom_line() +
  geom_point() +
  #facet_wrap(~sp_code, scales = 'free_y') +
  scale_color_brewer(palette = 'Set1') +
  theme_classic() +
  labs(x = 'Year', y = 'Catch area (X1000 km2)', col = 'Species',
       title = 'Area of LL catch') +
  gg.theme
p
#ggsave(p, file = paste0(results_wd, 'catch/catch_area_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 5. Catch density map indicator ----

# PS_catch2 |>
#   dplyr::filter(sp_code == 'SKJ' & yy %in% c(2015:2019) & lat >=-20 &lat <= 20) |>
#   #mutate(lat = round(lat,0), lon = round(lon,0))
#   group_by(lat, lon, yy) |>
#   summarise(catch2 = sum(catch, na.rm=T)) |>
#   ggplot() +
#   aes(lon, lat, fill = catch2) +
#   geom_tile() +
#   scale_fill_distiller(palette='Spectral') +
#   facet_wrap(~yy) +
#   geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
#   coord_sf(xlim = c(120, 250), ylim = c(-50,50)) +
#   #coord_equal() +
#   theme_bw() +
#   labs(x = 'Longitude', y = 'Latitude', fill='Catch (mt)') +
#   theme(strip.background = element_rect(fill=NA), axis.text = element_text(size=8)) +
#   ggtitle('Distribution of purse seine tuna catch')

catch_hist <- PS_catch2 |> 
  dplyr::filter(set_type == 'ALL' & yy %in% c(1990:2000) &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  group_by(lat, lon, set_type, sp_code) |>
  summarise(catch2 = mean(catch2, na.rm=T)) |>
  mutate(yy_bin = '1990-2000')

catch_2024 <-
  PS_catch2 |> 
  dplyr::filter(set_type == 'ALL' & yy %in% 2020:2024 &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy_bin = as.character(yy))

pal2 <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(11), 'white'))
p <- 
  bind_rows(catch_hist, catch_2024) |>
  dplyr::filter(sp_code == 'YFT') |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch2, fill = ..level..), bins = 11, h = 10) +  # Transparent fill
  geom_contour(aes(z = catch2), color = "grey50", linewidth = 0.5, bins = 11, h = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy_bin) +
  scale_fill_manual(values=pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - YFT") +
  gg.theme +
  theme(legend.position = 'none')
p  
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_YFT68.png'),height = 8, width = 11, units = "in", dpi = 200)


pal <- rev(c(brewer.pal(n = 9, name = "RdYlBu"), 'white'))
p <- 
PS_catch2 |>
  dplyr::filter(yy %in% c(2020:2024) & lat >=-20 &lat <= 20 & set_type == 'ALL' & sp_code == 'SKJ') |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch2, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch2), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy) +
  scale_fill_manual(values=pal) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - SKJ") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_SKJ.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
PS_catch2 |>
  dplyr::filter(lat >=-20 &lat <= 20 & set_type == 'ALL' & sp_code == 'SKJ') |>
  dplyr::filter(!(yy %in% c(2001:2019)) & !is.na(yy)) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy2 = case_when(yy %in% c(1990:2000) ~ '1990:2000',
                         yy %in% c(2020:2024) ~ as.character(yy))) |>
  group_by(lat, lon, sp_code, yy2) |>
  summarise(catch3 = mean(catch2, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch3, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch3), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy2) +
  scale_fill_manual(values=pal) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - SKJ") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_SKJ2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
  PS_catch2 |>
  dplyr::filter(lat >=-20 &lat <= 20 & set_type == 'ALL' & sp_code == 'YFT') |>
  dplyr::filter(!(yy %in% c(2001:2019))) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy2 = case_when(yy %in% c(1990:2000) ~ '1990:2000',
                         yy %in% c(2020:2024) ~ as.character(yy))) |>
  group_by(lat, lon, sp_code, yy2) |>
  summarise(catch3 = mean(catch2, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch3, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch3), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy2) +
  scale_fill_manual(values=pal) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - YFT") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_YFT2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
  PS_catch2 |>
  dplyr::filter(lat >=-20 &lat <= 20 & set_type == 'ALL' & sp_code == 'BET') |>
  dplyr::filter(!(yy %in% c(2001:2019))) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy2 = case_when(yy %in% c(1990:2000) ~ '1990:2000',
                         yy %in% c(2020:2024) ~ as.character(yy))) |>
  group_by(lat, lon, sp_code, yy2) |>
  summarise(catch3 = mean(catch2, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch3, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch3), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy2) +
  scale_fill_manual(values=pal) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - BET") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_BET2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# longline density plot - filter through spps
catch_hist <- tuna_catch |> 
  dplyr::filter(gear == 'L' & yy %in% c(1990:2000)) |>
  group_by(lat, lon, yy, sp_code) |>
  summarise(catch2 = sum(sp_mt, na.rm=T)) |>
  group_by(lat, lon, sp_code) |>
  summarise(catch2 = mean(catch2, na.rm=T)) |>
  mutate(yy_bin = '1990-2000')

catch_2024 <-
  tuna_catch |> 
  dplyr::filter(gear == 'L' & yy %in% 2020:2024) |>
  group_by(lat, lon, yy, sp_code) |>
  summarise(catch2 = sum(sp_mt, na.rm=T)) |>
  mutate(yy_bin = as.character(yy))

pal2 <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(11), 'white'))

p <- 
bind_rows(catch_hist, catch_2024) |>
  dplyr::filter(yy_bin %in% c('1990-2000','2024') & sp_code != 'SKJ') |>
  uncount(round(catch2,0)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_density_2d_filled(bins=12, h=10) +
  geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  #geom_contour_filled(aes(z = catch2, fill = ..level..), bins = 10, h=10) +  # Transparent fill
  #geom_contour(aes(z = catch2), color = "black", linewidth = 0.5, bins = 10, h=10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-50, 50)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  scale_fill_manual(values = pal2) +
  facet_wrap(~sp_code+yy_bin, nrow= 4) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Catch weight density', title = "Catch weight density map longline") +
  theme_classic() +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_LL_allsp.png'),height = 11, width = 8, units = "in", dpi = 200)

p <- 
  tuna_catch |> 
  filter(gear == 'L') |>
  dplyr::select(yy, gear, sp_code, lat, lon, catch = sp_mt) |>
  dplyr::filter(!(yy %in% c(2001:2019)) & gear == 'L' & sp_code == 'YFT') |>
  group_by(lat, lon, yy, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy2 = case_when(yy %in% c(1990:2000) ~ '1990:2000',
                         yy %in% c(2020:2024) ~ as.character(yy))) |>
  group_by(lat, lon, sp_code, yy2) |>
  summarise(catch3 = mean(catch2, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch3, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch3), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 220), ylim = c(-30, 30)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  scale_fill_manual(values = pal) +
  facet_wrap(~yy2) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Catch weight density', title = "Catch weight density map longline - YFT") +
  theme_classic() +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_LL_YFT2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
PS_catch2 |>
  dplyr::filter(lat >=-20 &lat <= 20 & set_type == 'ALL' & sp_code == 'BET') |>
  dplyr::filter(!(yy %in% c(2001:2019))) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy2 = case_when(yy %in% c(1990:2000) ~ '1990:2000',
                         yy %in% c(2020:2024) ~ as.character(yy))) |>
  group_by(lat, lon, sp_code, yy2) |>
  summarise(catch3 = mean(catch2, na.rm=T)) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch3, fill = ..level..), bins = 10) +  # Transparent fill
  geom_contour(aes(z = catch3), color = "black", linewidth = 0.5, bins = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~yy2) +
  scale_fill_manual(values=pal) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map - BET") +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_density_PS_BET2.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# 5. Proportion of catch > 180 degrees indicator ----

# Purse seine catch > 180 degrees by species
p <- 
PS_catch2 |>
  dplyr::filter(set_type == 'ALL') |>
  group_by(sp_code, yy) |>
  summarise(catch2 = sum(catch, na.rm = TRUE),  # Sum catch across species, year, and set type
  catch170 = sum(catch[lon >= 180], na.rm = TRUE)) |>
  mutate(prop = catch170/catch2) |>
  group_by(sp_code) |>
  mutate(mn_9020 = mean(prop[yy %in% c(1990:2000)]),
         prop_norm = prop-mn_9020) |>
  ggplot() +
  aes(yy, prop_norm*100, col = sp_code) +
  geom_line() +
  geom_point(size=0.8) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size=0.1) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Catch (%)', col = 'Species',
       title = 'Proportion of PS catch >= 180 degrees (normalised by 1990-2000 mean)') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_180prop_PS.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

p <- 
tuna_catch |>
  filter(gear == 'L' & sp_code != 'SKJ') |>
  #mutate(sp_code = ifelse(str_detect(sp_code, 'ALB'), 'ALB', sp_code)) |>
  group_by(sp_code, yy) |>
  summarise(catch2 = sum(sp_mt, na.rm = TRUE),  # Sum catch across species, year, and set type
            catch20 = sum(sp_mt[lat>=20 | lat <=-20], na.rm = TRUE)) |>
  mutate(prop = catch20/catch2) |>
  group_by(sp_code) |>
  mutate(mn_9020 = mean(prop[yy %in% c(1990:2000)]),
         prop_norm = prop-mn_9020) |>
  ggplot() +
  aes(yy, prop_norm*100, col = sp_code) +
  geom_line() +
  geom_point(size=0.8) +
  geom_smooth(method = 'lm', se=F, linetype='dashed', size=0.1) +
  geom_hline(yintercept = 0) +
  scale_color_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Catch (%)', col = 'Species',
       title = 'Proportion of longline catch outside 20 degrees latitude (normalised by 1990-2000 mean)') +
  gg.theme +
  theme(position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'catch/catch_20prop_LL.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)



  
  
  
  