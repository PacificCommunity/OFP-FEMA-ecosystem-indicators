# Estimating long term trends in the longitude of WCPFC purse seine effort
# Author: Nick Hill 4/6/25
# This analysis forms part of the SPC climate and ecosystem indicators report 2025 looking
# to provide a snapshot of current climate and any underlying climate driven trends in
# the environment and fishery.

# The intent of this analysis is to determine if there is an underlying longitudinal
# shift in purse seine effort that reflects the eastward
# shift in tuna biomass predicted under climate change by SEAPODYM (Bell et al. 2021).

# 1. Preamble ----
# libraries
library(tidyverse)
library(patchwork)
library(mgcv)
library(gratia)
library(sp)
library(flextable)
#library(RODBC)
#library(sdmTMB)

# directories
# data_wd <- './2025_analyses/data/'
# results_wd <- "./2025_analyses/results/"
data_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/data/'
results_wd <- 'P:/OFPEMA/WCPFC/SC21/Ecosystem_indicators/2025_analyses/results/'

# Load map
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

# Read in WCPFC-CA boundary info
wcp_ca = read.csv(paste0(data_wd, "wcpfc_ca_stat_area.csv"))
wcp = Polygon(wcp_ca)
wcp = SpatialPolygons(list(Polygons(list(wcp), ID = "CA")), 
                      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords = wcp@polygons[[1]]@Polygons[[1]]@coords

# Flags of interest 
flgs <- c('VU', 'US', 'TW', 'JP', 'FM', 'KR', 'KI', 'SB', 'PG', 'PH')

# Get ENSO data
# Here we extract ENSO data from NOAA website and add to dataset as a variable

# Get ONI data from NOAA website
# url <- "https://psl.noaa.gov/data/correlation/oni.data"
# destfile <- "../data/oni.data"
# download.file(url, destfile, mode = "wb")
destfile <- paste0(data_wd, 'long_deviance/oni.data.txt')

# Tidy data file
oni_data <- read.table(destfile, header = FALSE, fill = TRUE)[-1,] |>
  setNames(c("year", paste0('M',c(1:12)))) |>
  dplyr::filter(year %in% c(1950:2025)) |>
  mutate(across(starts_with("M"), as.numeric)) |>
  pivot_longer(-year, names_to = 'month', values_to = 'oni') |>
  mutate(month = as.numeric(str_remove(month, "M")),
         year = as.numeric(year),
         oniF = as.factor(ifelse(oni >= 0.5, "elnino", 
                                 ifelse(oni <= -0.5, "lanina", "neutral")))) |>
  dplyr::filter(oni != -99.9) |>
  dplyr::rename(yy = year, mm = month)

# 2. Extract catch and effort data from database ----

# 2.1 extract sbest PS data from database ----
# Here, we extract 1 degree SBEST PS catch and effort data that has set type information.
# This means it is not quite the 'complete' dataset, but set type an important model variable.

# NOUFAMESQL04 - new db - replace OFP_DBS with this
# # db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
# # channel <- odbcDriverConnect(db1)
# # 
# # PS effort
# ps_effort <- sqlQuery(channel, "select
#   sbsch.S_BEST_ID,
#   sbsch.S_SET_ID,
#   sba.YY as 'yy',
#   sba.MM as 'mm',
#   sba.FLAG_CODE as 'flag',
#   sba.FLEET_CODE as 'fleet',
#   ref.GetLonDfromLon(sba.lon_short) as 'lond',
#   ref.GetLatDFromLat(sba.lat_short) as 'latd',
#   sbsch.Schass_code as 'school',
#   sbsch.days_sch as 'stdeff',
#   sbsch.sets_sch as 'sets'
#   from best.S_BEST_AGG_SCHEFF sbsch
#   join best.S_BEST_AGG sba on sbsch.S_BEST_ID = sba.S_BEST_ID
#   WHERE yy>1989 AND yy<=2023
#   ",as.is = T)
# 
# # PS catch
# ps_catch <- sqlQuery(channel, " SELECT
#  sbc.S_BEST_ID,
#  sbc.S_SET_ID,
#  sba.YY as 'yy',
#  sbc.SP_CODE as 'sp_code',
#  sbc.SP_MT as 'sp_mt'
#  from best.S_BEST_AGG_CATCH sbc
#  left outer join best.S_BEST_AGG sba on sbc.S_BEST_ID = sba.S_BEST_ID
#  WHERE yy>1989 AND yy<=2023 AND sp_code IN ('BET', 'SKJ', 'YFT')
#  ", as.is = T)
# 
# odbcCloseAll()
# 
# ps_catch2 <-
#   ps_catch |>
#   pivot_wider(names_from = 'sp_code', values_from = 'sp_mt')
# 
# ps_dat <- left_join(ps_effort, ps_catch2, by = c('S_BEST_ID', 'S_SET_ID', 'yy')) |>
#   mutate(S_BEST_ID = as.factor(S_BEST_ID), yy = as.numeric(yy), mm = as.numeric(mm),
#              flag = as.factor(flag), fleet = as.factor(fleet), lond = as.numeric(as.character(lond)), 
#              latd = as.numeric(as.character(latd)), school = as.factor(school), 
#              stdeff = as.numeric(stdeff), sets = as.numeric(sets), S_SET_ID = as.factor(S_SET_ID),
#              SKJ = as.numeric(SKJ), BET = as.numeric(BET), YFT = as.numeric(YFT),
#          lond = ifelse(lond < 0, lond + 360, lond))

# save(ps_effort, file = paste0(data_wd, "effort/effort_PS_agg_data_1990-2023_upd.Rdata"))
# save(ps_catch, file = paste0(data_wd, "catch/catch_PS_agg_data_1990-2023_upd.Rdata"))
# save(ps_dat, file = paste0(data_wd, "long_deviance/combined_PS_agg_data_1990-2023_upd.Rdata"))

# 2.2 Extract logbook PS data from database ----
# Here, we extract logbook PS data from database

#NOUFAMESQL04 - new db - replace OFP_DBS with this
# db1 <- "driver=SQL Server;server=NOUFAMESQL04;database=FISH_MASTER"
# channel <- odbcDriverConnect(db1)
#
# logbook PS data
# logps_raw <- sqlQuery(channel, "select
#   ls.log_trip_id,
#   ls.log_set_id,
#   ls.logdate,
#   vi.flag_id,
#   vi.flag_rg_id,
#   lt.fleet_code,
#   lt.vessel_id,
#   ls.school_id,
#   ls.s_activity_id,
#   ls.latd,
#   ls.lond,
#   ls.effort_factor,
#   lc.sp_code,
#   lc.sp_mt
#   from log.sets_ps ls
#   left join log.catch_ps lc on ls.log_set_id = lc.log_set_id and ls.s_activity_id in (1)
#   left join log.trips_ps lt on ls.log_trip_id = lt.log_trip_id
#   left join ref.vessel_instances vi on lt.vessel_id = vi.vessel_id
#   where lc.sp_code in ('BET', 'SKJ', 'YFT')",as.is = T)
# 
# logps_raw2 <- sqlQuery(channel, "SELECT
#   ls.log_trip_id,
#   ls.log_set_id,
#   ls.logdate,
#   vi.flag_id,
#   vi.flag_rg_id,
#   lt.fleet_code,
#   lt.vessel_id,
#   ls.school_id,
#   ls.s_activity_id,
#   ls.latd,
#   ls.lond,
#   ls.effort_factor,
#   SUM(COALESCE(x.bet_mt, 0)) AS bet_mt,
#   SUM(COALESCE(x.skj_mt, 0)) AS skj_mt,
#   SUM(COALESCE(x.yft_mt, 0)) AS yft_mt
# FROM log.sets_ps ls
# LEFT JOIN (
#   SELECT
#     log_set_id,
#     SUM(CASE WHEN sp_code = 'BET' THEN sp_mt ELSE 0 END) AS bet_mt,
#     SUM(CASE WHEN sp_code = 'SKJ' THEN sp_mt ELSE 0 END) AS skj_mt,
#     SUM(CASE WHEN sp_code = 'YFT' THEN sp_mt ELSE 0 END) AS yft_mt
#   FROM log.catch_ps
#   GROUP BY log_set_id
# ) x ON x.log_set_id = ls.log_set_id
# LEFT JOIN log.trips_ps lt ON ls.log_trip_id = lt.log_trip_id
# LEFT JOIN ref.vessel_instances vi ON lt.vessel_id = vi.vessel_id
# WHERE ls.s_activity_id IN (1)
# GROUP BY
#   ls.log_trip_id, ls.log_set_id, ls.logdate, vi.flag_id, vi.flag_rg_id, 
#   lt.fleet_code, lt.vessel_id, ls.school_id, ls.s_activity_id, ls.latd, 
#   ls.lond, ls.effort_factor",as.is = T)
# 
# logps_raw <- sqlQuery(channel, "SELECT
# 		pt.log_trip_id,
# 		ps.log_set_id,
# 		v.vessel_id,
# 		vi.flag_id,
#     vi.flag_rg_id,
#     ps.school_id,
# 		log.getlatdfromlat(rtrim(ps.lat)) as lat,
# 		log.getlond360fromlon(rtrim(ps.lon)) as lon,
# 		case when ps.eez_code in ('KI','GL','LN','PX') then 'KI' else ps.eez_code end as ez_id,
# 		YEAR(logdate) as yy,  
# 		MONTH(logdate) as mm,
# 		ps.s_activity_id as s_act_id,
# 		ISNULL(cast(sum(case when sp_code = 'YFT' and discard = 0 then coalesce(sp_mt_est,00000.000) else 00000.000 end) as float),0.000) as YFT,
# 		ISNULL(cast(sum(case when sp_code = 'BET' and discard = 0 then coalesce(sp_mt_est,00000.000) else 00000.000 end) as float),0.000) as BET,
# 		ISNULL(cast(sum(case when sp_code = 'SKJ' and discard = 0 then coalesce(sp_mt_est,00000.000) else 00000.000 end) as float),0.000) as SKJ
# FROM log.trips_ps pt
# 		INNER JOIN ref.vessels v ON v.vessel_id = pt.vessel_id 
# 		INNER JOIN ref.vessel_instances vi ON vi.vessel_id = v.vessel_id and pt.depart_date between vi.start_date and vi.calculated_end_date
# 		INNER join log.sets_ps ps on  pt.log_trip_id = ps.LOG_TRIP_ID
# 		INNER join log.catch_ps pc on  ps.LOG_SET_ID = pc.LOG_SET_ID
# WHERE YEAR(logdate) >= 1990
# 		and YEAR(logdate) <= 2023
# 		and ps.s_activity_id = 1
# GROUP BY 
# 		pt.log_trip_id,
# 		ps.log_set_id,
# 		v.vessel_id,
# 		vi.flag_id,
# 		log.getlatdfromlat(rtrim(ps.lat)),
# 		log.getlond360fromlon(rtrim(ps.lon)),
# 		case when ps.eez_code in ('KI','GL','LN','PX') then 'KI' else ps.eez_code end,
# 		YEAR(logdate),  
# 		MONTH(logdate),
# 		ps.s_activity_id,
#         vi.flag_rg_id,
#         ps.school_id", as.is = T)

# odbcCloseAll()

#save(logps_raw, file = paste0(data_wd, "long_deviance/logbook_ps_raw_data.Rdata"))

# 3. Clean and summarise raw PS data ----

# 3.1 SBEST agregated data ----
#load(file = paste0(data_wd, "effort/effort_PS_agg_data_1990-2023_upd.Rdata"))
#load(file = paste0(data_wd, "catch/catch_PS_agg_data_1990-2023_upd.Rdata"))

load(file = paste0(data_wd, "long_deviance/combined_PS_agg_data_1990-2023_upd.Rdata"))
head(ps_dat)

# tidy sbest data
sbest_dat <- 
  ps_dat |>
  mutate(set_type = as.factor(case_when(school %in% c(3,4) ~ 'DFAD', # bin set type
                          school %in% c(5) ~ 'AFAD',
                          school %in% c(6,7) ~ 'WHALE',
                          school %in% c(1,2,-9) ~ 'FREE')),
    WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |> # add wcpfc coords
  mutate(across(c(BET, SKJ, YFT), ~ ifelse(is.na(.), 0, .))) |> # make tuna sp catch colunms
  dplyr::filter(sets >= 1) |>
  mutate(sets = round(sets, 0)) |>
  dplyr::filter(WCP_CA %in% c(1, 2)) |> # remove 0 set columns and sets outside wcpfc
  dplyr::select(-WCP_CA) |>
  dplyr::filter(!(latd > 10 | latd < -15) & lond >= 130) #|> # define spatial area
  #dplyr::filter(flag %in% flgs & !(flag == 'PH' & fleet == 'PH')) |> # drop flags (do this later)
  #dplyr::filter(!(school %in% c(6,7))) # drop whale sets

# Group by appropriate variables and join ONI data
sbest_dat2 <- 
sbest_dat |>
  dplyr::filter(flag %in% flgs & !(flag == 'PH' & fleet == 'PH')) |> # drop flags (do this later)
  dplyr::filter(!(school %in% c(6,7))) |> # drop whale sets
  left_join(oni_data, by = c('yy', 'mm')) |> # left join ONI data
  group_by(yy, mm, lond, latd, flag, set_type, oniF) |> # group_by to remove school/set type discrepancies
  summarise(stdeff = sum(stdeff, na.rm = T),
            sets = sum(sets, na.rm =T),
            SKJ = sum(SKJ, na.rm = T)) |> # define flags
  mutate(cpue = SKJ/stdeff, cpue_log = log(cpue + 1)) |>
  droplevels()

colSums(is.na(sbest_dat2))
#hist(sbest_dat2$lond)
#hist(sbest_dat2$cpue)
#hist(sbest_dat2$cpue_log)

save(sbest_dat2, file = paste0(data_wd, "long_deviance/PS_SBEST1_counted_1990-2023_clean_data2.Rdata"))

# 3.2 SBEST uncounted data ----
# Finalised data set 
sbest_dat3 <-
  sbest_dat2 |>
  uncount(sets)

nrow(sbest_dat2)
nrow(sbest_dat3)

# Any NAs left?
colSums(is.na(sbest_dat3))

# Check data structure
glimpse(sbest_dat3)
save(sbest_dat3, file = paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))

# 3.3 Plots summarising SBEST and checking for errors ----
# Note changing of df between sbest_dat and sbest_dat3 to ID flags and set types

# Check set locations
sbest_dat |>
  #dplyr::filter(yy %in% c(2019:2023)) |>
  group_by(latd, lond) |>
  summarise(stdeff = sum(stdeff, na.rm = T)) |>
  ggplot() +
  aes(lond, latd, fill = stdeff) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  geom_tile() +
  coord_sf(xlim = c(100, 300), ylim = c(-50, 50)) +
  scale_fill_viridis_c() +
  #facet_wrap(~yy) +
  gg.theme +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Stdeff')

# Check locations by flags - see PH domestic fleet
sbest_dat |>
  #dplyr::filter(yy %in% c(2019:2023)) |>
  group_by(latd, lond, flag) |>
  summarise(sets = sum(sets, na.rm = T)) |>
  ggplot() +
  aes(lond, latd, fill = sets) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  geom_tile() +
  coord_sf(xlim = c(110, 250), ylim = c(-20, 20)) +
  scale_fill_viridis_c() +
  facet_wrap(~flag) +
  gg.theme +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Stdeff')

# Check flags
summ_stats <- sbest_dat %>%
  mutate(flag2 = as.factor(ifelse(flag %in% c(flgs), paste0(flag,'*'), paste0(flag)))) |>
  group_by(yy, flag2) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE),
            q25 = quantile(lond, 0.25, na.rm = TRUE),
            q75 = quantile(lond, 0.75, na.rm = TRUE))
p <- 
  sbest_dat |>
  mutate(flag2 = as.factor(ifelse(flag %in% c(flgs), paste0(flag,'*'), paste0(flag)))) |>
  ggplot() +
  geom_violin(aes(x=as.numeric(as.character(yy)), y = lond, group = as.factor(yy)), fill = 'grey50', alpha = 0.2) + 
  geom_point(data = summ_stats, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), y = mean_lond), 
             color = "red", size = 1.5) +
  geom_errorbar(data = summary_stats2, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), 
                                           ymin = q25, ymax = q75),width = 0.2, color = "black") +
  facet_wrap(~flag2) +
  gg.theme +
  labs(x = 'Year', y = 'Longitude') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(sbest_dat$lond, na.rm=TRUE), linetype = "dashed", color = 'black')
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_lond_by_yr_flag.png'))

# stacked barplot of set type by year by flag
p <- 
  sbest_dat |>
  ggplot() +
  aes(as.numeric(as.character(yy)), fill = set_type) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~flag) +
  gg.theme +
  labs(x = 'Year', y = 'Sets (%)', fill = 'Set type') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_type_by_yr_flag.png'))

# Longitude distribution of sets by flag  and year stacked
p <- 
  sbest_dat3 |>
  ggplot() +
  aes(x = lond, fill = flag) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = 'Set3') +
  facet_wrap(~yy) +
  labs(x = 'Longitude', y = 'Sets (%)', fill = 'Flag') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
p 
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_lond_yr_%fill.png'))

# Explore set type
p <- 
sbest_dat |>
  ggplot() +
  aes(x = lond, fill = set_type) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~yy) +
  labs(x = 'Longitude', y = 'Sets', fill = 'Set type') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_settype_yr_stacked.png'))

# By ENSO
p <-
  sbest_dat2 |>
  ggplot() +
  aes(x = lond, y = sets) +
  geom_bar(stat = 'identity') +
  #scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~oniF, ncol = 1, scales = 'free_y') +
  labs(x = 'Longitude', y = 'Sets', fill = 'ENSO') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_ENSO.png'))

# 4. clean PS logbook data ----
load(file = paste0(data_wd, "long_deviance/logbook_ps_raw_data.Rdata"))

logps_dat <-
  logps_raw |>
  mutate(set_type = as.factor(case_when(school_id %in% c(3,4) ~ 'DFAD',
                                        school_id %in% c(5) ~ 'AFAD',
                                        school_id %in% c(6,7) ~ 'WHALE',
                                        school_id %in% c(1,2,-9) ~ 'FREE',
                                        school_id == 0 ~ NA)),
         yy = as.numeric(yy), mm = as.numeric(mm), log_set_id = as.factor(log_set_id),
         vessel_id = as.factor(vessel_id), flag_id = as.factor(flag_id),
         flag_rg_id = as.factor(flag_rg_id), latd = round(as.numeric(lat), 4), 
         lond = round(as.numeric(lon), 4), SKJ = round(as.numeric(SKJ), 3), 
         BET = round(as.numeric(BET), 3), YFT = round(as.numeric(YFT), 3),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2]),
         latd2 = round(latd / 5) * 5, lond2 = round(lond / 5) * 5) |>
  dplyr::filter(WCP_CA %in% c(1, 2)) |> # & latd >= -20 & latd <= 10 & lond >= 140 & lond <=210
  dplyr::select(-WCP_CA) |>
  dplyr::filter(!(latd > 10 | latd < -15) & lond >= 130) |>
  #dplyr::filter(flag_id %in% flgs & !(flag_id == 'PH' & flag_rg_id == 'PH') &
                  #!is.na(set_type)) |> 
  droplevels() |>
  dplyr::select(set_id = log_set_id, yy, mm, latd, lond, latd2, lond2, flag = flag_id, 
                flag2 = flag_rg_id, vessel_id, set_type, BET, SKJ, YFT) |>
  left_join(oni_data, by = c('yy', 'mm'))

logps_dat2 <- logps_dat |>
  dplyr::filter(flag %in% flgs & !(flag == 'PH' & flag2 == 'PH') & !is.na(set_type)) |>
  dplyr::filter(flag != 'PH') |>
  mutate(SKJ_log = log(SKJ + 1)) |>
  droplevels()
table(logps_dat2$yy, logps_dat2$flag)
table(logps_dat$yy, logps_dat$flag)

save(logps_dat2, file = paste0(data_wd, "long_deviance/logbook_ps_cleaned_data2.Rdata"))

# 4.1 Plot logbook data
#load(file = paste0(data_wd, "long_deviance/logbook_ps_raw_data.Rdata"))
load(file = paste0(data_wd, "long_deviance/logbook_ps_cleaned_data2.Rdata"))

# Map data by flag to ID errors
logps_dat |>
  group_by(lond2, latd2) |>
  summarise(sets = n()) |>
  ggplot() +
  aes(lond2, latd2, fill = sets) +
  geom_tile() +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 300), ylim = c(-50, 50)) +
  scale_fill_viridis_c() +
  #facet_wrap(~flag) +
  gg.theme +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Stdeff')

# Check flags
summ_stats <- logps_dat %>%
  mutate(flag2 = as.factor(ifelse(flag %in% c(flgs), paste0(flag,'*'), paste0(flag)))) |>
  group_by(yy, flag2) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE),
            q25 = quantile(lond, 0.25, na.rm = TRUE),
            q75 = quantile(lond, 0.75, na.rm = TRUE))

logps_dat |>
  mutate(flag2 = as.factor(ifelse(flag %in% c(flgs), paste0(flag,'*'), paste0(flag)))) |>
  ggplot() +
  geom_violin(aes(x=as.numeric(as.character(yy)), y = lond, group = as.factor(yy)), fill = 'grey50', alpha = 0.2) + 
  geom_point(data = summ_stats, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), y = mean_lond), 
             color = "red", size = 1.5) +
  geom_errorbar(data = summ_stats, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), 
                                           ymin = q25, ymax = q75),width = 0.2, color = "black") +
  facet_wrap(~flag2) +
  gg.theme +
  labs(x = 'Year', y = 'Longitude') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(logps_dat$lond, na.rm=TRUE), linetype = "dashed", color = 'black')

tmp1 <- sbest_dat |>
  group_by(yy) |>
  summarise(sets = sum(sets, na.rm = T), SKJ = sum(SKJ, na.rm = T)) |>
  mutate(data = 'sbest', yy = as.factor(yy))

tmp2 <- logps_dat |>
  group_by(yy) |>
  summarise(sets = n(), SKJ = sum(SKJ, na.rm = T)) |>
  mutate(data = 'logbook', yy = as.factor(yy))

p <- 
bind_rows(tmp1,tmp2) |>
  pivot_longer(-c(yy, data), names_to = 'metric', values_to = 'value') |>
  ggplot() +
  aes(yy, value/1000, fill = data) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Sets (*1000)', fill = 'Source') +
  facet_wrap(~metric, scales = 'free') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_catch_effort_logbook_vs_sbest.png'))

# 5. Final data prep for model ----

# 5.1 sbest data ----
load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))

# final checks
colSums(is.na(sbest_dat3))
table(sbest_dat3$yy, sbest_dat3$flag)
glimpse(sbest_dat3)

hist(sbest_dat3$lond)
hist(sbest_dat3$cpue_log)

# 5.2 logbook data ----
load(file = paste0(data_wd, "long_deviance/logbook_ps_cleaned_data2.Rdata"))

# final checks
colSums(is.na(logps_dat2))
table(logps_dat2$yy, logps_dat2$flag)
glimpse(logps_dat2)

hist(logps_dat2$lond)
hist(logps_dat2$SKJ_log)

# 6. Run models ----

mod.dir <- paste0(results_wd, 'long_deviance/')
sp <- 'SKJ'
#dw_flags <- c('CN', 'FM', 'JP', 'KI', 'KR', 'PG', 'TW', 'US', 'VU')
flgs <- c("VU", "US", "TW", "JP", "FM", "KR", "KI", "SB", "PG", "PH")

# 6.1 modI - SBEST uncounted no wts linear yy model ----

mod.dir <- paste0(results_wd, 'long_deviance/')
sp <- 'SKJ'

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))
head(sbest_dat3)

df <- sbest_dat3

fn <- lond ~ yy + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + s(flag, bs = "re", k = 10) + set_type + oniF

mod_nm <- paste0('/modI_sbest_uncounted_stes_flgs_nowts_lond_linyy')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_yy_linear <- mod

# 6.2 modJ - SBEST uncounted no wts factor yy model ----

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))
head(sbest_dat3)

df <- sbest_dat3 |>
  mutate(yy = as.factor(as.character(yy)))

fn <- lond ~ yy + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + flag  + set_type + oniF

mod_nm <- paste0('/modJ_sbest_uncounted_sets_flgs_nowts_lond_yyF')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_yyF <- mod

# 6.3 modK - SBEST uncounted no wts yr smooth per flag model ----

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))

df <- sbest_dat3 
fn <- lond ~ s(yy, by = flag, k = 20) + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + set_type + oniF

mod_nm <- paste0('/modK_sbest_uncounted_sets_flgs_nowts_lond_s(yy_byflag)')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_syearbyflag <- mod

# 6.4 modL - SBEST uncounted no wts shared yr flag smooth model ----

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))

df <- sbest_dat3 
fn <- lond ~ s(yy, flag, bs = "fs") + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + set_type + oniF

mod_nm <- paste0('/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_syearflag <- mod

# Model 6.4 performs best and so further tests done on this one

# 6.5 modM - SBEST uncounted logcpue wts shared yr flag smooth model ----

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))

df <- sbest_dat3 
fn <- lond ~ s(yy, flag, bs = "fs") + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + set_type + oniF

mod_nm <- paste0('/modM_sbest_uncounted_sets_flgs_logcpuewts_lond_s(yy_flag)_intx')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           weights = df$cpue_log,
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_syearflag_wts <- mod

# 6.6 modN - SBEST uncounted freeschool no wts shared yr flag smooth model ----

load(paste0(data_wd, "long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data2.Rdata"))
head(sbest_dat3)

df <- sbest_dat3 |>
  dplyr::filter(set_type  == 'FREE')|>
  droplevels()

table(df$yy, df$flag)

fn <- lond ~ s(yy, flag, bs = "fs") + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + oniF

mod_nm <- paste0('/modN_sbest_uncounted_FSsets_flgs_nowts_lond_s(yy_flag)_intx')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_syearflag_FS <- mod

# 6.7 modO - logbook no wts shared yr flag smooth model ----

load(file = paste0(data_wd, "long_deviance/logbook_ps_cleaned_data2.Rdata"))
head(logps_dat2)

df <- logps_dat2 |>
  droplevels() |>
  mutate(yy = as.numeric(as.character(yy)))

fn <- lond ~ s(yy, flag, bs = "fs") + s(mm, bs = "cc", k = 6) + s(latd, k = 5) + set_type + oniF

mod_nm <- paste0('/modO_logbook_sets_flgs_nowts_lond_s(yy_flag)_intx')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(fn,                           
           data = df,
           family = Gamma(link = "log"),
           method = "fREML",
           discrete = TRUE)
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
mod_syearflag_logbk <- mod

# Now go to long_deviance_model_eval.R for model evaluation
####################################################


# 7. Evaluate Jo models ----

# AIC for each model - note can't compare last 2 models - different input data
AIC(mod_yy_linear, 
    mod_yyF, 
    mod_syearbyflag,
    mod_syearflag,
    mod_syearflag_wts, 
    mod_syearflag_FS,
    mod_syearflag_logbk) 

# deviance explained
summary(mod_yy_linear)$dev.expl
summary(mod_yyF)$dev.expl
summary(mod_syearbyflag)$dev.expl
summary(mod_syearflag)$dev.expl
summary(mod_syearflag_wts)$dev.expl
summary(mod_syearflag_FS)$dev.expl
summary(mod_syearflag_logbk)$dev.expl

# Plot year effect
library(ggeffects)
d1 <- as.data.frame(ggpredict(mod_yy_linear, terms = "yy")) |> mutate(mod = 'mod_yy_linear')
d2 <- as.data.frame(ggpredict(mod_yyF, terms = "yy")) |> mutate(mod = 'mod_yyF', x = as.numeric(as.character(x))) 
d3 <- as.data.frame(ggpredict(mod_syearbyflag, terms = "yy")) |> mutate(mod = 'mod_syearbyflag')
d4 <- as.data.frame(ggpredict(mod_syearflag, terms = "yy")) |> mutate(mod = 'mod_syearflag')
d5 <- as.data.frame(ggpredict(mod_syearflag_wts, terms = "yy")) |> mutate(mod = 'mod_syearflag_wts')
d6 <- as.data.frame(ggpredict(mod_syearflag_FS, terms = "yy")) |> mutate(mod = 'mod_syearflag_FS')
d7 <- as.data.frame(ggpredict(mod_syearflag_logbk, terms = "yy")) |> mutate(mod = 'mod_syearflag_logbk')

bind_rows(p1, p2, p3, p4, p5,p6, p7) |>
  rename(yy = x) |>
  ggplot() +
  aes(x = yy, y = predicted, ymin = conf.low, ymax = conf.high, col = mod, fill = mod) +
  geom_line(size = 1) +
  geom_ribbon(col = NA, alpha = 0.2) +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') +
  gg.theme +
  labs(x = 'Year', y = 'Predicted longitude', col = 'Model', fill = 'Model')

# Plot
p1 <- plot(ggpredict(mod_yy_linear, terms = "yy")) + ggtitle('mod_yy_linear.pdf') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modI_sbest_uncounted_stes_flgs_nowts_lond_linyy/yr_effect_plot.pdf'))
p2 <- plot(ggpredict(mod_yyF, terms = "yy")) + ggtitle('mod_yyF.pdf') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modJ_sbest_uncounted_sets_flgs_nowts_lond_yyF/yr_effect_plot.pdf'))
p3 <- plot(ggpredict(mod_syearbyflag, terms = "yy")) + ggtitle('mod_syearbyflag') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modK_sbest_uncounted_sets_flgs_nowts_lond_s(yy_byflag)/yr_effect_plot.pdf'))
p4 <- plot(ggpredict(mod_syearflag, terms = "yy")) + ggtitle('mod_syearflag') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/yr_effect_plot.pdf'))
p5 <- plot(ggpredict(mod_syearflag_wts, terms = "yy")) + ggtitle('mod_syearflag_wts.pdf') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modM_sbest_uncounted_sets_flgs_logcpuewts_lond_s(yy_flag)_intx/yr_effect_plot.pdf'))
p6 <- plot(ggpredict(mod_syearflag_FS, terms = "yy")) + ggtitle('mod_syearflag_FS') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modN_sbest_uncounted_FSsets_flgs_nowts_lond_s(yy_flag)_intx/yr_effect_plot.pdf'))
p7 <- plot(ggpredict(mod_syearflag_logbk, terms = "yy")) + ggtitle('mod_syearflag_logbk') + theme_bw()
#ggsave(paste0(results_wd, 'long_deviance/SKJ/modO_logbook_sets_flgs_nowts_lond_s(yy_flag)_intx/yr_effect_plot.pdf'))

library(patchwork)
p1 + p2 + p3 + p4 + p5 +p6 + p7

mean_lond_by_year <- df |>
  group_by(yy) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE), 
            mean_lat = mean(latd, na.rm = TRUE))

# mean_lond_by_flag <- sbest_dat3 |>
#   #mutate(SKJ_log = log(SKJ + 1)) |>
#   dplyr::filter(flag %in% flgs)  |>
#   droplevels() |>
#   mutate(yy = as.numeric(as.character(yy)), set_type = as.factor(ifelse(set_type == 'FREE', 'unassoc', 'assoc'))) %>%
#   group_by(flag) %>%
#   summarise(mean_lond = mean(lond, na.rm = TRUE), .groups = "drop")

mean_lond_by_flag_year <- sbest_dat3 |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(flag %in% flgs & set_type != 'WHALE')  |>
  droplevels() |>
  mutate(yy = as.numeric(as.character(yy))) %>%
  group_by(flag, yy) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE), .groups = "drop")

p4_data <- ggpredict(mod_syearflag, terms = "yy")

p4_plot <- plot(p4_data)

plot(p7) + 
  geom_point(data = mean_lond_by_year, 
             aes(x = as.numeric(as.character(yy)), y = mean_lond), 
             color = "blue", size = 1.2) + 
  theme_bw()
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/yr_effect_plot2.pdf'))

# 
# p4_plot +
#   geom_point(data = mean_lond_by_flag,
#              aes(x = yy, y = mean_lond),
#              color = "gray40", size = 1.2,
#              inherit.aes = FALSE)

p4_plot + 
  geom_point(data = mean_lond_by_flag_year, 
             aes(x = yy, y = mean_lond, color = flag), 
             size = 1.2,
             inherit.aes = FALSE) +
  scale_color_manual(values = scales::hue_pal()(length(unique(mean_lond_by_flag_year$flag)))) + 
  theme_bw()
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/flag_yr_effect_plot.pdf'))


plot(mod_syearflag)

preds <- ggpredict(mod_syearbyflag, terms = c("yy", "flag")) 
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  theme_bw() +
  labs(title = "Predicted longitude over year by flag",
       x = "Year", y = "Predicted longitude") +
  theme(legend.position = "right") +
  ylim(150,200)

deriv <- derivatives(mod_syearflag, select = "s(yy,flag)", interval = "confidence")

sign_change_summary <- deriv %>%
  group_by(flag) %>%
  mutate(sign = sign(.derivative)) %>%
  summarise(
    n_sign_changes = sum(diff(sign) != 0, na.rm = TRUE),
    mostly_positive = mean(.derivative > 0, na.rm = TRUE),
    mostly_negative = mean(.derivative < 0, na.rm = TRUE)
  )
sign_change_summary

group1_flags <- c("KI",  "KR", "TW", "US", "VU")
group2_flags <- c( "FM", "JP", "PG", "PH", "SB")

preds_group1 <- subset(preds, group %in% group1_flags)
preds_group2 <- subset(preds, group %in% group2_flags)

ymin_all <- min(preds$conf.low, na.rm = TRUE)
ymax_all <- max(preds$conf.high, na.rm = TRUE)

p1 <- ggplot(preds_group1, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  theme_bw() +
  coord_cartesian(ylim = c(ymin_all, ymax_all)) +
  labs(title = "Predicted longitude over year by flag",
       x = "Year", y = "Predicted longitude") +
  theme(legend.position = "right")

p2 <- ggplot(preds_group2, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  theme_bw() +
  coord_cartesian(ylim = c(ymin_all, ymax_all)) +
  labs(title = "Predicted longitude over year by flag",
       x = "Year", y = "Predicted longitude") +
  theme(legend.position = "right")

p1|p2
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/randomeffects.by.groups.pdf'))


ggpredict(mod_syearflag, terms = "set_type") %>% 
  plot() + theme_bw()
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/mod_syearflag_settype.pdf'))

preds <- ggpredict(mod_syearflag, terms = c("yy", "set_type")) 
ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  theme_bw() +
  labs(title = "Predicted longitude over year by flag",
       x = "Year", y = "Predicted longitude") +
  theme(legend.position = "right")


ggpredict(mod_syearflag, terms = "oniF") %>% 
  plot() + theme_bw()
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/mod_syearflag_oniF.pdf'))


# inspection of residuals ----
df$resid <- residuals(mod_syearflag, type = "response")

ggplot(df, aes(x = yy, y = resid)) +
  geom_point(alpha = 0.3) +
  #geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~flag, scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Residual (response scale)")
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/mod_syearflag_resids_flag.pdf'))


ggplot(df, aes(x = oniF, y = resid)) +
  geom_boxplot() +
  facet_wrap(~flag, scales = "free_y") +
  theme_bw() +
  labs(x = "ONI phase", y = "Residual (response scale)")
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/mod_syearflag_resids_oniF.pdf'))

ggplot(df, aes(x = yy, y = resid, color = oniF)) +
  geom_point(alpha = 0.2, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  facet_wrap(~flag, scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Residual (response scale)") +
  scale_color_brewer(palette = "Set1") 
ggsave(paste0(results_wd, 'long_deviance/SKJ/modL_sbest_uncounted_sets_flgs_nowts_lond_s(yy_flag)_intx/mod_syearflag_resids_oniFyrflag.pdf'))

ggsave('residuals.ONI.by.year.flag.pdf')

##############################################
# OLD MODELS ----

# 7.1 SBEST uncounted all lond model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data.Rdata")
head(sbest_dat3)

df <- sbest_dat3 |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FR', 'EP', 'VN', 'PA')))  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5) 

mod_nm <- paste0('/modA_sbest_uncounted_sets_all_cpuelogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = Gamma(link = "log"), #Gamma(link = "log")
  weights = df$cpue_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.2 SBEST uncounted free school/DW lond model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_uncounted_1990-2023_clean_data.Rdata")
head(sbest_dat3)

df <- sbest_dat3 |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(flag %in% dw_flags & set_type == 'FREE')  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag  + oniF + s(latd, k=5) 

mod_nm <- paste0('/modB_sbest_uncounted_sets_freedw_cpuelogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = Gamma(link = "log"), #Gamma(link = "log")
  weights = df$cpue_log,
  method = "REML")

saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.3 SBEST counted all cpue model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

df <- sbest_dat2 |>
  dplyr::filter(!(flag %in% c('FR', 'EP', 'VN', 'PA')) & !is.infinite(cpue))  |>
  droplevels() |>
  mutate(sets_log = log(sets + 1), stdeff_log = log(stdeff + 1)) |>
  dplyr::filter(sets >= 1) |>
  mutate(yy = as.factor(yy))

fn <- cpue_log ~ yy + s(mm, k = 3, bs ="cc") + flag  + set_type + oniF + s(latd, k=5) + s(lond, by = yy)

mod_nm <- paste0('/modC_sbest_counted_sets_all_stdeffwts_cpue')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"), #Gamma(link = "log")
  weights = df$stdeff_log,
  method = "REML")

saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.4 SBEST counted FREE/DW subset cpue model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

df <- sbest_dat2 |>
  dplyr::filter(flag %in% dw_flags & set_type == 'FREE'& !is.infinite(cpue))  |>
  droplevels() |>
  mutate(sets_log = log(sets + 1), stdeff_log = log(stdeff + 1)) |>
  dplyr::filter(sets >= 1) |>
  mutate(yy = as.factor(yy))

fn <- cpue_log ~ yy + s(mm, k = 3, bs ="cc") + flag  + oniF + s(latd, k=5) + s(lond, by = yy)

mod_nm <- paste0('/modD_sbest_counted_sets_freedw_stdeffwts_cpue')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"), #Gamma(link = "log")
  weights = df$stdeff_log,
  method = "REML")

saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.5 logbook all lond model ----

load( file = "2025_analyses/data/long_deviance/PS_logbook_1990-2023_clean_data.Rdata")
head(logps_dat2)

df <- logps_dat2  |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FJ')))  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5) 

mod_nm <- paste0('/modE_logbk_sets_all_catchlogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = Gamma(link = "log"), #Gamma(link = "log")
  weights = df$SKJ_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.6 logbook DW/free lond model ----

load( file = "2025_analyses/data/long_deviance/PS_logbook_1990-2023_clean_data.Rdata")
head(logps_dat2)

df <- logps_dat2  |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(flag %in% dw_flags & set_type == 'FREE')  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + oniF + s(latd, k=5) 

mod_nm <- paste0('/modF_logbk_sets_freedw_catchlogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = Gamma(link = "log"), #Gamma(link = "log")
  weights = df$SKJ_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.7 logbook all sets cpue model ----

load( file = "2025_analyses/data/long_deviance/PS_logbook_1990-2023_clean_data.Rdata")
head(logps_dat2)

df <- logps_dat2  |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FJ')))  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- SKJ_log ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5) + s(lond, by = yy)

mod_nm <- paste0('/modG_logbk_sets_all_nowts_catch')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"),  #Gamma(link = "log")
  #weights = df$SKJ_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 7.8 logbook free/dw sets cpue model ----

load( file = "2025_analyses/data/long_deviance/PS_logbook_1990-2023_clean_data.Rdata")
head(logps_dat2)

load( file = "2025_analyses/data/long_deviance/PS_logbook_1990-2023_clean_data.Rdata")
head(logps_dat2)

df <- logps_dat2  |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FJ')))  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

df <- logps_dat2  |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(flag %in% dw_flags & set_type == 'FREE')  |>
  droplevels() |>
  mutate(yy = as.factor(yy))

fn <- SKJ_log ~ yy + s(mm, k = 3, bs ="cc") + flag + oniF + s(latd, k=5) + s(lond, by = yy)

mod_nm <- paste0('/modH_logbk_sets_dwfree_nowts_catch')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"),  #Gamma(link = "log")
  #weights = df$SKJ_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

##########################################################################
######################old##############################################
#######################################################################
# 6.1 SBEST counted all lond model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

df <- sbest_dat2 |>
  #mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FR', 'EP', 'VN', 'PA')) & !is.infinite(cpue))  |>
  droplevels()
fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5) 

mod_nm <- paste0('/mod_sbest_counted_sets_all_cpuelogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"), #Gamma(link = "log")
  weights = cpue_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 6.2 SBEST counted DW-free lond model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

dw_flags <- c('CN', 'FM', 'JP', 'KI', 'KR', 'PG', 'TW', 'US', 'VU')
df <- sbest_dat2 <-
  sbest_dat2 |>
  dplyr::filter(set_type == 'FREE' & flag %in% dw_flags & !is.infinite(cpue))

fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + oniF + s(latd, k=5) 

mod_nm <- paste0('/mod_sbest_counted_sets_DWfree_cpuelogwts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"),  #Gamma(link = "log")
  weights = cpue_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

# 6.3 SBEST counted all cpue model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

df <- sbest_dat2 |>
  mutate(SKJ_log = log(SKJ + 1)) |>
  dplyr::filter(!(flag %in% c('FR', 'EP', 'VN', 'PA')) & !is.infinite(cpue))  |>
  droplevels() |>
  mutate(yy = as.numeric(as.character(yy)))
fn <- cpue_log ~ yy + s(mm, k = 4, bs ="cc") + flag + set_type + oniF + s(latd, k=4) + s(lond, by = yy)
fn <- cpue_log ~ s(mm, k = 4, bs = "cc") + flag + set_type + oniF + s(latd, k = 4) + te(lond, as.numeric(yy), k = c(5,10))

mod_nm <- paste0('/mod_sbest_counted_sets_all_SKJwts_cpue')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
  fn, 
  data = df,
  family = gaussian(link = "identity"), #Gamma(link = "log")
  #weights = SKJ_log,
  method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))

#####################################
# te(lond, yy) interaction
# Convert year factor to numeric
#df <- df %>% mutate(yy_num = as.numeric(as.character(yy)))

years <- unique(df$yy)      # all years (factor levels)
lons <- seq(min(df$lond), max(df$lond), length.out = 100)  # longitude grid

# Create grid for prediction
new_data <- expand.grid(yy = years,lond = lons) |>
  mutate(yy = as.numeric(as.character(yy)),
    mm = median(df$mm, na.rm = TRUE),
         latd = median(df$latd, na.rm = TRUE),
         flag = names(sort(table(df$flag), decreasing = TRUE))[1],
         set_type = names(sort(table(df$set_type), decreasing = TRUE))[1],
         oniF = names(sort(table(df$oniF), decreasing = TRUE))[1])

pred_response <- predict(mod, newdata = new_data, type = "response")
new_data$pred_cpue <- pred_response


ggplot(new_data, aes(x = lond, y = exp(pred_cpue), color = factor(round(yy)))) +
  geom_line() +
  labs(x = "Longitude", y = "Predicted CPUE (log scale)", color = "Year") +
  theme_minimal()

ggplot(new_data, aes(x = lond, y = exp(pred_cpue))) +
  geom_line() +
  facet_wrap(~ factor(round(yy))) +
  labs(x = "Longitude", y = "Predicted CPUE (log scale)", title = "Smooth interaction te(lond, year)") +
  theme_minimal()

weighted_lon <- new_data %>%
  group_by(yy) %>%
  summarize(weighted_lond = sum(lond * pred_cpue) / sum(pred_cpue)) %>%
  ungroup()

ggplot(weighted_lon, aes(x = as.numeric(as.character(yy)), y = weighted_lond)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Weighted Mean Longitude of CPUE Over Years",
    x = "Year",
    y = "Weighted Mean Longitude"
  ) +
  theme_minimal()

################################
# lond yy interaction

years <- levels(df$yy)      # all years (factor levels)
lons <- seq(min(df$lond), max(df$lond), length.out = 100)  # longitude grid

# Create grid for predictions
new_data <- expand.grid(yy = years,lond = lons) |>
  mutate(mm = median(df$mm, na.rm = TRUE),
    latd = median(df$latd, na.rm = TRUE),
    flag = names(sort(table(df$flag), decreasing = TRUE))[1],
    set_type = names(sort(table(df$set_type), decreasing = TRUE))[1],
    oniF = names(sort(table(df$oniF), decreasing = TRUE))[1])

# Add typical values for other covariates used in model
# For numeric: median; for factor: most common level
new_data <- new_data %>%
  mutate(
    mm = median(df$mm, na.rm = TRUE),
    latd = median(df$latd, na.rm = TRUE),
    flag = names(sort(table(df$flag), decreasing = TRUE))[1],
    set_type = names(sort(table(df$set_type), decreasing = TRUE))[1],
    oniF = names(sort(table(df$oniF), decreasing = TRUE))[1])

# Identify only longitude by year smooth terms
lond_smooth_terms <- grep("^s\\(lond\\):yy", colnames(pred_terms$fit), value = TRUE)

# Predict only these terms
pred_terms <- predict(
  mod,
  newdata = new_data,
  type = "terms",
  se.fit = TRUE,
  terms = lond_smooth_terms
)

# Combine with new_data safely
preds_df <- new_data %>%
  bind_cols(as.data.frame(pred_terms$fit))  # now no duplicates!

# Pivot longitude smooths longer (wide to long)
preds_long <- preds_df %>%
  select(yy, lond, all_of(lond_terms)) %>%
  pivot_longer(
    cols = all_of(lond_terms),
    names_to = "term",
    values_to = "fit"
  ) %>%
  # Extract year from term string, e.g. s(lond):yy1990 -> 1990
  mutate(year = str_extract(term, "\\d{4}")) %>%
  filter(year == as.character(yy)) %>%   # keep only matching year rows
  select(year, lond, fit)

# Plot all years on same plot, colored by year:
ggplot(preds_long, aes(x = lond, y = fit, color = year)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Smooth effect of longitude by year",
       x = "Longitude",
       y = "Smooth term fit") +
  scale_color_viridis_d()

ggplot(preds_long, aes(x = lond, y = fit)) +
  geom_line() +
  facet_wrap(~ year) +
  theme_minimal() +
  labs(title = "Smooth effect of longitude by year",
       x = "Longitude",
       y = "Smooth term fit")

# Bind predicted CPUE to new_data
# Predict on link scale (log scale)
pred_link <- predict(mod, newdata = new_data, type = "link")

# Back-transform to response scale (CPUE)
pred_response <- exp(pred_link)
preds_df <- new_data %>%
  mutate(pred_cpue = pred_response)

# Calculate weighted mean longitude by year
weighted_lon <- preds_df %>%
  group_by(yy) %>%
  summarize(weighted_lond = sum(lond * pred_cpue) / sum(pred_cpue)) %>%
  ungroup()

ggplot(weighted_lon, aes(x = as.numeric(as.character(yy)), y = weighted_lond)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Weighted Mean Longitude of CPUE Over Years",
    x = "Year",
    y = "Weighted Mean Longitude"
  ) +
  theme_minimal()

residuals <- residuals(mod, type = "response")  # or "deviance"

qqnorm(residuals)
qqline(residuals, col = "red")

# 6.4 SBEST counted DW-free cpue model ----

load(file = "2025_analyses/data/long_deviance/PS_SBEST1_counted_1990-2023_clean_data.Rdata")
head(sbest_dat2)

dw_flags <- c('CN', 'FM', 'JP', 'KI', 'KR', 'PG', 'TW', 'US', 'VU')
df <- sbest_dat2 <-
  sbest_dat2 |>
  dplyr::filter(set_type == 'FREE' & flag %in% dw_flags)

# 6.2 SBEST model 2 - 

load(file = "2025_analyses/data/long_deviance/combined_PS_agg_data_1990-2023_upd_cleaned.Rdata")
head(ps_dat3)

hist_mn_lon <- mean(ps_dat3$lond, na.rm = T)

ps_dat3 |>
  ggplot() +
  aes(lond) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = hist_mn_lon, col = 'red')

# Create mod folder
mod.dir <- paste0(results_wd, 'long_deviance/')
sp <- 'SKJ'
df <- ps_dat3 |> dplyr::filter(sp_code == sp) 

# Define model function
fn <- lond ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5) 
fn <- catch ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5)  + s(lond, k=5) # lat*lon interaction
fn <- catch ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5)  + s(lond, k=5) #lon*yy interaction
weights = stdeff
#data_list <- split(ps_dat3, ps_dat3$sp_code)

# Model name      
mod_nm <- paste0('/mod8_uncounted_sets_cpuewts_lond')
dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)

mod <- bam(
    fn, 
    data = df,
    family = Gamma(link = "log"), #Gamma(link = "log")
    weights = cpue,
    method = "REML")
saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))


  
# 9. Comparing model outputs ----

# 9.1 Longitude year effect ----
load('2025_analyses/results/long_deviance/SKJ/mod6_uncounted_sets_nowts_lond/mod6_uncounted_sets_nowts_lond_fxpreds.Rdata')
fx.prd6 <- fx.prd |>
  mutate(mod = 'no_wts')
load('2025_analyses/results/long_deviance/SKJ/mod7_uncounted_sets_logcpuewts_lond/mod7_uncounted_sets_logcpuewts_lond_fxpreds.Rdata')
fx.prd7 <- fx.prd |>
  mutate(mod = 'cpuelog_wts')

fx.prd6$mod <- 'no_wts'
fx.prd7$mod <- 'cpuelog_wts'
  
fx.prd <- bind_rows(fx.prd6, fx.prd7)  
  
  
p <- 
  fx.prd |>
  dplyr::filter(fx.term == 'yy') |>
  group_by(mod) |>
  mutate(hist_period = ifelse(yy %in% c(1995:2005), 1, 0)) |>
  ggplot() +
  aes(as.numeric(as.character(yy)), est, ymin = lx, ymax = ux, col = mod) +
  geom_pointrange() +
  geom_line() +
  #geom_hline(yintercept = 1, linetype = 'dashed') +
  geom_hline(yintercept = hist_mn, linetype = 'dashed') +
  scale_color_brewer(palette = 'Set1') +
  #scale_color_manual(values = c("0" = "black", "1" = "red")) +
  #geom_rect(aes(xmin = 1995, xmax = 2005, ymin = 155, ymax = 180),
  #fill = NA, color = "red", size = 1) +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = 'bottom') +
  labs(x = 'Year', y = 'Longitude')  
p  
  
ggsave(p, file = paste0(mod.fld,'yreffect_lond_plot_mod6_vs_mod7.png')) 
  
  
# 9.2 Compare model performance ----
mod6 <- readRDS('2025_analyses/results/long_deviance/SKJ/mod6_uncounted_sets_nowts_lond/mod6_uncounted_sets_nowts_lond_model.Rdata')
mod7 <- readRDS('2025_analyses/results/long_deviance/SKJ/mod7_uncounted_sets_logcpuewts_lond/mod7_uncounted_sets_logcpuewts_lond_model.Rdata')

AIC(mod6, mod7) 

mod6$gcv.ubre
mod7$gcv.ubre  
  
summary(mod6)$dev.expl  
summary(mod7)$dev.expl   
  
summary(mod6)$r.sq
summary(mod7)$r.sq     
  
par(mfrow = c(2, 2))
plot(mod6, residuals = TRUE)
plot(mod7, residuals = TRUE) 

gam.check(mod6)
gam.check(mod7)
  
pred1 <- predict(mod6, newdata = val_data)
pred2 <- predict(mod7, newdata = val_data)

# RMSE:
sqrt(mean((val_data$y - pred1)^2))
sqrt(mean((val_data$y - pred2)^2))

# Or R² manually:
cor(val_data$y, pred1)^2
cor(val_data$y, pred2)^2  
  
# 10. Compare model results with COG indicators ----

# Effort
load(file = paste0(data_wd, "effort/effort_seine_COGs.Rdata"))

effort_cog <- 
  s_eff_COGs |>
  group_by(set_type) |>
  dplyr::mutate(mn_lon = mean(lon[set_type == 'all' & yy %in% c(1995:2005)]),
                   mn_inertia = mean(inertia[set_type == 'all'& yy %in% c(1995:2005)]),
                   mn_lat = mean(lat[set_type == 'all'& yy %in% c(1995:2005)]),
                indicator = 'effort',
                lond_dev = lon - mn_lon, yy = as.factor(yy)) |>
  dplyr::filter(set_type == 'all') |>
  dplyr::select(yy, indicator, set_type, lon, lond_dev)
  
# Catch
load(file = paste0(data_wd, 'catch/catch_seine_COGs.Rdata'))

catch_cog <- 
  PS_catch_COGs |>
  dplyr::filter(sp_code == 'SKJ' & set_type == 'all') |>
  group_by(set_type) |>
  dplyr::mutate(mn_lon = mean(lon[set_type == 'all' & yy %in% c(1995:2005)]),
                mn_inertia = mean(inertia[set_type == 'all'& yy %in% c(1995:2005)]),
                mn_lat = mean(lat[set_type == 'all'& yy %in% c(1995:2005)]),
                indicator = 'catch',
                lond_dev = lon - mn_lon, yy = as.factor(yy)) |>
  dplyr::select(yy, indicator, set_type, lon, lond_dev)

# Model
load('2025_analyses/results/long_deviance/SKJ/mod7_uncounted_sets_logcpuewts_lond/mod7_uncounted_sets_logcpuewts_lond_fxpreds.Rdata')

hist_mn <- mean(fx.prd$est[fx.prd$yy %in% c(1995:2005)])
mod_cog <- 
  fx.prd |>
  dplyr::filter(fx.term == 'yy') |>
  mutate(indicator = 'mod7', lond_dev = est - hist_mn, yy = as.factor(yy),
         lx = lx - hist_mn, ux = ux - hist_mn) |>
  dplyr::select(yy, indicator, lon = est, lond_dev, lx, ux)

# ENSO data 
oni <- oni_data %>%
  mutate(yy = as.factor(yy)) |>
  group_by(yy) %>%
  count(oniF) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(yy, oniF) |>
  mutate(ymin = -Inf, ymax = Inf, xmin = as.numeric(as.character(yy)) - 0.5,
    xmax = as.numeric(as.character(yy)) + 0.5, fill = oniF) %>%
  filter(as.numeric(as.character(yy)) >= 1989)

p <- 
bind_rows(effort_cog, catch_cog, mod_cog) |>
  left_join(oni, by = 'yy') |>
  ggplot() +
  aes(as.numeric(as.character(yy)), lond_dev, col = indicator, group = indicator) +
  geom_rect(data = oni,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE,alpha = 0.15) +
  geom_ribbon(aes(x = as.numeric(as.character(yy)), ymin = lx, ymax = ux), fill = 'grey', col = NA, alpha = 0.5) +
  geom_point() +
  geom_line() +
  #geom_hline(yintercept = hist_mn, col = 'black', linetype = 'dashed') +
  geom_hline(yintercept = 0, col = 'black', linetype = 'dashed') +
  #scale_color_brewer(palette = 'Set1') +
  scale_color_manual(values = c("catch" = "#4DAF4A",  
                                "effort" = "#FF7F00","mod7" = "black")) +
  scale_fill_manual(values = c("elnino" = "red", "lanina" = "blue"),na.value = "white") +
  labs(x = 'Year', y = 'Longitude change', col = 'Indicator') +
  guides(fill = "none") +  # 👈 removes fill legend
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'bottom') +
  xlim(1990,2023)
p
ggsave(p, file = paste0('2025_analyses/results/long_deviance/figs/cog_indices_combined.png')) 

# 11. Influence plots ----

library(influ)

myInfl = Influence$new(mod, data = mod$model)
myInfl$calc()
myInfl$calc
myInfl$summary
myInfl$stanPlot()
myInfl$stepPlot()
myInfl$influPlot()
myInfl$cdiPlot('month')
myInfl$cdiPlotAll()

############################################################
########OLD#############################################


ggplot() +
  geom_rect(
    data = oni_bg,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.15
  ) +
  scale_fill_manual(values = c("el_nino" = "red", "la_nina" = "blue")) 
  
  
gam.check(mod_gam)



#resids_resp <- residuals(mod_gam, type = 'response')
resids_pears1 <- residuals(mod_gam, type = 'pearson')
resids_resp1 <- residuals(mod_gam, type = 'response')
resids_dev1 <- residuals(mod_gam, type = 'deviance')
#resids_pears1 <- residuals(mod_gam, type = 'pearson')

plot(fitted, resids_pears,xlab = "Fitted", ylab = "Pearson residuals")
abline(h = 0, col = "red")
plot(fitted, resids_resp,xlab = "Fitted", ylab = "Response residuals")
abline(h = 0, col = "red")

hist(resids_pears1)
hist(resids_resp1)
hist(resids_resp)

qqnorm(resids_pears1); abline(0,1, col = 'red')
qqnorm(resids_resp1); abline(0,1, col = 'red')
qqnorm(resids_dev1); abline(0,1, col = 'red')



sanity(mod2)
resids_resp <- residuals(mod, type = 'response')
resids_pears <- residuals(mod2, type = 'pearson')
fitted <- fitted(mod2)

plot(fitted, resids_pears,xlab = "Fitted", ylab = "Pearson residuals")
abline(h = 0, col = "red")
plot(fitted, resids_resp,xlab = "Fitted", ylab = "Response residuals")
abline(h = 0, col = "red")

hist(resids_pears)
hist(resids_resp)

qqnorm(resids_pears); abline(0,1, col = 'red')
qqnorm(resids_resp); abline(0,1, col = 'red')

library(ggeffects)
ggeffects::ggpredict(mod, "yy [all]") |> plot()

df |>
  ggplot() +
  aes(lond_dev) +
  geom_histogram(binwidth = 0.5)
df |>
  ggplot() +
  aes(cpue_log) +
  geom_histogram(binwidth = 0.1)


# Create mod folder
mod.dir <- paste0(results_wd, 'long_deviance/')

fn <- lond_dev ~ yy + s(mm, k = 5, bs ="cc") + (1 | flag) + set_type + oniF + s(latd, k=5) 



data_list <- split(ps_dat3, ps_dat3$sp_code)

mod_nm <- paste0('/mod6_uncounted_sets_nowts_lond')
#df <- ps_dat3 |> dplyr::filter(sp_code == 'SKJ') 
library(mgcv)
# Fit and save each model
lapply('SKJ', function(sp) { #names(data_list)
  
  sp <- 'SKJ'
  dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)
  
  df <- ps_dat3 |> dplyr::filter(sp_code == 'SKJ') 
  #df <- data_list[[sp]]
  
  mod <- bam(
    lond ~ yy + s(mm, k = 5, bs ="cc") + flag + set_type + oniF + s(latd, k=5), 
    data = df,
    family = Gamma(link = "log"), #Gamma(link = "log")
    #weights = cpue_log,
    method = "REML")
  
  # mod <- sdmTMB(
  #   formula = fn,
  #   data = df,
  #   family = gaussian(link = "identity"),
  #   weights = df$catch_adj,
  #   spatial = FALSE)
    
    # mod <- sdmTMB(
    #   lond ~ yy + s(mm, k = 3, bs ="cc") + flag + set_type + oniF + s(latd, k=5), 
    #   data = df,
    #   family = Gamma(link = "log"), #Gamma(link = "log")
    #   #weights = cpue_log,
    #   spatial = F)

  
  # Save to RDS file
  saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
  #saveRDS(mod, file = paste0('2025_analyses/results/long_deviance/SKJ/mod6_uncounted_sets_nowts_lond_model.Rdata'))
  #saveRDS(mod, file = paste0(results_wd, 'long_deviance/', sp, '/mod1/mod1_dat_', sp, '_londev_yy_mm_flag_set_enso_lat.Rdata'))
  
  #return(mod)
})

# 7. model evaluation ----
source('2025_analyses/model_diagnostics_utils_nh3.R')
mod.dir <- paste0(results_wd, 'long_deviance/')

mod_files_all <- data.frame(path = list.files(path = mod.dir,
  #pattern = "^mod[0-9]+_dat", 
  pattern = "_model", 
  full.names = TRUE, recursive = T)) |>
  mutate(filename = basename(path), mod = str_extract(filename, "^mod[0-9]+"),
         sp_code = str_match(path, "/(SKJ|BET|YFT)/")[, 2],
         nm = str_extract(filename, ".*(?=_model\\.Rdata)"),
         folder  = paste0(mod.dir, sp_code, '/', nm, '/'))

mod_files <- mod_files_all |> filter(nm == 'mod5_uncounted_sets_catchadj_wts')


for(i in 1:nrow(mods)) { #nrow(mods)
  
  mod.fld <- mod_files$folder[i]
  sp <- mod_files$sp_code[i]
  nm <- str_extract(mod_files$filename[i], ".*(?=_model\\.Rdata)")
  mod <- readRDS(mod_files$path[i])

  mod.data <- mod$data |>
  rowid_to_column(var = 'id')
  mod.family <- mod$family$family
  mod.func <- mod$formula
  mod.vars <- all.vars(formula(mod))

  diagnostic.data <- mod.data %>%
  dplyr::select(., any_of(mod.vars)) %>%
  rename(., response = mod.vars[1]) |>
  rowid_to_column(var = 'id')

# 7.1 Residuals
  message("Extract residuals")
  diagnostic.data$resids_prsn <- residuals(mod, type = 'pearson')
  diagnostic.data$resids_resp <- residuals(mod, type = 'response')
  diagnostic.data <-  bind_cols(diagnostic.data, get_lpred(mod))

  save(diagnostic.data, file = paste0(mod.fld, nm, '_diags.Rdata'))

  message("Plot residuals")

# Pearson Residuals vs fitted
  png(file.path(mod.fld,'pearson_resid_vs_fitted_qqplot.png'), width = 6, height = 6, units = 'in', res = 72)
  plot(fitted(mod), diagnostic.data$resids_prsn,
     xlab = "Fitted", ylab = "Pearson residuals")
  abline(h = 0, col = "red")
  dev.off()
  
  # Response Residuals vs fitted
  png(file.path(mod.fld,'response_resid_vs_fitted_qqplot.png'), width = 6, height = 6, units = 'in', res = 72)
  plot(fitted(mod), diagnostic.data$resids_resp,
       xlab = "Fitted", ylab = "Response residuals")
  abline(h = 0, col = "red")
  dev.off()
  
  # pearson QQhist
  png(file.path(mod.fld,'pearson_qqhist.png'), width = 6, height = 6, units = 'in', res = 72)
  hist(diagnostic.data$resids_prsn)
  dev.off()
  
  # response QQhist
  png(file.path(mod.fld,'response_qqhist.png'), width = 6, height = 6, units = 'in', res = 72)
  hist(diagnostic.data$resids_resp)
  dev.off()
  
  # pearson QQplot
  png(file.path(mod.fld,'pearson_qqnorm.png'), width = 6, height = 6, units = 'in', res = 72)
  qqnorm(diagnostic.data$resids_prsn); abline(0,1, col = 'red')
  dev.off()
  
  # response QQplot
  png(file.path(mod.fld,'response_qqnorm.png'), width = 6, height = 6, units = 'in', res = 72)
  qqnorm(diagnostic.data$resids_resp); abline(0,1, col = 'red')
  dev.off()

# 7.2 sim data ----
message("Assess consistency of model with observed zeros and/or positives")

message(" - simulate responses from fitted model")
sim.data <- get_simulations(mod, n.draws = 30)

message(" - generating plots")
simulated_response_comparison_plots(mod.vars = mod.vars, data = diagnostic.data, 
                    sim.resp = sim.data, family = mod.family, path.str = file.path(mod.fld))

# 7.3 conditional effect plots

#good - check make_ref_levels_dfr() function
message("Generate (main) conditional effect plots")

#if(mod$family$delta) fx.prd <- main_fx_plots_hurdle(mod, file.path(mod.fld), 25)
#if(!mod$family$delta) 
fx.prd <- main_fx_plots(mod, file.path(mod.fld), 25)
save(fx.prd, file = paste0(mod.fld, nm, '_fxpreds.Rdata'))

}

diagnostic.data$resids_resp <- residuals(mod, type = "response")
diagnostic.data$fitted <- fitted(mod)

ggplot(diagnostic.data, aes(x = fitted, y = resids_resp)) +
  geom_point(alpha = 0.5) +
  #geom_smooth(method = "loess", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals")




message("'Standard' diagnostics of quantile (PIT) residuals")
generate_base_qrsd_diagnostics(mod.family, rsd, prd, file.path(mod.fld))
generate_base_qrsd_diagnostics(mod.family, diagnostic.data$resids_prsn, diagnostic.data$prd.pos, file.path(mod.fld))
generate_qrsd_variable_diagnostics(diagnostic.data, file.path(mod.fld))

library(DHARMa)

dharma_res <- createDHARMa(
  simulatedResponse = t(sim.data),  # needs transpose
  observedResponse = mod$data[[all.vars(formula(mod))[1]]],
  fittedPredictedResponse = fitted(mod),
  integerResponse = FALSE  # or TRUE if you're fitting a count model
)

plot(dharma_res)

sanity(mod)
#summary(mod)
tidy(mod)

p1 <- ggeffects::ggpredict(mod, "yy [all]") |> plot()
p2 <- ggeffects::ggpredict(mod, "mm [all]") |> plot()
p3 <- ggeffects::ggpredict(mod, "flag [all]") |> plot()
p4 <- ggeffects::ggpredict(mod, "set_type [all]") |> plot()
p5 <- ggeffects::ggpredict(mod, "oniF [all]") |> plot()
#p6 <- ggeffects::ggpredict(mod, "sp_code [all]") |> plot()
p7 <- ggeffects::ggpredict(mod, "latd [all]") |> plot()

p <- p1+p2+p8+p4+p5+p7+plot_layout(nrow=3)
p
resid_response <- residuals(mod, type = "response")
resids  <- residuals(mod, type = "pearson")
resids <- residuals(mod, type = "mle-mvn")

ps_dat2$resids  <- resids
qqnorm(resids); abline(0,1, col = 'red')
hist(resids)

ggplot(ps_dat2, aes(x = cpue, y = resids)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "CPUE (weights)", y = "Model residuals")

fixed_vals <- tibble(
    yy = 2013,
    mm = 5,
    latd = -3,
    set_type = as.factor("DFAD"),
    oniF = as.factor('neutral'),
    sp_code = as.factor('SKJ'),
    flag = as.factor('KR'),
    stdeff = 2.04)

pred_yy <- tibble(
  yy = as.factor(c(1990:2023)),
  mm = fixed_vals$mm,
  latd = fixed_vals$latd,
  set_type = fixed_vals$set_type,
  oniF = fixed_vals$oniF,
  sp_code = fixed_vals$sp_code,
  flag = fixed_vals$flag,
  catch_mt = mean(ps_dat2$catch_mt, na.rm = T)
)

preds <- predict(mod, newdata = pred_yy, re_form = NULL)

p1 <- 
preds |>
  ggplot() +
  aes(yy, est, group = 1) +
  geom_point() +
  #geom_line() +
  #geom_hline(yintercept = hist_mn, linetype = 'dashed') +
  gg.theme

pred_yy <- tibble(
  yy = as.factor(c(1990:2023)),
  month = fixed_vals$month,
  lat = fixed_vals$lat,
  set_type = fixed_vals$set_type,
  oniF = fixed_vals$oniF,
  sp_code = fixed_vals$sp_code,
  flag = fixed_vals$flag,
  catch = mean(PS_catch2$catch, na.rm = T)
)


newdata <- data.frame(flag = as.factor(levels(droplevels(ps_dat2$flag)))) |> 
  mutate(
    yy = fixed_vals$yy,
    mm = fixed_vals$month,
    latd = fixed_vals$latd,
    set_type = fixed_vals$set_type,
    oniF = fixed_vals$oniF,
    sp_code = fixed_vals$sp_code,
    #flag = fixed_vals$flag,
    stdeff = fixed_vals$stdeff
  )

preds <- predict(mod, newdata = newdata, re_form = NULL)  # include random effects

newdata$pred <- preds$est

library(ggplot2)
p8 <- 
ggplot(newdata, aes(x = flag, y = pred)) +
  geom_point() +
  theme_minimal() +
  labs(y = "Predicted lon_dev", title = "Random intercept predictions by flag")

#################################
# Run model in mgcv ----

library(mgcv)

mod_gam <- bam(
  lon_dev ~ yy + s(month, bs = "cc", k = 5) + s(lat, k = 5) +
    set_type + oniF + sp_code + s(flag, bs = "re"),
  data = PS_catch2,
  family = gaussian(),
  weights = catch,
  method = "REML"
)

resids <- residuals(mod_gam)
qqnorm(resids); abline(0,1, col = 'red')

summary(mod_gam)

par(mfrow = c(2, 2))
gam.check(mod_gam)

PS_catch2$resid <- residuals(mod_gam)
PS_catch2$fitted <- fitted(mod_gam)

ggplot(PS_catch2, aes(fitted, resid)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values")

plot(mod_gam$residuals, pch = 20, col = scales::alpha("black", 0.3),
     main = "Residuals", ylab = "Residuals")
abline(h = 0, col = "red")

library(gratia) 
draw(mod_gam)  # All smooths
summary(mod_gam)$s.table
# Individual smooths
#plot_smooths(mod_gam, "month", transform = NULL)
#plot_smooths(mod_gam, "lat")

summary(mod_gam)$dev.expl 
summary(mod_gam)$r.sq

pred_df <- expand.grid(
  yy = levels(PS_catch2$yy),
  month = 6,
  lat = -5,
  set_type = "DFAD",
  oniF = "neutral",
  sp_code = "SKJ",
  flag = "KR",
  catch = mean(PS_catch2$catch, na.rm = TRUE)
)

pred_df$pred <- predict(mod_gam, newdata = pred_df)

ggplot(pred_df, aes(x = yy, y = pred)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Yearly Effect on Longitudinal Displacement", y = "Predicted lon_dev")

AIC(mod_gam)

# mod <- sdmTMB(fn, data = ps_dat2, family = gaussian(link = "identity"),
#        weights = ps_dat2$cpue, #ps_dat2$stdeff,
#        spatial = F)



# mod <- bam(
#   lon_dev ~ s(yy, k = 6) + s(lat, k = 5) + s(month, bs = "cc", k = 5) +
#     set_type + oniF + sp_code + s(flag, bs = "re") + log(cpue + 1),
#   data = df,
#   weights = catch,  # or sets if that’s your effort measure
#   family = gaussian(),
#   method = "REML"
# )

mod.dir <- paste0(results_wd, 'long_deviance/')

fn <- catch_mt ~ yy + s(mm, k = 5, bs ="cc") + (1 | flag) + set_type + oniF + s(latd, k=5) + s(lond, k=5) 

data_list <- split(ps_dat3, ps_dat3$sp_code)

mod_nm <- paste0('/mod6_cpue_stnd_tst1')

# Fit and save each model
lapply('SKJ', function(sp) { #names(data_list)
  
  dir.create(paste0(mod.dir,sp, mod_nm), recursive = TRUE)
  
  
  df <- data_list[[sp]]
  
  mod <- sdmTMB(
    formula = fn,
    data = df,
    family = gaussian(link = "identity"),
    offset = df$stdeff,
    spatial = FALSE
  )
  
  # Save to RDS file
  saveRDS(mod, file = paste0(mod.dir,sp, mod_nm, mod_nm, '_model.Rdata'))
  #saveRDS(mod, file = paste0(results_wd, 'long_deviance/', sp, '/mod1/mod1_dat_', sp, '_londev_yy_mm_flag_set_enso_lat.Rdata'))
  
  #return(mod)
})

library(ggeffects)
ggeffects::ggpredict(mod, "lond [all]") |> plot()
visreg(mod, xvar = "lond")

qqnorm(residuals(mod, type = 'response')); abline(0,1, col = 'red')
