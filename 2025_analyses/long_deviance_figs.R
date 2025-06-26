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

# 2. Data filtering table ----

# 2.1 SBEST data ----
load(file = paste0(data_wd, "long_deviance/combined_PS_agg_data_1990-2023_upd.Rdata"))
head(ps_dat)

raw <- ps_dat |> 
  dplyr::filter(sets >= 0) |>
  uncount(round(sets,0)) |>
  nrow()
sets <- ps_dat |> dplyr::filter(sets >= 1) |>
  uncount(round(sets, 0)) |>
  nrow()
spatl <-  ps_dat |>
  dplyr::filter(sets >= 1) |>
  uncount(round(sets, 0)) |>
  mutate(WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  nrow()
flag <- ps_dat |>
  dplyr::filter(sets >= 1) |>
  uncount(round(sets,0)) |>
  mutate(set_type = as.factor(case_when(school %in% c(3,4) ~ 'DFAD',
  school %in% c(5) ~ 'AFAD',
  school %in% c(6,7) ~ 'WHALE',
  school %in% c(1,2,-9) ~ 'FREE')),
  WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  dplyr::filter(flag %in% flgs & !(flag == 'PH' & fleet == 'PH')) |>
  nrow()
whale <- ps_dat |>
  dplyr::filter(sets >= 1) |>
  uncount(round(sets, 0)) |>
  mutate(set_type = as.factor(case_when(school %in% c(3,4) ~ 'DFAD',
                                        school %in% c(5) ~ 'AFAD',
                                        school %in% c(6,7) ~ 'WHALE',
                                        school %in% c(1,2,-9) ~ 'FREE')),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2])) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  dplyr::filter(flag %in% flgs & !(flag == 'PH' & fleet == 'PH')) |>
  dplyr::filter(!(school %in% c(6,7))) |>
  nrow()

df1 <- 
  data.frame(filter = c('raw', '-sets', '-spatial boundary', #'-missing set type', 
                        '-flag', '-animal set'),
             nrow = c(raw, sets, spatl, 
                      flag, whale),
             diff = c(raw, raw-sets, sets-spatl, #sets-spatl, 
                      spatl-flag, flag-whale)) |>
  mutate(perc = (nrow/raw) * 100)

# 2.2 logbook data----
load(file = paste0(data_wd, "long_deviance/logbook_ps_raw_data.Rdata"))

raw <- logps_raw |> 
  nrow()
sets <- logps_raw |> 
  mutate(set_type = as.factor(case_when(school_id %in% c(3,4) ~ 'DFAD',
                                        school_id %in% c(5) ~ 'AFAD',
                                        school_id %in% c(6,7) ~ 'WHALE',
                                        school_id %in% c(1,2,-9) ~ 'FREE',
                                        school_id == 0 ~ NA))) |>
  dplyr::filter(!is.na(set_type)) |>
  nrow()
spatl <-  logps_raw |>
  mutate(latd = round(as.numeric(lat), 4), lond = round(as.numeric(lon), 4),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2]),
         set_type = as.factor(case_when(school_id %in% c(3,4) ~ 'DFAD',
                                        school_id %in% c(5) ~ 'AFAD',
                                        school_id %in% c(6,7) ~ 'WHALE',
                                        school_id %in% c(1,2,-9) ~ 'FREE',
                                        school_id == 0 ~ NA))) |>
  dplyr::filter(!is.na(set_type)) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  nrow()
flag <- logps_raw |>
  mutate(latd = round(as.numeric(lat), 4), lond = round(as.numeric(lon), 4),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2]),
         set_type = as.factor(case_when(school_id %in% c(3,4) ~ 'DFAD',
                                        school_id %in% c(5) ~ 'AFAD',
                                        school_id %in% c(6,7) ~ 'WHALE',
                                        school_id %in% c(1,2,-9) ~ 'FREE',
                                        school_id == 0 ~ NA))) |>
  dplyr::filter(!is.na(set_type)) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  dplyr::filter(flag_id %in% flgs & !(flag_id == 'PH' & flag_rg_id == 'PH')) |>
  nrow()
whale <- logps_raw |>
  mutate(latd = round(as.numeric(lat), 4), lond = round(as.numeric(lon), 4),
         WCP_CA = point.in.polygon(lond, latd, coords[,1], coords[,2]),
         set_type = as.factor(case_when(school_id %in% c(3,4) ~ 'DFAD',
                                        school_id %in% c(5) ~ 'AFAD',
                                        school_id %in% c(6,7) ~ 'WHALE',
                                        school_id %in% c(1,2,-9) ~ 'FREE',
                                        school_id == 0 ~ NA))) |>
  dplyr::filter(!is.na(set_type)) |>
  dplyr::filter(WCP_CA %in% c(1, 2) & !(latd > 10 | latd < -15) & lond >= 130) |>
  dplyr::filter(flag_id %in% flgs & !(flag_id == 'PH' & flag_rg_id == 'PH')) |>
  dplyr::filter(!(school_id %in% c(6,7))) |>
  nrow()

df2 <- 
  data.frame(filter = c('raw', '-sets', '-spatial boundary', #'-missing set type', 
                        '-flag', '-animal set'),
             nrow = c(raw, sets, spatl, 
                      flag, whale),
             diff = c(NA, raw-sets, sets-spatl, #sets-spatl, 
                      spatl-flag, flag-whale)) |>
  mutate(perc = (nrow/raw) * 100)
  
# 3. SBEST exploratory plots ----
tmp1 <- df1 |> 
  dplyr::filter(filter != 'raw') |>
  dplyr::select(filter, sbest_diff = diff, sbest_perc = perc)
tmp2 <- df2 |>
  dplyr::filter(filter != 'raw') |>
  dplyr::select(filter, lbk_diff = diff, lbk_perc = perc)

df3 <- left_join(tmp1, tmp2, by = 'filter') |>
  mutate(across(where(is.numeric), ~ round(.x, 1)),
         filter = ifelse(filter == '-sets', '-missing data', filter))

tbl3 <- df3 |>
  flextable() |>
  set_header_labels(
    filter = "Data filter",
    sbest_diff = "Sbest removal #",
    sbest_perc = 'Sbest remaining %',
    lbk_diff = "Logbook removal #",
    lbk_perc = 'Logbook remaining %') |>
  bold(part = "header") |>        
  bold(j = 1, part = "body")  |>
  bg(bg = "white", part = "all")  |>
  #word_wrap(part = "all", wrap = FALSE) |>  
  set_table_properties(layout = "autofit")  |>
  autofit()
tbl3
save_as_image(tbl3, paste0(results_wd,'long_deviance/figs/data_filter_tbl_lcombined.png'), 
              width = 6, height = 6, units = 'in',  res = 100)

# Plot species catch over time
p <- 
  sbest_dat |>
  pivot_longer(-c('S_BEST_ID', 'yy', 'mm', 'flag', 'fleet', 'latd', 'lond', 'school', 
                  'stdeff', 'sets', 'S_SET_ID', 'set_type'), names_to = 'sp_code', values_to = 'catch_mt') |>
  group_by(sp_code, yy) |>
  summarise(catch = sum(catch_mt, na.rm = T)) |>
  ggplot() +
  aes(yy, catch/1000, fill = sp_code) +
  geom_bar(stat = 'identity', position = 'stack') +
  gg.theme +
  facet_wrap(~sp_code, scales = 'free_y') +
  scale_fill_brewer(palette = 'Set1') +
  labs(x = 'Year', y = 'Catch (x1000t)', fill = 'Species') +
  theme(aspect.ratio = 1, legend.position = 'bottom')
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_catch_by_sp_yr.png'))

# Plot effort to see that its correct
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

# stacked bar plot of set type by year
p <- 
  sbest_dat |>
  uncount(round(sets, 0)) |>
  ggplot() +
  aes(yy, fill = set_type) +
  geom_bar(position = 'fill') +
  scale_fill_brewer(palette = 'Set1') +
  scale_y_continuous(labels = scales::percent_format()) +
  #facet_wrap(~yy) +
  gg.theme +
  labs(x = 'Year', y = 'Sets (%)', fill = 'Set type')
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_type_by_yr.png'))

# stacked barplot of set type by year by flag
p <- 
  sbest_dat3 |>
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
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_type_by_yr_flag.png'))

# Raw longitude by year
summary_stats <- sbest_dat %>%
  group_by(yy) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE),
            q25 = quantile(lond, 0.25, na.rm = TRUE),
            q75 = quantile(lond, 0.75, na.rm = TRUE))
p <- 
  sbest_dat |>
  ggplot() +
  geom_violin(aes(x=as.factor(yy), y = lond), fill = 'grey50', alpha = 0.2) + 
  geom_point(data = summary_stats, aes(x = as.factor(yy), y = mean_lond), 
             color = "red", size = 1.5) +
  geom_errorbar(data = summary_stats, aes(x = as.factor(yy), ymin = q25, ymax = q75),
                width = 0.2, color = "black") +
  gg.theme +
  labs(x = 'Year', y = 'Longitude') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(sbest_dat$lond, na.rm=TRUE), linetype = "dashed", color = "black")
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_lond_by_yr.png'))

# Longitude by year and flag
summary_stats2 <- sbest_dat %>%
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
  geom_point(data = summary_stats2, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), y = mean_lond), 
             color = "red", size = 1.5) +
  geom_errorbar(data = summary_stats2, aes(x = as.numeric(as.character(yy), group = as.factor(yy)), 
                                           ymin = q25, ymax = q75),width = 0.2, color = "black") +
  facet_wrap(~flag2) +
  gg.theme +
  labs(x = 'Year', y = 'Longitude') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(sbest_dat$lond, na.rm=TRUE), linetype = "dashed", color = 'black')
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_lond_by_yr_flag.png'))


p <- 
  sbest_dat |>
  dplyr::filter(!is.na(flag)) |>
  ggplot() +
  aes(x = lond, fill = flag) +
  geom_bar(position = 'identity') +
  #scale_y_continuous(labels = scales::percent_format()) +
  #scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~yy) +
  labs(x = 'Longitude', y = 'Sets', fill = 'Set type') +
  gg.theme
p 
ggsave(p, file = '2025_analyses/results/long_deviance/figs/ps_set_by_lond_yr_raw.png')

p <- 
  sbest_dat |>
  dplyr::filter(!is.na(flag)) |>
  ggplot() +
  aes(x = lond, fill = flag) +
  geom_bar(position = 'fill') +
  scale_y_continuous(labels = scales::percent_format()) +
  #scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~yy) +
  labs(x = 'Longitude', y = 'Sets (%)', fill = 'Set type') +
  gg.theme
p 
ggsave(p, file = '2025_analyses/results/long_deviance/figs/ps_set_by_lond_yr_%fill.png')

# Raw longitude by year
summary_stats <- sbest_dat %>%
  group_by(yy) %>%
  summarise(mean_lond = mean(lond, na.rm = TRUE),
            q25 = quantile(lond, 0.25, na.rm = TRUE),
            q75 = quantile(lond, 0.75, na.rm = TRUE))

p <- 
  sbest_dat |>
  ggplot() +
  geom_violin(aes(x=as.factor(yy), y = lond), fill = 'grey50', alpha = 0.2) + 
  geom_point(data = summary_stats, aes(x = as.factor(yy), y = mean_lond), 
             color = "red", size = 1.5) +
  geom_errorbar(data = summary_stats, aes(x = as.factor(yy), ymin = q25, ymax = q75),
                width = 0.2, color = "black") +
  gg.theme +
  labs(x = 'Year', y = 'Longitude') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_hline(yintercept = mean(sbest_dat$lond, na.rm=TRUE), linetype = "dashed", color = "black")
p
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_lond_by_yr.png'))

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

# Now look at finalised df with dropped flags
# Longitude distribution of sets by flag  and year raw
p <- 
  sbest_dat3 |>
  ggplot() +
  aes(x = lond, fill = flag) +
  geom_bar(position = 'identity') +
  #scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = 'Set3') +
  facet_wrap(~yy) +
  labs(x = 'Longitude', y = 'Sets', fill = 'Flag') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
p 
#ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_lond_yr_raw.png'))

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
  aes(x = lond, y = sets) +
  geom_bar(stat = 'identity') +
  #scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~set_type, scales = 'free_y') +
  labs(x = 'Longitude', y = 'Sets', fill = 'Set type') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_settype.png'))

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
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_settype_yr_stacked.png'))

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
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/ps_set_by_ENSO.png'))