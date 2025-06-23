# 1. Preamble ----
library(tidyverse)
#library(magrittr)
#library(maps)
#library(zoo)
#library(RODBC)
library(sp)
#library(mapdata)
#library(formatR)
library(RGeostats)
library(RColorBrewer)
library(raster)
library(Hmisc)
library(ggridges)

# Set dir.
#setwd('P:/OFPEMA/WCPFC/SC19/Ecosystem Indicators/Catch and distribution')
data_wd <- './2025_analyses/data/'
results_wd <- "./2025_analyses/results/"

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

# Effort data ----
# Load in cleaned PS effort data from effort_indicators_2025 script

load(file = paste0(data_wd, "effort/effort_seine_clean_data.Rdata"))

s_eff3 <- 
  s_eff3 |>
  mutate(ym = paste0(YY, str_pad(MM, width = 2, side = "left", pad = "0")))

# Enviro data ----
# Load in enviro data to extract values to
#loc <- "C:/Users/nickh/OneDrive - SPC/easifish/Dat/ExplanatoryVars/nh2/stacked/month"

mth_dates <- unique(s_eff3$ym) |> sort() |> str_remove('-')

mth_files <- data.frame(file = list.files(path="C:/Users/nickh/OneDrive - SPC/easifish/Dat/ExplanatoryVars/nh2/stacked/month", 
                        recursive = T, full.names = T)) |>
                          mutate(ym = str_extract(file, "(?<=_)(\\d{6})(?=\\.nc)"))
#dts <- str_extract(mth_files, "(?<=_)(\\d{6})(?=\\.nc)")

PS_env_dat <- tibble()

for(i in 1:nrow(mth_files)) {
  tryCatch({
    
    # Clean presence data
    pres <- s_eff3 |> 
      filter(ym == mth_files$ym[i]) #|>
      #dplyr::select(lon,lat,ym) # Create lat/lon presence df
    
    # load enviro stack
    preds <- stack(paste0(mth_files$file[i]))
    names(preds) <- c('ssha', 'mld', 'salinity', 'sst', 'sst100', 
                      'ucur', 'ucur100', 'vcur', 'vcur100', 'chl', 'o2', 'o2100', 'depth')
    
    # Extract values to points
    # Extract raster value to points
    x <- data.frame(as.data.frame(pres), as.data.frame(extract(preds, pres[,c('lon', 'lat')])))
    PS_env_dat <- bind_rows(PS_env_dat,x)
    rm(x, preds)
    
  },
  error = function(e){ # Error checker
    message(paste('error: run', i)) 
    print(e) 
  })
  
}

save(PS_env_dat, file = '2025_analyses/data/environment/PS_env_dat.Rdata')

# Explore data ----
load(paste0(data_wd, 'environment/PS_env_dat.Rdata'))

# Mean of each variable by year
p <- 
PS_env_dat |>
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  dplyr::select(YY, sst, sst100, chl, ssha, o2, o2100) |>
  pivot_longer(-YY, names_to = 'var', values_to = 'val') |>
  group_by(YY, var) |>
  summarise(val_mn = mean(val, na.rm=T), val_25 = quantile(val, 0.25, na.rm=T),
            val_75 = quantile(val, 0.75, na.rm=T)) |>
  ggplot() +
  aes(x = YY, y = val_mn, col = var) +
  geom_point() +
  geom_line() +
  geom_segment(aes(x= YY, y = val_25, yend = val_75, group = YY), col = 'black') +
  facet_wrap(~var, scales = 'free') +
  labs(x = 'Variable', y = 'Value', title = 'PS - Mean environment conditions across time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_mean_envs_yr.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)


# by set type
p <- 
  PS_env_dat |>
  #dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  dplyr::select(YY, set_type, sst) |>
  pivot_longer(-c(YY, set_type), names_to = 'var', values_to = 'val') |>
  group_by(YY, set_type, var) |>
  summarise(val_mn = mean(val, na.rm=T), val_25 = quantile(val, 0.25, na.rm=T),
            val_75 = quantile(val, 0.75, na.rm=T)) |>
  ggplot() +
  aes(x = YY, y = val_mn, col = set_type) +
  geom_point() +
  geom_line() +
  geom_segment(aes(x= YY, y = val_25, yend = val_75, group = YY), col = 'black') +
  facet_wrap(~set_type, nrow = 3) +
  labs(x = 'Variable', y = 'Value', title = 'PS - Mean SST by set type across time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_sst_set_type.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)



# Proportion of sets in >28 degree water by year
p <- 
PS_env_dat |>
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  group_by(YY) |>
  summarise(sets = n(), sst28 = sum(sst > 28, na.rm = T),
            sst29 = sum(sst > 29, na.rm = T), sst30 = sum(sst > 30, na.rm = T)) |>
  mutate(prop28 = sst28/sets, prop29 = sst29/sets, prop30 = sst30/sets) |>
  dplyr::select(YY, contains('prop')) |>
  pivot_longer(-YY, names_to = 'temp', values_to = 'prop') |>
  ggplot() +
  aes(YY, prop*100, col = temp) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  labs(y = 'Sets (%)', x = 'Year', col = 'SST',
       title = 'PS - proportion of sets above different SST thresholds') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_sst_props_yr.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Proportion of sets at 100m in >28 degree water by year
p <- 
PS_env_dat |>
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  group_by(YY) |>
  summarise(sets = n(), sst26 = sum(sst100 > 26, na.rm = T),
            sst27 = sum(sst100 > 27, na.rm = T), sst28 = sum(sst100 > 28, na.rm = T)) |>
  mutate(prop26 = sst26/sets, prop27 = sst27/sets, prop28 = sst28/sets) |>
  dplyr::select(YY, contains('prop')) |>
  pivot_longer(-YY, names_to = 'temp', values_to = 'prop') |>
  ggplot() +
  aes(YY, prop*100, col = temp) +
  geom_line() +
  geom_point() +
  scale_color_brewer(palette = 'Set1') +
  labs(y = 'Sets (%)', x = 'Year', col = 'Temp at 100m',
       title = 'PS - proportion of sets above different temp at 100m thresholds') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_sst100_props_yr.png'),height = 4.135, width = 5.845, units = "in", dpi = 200)

# Ridgeline plots

# SST
p <- 
PS_env_dat |> 
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  ggplot() +
  aes(sst, y = YY, group = YY, fill = YY) +
  geom_density_ridges(rel_min_height = 0.01, scale  = 5) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  geom_vline(aes(xintercept = 30), linetype = 'dashed') +
  scale_y_reverse() +
  labs(x = 'SST', y = 'Year', 
       title = 'PS - SST distribution over time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_sst_ridges_allyr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# temp at 100m
p <- 
PS_env_dat |> 
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  ggplot() +
  aes(sst100, y = YY, group = YY, fill = YY) +
  geom_density_ridges(rel_min_height = 0.01, scale  = 5) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  geom_vline(aes(xintercept = 28), linetype = 'dashed') +
  scale_y_reverse() +
  labs(x = 'Temperature at 100m', y = 'Year', 
       title = 'PS - Temp at 100m distribution over time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_sst100_ridges_allyr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# cholorophyll
p <- 
PS_env_dat |> 
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  ggplot() +
  aes(chl, y = YY, group = YY, fill = YY) +
  geom_density_ridges(rel_min_height = 0.01, scale  = 5) +
  scale_fill_distiller(palette = 'Blues', direction = 1) +
  scale_y_reverse() +
  xlim(0,0.5) +
  #geom_vline(aes(xintercept = 28), linetype = 'dashed') +
  labs(x = 'Chlorophyll', y = 'Year', 
       title = 'PS - Chlorophyll-a distribution over time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_chl_ridges_allyr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# Geom density
# PS_env_dat |>
#   uncount(round(sets,0)) |>
#   dplyr::filter((YY >= 1990 & YY <= 2000) | YY == max(YY))|>
#   mutate(yr_bin = ifelse(YY %in% c(1990:2000), '1990-2000', as.character(YY))) |>
#   ggplot(aes(x = sst, fill = yr_bin)) + 
#   geom_density(aes(y = after_stat(density)), alpha = 0.5) +
#   scale_fill_manual(
#     values = c('1990-2000' = 'gray50',  '2022' = 'blue')) +
#   labs(x = 'SST', y = 'PS sets (%)', fill = '',
#        title = 'Historical (1990:2000) vs 2022') +
#   gg.theme +
#   theme(aspect.ratio = 0.3, legend.position = 'none')

vars<- c('sst', 'sst100', 'chl', 'ssha', 'o2', 'o2100')
pal1 <- brewer.pal(length(vars), "Set1")

# Historical 1990:2000 vs 2022 environment ridges plot
p <- 
PS_env_dat |>
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  dplyr::filter((YY >= 1990 & YY <= 2000) | YY == max(YY)) |>
  mutate(yr_bin = ifelse(YY %in% 1990:2000, '1990-2000', as.character(YY))) |>
  dplyr::select(yr_bin, sst, sst100, chl, ssha, o2, o2100) |>
  pivot_longer(-yr_bin, names_to = 'var', values_to = 'value') |>
  mutate(fill_group = ifelse(yr_bin == '1990-2000', '1990-2000', var)) |>
  ggplot(aes(x = value, fill = fill_group)) +  # Map 'value' to the x-axis
  geom_density(aes(y = after_stat(density)), alpha = 0.5) +  # Density plot
  facet_wrap(~var, scales = 'free', strip.position = 'right', nrow=3) +  # Faceting by 'var'
  #scale_fill_identity() + 
  scale_fill_manual(
    values = c('1990-2000' = 'gray50',  
               setNames(pal1, unique(vars)))) +  # Colors for 'yr_bin'
  labs(x = 'Value', y = 'Density', fill = '',
       title = 'PS: Historical (1990:2000) vs 2022 environment') +
  gg.theme +
  theme(aspect.ratio = 0.3, legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_env_ridges_old_2022.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)


# Historical 1990:2000 vs most recent years SST
pal2 <- c( rev(brewer.pal(4, "Blues")), 'gray50')

p <- 
PS_env_dat |> 
  dplyr::filter(set_type == 'all') |>
  uncount(round(sets,0)) |>
  dplyr::filter((YY >= 1990 & YY <= 2000) | YY %in% c(2019,2020,2021,2022)) |>
  mutate(yr_bin = ifelse(YY %in% c(1990:2000), '1990-2000', as.character(YY)),
         yr_bin = factor(yr_bin, levels = c('2022', '2021','2020','2019','1990-2000'))) |>
  ggplot() +
  aes(sst, y = yr_bin, group = yr_bin, fill = yr_bin) +
  geom_density_ridges(rel_min_height = 0.01, scale  = 5) +
  scale_fill_manual(values = pal2) +
  labs(x = 'SST', y = 'Year', 
       title = 'PS - Historical SST vs recent years') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_PS_env_ridges_old_recent.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

###############################################
# Longline data

load(paste0(data_wd, "effort/effort_LL_clean_data.Rdata"))

l_eff2 <- 
  l_eff2 |>
  mutate(ym = paste0(YY, str_pad(MM, width = 2, side = "left", pad = "0")))

# Enviro data ----
# Load in enviro data to extract values to
#loc <- "C:/Users/nickh/OneDrive - SPC/easifish/Dat/ExplanatoryVars/nh2/stacked/month"

mth_dates <- unique(l_eff2$ym) |> sort() |> str_remove('-')

mth_files <- data.frame(file = list.files(path="C:/Users/nickh/OneDrive - SPC/easifish/Dat/ExplanatoryVars/nh2/stacked/month", 
                                          recursive = T, full.names = T)) |>
  mutate(ym = str_extract(file, "(?<=_)(\\d{6})(?=\\.nc)"))
#dts <- str_extract(mth_files, "(?<=_)(\\d{6})(?=\\.nc)")

LL_env_dat <- tibble()

for(i in 1:nrow(mth_files)) {
  tryCatch({
    
    # Clean presence data
    pres <- l_eff2 |> 
      filter(ym == mth_files$ym[i]) #|>
    #dplyr::select(lon,lat,ym) # Create lat/lon presence df
    
    # load enviro stack
    preds <- stack(paste0(mth_files$file[i]))
    names(preds) <- c('ssha', 'mld', 'salinity', 'sst', 'sst100', 
                      'ucur', 'ucur100', 'vcur', 'vcur100', 'chl', 'o2', 'o2100', 'depth')
    
    # Extract values to points
    # Extract raster value to points
    x <- data.frame(as.data.frame(pres), as.data.frame(extract(preds, pres[,c('lon', 'lat')])))
    LL_env_dat <- bind_rows(LL_env_dat,x)
    rm(x, preds)
    
  },
  error = function(e){ # Error checker
    message(paste('error: run', i)) 
    print(e) 
  })
  
}

save(LL_env_dat, file = '2025_analyses/data/environment/LL_env_dat.Rdata')

# Explore data ----
load(paste0(data_wd, 'environment/LL_env_dat.Rdata'))

# Mean of each variable by year
p <- 
LL_env_dat |>
  #mutate(hhooks = round(hhooks, 0)) |>
  dplyr::select(YY, sst, sst100, chl, ssha, o2, o2100, hhooks) |>
  pivot_longer(-c(YY, hhooks), names_to = 'var', values_to = 'val') |>
  group_by(YY, var) |>
  summarise(val_mn = weighted.mean(val, hhooks, na.rm = TRUE),  # Weighted mean based on hhooks
    val_25 = wtd.quantile(val, weights = hhooks, probs = 0.25, na.rm = TRUE),  # Weighted 25th percentile
    val_75 = wtd.quantile(val, weights = hhooks, probs = 0.75, na.rm = TRUE),  # Weighted 75th percentile
    .groups = 'drop') |>
  ggplot() +
  aes(x = YY, y = val_mn, col = var) +
  geom_point() +
  geom_line() +
  geom_segment(aes(x = YY, y = val_25, yend = val_75, group = YY), col = 'black') +
  facet_wrap(~var, scales = 'free') +
  labs(x = 'Variable', y = 'Value', title = 'LL: Mean environmenta conditions across time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_mean_envs_yr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# Proportion of sets in >28 degree water by year
p <- 
LL_env_dat |>
  #uncount(round(sets,0)) |>
  group_by(YY) |>
  summarise(hooks = sum(hhooks), sst25 = sum(hhooks[sst > 25], na.rm = T),
            sst26 = sum(hhooks[sst > 26], na.rm = T), sst28 = sum(hhooks[sst > 28], na.rm = T)) |>
  mutate(prop25 = sst25/hooks, prop26 = sst26/hooks, prop28 = sst28/hooks) |>
  dplyr::select(YY, contains('prop')) |>
  pivot_longer(-YY, names_to = 'temp', values_to = 'prop') |>
  ggplot() +
  aes(YY, prop*100, col = temp) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  labs(y = 'Sets (%)', x = 'Year', col = 'SST',
       title = 'LL - % sets above different SST thresholds') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_sst_props_yr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# Proportion of sets at 100m in >28 degree water by year
p <- 
LL_env_dat |>
  #uncount(round(sets,0)) |>
  group_by(YY) |>
  summarise(hooks = sum(hhooks), sst22 = sum(hhooks[sst100 > 22], na.rm = T),
            sst23 = sum(hhooks[sst100 > 23], na.rm = T), sst24 = sum(hhooks[sst100 > 24], na.rm = T)) |>
  mutate(prop22 = sst22/hooks, prop23 = sst23/hooks, prop24 = sst24/hooks) |>
  dplyr::select(YY, contains('prop')) |>
  pivot_longer(-YY, names_to = 'temp', values_to = 'prop') |>
  ggplot() +
  aes(YY, prop*100, col = temp) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  labs(y = 'Sets (%)', x = 'Year', col = 'Temp at 100m',
       title = 'LL - % sets above different temp at 100m thresholds') +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_sst100_props_yr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# Ridgeline plots

# SST
p <- 
LL_env_dat |>
  mutate(hhooks = round(hhooks, 0)) |>
  # Calculate weighted density for each year and variable
  group_by(YY) |>
  do({
    d <- density(.$sst, weights = .$hhooks, na.rm = TRUE)  # Compute weighted density
    data.frame(x = d$x, y = d$y,
               YY = unique(.$YY))}) |>
  mutate(YY = factor(YY, levels = sort(unique(YY), decreasing = TRUE))) |>
  ggplot(aes(x = x, y = YY, height = y, fill = YY)) + 
  geom_density_ridges(stat = "identity", scale = 5, rel_min_height = 0.01) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Blues"))(length(unique(LL_env_dat$YY)))) +
  scale_y_discrete(limits = rev(levels(factor(LL_env_dat$YY)))) +  # Explicitly reverse the order of y-axis
  labs(x = 'SST', y = 'Year', 
       title = 'LL - SST distribution over time') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_sst_ridges_allyr.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# Ridges historical vs most recent year
vars<- c('sst', 'sst100', 'chl', 'ssha', 'o2', 'o2100')
pal1 <- brewer.pal(length(vars), "Set1")

# Calculate density with weights and percentage density for each group
density_data <- LL_env_dat |>
  dplyr::filter((YY >= 1990 & YY <= 2000) | YY == max(YY)) |>
  dplyr::select(YY, hhooks, sst, sst100, chl, ssha, o2, o2100) |>
  pivot_longer(cols = -c(hhooks, YY), names_to = 'var', values_to = 'value') |>
  mutate(yr_bin = ifelse(YY %in% 1990:2000, '1990-2000', as.character(YY))) |>
  group_by(var, yr_bin) |>
  do({# Calculate weighted density for each variable and year bin
    d <- density(.$value, weights = .$hhooks, na.rm = TRUE)
    # Normalize the density to percentage (area under curve = 1)
    density_area <- sum(d$y)
    normalized_density <- d$y / density_area * 100  # Convert to percentage density
    # Return the results in a data frame
    data.frame(x = d$x, y = normalized_density, var = unique(.$var), yr_bin = unique(.$yr_bin))}) |>
  mutate(fill_group = ifelse(yr_bin == '1990-2000', '1990-2000', var))


# Plot with ggplot: density with transparency and color for 1990-2000 and 2022
p <- 
ggplot(density_data, aes(x = x, y = y, fill = fill_group)) + 
  geom_area(alpha = 0.5, position = 'identity') +  # Use area plot for density with transparency
  facet_wrap(~var, scales = 'free', strip.position = 'right', nrow = 3) +  # Faceting by 'var'
  scale_fill_manual(
    values = c('1990-2000' = 'gray50',  
               setNames(pal1, unique(vars)))) +
  labs(x = 'Value', y = 'Density', fill = '',
       title = 'LL: Historical (1990:2000) vs 2022 environment') +
  gg.theme +
  theme(aspect.ratio = 0.3, legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_env_ridges_old_2022.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

# ggridges sst historical vs recent yrs
pal2 <- c( rev(brewer.pal(4, "Blues")), 'gray50')

p <- 
LL_env_dat |>
  dplyr::filter((YY >= 1990 & YY <= 2000) | YY %in% c(2019,2020,2021,2022)) |>
  mutate(yr_bin = ifelse(YY %in% 1990:2000, '1990-2000', as.character(YY)),
         yr_bin = factor(yr_bin, levels = c('2022', '2021', '2020', '2019', '1990-2000'))) |>
  ggplot() +
  aes(x = sst100, y = yr_bin, group = yr_bin, fill = yr_bin) +
  geom_density_ridges(aes(weight = hhooks), rel_min_height = 0.01, scale = 5) +  # Weight density by 'hhooks'
  scale_fill_manual(values = pal2) +  # Assuming 'pal6' contains the colors for each year group
  labs(x = 'SST', y = 'Year', title = 'LL - SST Historical (1990:2000) vs recent years') +
  gg.theme +
  theme(legend.position = 'none')
p
ggsave(p, file = paste0(results_wd, 'environment/fig_LL_SST_ridges_old_recent.png'),
       height = 4.135, width = 5.845, units = "in", dpi = 200)

LL_env_dat |>
  group_by(lat, lon, YY) |>
  summarise(hooks = sum(hhooks, na.rm=T), sst = mean(sst, na.rm = T)) |>
  dplyr::filter(YY %in% c(1993, 2003, 2013, 2022)) |>
  ggplot() +
  aes(lon, lat, fill = sst) +
  geom_tile() +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 300), ylim = c(-60, 50)) +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_fill_distiller(palette = 'Spectral') +
  facet_wrap(~YY) +
  theme_classic() 

PS_env_dat |>
  dplyr::filter(set_type == 'una') |>
  group_by(lat, lon, YY) |>
  summarise(sets = sum(sets, na.rm=T), sst = mean(sst, na.rm = T)) |>
  dplyr::filter(YY %in% c(1993, 2003, 2013, 2022)) |>
  ggplot() +
  aes(lon, lat, fill = sst) +
  geom_tile() +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(100, 300), ylim = c(-60, 50)) +
  labs(x = 'Longitude', y = 'Latitude') +
  scale_fill_distiller(palette = 'Spectral') +
  facet_wrap(~YY) +
  theme_classic() 
