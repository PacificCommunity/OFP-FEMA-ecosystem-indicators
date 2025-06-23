



load(file = paste0(data_wd, 'catch/catch_seine_COGs.Rdata'))
load(file = paste0(data_wd, 'effort/effort_seine_COGs.Rdata'))
head(s_eff_COGs)
head(PS_catch_COGs)

eff <- s_eff_COGs |> #dplyr::rename(effort = lon) |>
  mutate(sp_code = 'Effort') |>
  dplyr::select(sp_code,set_type, yy, lon)
catch <- PS_catch_COGs |> #dplyr::rename(catch = lon) |>
  mutate(data = 'catch') |>
  dplyr::select(sp_code, set_type, yy, lon)

mns <- eff |> 
  group_by(set_type) |>
  summarise(mn_COG = mean(lon[yy %in% 1990:2000]))

pal <- c(brewer.pal(n = 3, name = "Set1"), 'black')
p <- 
bind_rows(eff, catch) |>
  mutate(sp_code = factor(sp_code, levels = c('BET', 'SKJ', 'YFT', 'Effort')),
         #lon = rollmean(lon, k = 3, fill = NA,align = "right")
         ) |>
  ggplot() +
  aes(reorder(yy, desc(yy)), lon, col = sp_code, group = sp_code) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  coord_flip() +
  #geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  facet_wrap(~set_type) +
  #scale_color_brewer(palette = 'Set1') +
  scale_color_manual(values = pal) +
  geom_hline(data = mns, aes(yintercept = mn_COG), linetype = 'dashed', col = 'grey50') +
  #geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lon), color='black') +
  labs(x = 'Year', y = 'Longitude', col = '') +
  scale_x_discrete(breaks = seq(min(s_eff_COGs$yy), max(s_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Longitudinal centre of gravity of PS effort and catch") +
  #ylim(140,180) +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'combined/combined_PS_COGs.png'),height = 8, width = 11, units = "in", dpi = 200)


# more facets to investigate data better
eff2 <- eff |> dplyr::select(-sp_code) |>
  dplyr::rename(effort = lon)



p <- 
catch |>
 dplyr::rename(catch = lon) |>
  left_join(eff2, by = c('set_type', 'yy')) |>
  pivot_longer(-c(sp_code, set_type, yy), names_to = 'data', values_to = 'lon') |>
  ggplot() +
  aes(reorder(yy, desc(yy)), lon, col = data, group = data) + #reorder(yy, desc(yy)),
  geom_line() +
  geom_point(size = 1) +
  #scale_y_discrete(limits=rev) +
  coord_flip() +
  #geom_smooth(method = 'lm', se=F, linetype='dashed', size = 0.1) +
  facet_wrap(~set_type + sp_code) +
  #scale_color_brewer(palette = 'Set1') +
  scale_color_manual(values = pal) +
  geom_hline(data = mns, aes(yintercept = mn_COG), linetype = 'dashed', col = 'grey50') +
  #geom_abline(data = COG_mns, aes(slope=0, intercept = mn_lon), color='black') +
  labs(x = 'Year', y = 'Longitude', col = '') +
  scale_x_discrete(breaks = seq(min(s_eff_COGs$yy), max(s_eff_COGs$yy), by = 5)) +  # Show every 5th year
  ggtitle("Longitudinal centre of gravity of PS effort and catch") +
  #ylim(140,180) +
  gg.theme +
  theme(legend.position = 'bottom')
p
ggsave(p, file = paste0(results_wd, 'combined/combined_PS_COGs2.png'),height = 8, width = 11, units = "in", dpi = 200)

# Catch and effort density maps combined
load(file = paste0(data_wd, 'catch/catch_PS_clean_data.Rdata'))
pal <- rev(c(brewer.pal(n = 9, name = "Spectral"), 'white'))


catch_hist <- PS_catch2 |> 
  dplyr::filter(set_type == 'all' & yy %in% c(1990:2000) &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, yy, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  group_by(lat, lon, set_type, sp_code) |>
  summarise(catch2 = mean(catch2, na.rm=T)) |>
  mutate(yy_bin = '1990-2000')

catch_2023 <-
  PS_catch2 |> 
  dplyr::filter(set_type == 'all' & yy %in% 2023 &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, set_type, sp_code) |>
  summarise(catch2 = sum(catch, na.rm=T)) |>
  mutate(yy_bin = '2023')

pal2 <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(11), 'white'))
p <- 
  bind_rows(catch_hist, catch_2023) |>
  #dplyr::filter(sp_code == 'YFT') |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_contour_filled(aes(z = catch2, fill = ..level..), bins = 11, h = 10) +  # Transparent fill
  geom_contour(aes(z = catch2), color = "grey50", linewidth = 0.5, bins = 11, h = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  #scale_fill_brewer(palette = 'RdYlBu', direction = -1) +
  facet_wrap(~sp_code + yy_bin, nrow=3) +
  scale_fill_manual(values=pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map") +
  gg.theme +
  theme(legend.position = 'none')
p  

catch_normalized <- bind_rows(catch_hist, catch_2023) |>
  dplyr::filter(sp_code != 'NA') |>
  group_by(sp_code) |>
  mutate(catch2_normalized = catch2/max(catch2, na.rm=T),
         catch2_normalized_scaled = round(catch2_normalized*100,0)) |>
  ungroup()

# Plot the normalized data
catch_normalized |>
  uncount(catch2_normalized_scaled) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_density_2d_filled(bins=12, h=10) +
  geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  #geom_contour_filled(aes(z = catch2_normalized, fill = ..level..), bins = 10, h = 10) +  # Transparent fill
  #geom_contour(aes(z = catch2_normalized), color = "grey50", linewidth = 0.5, bins = 10, h = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group), fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  facet_wrap(~sp_code + yy_bin, nrow=3) +
  scale_fill_manual(values = pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "Catch weight PS density map (Normalized)") +
  gg.theme +
  theme(legend.position = 'none')

# load effort data 
load(file = paste0(data_wd, "effort/effort_seine_clean_data.Rdata"))

pal2 <- rev(c(colorRampPalette(brewer.pal(9, 'Spectral'))(11), 'white'))

eff_hist <- s_eff3 |> 
  dplyr::filter(YY %in% c(1990:2000) &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, YY, set_type) |>
  summarise(sets = sum(sets, na.rm=T)) |>
  group_by(lat, lon, set_type) |>
  summarise(sets2 = mean(sets, na.rm=T)) |>
  mutate(yy_bin = '1990-2000')

eff_2023 <-
  s_eff3 |> 
  dplyr::filter(YY %in% 2023 &
                  lat >= -20 & lat <= 10 & lon >= 140 & lon <= 210) |>
  group_by(lat, lon, set_type) |>
  summarise(sets2 = sum(sets, na.rm=T)) |>
  mutate(yy_bin = '2023') 

eff_normalized <- bind_rows(eff_hist, eff_2023) |>
  # Normalize the sets2 data to make it relative
  group_by(yy_bin, set_type) |>
  mutate(sets2_normalized = sets2 / max(sets2, na.rm = TRUE),
         sets2_normalized_scaled = round(sets2_normalized * 100, 0)) |>
  ungroup()

# Plot the normalized data
p <- 
eff_normalized |>
  uncount(sets2_normalized_scaled) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_density_2d_filled(bins=12, h=10) +
  geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  #geom_contour_filled(aes(z = sets2_normalized_scaled, fill = ..level..), bins = 11, h = 10) +  # Transparent fill
  #geom_contour(aes(z = sets2_normalized), color = "grey50", linewidth = 0.5, bins = 11, h = 10) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group), fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  facet_wrap(~set_type+yy_bin, nrow=3) +
  scale_fill_manual(values = pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "PS effort density map") +
  gg.theme +
  theme(legend.position = 'none')
ggsave(p, file = paste0(results_wd, 'combined/combined_PSeffort_density_map_histVrec.png'),height = 11, width = 8, units = "in", dpi = 200)


# Combining catch and effort onto one plot
tmp1 <- 
  catch_normalized |>
  dplyr::rename(density = catch2_normalized_scaled) |>
  dplyr::select(lat,lon, sp_code, yy_bin, density)

tmp2 <- 
  eff_normalized |>
  mutate(sp_code = 'PS_effort') |>
  dplyr::rename(density = sets2_normalized_scaled) |>
  dplyr::select(lat,lon, sp_code, yy_bin, density)

p <- 
bind_rows(tmp1,tmp2) |>
  mutate(sp_code = factor(sp_code, levels = c('BET', 'SKJ', 'YFT', 'PS_effort'))) |>
  uncount(density) |>
  ggplot() +
  aes(x = lon, y = lat) +
  geom_density_2d_filled(bins=12, h=10) +
  geom_density_2d(bins=11, col = 'grey50', h=10, size=0.5) +
  geom_polygon(data=map_bg2, aes(long, lat, group=group), fill = 'grey50', col = 'black') +
  coord_sf(xlim = c(120, 220), ylim = c(-25, 25)) +
  facet_wrap(~sp_code + yy_bin, ncol=2) +
  scale_fill_manual(values = pal2) +
  labs(x = 'Longitude', y = 'Latitude', title = "PS catch and effort density") +
  gg.theme #+
  #theme(legend.position = 'none')
p 
ggsave(p, file = paste0(results_wd, 'combined/combined_PScatch_effort_density_map.png'),height = 11, width = 8, units = "in", dpi = 200)

  