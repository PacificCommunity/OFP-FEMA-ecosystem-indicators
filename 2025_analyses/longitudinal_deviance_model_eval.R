# Evaluate longitude deviance modele
# AUthor: Nick Hill
# Date: 11/6/25

# The intent of this script is to evaluate models from longitude_deviance_model.R
# which aim to determine if PS skipjack biomass/catch/effort is moving with climate change.
# Here, we read in the models, extract their residuals, variable response curves etc

# 1. Preamble ----
# libraries
library(tidyverse)
library(patchwork)
library(mgcv)
library(gratia)
library(sp)
#library(RODBC)
#library(sdmTMB)

# directories
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


# Evaluations statistics functions
source('2025_analyses/model_diagnostics_utils_nh3.R')

# 2. Extract model files from directory ----

# model directory
mod.dir <- paste0(results_wd, 'long_deviance/')

# Extract all model files from directory
mod_files_all <- data.frame(path = list.files(path = mod.dir,
                                              #pattern = "^mod[0-9]+_dat", 
                                              pattern = "_model", 
                                              full.names = TRUE, recursive = T)) |>
  mutate(filename = basename(path), mod = str_extract(filename, "^mod[^_]*"),
         sp_code = str_match(path, "/(SKJ|BET|YFT)/")[, 2],
         nm = str_extract(filename, ".*(?=_model\\.Rdata)"),
         folder  = paste0(mod.dir, sp_code, '/', nm, '/'))

# Filter to model(s) of interest
mod_files <- mod_files_all |> filter(str_detect(mod, 'modI|modJ|modK|modL'))

# 3. Model evaluation loop ----

for(i in 2:nrow(mod_files)) { #nrow(mods)
  
  # Define model params and load model
  mod.fld <- mod_files$folder[i]
  sp <- mod_files$sp_code[i]
  nm <- str_extract(mod_files$filename[i], ".*(?=_model\\.Rdata)")
  mod <- readRDS(mod_files$path[i])
  
  mod.data <- mod$model |>
    rowid_to_column(var = 'id')
  mod.family <- mod$family$family
  mod.func <- mod$formula
  mod.vars <- all.vars(formula(mod))
  mod.resp <- as.character(formula(mod))[2]
  
  # 3.1 extract model diags and plot residuals ----
  diagnostic.data <- mod.data %>%
    dplyr::select(., any_of(mod.vars)) %>%
    rename(., response = mod.vars[1]) |>
    rowid_to_column(var = 'id') |>
    mutate(resids_dev = residuals(mod, type = 'deviance'),
           fitted = fitted(mod),
           prd = predict(mod, type = "link"))
  
  save(diagnostic.data, file = paste0(mod.fld, nm, '_diags.Rdata'))
  
  message("Plot residuals")
  
  # deviance Residuals vs fitted
  png(paste0(mod.fld,'dev_resid_vs_fitted_qqplot.png'), width = 6, height = 6, units = 'in', res = 72)
  plot(diagnostic.data$fitted, diagnostic.data$resids_dev,
       xlab = "Fitted", ylab = "Deviance residuals")
  abline(h = 0, col = "red")
  dev.off()
  
  # deviance QQhist
  png(file.path(mod.fld,'deviance_qqhist.png'), width = 6, height = 6, units = 'in', res = 72)
  hist(diagnostic.data$resids_dev)
  dev.off()
  
  # pearson QQplot
  png(file.path(mod.fld,'dev_qqnorm.png'), width = 6, height = 6, units = 'in', res = 72)
  qqnorm(scale(diagnostic.data$resids_dev)); abline(0,1, col = 'red')
  dev.off()
  
  # appraise plot
  p <- appraise(mod)
  ggsave(p, file = paste0(mod.fld,'residuals_appraise_plot.png'))
  
  # 3.2 sim data ----
  message("Assess consistency of model with observed zeros and/or positives")
  
  message(" - simulate responses from fitted model")
  sim.data <- get_simulations(mod, n.draws = 30)
  
  message(" - generating plots")
  simulated_response_comparison_plots(mod.vars = mod.vars, data = diagnostic.data, 
                                      sim.resp = sim.data, family = mod.family, path.str = file.path(mod.fld))
  
  # 3.3 conditional effect plots
  
  #good - check make_ref_levels_dfr() function
  message("Generate (main) conditional effect plots")
  
  #if(mod$family$delta) fx.prd <- main_fx_plots_hurdle(mod, file.path(mod.fld), 25)
  #if(!mod$family$delta) 
  fx.prd <- main_fx_plots_bam(mod, file.path(mod.fld), 25)
  save(fx.prd, file = paste0(mod.fld, nm, '_fxpreds.Rdata'))
  
  # if(str_detect(mod.resp, 'cpue|SKJ_log')) {intrx <- main_fx_plots_bam_interact(object = mod, 
  #                                                 out.path = file.path(mod.fld), n = 100)
  # save(intrx, file = paste0(mod.fld, nm, '_intrxpreds.Rdata'))}
  # 
  # # 3.4 year effect plot
  # if(str_detect(mod.resp, 'lond')) {
  # hist_mn <- mean(fx.prd[[1]]$est[fx.prd[[1]]$yy %in% c(1995:2005) & fx.prd[[1]]$fx.term == 'yy'])
  # 
  # p <- 
  #   fx.prd[[1]] |>
  #   dplyr::filter(fx.term == 'yy') |>
  #   mutate(hist_period = ifelse(yy %in% c(1995:2005), 1, 0)) |>
  #   ggplot() +
  #   aes(as.numeric(as.character(yy)), est, ymin = lx, ymax = ux) +
  #   geom_pointrange(aes(col = as.factor(hist_period))) +
  #   geom_line(group = 1) +
  #   #geom_hline(yintercept = 1, linetype = 'dashed') +
  #   geom_hline(yintercept = hist_mn, linetype = 'dashed') +
  #   scale_color_manual(values = c("0" = "black", "1" = "red")) +
  #   #geom_rect(aes(xmin = 1995, xmax = 2005, ymin = 155, ymax = 180),
  #   #fill = NA, color = "red", size = 1) +
  #   gg.theme +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
  #         legend.position = 'none') +
  #   labs(x = 'Year', y = 'Longitude')
  # p
  # ggsave(p, file = paste0(mod.fld,'yreffect_lond_plot.png'))
  # }
}

# 4. Combined eval plots ----
# Here, we read in eval data for models and combine them to plot, explore

# Combined interaction model long indicator
# model directory
mod.dir <- paste0(results_wd, 'long_deviance/')

# Extract all model files from directory
mod_files_intx <- data.frame(path = list.files(path = mod.dir,
                                               #pattern = "^mod[0-9]+_dat", 
                                               pattern = "_intrxpreds", 
                                               full.names = TRUE, recursive = T)) |>
  mutate(filename = basename(path), mod = str_extract(filename, "^mod[^_]*"),
         sp_code = str_match(path, "/(SKJ|BET|YFT)/")[, 2],
         nm = str_extract(filename, ".*(?=_intrxpreds\\.Rdata)"),
         folder  = paste0(mod.dir, sp_code, '/', nm, '/'))

intrx_df <- tibble() 

for(i in 1:nrow(mod_files_intx)) {
  
  load(mod_files_intx$path[i])
  
  d <- as.data.frame(intrx[[2]]) |>
    mutate(nm = mod_files_intx$nm[i],
           mod = mod_files_intx$mod[i])
  
  intrx_df <- bind_rows(intrx_df, d)
}

hist_mn <- mean(intrx_df$est[intrx_df$yy %in% c(1995:2005)])

p <-
  intrx_df |>
  #mutate(hist_bin = as.factor(ifelse(yy %in% c(1995:2005),1,0))) |>
  ggplot() +
  aes(x = as.numeric(as.character(yy)), y = est, col = mod) +
  geom_pointrange(aes(ymin = lx, ymax = ux)) +
  scale_color_brewer(palette = 'Set1') +
  geom_line() +
  geom_hline(yintercept = hist_mn, linetype = 'dashed') +
  labs(x = "Year",y = "Weighted Mean Longitude") +
  gg.theme +
  theme(legend.position = 'bottom')
p

ggsave(p, file = paste0(path = results_wd,'long_deviance/figs/yreffect_allmods_combined_intrx_yrlon_plot.png'), 
       width = 6, height = 6, units = 'in',  dpi = 100)

# 5. combined longiutude models longitude:year plot ----

mod_files_lond <- data.frame(path = list.files(path = mod.dir,
                                               #pattern = "^mod[0-9]+_dat", 
                                               pattern = "_fxpreds", 
                                               full.names = TRUE, recursive = T)) |>
  mutate(filename = basename(path), mod = str_extract(filename, "^mod[^_]*"),
         sp_code = str_match(path, "/(SKJ|BET|YFT)/")[, 2],
         nm = str_extract(filename, ".*(?=_fxpreds\\.Rdata)"),
         folder  = paste0(mod.dir, sp_code, '/', nm, '/')) |>
  dplyr::filter(str_detect(mod, 'modA|modB|modE|modF'))

lond_df <- tibble() 

for(i in 1:nrow(mod_files_lond)) {
  
  load(mod_files_lond$path[i])
  
  d <- as.data.frame(fx.prd[[1]]) |>
    mutate(nm = mod_files_lond$nm[i],
           mod = mod_files_lond$mod[i])
  
  lond_df <- bind_rows(lond_df, d)
}

hist_mn1 <- mean(intrx_df$est[intrx_df$yy %in% c(1995:2005)])
hist_mn2 <- mean(lond_df$est[lond_df$yy %in% c(1995:2005)])

p <-
  lond_df |>
  dplyr::filter(fx.term == 'yy') |>
  #mutate(hist_bin = as.factor(ifelse(yy %in% c(1995:2005),1,0))) |>
  ggplot() +
  aes(x = as.numeric(as.character(yy)), y = est, col = mod) +
  geom_pointrange(aes(ymin = lx, ymax = ux)) +
  scale_color_brewer(palette = 'Set1') +
  geom_line() +
  geom_hline(yintercept = hist_mn2, linetype = 'dashed') +
  labs(x = "Year",y = "Weighted Mean Longitude") +
  gg.theme +
  theme(legend.position = 'bottom')
p

ggsave(p, file = paste0(path = results_wd,'long_deviance/figs/yreffect_allmods_combined_lond_yrlon_plot.png'), 
       width = 6, height = 6, units = 'in',  dpi = 100)

p <- 
  lond_df |>
  dplyr::filter(fx.term == 'yy') |>
  bind_rows(intrx_df) |>
  ggplot() +
  aes(x = as.numeric(as.character(yy)), y = est, col = mod) +
  geom_pointrange(aes(ymin = lx, ymax = ux)) +
  scale_color_brewer(palette = 'Set1') +
  geom_line() +
  geom_hline(yintercept = hist_mn1, linetype = 'dashed') +
  geom_hline(yintercept = hist_mn2, linetype = 'dashed') +
  labs(x = "Year",y = "Weighted Mean Longitude") +
  gg.theme +
  theme(legend.position = 'bottom')
p

ggsave(p, file = paste0(path = results_wd,'long_deviance/figs/yreffect_allmods_combined_allmods_yrlon_plot.png'), 
       width = 6, height = 6, units = 'in',  dpi = 100)

# 6. Extract model eval statistics and produce table ----

mod_files_all <- data.frame(path = list.files(path = mod.dir,
                                              #pattern = "^mod[0-9]+_dat", 
                                              pattern = "_model", 
                                              full.names = TRUE, recursive = T)) |>
  mutate(filename = basename(path), mod = str_extract(filename, "^mod[^_]*"),
         sp_code = str_match(path, "/(SKJ|BET|YFT)/")[, 2],
         nm = str_extract(filename, ".*(?=_model\\.Rdata)"),
         folder  = paste0(mod.dir, sp_code, '/', nm, '/'))

# Filter to model(s) of interest
mod_files <- mod_files_all |> filter(str_detect(mod, "^mod[A-Za-z]$"))

eval_tbl <- tibble()

extract_model_info <- function(model) {
  s <- summary(model)
  tibble::tibble(
    AIC = AIC(model),
    BIC = BIC(model),
    DevianceExplained = s$dev.expl * 100,
    AdjR2 = s$r.sq,
    EDF_total = sum(s$s.table[, "edf"]),
    Residual_DF = model$df.residual,
    GCV_UBRE = s$gcv.ubre
  )
}

for(i in 1:nrow(mod_files)) { 
  
  # Define model params and load model
  mod.fld <- mod_files$folder[i]
  sp <- mod_files$sp_code[i]
  nm <- str_extract(mod_files$filename[i], ".*(?=_model\\.Rdata)")
  model <- readRDS(mod_files$path[i])
  
  d <- extract_model_info(model) %>% 
    mutate(nm = nm, mod = mod_files$mod[i])
  
  eval_tbl <- bind_rows(eval_tbl, d)
}

library(flextable)

tbl <- 
  eval_tbl |>
  dplyr::select(Model = mod, Name = nm, df = EDF_total, AIC, BIC, Deviance = DevianceExplained, Adj_R2 = AdjR2) |>
  mutate(df = round(df, 2), AIC = round(AIC, 0), BIC = round(BIC, 0), Deviance = round(Deviance, 1),
         Adj_R2 = round(Adj_R2, 2),
         mod_lbl = c('modA', 'modB', 'modC', 'modD'),
         mod_lbl2 = c('Linear', 'Factor', 'Year:flag ind smooth', 'Year:flag common smooth')) |>
  dplyr::select(-c(Model, Name)) |>
  dplyr::select(Model = mod_lbl, Name = mod_lbl2, df, AIC, BIC, Deviance, Adj_R2) |>
  flextable() |>
  bold(part = "header") |>            # Bold all column headers
  bold(j = 1, part = "body")  |>
  bg(bg = "white", part = "all") 
tbl

save_as_image(tbl, paste0(results_wd,'long_deviance/figs/model_eval_table2.png'), 
              width = 6, height = 6, units = 'in',  res = 100)

# 7. Combine year/lond models with raw COG ----

# Load in effort and catch COG plus model dfs from above
load(paste0(data_wd, 'catch/catch_seine_COGs.Rdata'))
head(PS_catch_COGs)

load(paste0(data_wd, 'effort/effort_seine_COGs.Rdata'))
head(s_eff_COGs)

# catch COG df
tmp1 <- PS_catch_COGs |>
  ungroup() |>  
  dplyr::filter(sp_code == 'SKJ' & set_type == 'all') |>
  mutate(index = 'COG_catch', yy = as.factor(yy)) |>
  dplyr::select(c(yy, est = lon, index))

# effort COG df
tmp2 <-
  s_eff_COGs |>
  ungroup() |>  
  dplyr::filter(set_type == 'all') |>
  mutate(index = 'COG_effort', yy = as.factor(yy)) |>
  dplyr::select(c(yy, est = lon, index))

# cpue mods df
# tmp3 <- intrx_df |>
#   group_by(yy) |>
#   summarise(est = mean(est, na.rm = T),
#             lx = mean(lx, na.rm = T),
#             ux = mean(ux, na.rm = T)) |>
#   mutate(index = 'mod_cpue')

# lond mods df
tmp4 <- lond_df |>
  group_by(yy) |>
  summarise(est = mean(est, na.rm = T),
            lx = mean(lx, na.rm = T),
            ux = mean(ux, na.rm = T)) |>
  mutate(index = 'mod_lond')

# Combine all indices of interest
indx_df <- bind_rows(tmp1, tmp2, tmp3, tmp4) 

# Load in ENSO data
destfile <- '2025_analyses/data/long_deviance/oni.data.txt'

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
  indx_df |>
  #left_join(oni, by = 'yy') |> 
  ggplot() +
  aes(as.numeric(as.character(yy)), est, ymin = lx, ymax = ux, col = index, group = index) +
  geom_rect(data = oni,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE,alpha = 0.15) +
  geom_pointrange() +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_manual(values = c("elnino" = "red", "lanina" = "blue"),na.value = "white") +
  gg.theme +
  labs(x = 'Year', col = 'Index', y = 'Mean longitude') +
  guides(fill = "none") +  # 👈 removes fill legend
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'bottom') +
  xlim(1990,2023)
p
#ggsave(p, file = paste0(path = results_wd,'long_deviance/figs/yreffect_allmods_combined_allmods_yrlon_plot_ENSO.png'), 
#width = 6, height = 6, units = 'in',  dpi = 100)

#########################################
# Compare with Thom's sdmTMB ----

library(sf)

#load('2025_analyses/data/long_deviance/center.of.gravity/cog.pred.df.Rdata')
load('2025_analyses/data/long_deviance/center.of.gravity/cog.Rdata')

# Example: your data with projected coordinates
sf_df <- data.frame(x = cog$est_x, y = cog$est_y)

# Convert to sf object with correct CRS (replace with your actual projection)
sf_pts <- st_as_sf(sf_df, coords = c("x", "y"), crs = "+proj=tpeqd +lat_1=-2 +lon_1=148 +lat_2=-2 +lon_2=202 +x_0=0 +y_0=0")  # UTM zone 61N (just an example)

# Transform to WGS84 (longitude/latitude)
sf_pts <- st_transform(sf_pts, crs = 4326)

# Extract lon/lat as columns
coords <- st_coordinates(sf_pts)
sf_df <- sf_df |>
  mutate(lon = coords[,1], lat = coords[,2],
         lon = ifelse(lon < 0, lon + 360, lon)) |>
  rowid_to_column(var = 'id')

cog <- cog |> 
  rowid_to_column(var = 'id') |>
  left_join(sf_df, by = 'id') 

cog |>
  ggplot() +
  aes(yrqtr, x = est_x, ymin = ) +
  #geom_polygon(data=map_bg2, aes(long, lat, group=group),fill = 'grey50', col = 'black') +
  geom_point() +
  geom_line() +
  gg.theme +
  labs(x = 'Year quarter', y = 'Longitudinal COG')
#coord_sf(xlim = c(100, 300), ylim = c(-50, 50)) +

cog |>
  ggplot() +
  aes(x = est_x, y = est_y, ymin = lwr_y, ymax = upr_y, xmin = lwr_x, xmax = upr_x, col = yrqtr) +
  geom_errorbar(width = 0.1) +         # vertical bounds
  geom_errorbarh(height = 0.1) +  
  geom_point() +
  gg.theme +
  #scale_color_distiller(palette = 'Spectral') +
  scale_color_viridis_c(direction = -1) +
  labs(x = 'Eastings', y = 'Northings', col = 'Year/\nquarter')

cog_yr <- cog |>
  mutate(yy = as.factor(as.integer(floor(yrqtr))),
         index = 'sdmTMB') |>
  group_by(yy, index) |>
  summarise(est = mean(lon, na.rm = T)) 

#x <- 
indx_df |>
  bind_rows(cog_yr) |>
  ggplot() +
  aes(as.numeric(as.character(yy)), est, ymin = lx, ymax = ux, col = index, group = index) +
  geom_rect(data = oni,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            inherit.aes = FALSE,alpha = 0.15) +
  geom_pointrange() +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_manual(values = c("elnino" = "red", "lanina" = "blue"),na.value = "white") +
  gg.theme +
  labs(x = 'Year', col = 'Index', y = 'Mean longitude') +
  guides(fill = "none") +  # 👈 removes fill legend
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'bottom') +
  xlim(1990,2023)

############ Jo update

# Combine year effect plots across 4 modes ----

# Filter to model(s) of interest
mod_files <- mod_files_all |> filter(str_detect(mod, 'modI|modJ|modK|modL'))

yy_df <- tibble()
for(i in 1:nrow(mod_files)) { #nrow(mods)
  
  # Define model params and load model
  mod.fld <- mod_files$folder[i]
  sp <- mod_files$sp_code[i]
  nm <- str_extract(mod_files$filename[i], ".*(?=_model\\.Rdata)")
  mod <- readRDS(mod_files$path[i])
  
  d <- as.data.frame(ggpredict(mod, terms = "yy")) |> 
    mutate(mod = nm, x = as.numeric(as.character(x))) |>
    dplyr::rename(yy = x)
  
  yy_df <- bind_rows(yy_df, d)
  
}

mn_lond <- 161.2
head(df)
mn_londyy <- df |>
  group_by(yy) |>
  summarise(mn_lond = mean(lond, na.rm = T))

p <- 
  yy_df |>
  mutate(lbl = case_when(str_detect(mod, 'modI') ~ 'Linear',
                         str_detect(mod, 'modJ') ~ 'Factor',
                         str_detect(mod, 'modK') ~ 'Independent smooth',
                         str_detect(mod, 'modL') ~ 'Common smooth'),
         lbl = factor(lbl, levels = c('Linear', 'Factor', 'Independent smooth', 'Common smooth'))) |>
  ggplot() +
  geom_line(aes(yy, predicted)) +
  geom_ribbon(aes(yy, predicted, ymin = conf.low, ymax = conf.high), col = NA, alpha = 0.2) +
  geom_hline(yintercept = mn_lond, linetype = 'dashed') +
  geom_point(data = mn_londyy, aes(x = yy, y = mn_lond), col = 'red') +
  gg.theme +
  labs(x = 'Year', y = 'Predicted longitude') +
  theme(legend.position = 'bottom') +
  facet_wrap(~lbl)
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/yr_effect_allmods.png'))

# standardised vs unstandardised plot ----

load(paste0(data_wd, 'catch/catch_seine_COGs.Rdata'))
head(PS_catch_COGs)

load(paste0(data_wd, 'effort/effort_seine_COGs.Rdata'))
head(s_eff_COGs)

# catch COG df
tmp1 <- PS_catch_COGs |>
  ungroup() |>  
  dplyr::filter(sp_code == 'SKJ' & set_type == 'all') |>
  mutate(index = 'COG_catch', yy = as.factor(yy)) |>
  dplyr::select(c(yy, est = lon, index))

# effort COG df
tmp2 <-
  s_eff_COGs |>
  ungroup() |>  
  dplyr::filter(set_type == 'all') |>
  mutate(index = 'COG_effort', yy = as.factor(yy)) |>
  dplyr::select(c(yy, est = lon, index))

tmp3 <- yy_df |> dplyr::filter(str_detect(mod, 'modL')) |>
  dplyr::select(yy, est = predicted, lx = conf.low, ux = conf.high) |>
  mutate(index = 'modeled', yy = as.factor(yy))

mn_lond <- 161.2

library(RColorBrewer)
pal <- c(brewer.pal(2, "Set1")[1:2], 'black')

bind_rows(tmp1,tmp2,tmp3) |>
  ggplot() +
  aes(yy, est, ymin = lx, ymax = ux, col = index, group = index) +
  geom_line() +
  geom_ribbon(col = NA, alpha = 0.2, col = 'grey') +
  geom_hline(yintercept = mn_lond, linetype = 'dashed') +
  gg.theme +
  scale_color_manual(values = pal) +
  labs(y = 'Longitude', x = 'Year') +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

destfile <- paste0(data_wd, 'long_deviance/oni.data.txt')

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

oni$fill[oni$fill == "neutral"] <- NA
oni$fill <- factor(oni$fill, levels = c("El Nino", "La Nina", "Neutral"))

mn_lond <- 161.2
head(df)
mn_londyy <- df |>
  group_by(yy) |>
  summarise(mn_lond = mean(lond, na.rm = T))

p <- 
  bind_rows(tmp1,tmp2,tmp3) |>
  ggplot() +
  aes(as.numeric(as.character(yy)), est, ymin = lx, ymax = ux, col = index, group = index) +
  # geom_rect(data = oni,
  #           aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
  #           inherit.aes = FALSE, alpha = 0.15) +
  geom_line() +
  geom_ribbon(colour = NA, alpha = 0.2, fill = 'grey50') +
  geom_hline(yintercept = mn_lond, linetype = 'dashed') +
  gg.theme +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = c("elnino" = "red", "lanina" = "blue", "neutral" = 'white')) +
  #scale_fill_manual(values = c("El Nino" = "red", "La Nina" = "blue", "Neutral" = NA)) +
  labs(y = 'Longitude', x = 'Year', col = 'Index') +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = "none") +
  xlim(1990,2023)
p
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/modeled_vs_COG_indices_enso_modL2.png'))


# Variable response plot

mod_files <- mod_files_all |> filter(str_detect(mod, 'modI|modJ|modK|modL'))

mod <- readRDS(mod_files$path[4])  


preds <- as.data.frame(ggpredict(mod, terms = c("yy", "flag"))) |>
  mutate(bin = ifelse(group %in% c('KI', 'KR', 'TW', 'US', 'VU'), 'DW', 'PICT')) 
p1 <- 
  ggplot(preds, aes(x = x, y = predicted, color = group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(x = 'Year', y = 'Predicted longitude', fill = 'Flag', color = 'Flag', title = '') +
  scale_fill_brewer(palette = 'Paired') +
  scale_color_brewer(palette = 'Paired') +
  facet_wrap(~bin) +
  gg.theme +
  theme(aspect.ratio = 1, strip.text = element_blank(), legend.position = 'bottom')
p1
ggsave(p1, file = paste0(results_wd, 'long_deviance/figs/modL_flag_yy_smooths_plot.png'))

# preds <- ggpredict(mod, terms = c("yy", "flag")) 
# p1 <- 
#   ggpredict(mod, terms = "yy") |>
#   plot() +
#   gg.theme +
#   labs(x = 'Year', y = '', title = '') +
#   ylim(150,170)

p2 <- 
  ggpredict(mod, terms = "set_type") |>
  plot() + 
  gg.theme +
  labs(x = 'Set type', y = '', title = '')+
  ylim(150,170)

p3 <- 
  ggpredict(mod, terms = "mm") %>% 
  plot() + 
  gg.theme +
  labs(x = 'Month', y = '', title = '')+
  ylim(150,170)

p4 <- 
  ggpredict(mod, terms = "oniF") %>% 
  plot() + 
  gg.theme +
  labs(x = 'ENSO', y = '', title = '')+
  ylim(150,170)

p5 <- 
  ggpredict(mod, terms = "flag") |>
  plot() +
  gg.theme +
  labs(x = 'Flag', y = '', title = '')+
  ylim(150,170)

p1+p2+p3+p4+p5 + 
  plot_layout(ncol = 2)


combined <- (p2 + p3 + p4) + plot_layout(ncol = 2)

p <- 
  ggdraw() +
  draw_plot(combined) +
  draw_label("Predicted Longitude", x = 0.02, y = 0.5, angle = 90, vjust = 1, size = 12, fontface = 'bold')
ggsave(p, file = paste0(results_wd, 'long_deviance/figs/modL_var_response_plot2.png'))


# Variable importance ----

# Define full model explained deviance
full_dev_expl <- summary(mod)$dev.expl

# Define model terms to test (must match model components)
terms <- c("s(yy, flag, bs = 'fs')", " s(mm, bs = 'cc', k = 6)", "s(latd)", "set_type", "oniF")

# Extract dev.expl (%) from each reduced model (i.e., after removing a term)
dev_expl_each <- sapply(terms, function(term) {
  reduced_formula <- update.formula(formula(mod), paste(". ~ . -", term))
  reduced_mod <- bam(reduced_formula, data = mod$model)
  summary(reduced_mod)$dev.expl
})

# Sort descending for readability (optional)
dev_expl_each <- sort(dev_expl_each, decreasing = TRUE)

# View results
print(round(dev_expl_each, 4))  # shows % deviance explained (0–1)

100 * (full_dev_expl - dev_expl_each)/ sum(full_dev_expl - dev_expl_each)
