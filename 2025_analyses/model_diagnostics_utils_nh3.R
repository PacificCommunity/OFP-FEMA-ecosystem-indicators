# Tom P model evaluation util functions

## Make a folder if it doesn't already exist
make_folder <- function(path, ...) {
  if(!dir.exists(path)) dir.create(path, ...)
}

## Colour blind friendly palette by Okabe & Ito
ito_cols <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#0072B2", "#D55E00", "#000000")
names(ito_cols) <- c("light blue", "orange", "green", "yellow", "pink", "dark blue", "red", "black")

# Rename Rdata
rename_RData <- function(...) {
  obj_name <- load(...)
  get(obj_name)
}

# Add x and y as a variable
add_xy_as_variables <- function(x, dim.names) {
  y <- x %>% st_coordinates(.)
  stopifnot(nrow(x) == nrow(y))
  
  x[, dim.names[1]] <- y[, "X"]
  x[, dim.names[2]] <- y[, "Y"]
  
  x
}

# Make reference level df for model predictions
make_ref_levels_dfr2 <- function(data) {
  ref <- data[1, , drop = FALSE]  # use structure of data
  
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      ref[[col]] <- median(data[[col]], na.rm = TRUE)
    } else if (is.factor(data[[col]])) {
      # get the mode (most frequent level)
      ref[[col]] <- names(which.max(table(data[[col]])))
    } else {
      ref[[col]] <- data[[col]][1]  # fallback
    }
  }
  
  return(ref)
}

make_ref_levels_dfr <- function() {
  data.frame(
    yy = "2015", year = 2015, mm = "5", month = 5L, qtr = "2", #set.type = "FS", 
    flag.id = "US",
    oni.class = "neutral", #depth.20 = 180, 
    lon_proj = 0, lat_proj = 0, hbf = 21, sst100 = 24, chl = 0.1, chl_log = log(0.1 + 1),
    wiretrace = F, hk.bin = 'C.sml', bt.reg = 'fsh', sst = 27,
    lon.proj = 0, lat.proj = 0, chl.log = log(0.1 + 1),
    hk.shape = 'C', tar.clust = 'ALB', hooks = 1000,
    iso20 = 150, htc300 = 24.82412
  )
}
#make_ref_levels_dfr()

split_var_levels <- function(x, n.levels) {
  ## for positive x with relatively large proportions of 0s, create bin just for 0s
  if(all(x >= 0) & (sum(x %in% 0) / length(x)) > 0.05) {
    brk <- quantile(x[x > 0], probs = seq(0, 1, length.out = n.levels), na.rm = T)
    brk <- c(0, brk)
    brk[length(brk)] <- 1.001 * brk[length(brk)]
  } else {
    ## get quantiles of var
    ##  - manually force 100 % quantile to be slightly larger than max
    brk <- quantile(x, probs = seq(0, 1, length.out = n.levels + 1), na.rm = TRUE)
    brk[length(brk)] <- 1.001 * brk[length(brk)]
  }
  
  ## cut var using brk
  cut(x, breaks = unique(brk), right = FALSE)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 'Standard' quantile residual diagnostics

qq_rsd_plot <- function(rsd) {
  qqnorm(rsd[is.finite(rsd)], las = 1, main = '')
  qqline(rsd[is.finite(rsd)], col = "red")
}

rsd_density_plot <- function(rsd) {
  plot(density(rsd), xlab = 'Quantile residuals', main = '')
}

rsd_hist_plot <- function(rsd, rsd.range = c(-4, 4)) {
  rsd <- rsd[rsd > rsd.range[1] & rsd < rsd.range[2]]
  hist(rsd, breaks = seq(rsd.range[1], rsd.range[2], length.out = 40), xlab = 'Quantile residuals', main = '')
}

acf_plot <- function(rsd) {
  acf(rsd, main = '')
  pacf(rsd, main = '')
}

rsd_ndx_plot <- function(rsd) {
  plot(rsd, col = rgb(0, 0, 0, alpha = 0.05), ylab = 'Quantile residuals', xlab = 'Index')
}

rsd_fitted_plot <- function(ftd, rsd) {
  plot(x = ftd, y = rsd, col = rgb(0, 0, 0, alpha = 0.05), ylab = 'Quantile residuals', xlab = 'Fitted value')
}

rsd_fitted_bwplot <- function(ftd, rsd) {
  dfr <- data.frame(lpred = ftd, qrsd = rsd)
  dfr <- dfr %>% mutate(., lpred = split_var_levels(lpred, 10))
  
  dfr %>% boxplot_fn(., lpred, qrsd, "Linear predictor (binned)") +
    ylab("Quantile residuals")
}

obs_fitted_plot <- function(ftd, obs) {
  plot(x = obs, y = ftd, col = rgb(0, 0, 0, alpha = 0.05), ylab = 'Fitted', xlab = 'Observed')
}

## Convenience wrapper to generate 'standard' conditional residual plots
base_conditional_qrsd_plots <- function(rsd, prd, path, prefix = ""){
  
  stopifnot(length(rsd) == length(prd))
  
  ndx <- !is.finite(rsd)
  if(sum(ndx) > 0) {
    message(" - excluding ", sum(ndx), " residuals which are not finite")
    rsd <- rsd[!ndx]
    prd <- prd[!ndx]
  }
  
  png(file.path(path, paste0(prefix, 'a_qrsd_qqplot.png')), width = 6, height = 6, units = 'in', res = 72)
  qq_rsd_plot(rsd)
  dev.off()
  
  png(file.path(path, paste0(prefix, 'a_qrsd_density.png')), width = 6, height = 6, units = 'in', res = 72)
  rsd_density_plot(rsd)
  dev.off()
  
  png(file.path(path, paste0(prefix, 'a_qrsd_hist.png')), width = 6, height = 6, units = 'in', res = 72)
  rsd_hist_plot(rsd)
  dev.off()
  
  png(file.path(path, paste0(prefix, 'a_qrsd_ndx_plot.png')), width = 6, height = 6, units = 'in', res = 72)
  rsd_ndx_plot(rsd)
  dev.off()
  
  png(file.path(path, paste0(prefix, 'a_qrsd_fitted_plot.png')), width = 6, height = 6, units = 'in', res = 72)
  rsd_fitted_plot(prd, rsd)
  dev.off()
  
  plt <- rsd_fitted_bwplot(prd, rsd)
  ggsave(file.path(path, paste0(prefix, 'a_qrsd_fitted_bwplot.png')), plot = plt, width = 8, height = 6, units = 'in')
  
  png(file.path(path, paste0(prefix, 'a_qrsd_acf_plot.png')), width = 10, height = 6, units = 'in', res = 72)
  par(mfrow = c(1, 2))
  acf_plot(rsd)
  dev.off()
  
  return("finished plots")
}

generate_base_qrsd_diagnostics <- function(family, rsd, prd, path) {
  
  if("rsd.delta" %in% colnames(rsd)) {
    base_conditional_qrsd_plots(rsd$rsd.delta, prd$prd.delta, path, prefix = "delta_")
  }
  
  if("rsd.pos" %in% colnames(rsd)) {
    base_conditional_qrsd_plots(rsd$rsd.pos, prd$prd.pos, path, prefix = "positives_")
  }
  
  return("Generated conditional residual diagnostic plots")
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Quantile residuals v modelled & unmodelled covariates

## get quantiles for boxplots
summary_fn_boxplot <- function(x, var) {
  summarise(x, lq = as.numeric(quantile({{ var }}, probs = 0.025)), lmq = as.numeric(quantile({{ var }}, probs = 0.25)),
            mq = as.numeric(quantile({{ var }}, probs = 0.5)),
            umq = as.numeric(quantile({{ var }}, probs = 0.75)), uq = as.numeric(quantile({{ var }}, probs = 0.975)),
            mn = mean({{ var }}))
}

boxplot_fn <- function(data, x, y, x_lab) {
  ## get quantiles
  data <- data %>% group_by(., {{ x }}) %>% summary_fn_boxplot(., {{ y }})
  
  data %>% ggplot(.) +
    geom_boxplot(aes(x = {{ x }}, ymin = lq, lower = lmq, middle = mq, upper = umq, ymax = uq), stat = "identity") +
    geom_hline(yintercept = c(-1.96, 0, 1.96), alpha = 0.3, linetype = 2) +
    coord_cartesian(ylim = c(-2.5, 2.5)) + ylab("Quantile residuals") +
    theme_bw() + xlab(x_lab)
}

rsd_cnt_covariate_plot <- function(data, x, rsd.var, n.lvl) {
  
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  ## If necessary, cut
  obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
  if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
  
  data <- data %>% mutate(., x.var = as.factor(x.var))
  data %>% boxplot_fn(., x = x.var, y = {{ rsd.var }}, x_lab = x.string) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
}

rsd_cat_covariate_plot <- function(data, x, rsd.var) {
  
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  data %>% boxplot_fn(., x = x.var, y = {{ rsd.var }}, x_lab = x.string)
}

## Convenience wrapper to generate plots of residuals v variables in data
qrsd_variable_plots <- function(data, rsd.var = 'rsd.pos', path.str, prefix = "") {
  ## get all variables in data, except l.set.id
  var.names <- colnames(data)
  var.names <- var.names[!var.names %in% c("l.set.id", "response")]
  var.names <- var.names[!grepl("^rsd", var.names)]
  
  ## Factorise mm so it's categorical and ordered appropriately
  if("mm" %in% colnames(data)) data <- data %>% mutate(., mm = factor(mm, levels = 1:12))
  if("month" %in% colnames(data)) data <- data %>% mutate(., mm = factor(month, levels = 1:12))
  
  lapply(var.names, function(x) {
    if(is.character(data[, x]) | is.factor(data[, x])) {
      plt <- rsd_cat_covariate_plot(data, sym(x), {{ rsd.var }})
    } else {
      plt <- rsd_cnt_covariate_plot(data, sym(x), {{ rsd.var }}, n.lvl = 10)
    }
    ggsave(paste0(prefix, 'b_qrsd_v_', x, '.png'), plot = plt, device = 'png',
           path = path.str, width = 8, height = 6, units = 'in')
    return("done")
  })
  return('finished plots')
}

generate_qrsd_variable_diagnostics <- function(data, path.str) {
  
  if("rsd.delta" %in% colnames(data)) {
    data %>% filter(., is.finite(rsd.delta)) %>%
      qrsd_variable_plots(., rsd.delta, path.str, prefix = "delta_")
  }
  
  if("rsd.pos" %in% colnames(data)) {
    data %>% filter(., is.finite(rsd.pos)) %>%
      qrsd_variable_plots(., rsd.pos, path.str, prefix = "positives_")
  }
  
  return('Generated plots of residuals against variables')
}

## Get PIT residuals
get_pit_residuals <- function(object, ...) {
  family <- object$family$family
  
  if(object$family$delta) {
    rsd.i <- residuals(object, model = 1, ...)
    rsd <- data.frame(rsd.delta = rsd.i)
    
    ## Note pit residuals not currently implemeted for trunc_nbinom and lognormal_mix distributions
    if(!grepl("truncated_nbinom|lognormal_mix", family[2])) {
      rsd.ii <- residuals(object, model = 2, ...)
      rsd <- rsd %>% mutate(., rsd.pos = rsd.ii)
    }
  }
  
  if(length(family) == 1 & grepl("^binomial", family[1])) {
    rsd <- residuals(object, ...)
    rsd <- data.frame(rsd.delta = rsd)
  }
  
  if(length(family) == 1 & !grepl("^binomial", family[1])) {
    rsd <- residuals(object, ...)
    rsd <- data.frame(rsd.pos = rsd)
  }
  
  stopifnot(nrow(rsd) == nrow(object$data))
  rsd
}

## Get linear predictor
##  - note that in case of truncated distributions, linear predictor may not correspond to mean
##  - see https://github.com/pbs-assess/sdmTMB/issues/350
get_lpred <- function(object) {
  family <- object$family$family
  
  # if (!is.null(object$family$delta) && object$family$delta) {
  #   prd <- predict(object, type = "link", re_form = NULL, model = NA)
  #   prd <- data.frame(prd.delta = prd[, "est1", drop = TRUE],
  #                     prd.pos = prd[, "est2", drop = TRUE])
  #   
  # } else if (length(family) == 1 && grepl("^binomial", family[1])) {
  #   prd <- predict(object, type = "link", re_form = NULL)
  #   prd <- data.frame(prd.delta = prd[, "est", drop = TRUE])
  
  #} else {
  prd <- predict(object, type = "link", re_form = NULL)
  prd <- data.frame(prd.pos = prd[, "est", drop = TRUE])
  # }
  
  stopifnot(nrow(prd) == nrow(object$data))
  return(prd)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Random effects diagnostics

## Convenience wrapper to generate 'standard' diagnostics for a random intercept
base_rfx_plots <- function(rfx, path, rfx_term) {
  png(file.path(path, paste0("a_rfx_", rfx_term, "_qqplot.png")), width = 6, height = 6, units = 'in', res = 72)
  qq_rsd_plot(rfx)
  dev.off()
  
  png(file.path(path, paste0("a_rfx_", rfx_term, "_density.png")), width = 6, height = 6, units = 'in', res = 72)
  rsd_density_plot(rfx)
  dev.off()
  
  png(file.path(path, paste0("a_rfx_", rfx_term, "_hist.png")), width = 6, height = 6, units = 'in', res = 72)
  rsd_hist_plot(rfx)
  dev.off()
  
  return('Generated diagnostic plots of random intercepts')
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Comparisons of simulated responses compared with observations

cols_statistic <- ito_cols[c("light blue", "dark blue")]
names(cols_statistic) <- c("median", "mean")

shapes_statistic <- c(2, 4)
names(shapes_statistic) <- c("median", "mean")

## Compare presence and positives against simulated
simulated_data_covariate_plot_both <- function(data, sim.data, x, n.lvl) {
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  ## Cut numeric variables in to bins based on quantiles
  if(is.numeric(data[, x.string])) {
    
    obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
    if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
    
    data <- data %>% mutate(., x.var = as.factor(x.var))
    
  }
  
  ## Add x.var to sim.data
  sim.data <- data %>% dplyr::select(., l.set.id, x.var) %>%
    left_join(sim.data, ., by =  "l.set.id")
  
  ## Get probability of zeros, and other quantiles
  data.delta <- data %>% group_by(., x.var) %>%
    summarise(., prop.zero = sum(response == 0) / n()) %>% ungroup(.) %>%
    mutate(., statistic = "mean")
  
  sim.delta <- sim.data %>% group_by(., id.draw, x.var) %>%
    summarise(., prop.zero = sum(sim.positives == 0) / n()) %>% ungroup(.)
  
  sim.delta <- sim.delta %>% group_by(., x.var) %>%
    summary_fn_boxplot(., prop.zero) %>%
    mutate(., statistic = "mean")
  
  ## Get mean and 90% quantile for positives
  data.positives <- data %>% filter(., response > 0) %>%
    group_by(., x.var) %>%
    summarise(., mean = mean(response), median = median(response),
              high = as.numeric(quantile(response, probs = 0.9))) %>%
    ungroup(.)
  
  data.positives.high <- data.positives %>% dplyr::select(., x.var, high) %>%
    mutate(., statistic = "mean")
  
  data.positives.central <- data.positives %>% dplyr::select(., - high) %>%
    pivot_longer(., c(mean, median), names_to = "statistic", values_to = "obs")
  
  sim.positives <- sim.data %>% filter(., sim.positives > 0) %>%
    group_by(., id.draw, x.var) %>%
    summarise(., mean.positive = mean(sim.positives), median.positive = median(sim.positives),
              high.positive = as.numeric(quantile(sim.positives, probs = 0.9))) %>%
    ungroup(.)
  
  sim.positives.mean <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., mean.positive)
  
  sim.positives.median <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., median.positive)
  
  sim.positives.central <- sim.positives.mean %>% mutate(., statistic = "mean")
  sim.positives.central <- sim.positives.median %>% mutate(., statistic = "median") %>%
    bind_rows(sim.positives.central, .)
  
  sim.positives.high <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., high.positive) %>%
    mutate(., statistic = "mean")
  
  ## Plot of observed vs predicted probability of zeros
  plt.delta <- sim.delta %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Proportion of zeros") +
    coord_cartesian(ylim = c(0, NA)) + theme(legend.position = 'bottom')
  
  plt.delta <- plt.delta +
    geom_point(data = data.delta, aes(x = x.var, y = prop.zero, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  ## Plot of observed vs predicted means and medians for response
  plt.positives.central <- sim.positives.central %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic), position = position_dodge(0.25)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (mean and median)") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.positives.central <- plt.positives.central +
    geom_point(data = data.positives.central, aes(x = x.var, y = obs, shape = statistic), position = position_dodge(0.25)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  ## Plot of 90% quantile
  plt.positives.high <- sim.positives.high %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (90 percentile)") +
    coord_cartesian(ylim = c(0, NA)) + theme(legend.position = 'bottom')
  
  plt.positives.high <- plt.positives.high +
    geom_point(data = data.positives.high, aes(x = x.var, y = high, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  # plt <- ggpubr::ggarrange(plt.delta, plt.positives.central, plt.positives.high, bg = "white",
  #                          nrow = 3, ncol = 1, common.legend = TRUE, legend = "bottom")
  plt <- plt.delta / plt.positives.central / plt.positives.high +  # Control plot heights if needed
    #theme(legend.position = 'bottom') + 
    plot_layout(guides = "collect", heights = c(1, 1, 1)) 
  
  plt
}

## Compare presence and positives against simulated
simulated_data_covariate_plot_hurdle <- function(data, sim.data, x, n.lvl) {
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  ## Cut numeric variables in to bins based on quantiles
  if(is.numeric(data[, x.string])) {
    
    obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
    if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
    
    data <- data %>% mutate(., x.var = as.factor(x.var))
    
  }
  
  ## Add x.var to sim.data
  sim.data <- data %>% dplyr::select(., l.set.id, x.var) %>%
    left_join(sim.data, ., by =  "l.set.id")
  
  ## Get probability of zeros, and other quantiles
  data.delta <- data %>% group_by(., x.var) %>%
    summarise(., prop.zero = sum(response == 0) / n()) %>% ungroup(.) %>%
    mutate(., statistic = "mean")
  
  sim.delta <- sim.data %>% group_by(., id.draw, x.var) %>%
    summarise(., prop.zero = sum(sim.delta == 0) / n()) %>% ungroup(.)
  
  sim.delta <- sim.delta %>% group_by(., x.var) %>%
    summary_fn_boxplot(., prop.zero) %>%
    mutate(., statistic = "mean")
  
  ## Get mean and 90% quantile for positives
  data.positives <- data %>% filter(., response > 0) %>%
    group_by(., x.var) %>%
    summarise(., mean = mean(response), median = median(response),
              high = as.numeric(quantile(response, probs = 0.9))) %>%
    ungroup(.)
  
  data.positives.high <- data.positives %>% dplyr::select(., x.var, high) %>%
    mutate(., statistic = "mean")
  
  data.positives.central <- data.positives %>% dplyr::select(., - high) %>%
    pivot_longer(., c(mean, median), names_to = "statistic", values_to = "obs")
  
  ## Filter simulated positives for records where catches were observed
  sim.positives <- data %>% filter(., response > 0) %>%
    semi_join(sim.data, ., by = "l.set.id")
  
  sim.positives <- sim.positives %>%
    group_by(., id.draw, x.var) %>%
    summarise(., mean.positive = mean(sim.positives), median.positive = median(sim.positives),
              high.positive = as.numeric(quantile(sim.positives, probs = 0.9))) %>%
    ungroup(.)
  
  sim.positives.mean <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., mean.positive)
  
  sim.positives.median <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., median.positive)
  
  sim.positives.central <- sim.positives.mean %>% mutate(., statistic = "mean")
  sim.positives.central <- sim.positives.median %>% mutate(., statistic = "median") %>%
    bind_rows(sim.positives.central, .)
  
  sim.positives.high <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., high.positive) %>%
    mutate(., statistic = "mean")
  
  ## Plot of observed vs predicted probability of zeros
  plt.delta <- sim.delta %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Proportion of zeros") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.delta <- plt.delta +
    geom_point(data = data.delta, aes(x = x.var, y = prop.zero, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  ## Plot of observed vs predicted means and medians for response
  plt.positives.central <- sim.positives.central %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic), position = position_dodge(0.25)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (mean and median)") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.positives.central <- plt.positives.central +
    geom_point(data = data.positives.central, aes(x = x.var, y = obs, shape = statistic), position = position_dodge(0.25)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  ## Plot of 90% quantile
  plt.positives.high <- sim.positives.high %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (90 percentile)") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.positives.high <- plt.positives.high +
    geom_point(data = data.positives.high, aes(x = x.var, y = high, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  plt <- ggpubr::ggarrange(plt.delta, plt.positives.central, plt.positives.high, bg = "white",
                           nrow = 3, ncol = 1, common.legend = TRUE, legend = "bottom")
  plt
}

## Compare presence against simulated
simulated_data_covariate_plot_presence <- function(data, sim.data, x, n.lvl) {
  
  stop("Needs updating for aggregated modelled dataset!")
  
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  ## Cut numeric variables in to bins based on quantiles
  if(is.numeric(data[, x.string])) {
    
    obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
    if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
    
    data <- data %>% mutate(., x.var = as.factor(x.var))
    
  }
  
  ## Add x.var to sim.data
  sim.data <- data %>% dplyr::select(., l.set.id, x.var) %>%
    left_join(sim.data, ., by =  "l.set.id")
  
  ## Get probability of zeros, and other quantiles
  data.delta <- data %>% group_by(., x.var) %>%
    summarise(., prop.zero = sum(response == 0) / n()) %>% ungroup(.) %>%
    mutate(., statistic = "mean")
  
  sim.delta <- sim.data %>% group_by(., id.draw, x.var) %>%
    summarise(., prop.zero = sum(sim.delta == 0) / n()) %>% ungroup(.)
  
  sim.delta <- sim.delta %>% group_by(., x.var) %>%
    summary_fn_boxplot(., prop.zero) %>%
    mutate(., statistic = "mean")
  
  ## Plot of observed vs predicted probability of zeros
  plt.delta <- sim.delta %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Proportion of zeros") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.delta <- plt.delta +
    geom_point(data = data.delta, aes(x = x.var, y = prop.zero, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  plt.delta
}
#simulated_data_covariate_plot_positives(data, sim.data, x, n.lvl)
## Compare positives against simulated
simulated_data_covariate_plot_positives <- function(data, sim.data, x, n.lvl) {
  ## get original name of variable, and rename
  x.string <- rlang::as_string({{ x }})
  data <- data %>% mutate(., x.var = {{ x }})
  
  ## Cut numeric variables in to bins based on quantiles
  if(is.numeric(data[, x.string])) {
    
    obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
    if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
    
    data <- data %>% mutate(., x.var = as.factor(x.var))
    
  }
  
  ## Add x.var to sim.data
  sim.data <- data %>% dplyr::select(., id, x.var) %>%
    left_join(sim.data, ., by =  "id")
  
  ## Get mean and 90% quantile for positives
  data.positives <- data %>%
    group_by(., x.var) %>%
    summarise(., mean = mean(response), median = median(response),
              high = as.numeric(quantile(response, probs = 0.9))) %>%
    ungroup(.)
  
  data.positives.high <- data.positives %>% dplyr::select(., x.var, high) %>%
    mutate(., statistic = "mean")
  
  data.positives.central <- data.positives %>% dplyr::select(., - high) %>%
    pivot_longer(., c(mean, median), names_to = "statistic", values_to = "obs")
  
  sim.positives <- sim.data %>% filter(., sim.positives > 0) %>%
    group_by(., id.draw, x.var) %>%
    summarise(., mean.positive = mean(sim.positives), median.positive = median(sim.positives),
              high.positive = as.numeric(quantile(sim.positives, probs = 0.9))) %>%
    ungroup(.)
  
  sim.positives.mean <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., mean.positive)
  
  sim.positives.median <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., median.positive)
  
  sim.positives.central <- sim.positives.mean %>% mutate(., statistic = "mean")
  sim.positives.central <- sim.positives.median %>% mutate(., statistic = "median") %>%
    bind_rows(sim.positives.central, .)
  
  sim.positives.high <- sim.positives %>% group_by(., x.var) %>%
    summary_fn_boxplot(., high.positive) %>%
    mutate(., statistic = "mean")
  
  ## Plot of observed vs predicted means and medians for response
  plt.positives.central <- sim.positives.central %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic), position = position_dodge(0.25)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (mean and median)") +
    coord_cartesian(ylim = c(0, NA))
  
  plt.positives.central <- plt.positives.central +
    geom_point(data = data.positives.central, aes(x = x.var, y = obs, shape = statistic), position = position_dodge(0.25)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  
  ## Plot of 90% quantile
  plt.positives.high <- sim.positives.high %>% ggplot(.) +
    geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
    scale_colour_manual("Statistic", values = cols_statistic) +
    theme_bw() + xlab(x.string) + ylab("Positive catch (90 percentile)") +
    coord_cartesian(ylim = c(0, NA)) + theme(legend.position = 'none')
  
  plt.positives.high <- plt.positives.high +
    geom_point(data = data.positives.high, aes(x = x.var, y = high, shape = statistic)) +
    scale_shape_manual("Statistic", values = shapes_statistic) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
  plt <- plt.positives.central / plt.positives.high +  # Control plot heights if needed
    #theme(legend.position = 'bottom') + 
    plot_layout(guides = "collect", heights = c(1, 1)) 
  
  
  #plt <- ggpubr::ggarrange(plt.positives.central, plt.positives.high, bg = "white",
  #nrow = 2, ncol = 1, common.legend = TRUE, legend = "bottom")
  plt
}

# simulated_data_covariate_plot_positives <- function(data, sim.data, x, n.lvl) {
#   ## get original name of variable, and rename
#   x.string <- rlang::as_string({{ x }})
#   data <- data %>% mutate(., x.var = {{ x }})
#   
#   ## Cut numeric variables in to bins based on quantiles
#   if(is.numeric(data[, x.string])) {
#     
#     obs.lvl <- data %>% dplyr::select(., {{ x }}) %>% distinct(.) %>% nrow(.)
#     if(obs.lvl > n.lvl) data <- data %>% mutate(., x.var = split_var_levels({{ x }}, n.levels = n.lvl))
#     
#     data <- data %>% mutate(., x.var = as.factor(x.var))
#     
#   }
#   
#   ## Add x.var to sim.data
#   sim.data <- data %>% dplyr::select(., l.set.id, x.var) %>%
#     left_join(sim.data, ., by =  "l.set.id")
#   
#   ## Get mean and 90% quantile for positives
#   data.positives <- data %>%
#     group_by(., x.var) %>%
#     summarise(., mean = mean(response), median = median(response),
#               high = as.numeric(quantile(response, probs = 0.9))) %>%
#     ungroup(.)
#   
#   data.positives.high <- data.positives %>% dplyr::select(., x.var, high) %>%
#     mutate(., statistic = "mean")
#   
#   data.positives.central <- data.positives %>% dplyr::select(., - high) %>%
#     pivot_longer(., c(mean, median), names_to = "statistic", values_to = "obs")
#   
#   sim.positives <- sim.data %>% filter(., sim.positives > 0) %>%
#     group_by(., id.draw, x.var) %>%
#     summarise(., mean.positive = mean(sim.positives), median.positive = median(sim.positives),
#               high.positive = as.numeric(quantile(sim.positives, probs = 0.9))) %>%
#     ungroup(.)
#   
#   sim.positives.mean <- sim.positives %>% group_by(., x.var) %>%
#     summary_fn_boxplot(., mean.positive)
#   
#   sim.positives.median <- sim.positives %>% group_by(., x.var) %>%
#     summary_fn_boxplot(., median.positive)
#   
#   sim.positives.central <- sim.positives.mean %>% mutate(., statistic = "mean")
#   sim.positives.central <- sim.positives.median %>% mutate(., statistic = "median") %>%
#     bind_rows(sim.positives.central, .)
#   
#   sim.positives.high <- sim.positives %>% group_by(., x.var) %>%
#     summary_fn_boxplot(., high.positive) %>%
#     mutate(., statistic = "mean")
#   
#   ## Plot of observed vs predicted means and medians for response
#   plt.positives.central <- sim.positives.central %>% ggplot(.) +
#     geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic), position = position_dodge(0.25)) +
#     scale_colour_manual("Statistic", values = cols_statistic) +
#     theme_bw() + xlab(x.string) + ylab("Positive catch (mean and median)") +
#     coord_cartesian(ylim = c(0, NA))
#   
#   plt.positives.central <- plt.positives.central +
#     geom_point(data = data.positives.central, aes(x = x.var, y = obs, shape = statistic), position = position_dodge(0.25)) +
#     scale_shape_manual("Statistic", values = shapes_statistic) +
#     scale_x_discrete(guide = guide_axis(n.dodge = 2))
#   
#   ## Plot of 90% quantile
#   plt.positives.high <- sim.positives.high %>% ggplot(.) +
#     geom_pointrange(aes(x = x.var, y = mn, ymin = lq, ymax = uq, colour = statistic)) +
#     scale_colour_manual("Statistic", values = cols_statistic) +
#     theme_bw() + xlab(x.string) + ylab("Positive catch (90 percentile)") +
#     coord_cartesian(ylim = c(0, NA)) + theme(legend.position = 'none')
#   
#   plt.positives.high <- plt.positives.high +
#     geom_point(data = data.positives.high, aes(x = x.var, y = high, shape = statistic)) +
#     scale_shape_manual("Statistic", values = shapes_statistic) +
#     scale_x_discrete(guide = guide_axis(n.dodge = 2))
#   plt <- plt.positives.central / plt.positives.high +  # Control plot heights if needed
#     #theme(legend.position = 'bottom') + 
#     plot_layout(guides = "collect", heights = c(1, 1)) 
#   
#   
#   #plt <- ggpubr::ggarrange(plt.positives.central, plt.positives.high, bg = "white",
#   #nrow = 2, ncol = 1, common.legend = TRUE, legend = "bottom")
#   plt
# }
#simulated_response_comparison_plots( # not working
#diagnostic.data, sim.data, mod.family, file.path(mod.fld))

simulated_response_comparison_plots <- function(mod.vars, data, sim.resp, family, path.str) {
  
  ## get all variables in data, except l.set.id
  var.names <- mod.vars[-1]
  #var.names <- var.names[!var.names %in% c("l.set.id", "response", "year", "lat", "lon",
  #"lat.proj", "lon.proj")]
  #var.names <- var.names[!grepl("^rsd", var.names)]
  
  ## Convert variables to factors where appropriate
  data <- data %>%
    mutate(mm = factor(mm, levels = 1:12),
           oniF = as.factor(oniF))
  
  # ## For hurdle / delta models
  # if(length(family) > 1 & grepl("binomial", family[1])) {
  # 
  #     lapply(var.names, function(x) {
  #         plt <- simulated_data_covariate_plot_hurdle(data, sim.resp, sym(x), 10)
  #         ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
  #                path = path.str, width = 8, height = 12, units = 'in')
  #     })
  # }
  
  ## For families modelling only presence
  # if(length(family) == 1 & grepl("^binomial", family[1])) {
  #   
  #   lapply(var.names, function(x) {
  #     plt <- simulated_data_covariate_plot_presence(data, sim.resp, sym(x), 10)
  #     ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
  #            path = path.str, width = 8, height = 8, units = 'in')
  #   })
  # }
  
  ## For single family models
  # if(length(family) == 1 & !grepl("^binomial", family[1])) {
  #   
  #   if(grepl("^nbinom|^poisson|^tweedie", family[1])) {
  #     lapply(var.names, function(x) {
  #       plt <- simulated_data_covariate_plot_both(data, sim.resp, sym(x), 10)
  #       ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
  #              path = path.str, width = 8, height = 8, units = 'in')
  #     })
  #     
  #} else {
  
  lapply(var.names, function(x) {
    plt <- simulated_data_covariate_plot_positives(data, sim.resp, sym(x), 10)
    # ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
    #        path = path.str, width = 8, height = 8, units = 'in')
    ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
           path = path.str, width = 8, height = 8, units = 'in')
  })
  #}
  #}
  
  return('Generated plots of simulated responses against observations')
}

## reformat matrix of simulated responses to dataframe
reformat_simulated_responses <- function(x, data, n.draws) {
  dimnames(x)[[2]] <- 1:n.draws
  x <- as.data.frame(x) %>%
    mutate(., id = data$id) %>%
    dplyr::select(., .id, everything())
  
  x %>% pivot_longer(., !matches("id"), names_to = "id.draw",
                     values_to = "sim.response")
}

# reformat_simulated_responses <- function(x, data, n.draws) {
#   dimnames(x)[[2]] <- 1:n.draws
#   x <- as.data.frame(x) %>%
#     mutate(., l.set.id = data$l.set.id) %>%
#     dplyr::select(., l.set.id, everything())
#   
#   x %>% pivot_longer(., !matches("l.set.id"), names_to = "id.draw",
#                      values_to = "sim.response")
# }

# simulated_response_comparison_plots <- function(data, sim.resp, family, path.str) {
#   
#   ## get all variables in data, except l.set.id
#   var.names <- colnames(data)
#   var.names <- var.names[!var.names %in% c("l.set.id", "response", "year", "lat", "lon",
#                                            "lat.proj", "lon.proj")]
#   var.names <- var.names[!grepl("^rsd", var.names)]
#   
#   ## Convert variables to factors where appropriate
#   data <- data %>%
#     mutate(mm = factor(month, levels = 1:12),
#            oni.class = factor(oni.class, levels = c("el.nino", "neutral", "la.nina")))
#   
#   # ## For hurdle / delta models
#   # if(length(family) > 1 & grepl("binomial", family[1])) {
#   # 
#   #     lapply(var.names, function(x) {
#   #         plt <- simulated_data_covariate_plot_hurdle(data, sim.resp, sym(x), 10)
#   #         ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
#   #                path = path.str, width = 8, height = 12, units = 'in')
#   #     })
#   # }
#   
#   ## For families modelling only presence
#   if(length(family) == 1 & grepl("^binomial", family[1])) {
#     
#     lapply(var.names, function(x) {
#       plt <- simulated_data_covariate_plot_presence(data, sim.resp, sym(x), 10)
#       ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
#              path = path.str, width = 8, height = 8, units = 'in')
#     })
#   }
#   
#   ## For single family models
#   if(length(family) == 1 & !grepl("^binomial", family[1])) {
#     
#     if(grepl("^nbinom|^poisson|^tweedie", family[1])) {
#       lapply(var.names, function(x) {
#         plt <- simulated_data_covariate_plot_both(data, sim.resp, sym(x), 10)
#         ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
#                path = path.str, width = 8, height = 8, units = 'in')
#       })
#       
#     } else {
#       
#       lapply(var.names, function(x) {
#         plt <- simulated_data_covariate_plot_positives(data, sim.resp, sym(x), 10)
#         # ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
#         #        path = path.str, width = 8, height = 8, units = 'in')
#         ggsave(paste0('a_simulated_response_v_', x, '.png'), plot = plt, device = 'png',
#                path = path.str, width = 8, height = 8, units = 'in')
#       })
#     }
#   }
#   
#   return('Generated plots of simulated responses against observations')
# }

## reformat matrix of simulated responses to dataframe
reformat_simulated_responses <- function(x, data, n.draws) {
  dimnames(x)[[2]] <- 1:n.draws
  x <- as.data.frame(x) %>%
    rowid_to_column(var = 'id') |>
    #mutate(., id = data$id) %>%
    dplyr::select(id, everything())
  
  x %>% pivot_longer(., !matches("id"), names_to = "id.draw",
                     values_to = "sim.response")
}

# reformat_simulated_responses <- function(x, data, n.draws) {
#   dimnames(x)[[2]] <- 1:n.draws
#   x <- as.data.frame(x) %>%
#     mutate(., l.set.id = data$l.set.id) %>%
#     dplyr::select(., l.set.id, everything())
#   
#   x %>% pivot_longer(., !matches("l.set.id"), names_to = "id.draw",
#                      values_to = "sim.response")
# }

## get simulated responses
##  - for delta / hurdle models, returns presence and positives separately
get_simulations <- function(object, n.draws, ...) {
  
  family <- object$family$family
  
  # if(object$family$delta) {
  #   sim.i <- simulate(object, model = 1, nsim = n.draws)
  #   sim.i <- reformat_simulated_responses(sim.i, object$data, n.draws)
  #   sim.i <- sim.i %>% rename(., sim.delta = sim.response)
  #   
  #   sim.ii <- simulate(object, model = 2, nsim = n.draws)
  #   sim.ii <- reformat_simulated_responses(sim.ii, object$data, n.draws)
  #   sim.ii <- sim.ii %>% rename(., sim.positives = sim.response)
  #   
  #   sim <- sim.i %>% left_join(., sim.ii, by = c("l.set.id", "id.draw"), relationship = "one-to-one")
  # }
  # 
  # if(length(family) == 1 & grepl("^binomial", family[1])) {
  #   sim <- simulate(object, nsim = n.draws)
  #   sim <- reformat_simulated_responses(sim, object$data, n.draws)
  #   sim <- sim %>% rename(., sim.delta = sim.response)
  # }
  
  #if(length(family) == 1 & !grepl("^binomial", family[1])) {
  sim <- simulate(object, nsim = n.draws)
  sim <- reformat_simulated_responses(sim, object$data, n.draws)
  sim <- sim %>% dplyr::rename(., sim.positives = sim.response)
  #}
  
  stopifnot(nrow(sim) == n.draws * nrow(object$data))
  sim
}
# get_simulations <- function(object, n.draws, ...) {
#   
#   family <- object$family$family
#   
#   if(object$family$delta) {
#     sim.i <- simulate(object, model = 1, nsim = n.draws)
#     sim.i <- reformat_simulated_responses(sim.i, object$data, n.draws)
#     sim.i <- sim.i %>% rename(., sim.delta = sim.response)
#     
#     sim.ii <- simulate(object, model = 2, nsim = n.draws)
#     sim.ii <- reformat_simulated_responses(sim.ii, object$data, n.draws)
#     sim.ii <- sim.ii %>% rename(., sim.positives = sim.response)
#     
#     sim <- sim.i %>% left_join(., sim.ii, by = c("l.set.id", "id.draw"), relationship = "one-to-one")
#   }
#   
#   if(length(family) == 1 & grepl("^binomial", family[1])) {
#     sim <- simulate(object, nsim = n.draws)
#     sim <- reformat_simulated_responses(sim, object$data, n.draws)
#     sim <- sim %>% rename(., sim.delta = sim.response)
#   }
#   
#   if(length(family) == 1 & !grepl("^binomial", family[1])) {
#     sim <- simulate(object, nsim = n.draws)
#     sim <- reformat_simulated_responses(sim, object$data, n.draws)
#     sim <- sim %>% dplyr::rename(., sim.positives = sim.response)
#   }
#   
#   stopifnot(nrow(sim) == n.draws * nrow(object$data))
#   sim
# }


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Spatial residual diagnostics

spatial_qrsd_diagnostics <- function(qrsd.data, rsd.var, out.path, prefix = "") {
  ## Add binned years
  qrsd.data <- qrsd.data %>% mutate(., yy.bin = as.numeric(as.character(yy))) %>%
    mutate(., yy.bin = 5 * floor(yy.bin / 5))
  qrsd.data$yy.bin[qrsd.data$yy.bin %in% 2005] <- 2000
  
  ## if necessary, cast mm back to numeric
  if(is.factor(qrsd.data$mm) | is.character(qrsd.data$mm)) {
    qrsd.data <- qrsd.data %>% mutate(., mm = as.numeric(as.character(mm)))
  }
  
  ## Convert variable classes
  qrsd.data <- qrsd.data %>%
    mutate(., qtr = ceiling(month / 3), mm = factor(month, levels = 1:12),
           qtr = factor(qtr, levels = 1:4),
           oni.class = factor(oni.class, levels = c("el.nino", "neutral", "la.nina")),
           yy.bin = factor(yy.bin))#,
  #set.type = factor(set.type, levels = c('aFAD', 'dFAD', 'log', 'FS')))
  
  ## Filter for finite residuals
  qrsd.data <- qrsd.data %>% filter(., is.finite({{ rsd.var }}))
  qrsd.data <- qrsd.data %>% mutate(., response = {{ rsd.var }})
  
  ## Fit spatial surface to residuals
  mod.rsd.ss <- bam(response ~ te(lon, lat, bs = "cs"),
                    family = gaussian, data = qrsd.data, gamma = 1.4)
  
  png(file.path(out.path, paste0(prefix, "c_qrsd_spatial.png")),
      width = 6, height = 6, units = "in", res = 72)
  plot(mod.rsd.ss, scheme = 2, hcolors = hcl.colors(30, palette = "viridis"))
  dev.off()
  
  ## Fit quarterly spatial surface to residuals
  mod.rsd.ss.qtr <- bam(response ~ qtr + te(lon, lat, bs = "cs") + ti(lon, lat, by = qtr, bs = "cs"),
                        family = gaussian, data = qrsd.data, gamma = 1.4)
  
  png(file.path(out.path, paste0(prefix, "c_qrsd_spatial_by_qtr.png")),
      width = 8, height = 8, units = "in", res = 72)
  par(mfrow = c(2, 2))
  plot(mod.rsd.ss.qtr, scheme = 2, select = 2, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.qtr, scheme = 2, select = 3, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.qtr, scheme = 2, select = 4, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.qtr, scheme = 2, select = 5, hcolors = hcl.colors(30, palette = "viridis"))
  dev.off()
  
  ## Fit ONI-class specific spatial surface to residuals
  mod.rsd.ss.oni <- bam(response ~ oni.class + te(lon, lat, bs = "cs") + ti(lon, lat, by = oni.class, bs = "cs"),
                        family = gaussian, data = qrsd.data, gamma = 1.4)
  
  png(file.path(out.path, paste0(prefix, "c_qrsd_spatial_by_ONI.png")),
      width = 6, height = 12, units = "in", res = 72)
  par(mfrow = c(3, 1))
  plot(mod.rsd.ss.oni, scheme = 2, select = 2, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.oni, scheme = 2, select = 3, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.oni, scheme = 2, select = 4, hcolors = hcl.colors(30, palette = "viridis"))
  dev.off()
  
  ## Fit temporal spatial surfaces smooth to residuals
  mod.rsd.ss.yy <- bam(response ~ yy.bin + te(lon, lat, bs = "cs") + ti(lon, lat, by = yy.bin, bs = "cs"),
                       family = gaussian, data = qrsd.data, gamma = 1.4)
  
  png(file.path(out.path, paste0(prefix, "c_qrsd_spatial_by_yy_bin.png")),
      width = 8, height = 8, units = "in", res = 72)
  par(mfrow = c(2, 2))
  plot(mod.rsd.ss.yy, scheme = 2, select = 2, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.yy, scheme = 2, select = 3, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.yy, scheme = 2, select = 4, hcolors = hcl.colors(30, palette = "viridis"))
  plot(mod.rsd.ss.yy, scheme = 2, select = 5, hcolors = hcl.colors(30, palette = "viridis"))
  dev.off()
  
  ## Fit set-type specific spatial surfaces to residuals
  # mod.rsd.ss.type <- bam(response ~ qtr + te(lon, lat, bs = "cs") + ti(lon, lat, by = set.type, bs = "cs"),
  #                       family = gaussian, data = qrsd.data, gamma = 1.4)
  # 
  # png(file.path(out.path, paste0(prefix, "c_qrsd_spatial_by_set-type.png")),
  #     width = 8, height = 8, units = "in", res = 72)
  # par(mfrow = c(2, 2))
  # plot(mod.rsd.ss.type, scheme = 2, select = 2, hcolors = hcl.colors(30, palette = "viridis"))
  # plot(mod.rsd.ss.type, scheme = 2, select = 3, hcolors = hcl.colors(30, palette = "viridis"))
  # plot(mod.rsd.ss.type, scheme = 2, select = 4, hcolors = hcl.colors(30, palette = "viridis"))
  # plot(mod.rsd.ss.type, scheme = 2, select = 5, hcolors = hcl.colors(30, palette = "viridis"))
  # dev.off()
  
  return('Finished spatial residual diagnostics')
}

generate_spatial_qrsd_diagnostics <- function(data, path.str) {
  
  ## Round locations to 5 degree cells
  data <- data %>% mutate(., across(c(lon, lat), ~ 2.5 + 5 * floor(.x / 5)))
  
  ## Add simplified set-types
  #data <- data %>% mutate(., set.type = assign_set_type(association))
  
  if("rsd.delta" %in% colnames(data)) {
    data %>% filter(., is.finite(rsd.delta)) %>%
      spatial_qrsd_diagnostics(., rsd.delta, path.str, prefix = "delta_")
  }
  
  if("rsd.pos" %in% colnames(data)) {
    data %>% filter(., is.finite(rsd.pos)) %>%
      spatial_qrsd_diagnostics(., rsd.pos, path.str, prefix = "positives_")
  }
  
  return('Generated plots of residuals against variables')
  
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Maps of spatio-temporal predictions

## Make dataframe used to plot fitted spatial-temporal effects
make_spatial_fx_data <- function(object, loc.dat) {
  dat <- object$data
  
  ## Get time variable
  t.var <- object$time
  
  ## Get reference levels for variables
  ref.levels <- make_ref_levels_dfr()
  stopifnot(t.var %in% c(colnames(ref.levels), "_sdmTMB_time"))
  
  ## Get common columns between mod.data and ref.levels
  common_cols <- intersect(colnames(ref.levels), colnames(dat))
  
  ## Factorize categorical variables as required
  for(j in common_cols) {
    if (is.factor(mod.data[[j]])) {
      ref.levels[[j]] <- factor(ref.levels[[j]], levels = levels(dat[[j]]))
    }
  }
  
  ## factorise categorical variables as required
  # for(j in colnames(ref.levels)) {
  #     if(is.factor(dat[, j])) ref.levels[, j] <- factor(ref.levels[, j], levels = levels(dat[, j]))
  # }
  
  ## Get unique locations to predict on
  ## - use cells with reported effort defined in rep.sf.points object
  #loc.dat <- rep.sf.points %>% st_drop_geometry(.)
  loc.dat <- loc.dat %>% distinct(.)
  loc.dat <- loc.dat %>% mutate(., across(c(lon, lat), ~ 2.5 + 5 * floor(.x / 5)))
  
  ## Get unique values of time variable
  ## and duplicate locations for each value in time variable
  t.dat <- dat %>% dplyr::select(., matches(t.var)) %>% distinct(.)
  t.dat <- t.dat %>% mutate(., across(matches(t.var), .names = "time_ndx"))
  loc.dat <- expand_grid(loc.dat, t.dat)
  
  if(t.var == "_sdmTMB_time") {
    loc.dat$time_ndx <- "invariant"
  }
  
  drop.ndx <- paste0(c(t.var, colnames(loc.dat)), collapse = "|")
  ref.levels %>%
    dplyr::select(., ! matches(drop.ndx)) %>%
    expand_grid(loc.dat, .)
}

# generate_spatial_predictions(mod, 30, file.path(mod.fld),loc.dat = xys)
## Generate and save spatial-temporal effects
generate_spatial_predictions <- function(object, n, fig.path, loc.dat) {
  spatial.fx.data <- make_spatial_fx_data(object, loc.dat)
  
  ## predict at linear predictor scale (link of positives distribution)
  ##  - note that in case of truncated distributions, linear predictor may not correspond to mean
  ##  - see https://github.com/pbs-assess/sdmTMB/issues/350
  
  # Updated to convert back from log NH!!!
  linkinv.pred <- object$family$linkin # extract transformation from model
  
  st_time <- proc.time()
  fx.spatial.prd.sim <- predict(object, newdata = spatial.fx.data, nsim = n, offset = log(spatial.fx.data$hooks))
  fx.spatial.prd.sim <- linkinv.pred(fx.spatial.prd.sim) # transform back - units = catch/1000hks
  proc.time() - st_time
  
  spatial.fx.data <- spatial.fx.data %>%
    mutate(., est = apply(fx.spatial.prd.sim, 1, mean), #function(x) mean(exp(x))
           est_se = apply(fx.spatial.prd.sim, 1, sd)) #|> #function(x) sd(exp(x)))
  
  ## Transform from linear predictor to response
  #if(object$family$delta) spatial.fx.data <- spatial.fx.data %>% mutate(., est = object$family[[2]]$linkinv(est))
  #if(!object$family$delta) spatial.fx.data <- spatial.fx.data %>% mutate(., est = object$family$linkinv(est))
  
  n.panels <- spatial.fx.data %>% dplyr::select(., time_ndx) %>% distinct(.) %>% nrow()
  
  # Catch per 1000 hooks
  plt.map.fit <- spatial.fx.data %>%
    ggplot() +
    geom_tile(aes(lon, lat, fill = est), colour = NA) +
    #scale_fill_distiller(palette = 'Spectral') +
    scale_fill_viridis_c("Linear\npredictor") +
    facet_wrap(vars(time_ndx), ncol = 1) +
    theme_bw() + xlab("Longitude") + ylab("Latitude")
  ggsave("spatial_predictions.png", plt.map.fit, path = fig.path,
         width = 8, height = 3 * n.panels, units = "in")
  
  plt.map.se <- spatial.fx.data %>%
    ggplot(.) +
    geom_tile(aes(lon, lat, fill = est_se), colour = NA) +
    scale_fill_viridis_c("SE of\nlpred") +
    facet_wrap(vars(time_ndx), ncol = 1) +
    theme_bw() + xlab("Longitude") + ylab("Latitude")
  ggsave("spatial_predictions_SE.png", plt.map.se, path = fig.path,
         width = 8, height = 3 * n.panels, units = "in")
  return(spatial.fx.data)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Effects plots

# make_ref_levels_dfr <- function() {
#   data.frame(
#     yy = "2015", mm = "5", month = 5L, qtr = "2", #set.type = "FS", 
#     flag.id = "US",
#     oni.class = "neutral", #depth.20 = 180, 
#     lon.proj = 0, lat.proj = 0, hbf = 21, sst100 = 24
#   )
# }
#lapply(vv.pred, make_main_fx_data, data, n)
make_main_fx_data <- function(x, mod.data, n) {
  stopifnot(x %in% colnames(mod.data))
  
  ## Get reference levels for variables
  # cols_to_keep <- intersect(names(make_ref_levels_dfr()), names(mod.data))
  # ref.levels <- make_ref_levels_dfr() |> 
  #   dplyr::select(all_of(cols_to_keep))
  
  ## Get reference levels for variables
  ref.levels <- make_ref_levels_dfr2(mod.data)
  stopifnot(x %in% colnames(ref.levels))
  
  ## Get common columns between mod.data and ref.levels
  common_cols <- intersect(colnames(ref.levels), colnames(mod.data))
  
  ## Factorize categorical variables as required
  for(j in common_cols) {
    if (is.factor(mod.data[[j]])) {
      ref.levels[[j]] <- factor(ref.levels[[j]], levels = levels(mod.data[[j]]))
    }
  }
  
  # ## factorise categorical variables as required
  # for(j in colnames(ref.levels)) {
  #   if(is.factor(mod.data[, j])) ref.levels[, j] <- factor(ref.levels[, j], levels = levels(mod.data[, j]))
  # }
  
  #  ref.levels <- make_ref_levels_dfr() #|>
  # #   dplyr::select(any_of(names(mod.data)))
  # stopifnot(x %in% colnames(ref.levels))
  # 
  # # factorise categorical variables as required
  # for(j in colnames(ref.levels)) {
  #     if(is.factor(mod.data[, j])) ref.levels[, j] <- factor(ref.levels[, j], levels = levels(mod.data[, j]))
  # }
  
  ## Get values for x variable to predict on
  mod.data <- ungroup(mod.data)
  if(is.numeric(mod.data[, x])) {
    x.data <- data.frame(x = seq(from = min(mod.data[, x], na.rm = TRUE), to = max(mod.data[, x], na.rm = TRUE), length.out = n))
    colnames(x.data) <- x
  } else {
    x.data <- mod.data %>% dplyr::select(., matches(x)) %>% distinct(.)
  }
  
  ## Add variable to identify what term varies
  x.data <- x.data %>% mutate(., fx.term = x) %>% dplyr::select(., fx.term, everything())
  
  ref.levels %>%
    ungroup() |>
    dplyr::select(!matches(x)) %>%
    expand_grid(x.data, .)
}

id_terms <- function(vv, ff) {
  ff <- as.character(ff)
  ff <- ff[length(ff)]
  
  ndx <- sapply(vv, grepl, x = ff)
  vv[ndx]
}

fx_plot_cat <- function(plt.dat, xx) {
  xx.string <- rlang::as_string({{ xx }})
  
  plt.dat %>% ggplot() +
    geom_pointrange(aes(x = {{ xx }}, y = est, ymin = lx, ymax = ux)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(xx.string) + ylab("Response") +
    #coord_cartesian(ylim = c(0, NA)) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
}

fx_plot_cnt <- function(plt.dat, xx) {
  xx.string <- rlang::as_string({{ xx }})
  
  plt.dat %>% ggplot() +
    geom_ribbon(aes(x = {{ xx }}, ymin = lx, ymax = ux), col = "grey", fill = "grey") +
    geom_line(aes(x = {{ xx }}, y = est)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    xlab(xx.string) + ylab("Response") #+
    #coord_cartesian(ylim = c(0, NA))
}

## Main effects for hurdle models


main_fx_plots_hurdle <- function(object, out.path, n) {
  
  data <- object$data
  form <- object$formula
  
  ## get potential effects from data
  vv <- colnames(data)
  
  ## get inverse-link function for delta component, and formula terms
  linkinv.delta <- object$family[[1]]$linkin
  vv.delta <- id_terms(vv, form[[1]])
  
  ## get inverse-link function for positives, and formula terms
  linkinv.pos <- object$family[[2]]$linkinv
  vv.pos <- id_terms(vv, form[[2]])
  
  ## remove partial matches to formula terms
  ref.levels <- make_ref_levels_dfr()
  vv.delta <- vv.delta[vv.delta %in% colnames(ref.levels)]
  vv.pos <- vv.pos[vv.pos %in% colnames(ref.levels)]
  
  ## get unique model terms
  vv.pred <- c(vv.delta, vv.pos)
  vv.pred <- unique(vv.pred)
  
  ## generate predictions for both delta and positives component
  fx.data.pred <- lapply(vv.pred, make_main_fx_data, data, n)
  fx.data.pred <- bind_rows(fx.data.pred) %>% data.frame(.)
  
  ## get predicted means
  fx.prd <- predict(object, newdata = fx.data.pred, re_form = NULL)
  fx.prd.delta.se <- predict(object, newdata = fx.data.pred, re_form = NULL, model = 1, nsim = 30)
  fx.prd.pos.se <- predict(object, newdata = fx.data.pred, re_form = NULL, model = 2, nsim = 30)
  
  ## get approximate se's
  fx.prd.delta.se <- apply(fx.prd.delta.se, 1, sd)
  fx.prd.pos.se <- apply(fx.prd.pos.se, 1, sd)
  
  ## transform estimates and intervals back to response
  fx.prd.delta <- fx.prd %>% rename(., est = est1) %>% mutate(., est_se = as.numeric(fx.prd.delta.se))
  fx.prd.delta <- fx.prd.delta %>% mutate(., lx = est - est_se, ux = est + est_se)
  fx.prd.delta <- fx.prd.delta %>% mutate(., across(c(est, lx, ux), linkinv.delta))
  
  fx.prd.pos <- fx.prd %>% rename(., est = est2) %>% mutate(., est_se = as.numeric(fx.prd.pos.se))
  fx.prd.pos <- fx.prd.pos %>% mutate(., lx = est - est_se, ux = est + est_se)
  fx.prd.pos <- fx.prd.pos %>% mutate(., across(c(est, lx, ux), linkinv.pos))
  
  ## effects plots for delta component
  fx.plt.list.delta <- lapply(vv.delta, function(x, prd.dat) {
    prd.dat <- prd.dat %>% filter(., fx.term %in% x)
    
    if(is.character(prd.dat[, x]) | is.factor(prd.dat[, x])) {
      plt <- prd.dat %>% fx_plot_cat(., sym(x))
    } else {
      plt <- prd.dat %>% fx_plot_cnt(., sym(x))
    }
    #ggsave(paste0('fx_delta_', x, '.png'), plot = plt, device = 'png',
    #       path = out.path, width = 6, height = 4, units = 'in')
    return(plt)
  }, prd.dat = fx.prd.delta)
  
  nrows <- ceiling(length(vv.delta)/2)
  fx.plt.delta <- ggpubr::ggarrange(plotlist = fx.plt.list.delta, ncol = 2, nrow = nrows)
  
  ggsave('fx_delta_combined.pdf', plot = fx.plt.delta, bg = "white",
         path = out.path, width = 10, height = nrows * 4, units = 'in')
  
  
  ## effects plots for positives component
  fx.plt.list.pos <- lapply(vv.pos, function(x, prd.dat) {
    prd.dat <- prd.dat %>% filter(., fx.term %in% x)
    
    if(is.character(prd.dat[, x]) | is.factor(prd.dat[, x])) {
      plt <- prd.dat %>% fx_plot_cat(., sym(x))
    } else {
      plt <- prd.dat %>% fx_plot_cnt(., sym(x))
    }
    #ggsave(paste0('fx_positives_', x, '.png'), plot = plt, device = 'png',
    #       path = out.path, width = 6, height = 4, units = 'in')
    return(plt)
  }, prd.dat = fx.prd.pos)
  
  nrows <- ceiling(length(vv.pos)/2)
  fx.plt.pos <- ggpubr::ggarrange(plotlist = fx.plt.list.pos, ncol = 2, nrow = nrows)
  
  ggsave('fx_positives_combined.pdf', plot = fx.plt.pos, bg = "white",
         path = out.path, width = 10, height = nrows * 4, units = 'in')
  
  return(list(delta = fx.prd.delta, positives = fx.prd.pos))
}

## Main effects for non-hurdle models
#fx.prd <- main_fx_plots(mod, file.path(mod.fld), 25)

main_fx_plots <- function(object, out.path, n) {
  
  data <- object$data
  form <- object$formula
  
  ## get potential effects from data
  vv <- colnames(data)
  
  ## generate predictions for positives component
  linkinv.pred <- object$family$linkin
  vv.pred <- id_terms(vv, form[[1]])
  
  #ff <- as.character(ff)
  #ff <- ff[length(ff)]
  
  #ndx <- sapply(vv, grepl, x = ff)
  #vv[ndx]
  
  ## remove partial matches to formula terms
  # NH: How to make this flexible to show changing vars in model?
  ref.levels <- make_ref_levels_dfr2(mod$data)
  vv.pred <- vv.pred[vv.pred %in% colnames(ref.levels)]
  
  fx.data.pred <- lapply(vv.pred, make_main_fx_data, data, n)
  fx.data.pred <- bind_rows(fx.data.pred) %>% data.frame(.) #|>
    #mutate(hooks = 1000)
  
  linkinv.pred <- object$family$linkin
  
  ## get predicted means
  # NH: check - need to add cpue weights herer?
  fx.prd <- predict(object, newdata = fx.data.pred, weights = fx.data.pred$cpue) #|> #, offset = log(fx.data.pred$hooks)
    #mutate(est = linkinv.pred(est))
  
  ## get approximate se's
  fx.prd.se <- predict(object, newdata = fx.data.pred, nsim = 30, weights = fx.data.pred$cpue) #, offset = log(fx.data.pred$hooks)
  #fx.prd.se <- linkinv.pred(fx.prd.se) # NH - check this need to apply for climate indicaotrs work?
  fx.prd.se <- apply(fx.prd.se, 1, sd)
  fx.prd <- fx.prd %>% mutate(., est_se = as.numeric(fx.prd.se)) 
  
  ## transform estimates and intervals back to response
  # NH - need this?
  fx.prd <- fx.prd %>% mutate(., lx = est - est_se, ux = est + est_se)
 # fx.prd <- fx.prd %>% mutate(., across(c(est, lx, ux), linkinv.pred))
  
  # fx.prd %>% 
  #   dplyr::filter(fx.term == 'hk.shape') |>
  #   ggplot() +
  #   aes(hk.shape, est, col = as.factor(wiretrace)) +
  #   geom_boxplot()
  
  # fx.prd %>% 
  #   dplyr::filter(fx.term == 'wiretrace') |>
  #   ggplot() +
  #   aes(wiretrace, est, col = as.factor(hk.shape)) +
  #   geom_boxplot()
  
  fx.plt.list <- lapply(vv.pred, function(x, prd.dat) {
    prd.dat <- prd.dat %>% filter(., fx.term %in% x)
    
    if(is.character(prd.dat[, x]) | is.factor(prd.dat[, x])) {
      plt <- prd.dat %>% fx_plot_cat(., sym(x))
    } else {
      plt <- prd.dat %>% fx_plot_cnt(., sym(x))
    }
    #ggsave(paste0('fx_', x, '.png'), plot = plt, device = 'png',
    #       path = out.path, width = 6, height = 4, units = 'in')
    return(plt)
  }, prd.dat = fx.prd)
  
  nrows <- ceiling(length(vv.pred)/2)
  fx.plt <- ggpubr::ggarrange(plotlist = fx.plt.list, ncol = 2, nrow = nrows)
  
  ggsave('fx_combined.pdf', plot = fx.plt, bg = "white",
         path = out.path, width = 10, height = nrows * 4, units = 'in')
  
  return(fx.prd)
}

main_fx_plots_bam <- function(object, out.path, n) {
  
  data <- object$model #|>
    #mutate(yy = as.factor(yy))
  form <- object$formula
  
  ## get potential effects from data
  vv <- colnames(data)
  
  ## generate predictions for positives component
  linkinv.pred <- object$family$linkin
  #vv.pred <- id_terms(vv, form[[3]])
  #vv.pred <- setdiff(as.vector(vv), as.vector(paste0(form[[2]])))
  vv.pred <- setdiff(all.vars(formula(mod)), as.vector(paste0(form[[2]])))
  #ff <- as.character(ff)
  #ff <- ff[length(ff)]
  
  #ndx <- sapply(vv, grepl, x = ff)
  #vv[ndx]
  
  ## remove partial matches to formula terms
  # NH: How to make this flexible to show changing vars in model?
  ref.levels <- make_ref_levels_dfr2(data)
  vv.pred <- vv.pred[vv.pred %in% colnames(ref.levels)]
  
  fx.data.pred <- lapply(vv.pred, make_main_fx_data, data, n)
  fx.data.pred <- bind_rows(fx.data.pred) %>% data.frame(.) #|>
   #mutate(hooks = 1000)
  
  linkinv <- mod$family$linkinv
  linkfun <- mod$family$linkfun
  link <- mod$family$link
  
  ## get predicted means
  # NH: check - old and new method
  fx.prd <- data.frame(fx.data.pred)
  
  # Traditional approach - SE around mean
  pred_list <- predict(object, newdata = fx.data.pred, type = "link", se =T)

  # fx.prd1 <- fx.prd %>%
  #   mutate(est = linkinv(pred_list$fit),
  #     est_se = if (link == "log") {est * pred_list$se.fit} else {pred_list$se.fit},
  #     lx = linkinv(pred_list$fit - 1.96 * pred_list$se.fit),
  #     ux = linkinv(pred_list$fit + 1.96 * pred_list$se.fit))
  #     # est = pred_list$fit,
  #     # se = pred_list$se.fit,
  #     # lx = est - 1.96 * se,
  #     # ux = est + 1.96 * se)
  
  fx.prd1 <- fx.prd %>%
    mutate(fit_link = pred_list$fit,          
      se_link = pred_list$se.fit,       
      est = linkinv(fit_link),
      est_se = if (link == "log") {est * se_link
      } else {se_link},
      lx = linkinv(fit_link - 1.96 * se_link),
      ux = linkinv(fit_link + 1.96 * se_link))


  ## sim data to show posterior uncertainty reflect overall uncertainty
  sim_draws <- gratia::predicted_samples(object, data = fx.data.pred, n = 100)
  # sim_draws <- sim_draws %>%
  #   mutate(response = pmax(exp(.response), .Machine$double.eps)) %>%
  #   group_by(.row) %>%
  #   summarise(
  #     est = mean(response),
  #     est_se = sd(response),
  #     lx = quantile(response, probs = 0.025),
  #     ux = quantile(response, probs = 0.975)
  #   )
  #linkinv.pred <- object$family$linkin
  
  sim_draws <- sim_draws %>%
    group_by(.row) %>%
    summarise(est = mean(.response),
      est_se = sd(.response),
      lx = est - est_se,
      ux = est + est_se) #|>
  #mutate(across(c(est, lx, ux), linkinv.pred))
  
  fx.prd2 <- bind_cols(fx.prd, sim_draws)
  
  fx.plt.list1 <- lapply(vv.pred, function(x, prd.dat) {
    prd.dat <- prd.dat %>% filter(., fx.term %in% x)
    
    if(is.character(prd.dat[, x]) | is.factor(prd.dat[, x])) {
      plt <- prd.dat %>% fx_plot_cat(., sym(x))
    } else {
      plt <- prd.dat %>% fx_plot_cnt(., sym(x))
    }
    #ggsave(paste0('fx_', x, '.png'), plot = plt, device = 'png',
    #       path = out.path, width = 6, height = 4, units = 'in')
    return(plt)
  }, prd.dat = fx.prd1)
  
  nrows <- ceiling(length(vv.pred)/2)
  fx.plt1 <- ggpubr::ggarrange(plotlist = fx.plt.list1, ncol = 2, nrow = nrows)
  
  ggsave('fx_combined1.pdf', plot = fx.plt1, bg = "white",
         path = out.path, width = 10, height = nrows * 4, units = 'in')
  
  fx.plt.list2 <- lapply(vv.pred, function(x, prd.dat) {
    prd.dat <- prd.dat %>% filter(., fx.term %in% x)
    
    if(is.character(prd.dat[, x]) | is.factor(prd.dat[, x])) {
      plt <- prd.dat %>% fx_plot_cat(., sym(x))
    } else {
      plt <- prd.dat %>% fx_plot_cnt(., sym(x))
    }
    #ggsave(paste0('fx_', x, '.png'), plot = plt, device = 'png',
    #       path = out.path, width = 6, height = 4, units = 'in')
    return(plt)
  }, prd.dat = fx.prd2)
  
  nrows <- ceiling(length(vv.pred)/2)
  fx.plt2 <- ggpubr::ggarrange(plotlist = fx.plt.list2, ncol = 2, nrow = nrows)
  
  ggsave('fx_combined2.pdf', plot = fx.plt2, bg = "white",
         path = out.path, width = 10, height = nrows * 4, units = 'in')
  
  
  return(list(fx.prd1, fx.prd2))
}

main_fx_plots_bam_interact <- function(object, out.path, n = 100) {
  
  # Extract relvent model params
  mod <- object
  vars <- all.vars(formula(mod))
  mod.data <- mod$model
  lond_vals <- seq(min(mod.data$lond), max(mod.data$lond), by = 1)
  years <- sort(unique(mod.data$yy))
  
  # expand lon/yy and append fixed other variables
  ref.levels <- make_ref_levels_dfr2(mod$model) |>
    dplyr::select(-yy, -lond)
  
  # make dataframe to predict to
  ref.levels <- expand.grid(
    lond = lond_vals,yy = years) |>
    bind_cols(ref.levels)
  
  # predict with model and pred.data with SEs
  preds <- predict(mod, newdata = ref.levels, type = "response", se.fit = T)
  
  # Appends preds to df
  pred.data <- ref.levels %>%
    mutate(fit = preds$fit, se = preds$se.fit,
           lx = fit - 1.96 * se, ux = fit + 1.96 * se)
  
  # Plot all years by colour
  p <- 
    pred.data |> 
    ggplot() +
    aes(x = lond, y = fit, color = as.numeric(as.character(yy)), 
        group = as.numeric(as.character(yy))) +
    geom_line() +
    scale_color_viridis_c() +
    labs(x = "Longitude",y = "Smooth (partial effect)",color = "Year") +
    gg.theme
  ggsave(p, file= paste0(path = out.path,'/lond_smooth_x_yr_plot.png'), 
         width = 6, height = 6, units = 'in', dpi = 100)
  
  # Facet by year
  p <- 
    pred.data |>
    ggplot() +
    aes(x = lond, y = fit, ymin = lx, ymax = ux) +
    geom_ribbon(alpha = 0.2, color = NA) +
    geom_line() +
    facet_wrap(~yy, scales = 'free_y') +
    labs(x = "Longitude",y = "Smooth (partial effect)") +
    gg.theme
  ggsave(p, file = paste0(path = out.path,'/lond_smooth_x_yr_facet_plot.png'), 
         width = 6, height = 6, units = 'in',  dpi = 100)
  
  # Simulate to get posterior uncertainty
  sim_draws <- gratia::predicted_samples(mod, data = ref.levels, n = n) 
  
  sim_summary <- sim_draws |>
    group_by(.row) %>%
    summarise(est_sim = mean(.response),est_se_sim = sd(.response),
              lx_sim = est_sim - est_se_sim, ux_sim = est_sim + est_se_sim)
  fx.prd.int <- bind_cols(pred.data, sim_summary)
  
  ref.levels <- ref.levels |> rowid_to_column(var = 'row_id')
  
  sim_with_data <- sim_draws %>%
    left_join(ref.levels %>% select(row_id, lond, yy), by = c(".row" = "row_id"))
  
  # Calculate weighted mean lond across sims and then year
  fx.prd.int2 <- sim_with_data %>%
    group_by(yy, .draw) %>%
    summarise(weighted_lond = sum(lond * .response) / sum(.response),
              .groups = "drop") |>
    group_by(yy) %>%
    summarise(est = mean(weighted_lond),
              lx = quantile(weighted_lond, 0.025), ux = quantile(weighted_lond, 0.975),
              .groups = "drop")
  
  hist_mn <- mean(fx.prd.int2$est[fx.prd.int2$yy %in% c(1995:2005)])
  
  p <- 
    fx.prd.int2 |>
    mutate(hist_bin = as.factor(ifelse(yy %in% c(1995:2005),1,0))) |>
    ggplot() +
    aes(x = as.numeric(as.character(yy)), y = est, col = hist_bin, group = 1) +
    geom_pointrange(aes(ymin = lx, ymax = ux)) +
    scale_color_manual(values = c("black", "red")) +
    geom_line() +
    geom_hline(yintercept = hist_mn, linetype = 'dashed') +
    labs(x = "Year",y = "Weighted Mean Longitude") +
    gg.theme +
    theme(legend.position = 'none')
  ggsave(p, file = paste0(path = out.path,'/yreffect_lond_plot_interaction.png'), 
         width = 6, height = 6, units = 'in',  dpi = 100)
  
  return(list(fx.prd.int = fx.prd.int, fx.prd.int2 = fx.prd.int2))
}


