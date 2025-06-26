# 
# influ_plots <- function(mod, var = 'yy') {
#   
#   # Assume `mod` is your fitted bam model, and you're analyzing influence of `focus_term`
#   response_name <- names(mod$model)[1]
#   focus_term <- var # e.g. "year"
#   
#   observed <- mod$model[[response_name]]
#   
#   # Check if the response is log-transformed
#   logged <- substr(response_name, 1, 4) == 'log('
#   
#   # Create a data frame to hold index results
#   focus_factor <- mod$model[[focus_term]]
#   focus_levels <- levels(factor(focus_factor))  # Ensure ordered factor
#   indices <- data.frame(level = focus_levels)
#   
#   # Compute unstandardised index
#   if (logged || all(observed > 0)) {
#     log_observed <- if (logged) observed else log(observed)
#     unstan <- aggregate(log_observed, list(level = focus_factor), mean)
#     colnames(unstan)[2] <- "unstan"
#     unstan$unstan <- exp(unstan$unstan - mean(unstan$unstan))  # Relative index
#   } else {
#     unstan <- aggregate(observed, list(level = focus_factor), mean)
#     colnames(unstan)[2] <- "unstan"
#     unstan$unstan <- unstan$unstan / mean(unstan$unstan)
#   }
#   
#   # Merge with index structure
#   indices <- merge(indices, unstan, by = "level", all.x = TRUE)
#   
#   # View resulting indices
#   #print(indices)
#   
#   # ---- Add standardised index ----
#   
#   # Extract coefficients and standard errors for the focus term
#   coefs <- coef(mod)
#   vcov_mat <- vcov(mod)
#   
#   # Identify coefficients associated with the focus term
#   term_pattern <- paste0("^", focus_term)
#   term_indices <- grep(term_pattern, names(coefs))
#   term_coefs <- coefs[term_indices]
#   
#   # Add base level coefficient (assumed 0 for factor contrasts)
#   coeffs <- c(0, term_coefs)
#   
#   # Compute standard errors using Francis method
#   V <- vcov_mat[term_indices, term_indices, drop = FALSE]
#   n <- length(term_coefs) + 1
#   Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
#   Q[-1, ] <- Q[-1, ] + diag(n - 1)
#   V0 <- Q %*% V %*% t(Q)
#   ses <- sqrt(diag(V0))
#   
#   # Center coefficients
#   base <- mean(coeffs)
#   
#   # Add standardized index and CIs to indices
#   indices$stan <- exp(coeffs - base)
#   indices$stanLower <- exp(coeffs - base - 2 * ses)
#   indices$stanUpper <- exp(coeffs - base + 2 * ses)
#   
#   # ---- Create an empty model summary for next steps ----
#   summary_df <- NULL
#   
#   terms_vec <- attr(mod$terms, "term.labels")  # all model terms
#   
#   #indices <- data.frame(level=levels(mod$model[[focus_term]]))  # starting indices DF
#   
#   for (termCount in 0:length(terms_vec)) {
#     if (termCount > 0) {
#       term <- terms_vec[termCount]
#       
#       # Build new formula adding terms one by one
#       base_formula <- formula(mod)
#       
#       # Create RHS formula with intercept plus terms[1:termCount]
#       rhs_terms <- paste(terms_vec[1:termCount], collapse = " + ")
#       new_formula <- as.formula(paste(". ~", rhs_terms))
#       
#       # Update the bam model with the reduced formula
#       model_update <- update(mod, new_formula)
#       
#       # Calculate influence index for this model
#       # You need to define an effects() function or replicate it for bam
#       # For example, predicted mean by factor levels of the focus term:
#       # (Modify as per your influence index definition)
#       effect_index <- with(model_update$model, tapply(fitted(model_update), model_update[[focus_term]], mean))
#       
#       # Add this index as a new column
#       indices <- cbind(indices, effect_index)
#       
#       # Name the new column appropriately
#       colname <- if (termCount == 1) term else paste("+", term)
#       names(indices)[ncol(indices)] <- colname
#       
#     } else {
#       term <- "intercept"
#       
#       # Fit intercept-only model
#       model_update <- update(mod, . ~ 1)
#     }
#     
#     # Extract log-likelihood (note: mgcv::bam may not have logLik method; 
#     # use REML or GCV scores or an approximation if needed)
#     logLik_val <- tryCatch({
#       logLik(model_update)
#     }, error = function(e) NA)
#     
#     # Extract fitted values
#     fitted_vals <- fitted(model_update)
#     
#     # (You could store or print logLik_val, fitted_vals, or use them downstream)
#     
#     # For example:
#     # print(paste("Term count:", termCount, "LogLik:", logLik_val))
#   }
#   
# }

inds <- compute_influence_indices(mod, 'yy')

indices |>
  pivot_longer(-c(level, unstan, stan, stanLower, stanUpper), names_to = 'var', values_to = 'val') |>
  ggplot() +
  aes(level, val, col = var, group = var) +
  geom_line() +
  gg.theme

compute_influence_indices <- function(mod, focus_term) {
  response_name <- names(mod$model)[1]
  observed <- mod$model[[response_name]]
  
  # Check if response is log-transformed (simple check)
  logged <- substr(response_name, 1, 4) == 'log('
  
  # Prepare factor for focus term, ensure consistent levels
  focus_factor <- factor(mod$model[[focus_term]])
  focus_levels <- levels(focus_factor)
  indices <- data.frame(level = focus_levels)
  
  # Compute unstandardised index
  if (logged || all(observed > 0)) {
    log_observed <- if (logged) observed else log(observed)
    unstan <- aggregate(log_observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- exp(unstan$unstan - mean(unstan$unstan))  # Relative index
  } else {
    unstan <- aggregate(observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- unstan$unstan / mean(unstan$unstan)
  }
  
  indices <- merge(indices, unstan, by = "level", all.x = TRUE)
  
  # Add standardised index
  
  coefs <- coef(mod)
  vcov_mat <- vcov(mod)
  
  # Extract coefficients related to focus_term
  term_pattern <- paste0("^", focus_term)
  term_indices <- grep(term_pattern, names(coefs))
  term_coefs <- coefs[term_indices]
  
  # Base level coef = 0 for contrasts
  coeffs <- c(0, term_coefs)
  
  # Calculate standard errors (Francis method)
  V <- vcov_mat[term_indices, term_indices, drop = FALSE]
  n <- length(term_coefs) + 1
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1, ] <- Q[-1, ] + diag(n - 1)
  V0 <- Q %*% V %*% t(Q)
  ses <- sqrt(diag(V0))
  
  base <- mean(coeffs)
  
  indices$stan <- exp(coeffs - base)
  indices$stanLower <- exp(coeffs - base - 2 * ses)
  indices$stanUpper <- exp(coeffs - base + 2 * ses)
  
  # Prepare for incremental models
  terms_vec <- attr(mod$terms, "term.labels")
  
  # Make sure indices data frame uses factor levels for focus_term again
  #indices <- data.frame(level = focus_levels)
  
  for (termCount in 1:length(terms_vec)) {
    term <- terms_vec[termCount]
    rhs_terms <- paste(terms_vec[1:termCount], collapse = " + ")
    new_formula <- as.formula(paste(response_name, "~", rhs_terms))
    model_update <- update(mod, formula = new_formula, data = mod$model)
    
    updated_factor <- factor(model_update$model[[focus_term]], levels = focus_levels)
    effect_index <- tapply(fitted(model_update), updated_factor, mean)
    
    # Standardise effect index relative to geometric mean
    effect_index <- exp(log(effect_index) - mean(log(effect_index)))
    
    indices <- cbind(indices, effect_index)
    colname <- if (termCount == 1) term else paste("+", term)
    names(indices)[ncol(indices)] <- colname
    
    # Optionally capture logLik or other stats here
    logLik_val <- tryCatch(logLik(model_update), error = function(e) NA)
  }
  
  return(indices)
}


base_cols <- 5
step_cols <- names(indices)[(base_cols + 1):ncol(indices)]

# Convert to long format for ggplot
long_df <- indices %>%
  select(level, all_of(step_cols)) %>%
  pivot_longer(-level, names_to = "Model", values_to = "Index") %>%
  mutate(level = as.factor(level),
         Model = factor(Model, levels = step_cols))  # preserve order

# Plot using ggplot
ggplot(long_df, aes(x = level, y = Index, group = Model)) +
  geom_line(aes(linetype = Model, color = Model), size = 1) +
  geom_point(aes(color = Model), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Level", y = "Index", title = "Influence Step Plot") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank())

#inds |>
stepPlotBaseR(indices = inds, base_cols = 5, setpar = TRUE)

stepPlotBaseR <- function(indices, base_cols = 5, setpar = TRUE) {
  step_cols <- names(indices)[(base_cols + 1):ncol(indices)]
  n_steps <- length(step_cols)
  levels <- as.integer(as.character(indices$level))  # Convert levels to numeric for x-axis
  
  y_max <- max(indices[, step_cols], na.rm = TRUE)
  x_range <- range(levels)
  
  if (setpar) {
    par(mfrow = c(n_steps, 1), mar = c(0, 4, 0, 1), oma = c(4, 1, 1, 1))
  }
  
  for (i in seq_along(step_cols)) {
    current_col <- step_cols[i]
    current_index <- indices[[current_col]]
    
    plot(NA, xaxt = 'n', las = 1,
         ylab = "Index", xlim = x_range, ylim = c(0, y_max))
    
    # Draw grey lines for steps BEFORE the immediate previous one (if any)
    if (i > 2) {
      for (j in seq_len(i - 2)) {
        lines(levels, indices[[step_cols[j]]], col = "grey", lwd = 1.5)
      }
    }
    
    # Draw immediate previous step as dashed black line
    if (i > 1) {
      lines(levels, indices[[step_cols[i - 1]]], col = "black", lwd = 1.5, lty = 3)
    }
    
    # Draw current step with points and solid black line
    points(levels, current_index, type = "o", pch = 16, col = "black", cex = 1.25)
    
    legend("topleft", legend = current_col, bty = "n")
  }
  
  # Add x-axis labels only on bottom plot
  axis(1, at = levels, labels = levels)
}

influence_calc <- function(mod, focus, response) {
  # Extract model frame and data
  model_data <- mod$model
  observed <- model_data[[response]]
  
  if (any(is.na(observed))) {
    stop("Observed response contains NA values after conversion to numeric.")
  }
  
  if (inherits(observed, "Surv")) observed <- as.numeric(observed[,1])
  
  logged <- substr(response, 1, 4) == "log("
  
  indices <- data.frame(level = levels(model_data[[focus]]))
  
  # Unstandardised index
  if (logged || all(observed > 0)) {
    log_obs <- if (logged) observed else log(observed)
    tmp <- aggregate(log_obs, list(level = model_data[[focus]]), mean)
    colnames(tmp)[2] <- "unstan"
    tmp$unstan <- exp(tmp$unstan - mean(tmp$unstan))
  } else {
    tmp <- aggregate(observed, list(level = model_data[[focus]]), mean)
    colnames(tmp)[2] <- "unstan"
    tmp$unstan <- tmp$unstan / mean(tmp$unstan)
  }
  indices <- merge(indices, tmp, by = "level", all.x = TRUE)
  
  # Coefficients and SEs
  smry <- summary(mod)
  coefs <- smry$p.coeff
  ses <- smry$se[1:length(coefs)]
  
  # Build stan index for focus factor levels
  n_levels <- length(levels(model_data[[focus]]))
  stan_vec <- numeric(n_levels)
  names(stan_vec) <- levels(model_data[[focus]])
  stan_vec[1] <- 0  # baseline reference
  
  for (i in 2:n_levels) {
    lvl_name <- paste0(focus, levels(model_data[[focus]])[i])
    if (lvl_name %in% names(coefs)) {
      idx <- which(names(coefs) == lvl_name)
      stan_vec[i] <- coefs[idx]
    }
  }
  base_coef <- mean(stan_vec)
  
  ses_vec <- numeric(n_levels)
  ses_vec[1] <- 0
  for (i in 2:n_levels) {
    lvl_name <- paste0(focus, levels(model_data[[focus]])[i])
    if (lvl_name %in% names(coefs)) {
      idx <- which(names(coefs) == lvl_name)
      ses_vec[i] <- ses[idx]
    }
  }
  
  indices$stan <- exp(stan_vec - base_coef)
  indices$stanLower <- exp(stan_vec - base_coef - 2 * ses_vec)
  indices$stanUpper <- exp(stan_vec - base_coef + 2 * ses_vec)
  
  # Stepwise terms
  terms_all <- attr(terms(mod), "term.labels")
  
  summary_df <- NULL
  
  # ---- Step 1: Intercept-only model ----
  intercept_formula <- as.formula(paste(response, "~ 1"))
  m_intercept <- mgcv::bam(intercept_formula, data = mod$model, family = mod$family)
  
  logLikInterceptOnly <- tryCatch(logLik(m_intercept), error = function(e) NA)
  fitted_intercept <- tryCatch(predict(m_intercept, type = "response"), error = function(e) fitted(m_intercept))
  
  summary_df <- data.frame(
    term = "intercept",
    k = length(coef(m_intercept)),
    logLik = as.numeric(logLikInterceptOnly),
    aic = extractAIC(m_intercept)[2],
    r2 = 0,
    r2Dev = NA,
    r2Negel = NA
  )
  
  
  
  for (termCount in 0:length(terms_all)) {
    #if (termCount > 0) {
      term <- terms_all[termCount]
      rhs_terms <- paste(terms_all[1:termCount], collapse = " + ")
      new_formula <- as.formula(paste(response, "~", rhs_terms))
      m_step <- update(mod, formula = new_formula)
      
      # Compute effect index for focus term levels
      # This assumes .$effects(m_step) functionality — let's use predicted values per level:
      pred_df <- model_data
      pred_df[[focus]] <- factor(pred_df[[focus]], levels = levels(model_data[[focus]]))
      pred_vals <- predict(m_step, newdata = pred_df, type = "response")
      # Aggregate means by focus levels
      agg_index <- aggregate(pred_vals, list(level = pred_df[[focus]]), mean)
      names(agg_index)[2] <- if (termCount == 1) term else paste0("+", term)
      
      # Standardize index around 1
      agg_index[[2]] <- agg_index[[2]] / mean(agg_index[[2]], na.rm = TRUE)
      
      indices <- merge(indices, agg_index, by = "level", all.x = TRUE)
    #} #else {
      #term <- "intercept"
      #m_step <- update(mod, formula = as.formula(paste(response, "~ 1")))
    #}
    
    # Summary stats
    logLik_val <- tryCatch(logLik(m_step), error = function(e) NA)
    fitted_vals <- tryCatch(predict(m_step, type = "response"), error = function(e) fitted(m_step))
    
    if (termCount == 0) {
      r2 <- 0
      logLikInterceptOnly <- logLik_val
    } else {
      r2 <- cor(log(observed), log(fitted_vals))^2
    }
    
    r2Dev <- tryCatch({
      (m_step$null.deviance - m_step$deviance) / m_step$null.deviance
    }, error = function(e) NA)
    
    n <- length(observed)
    r2Negel <- if (!is.na(logLikInterceptOnly) && !is.na(logLik_val)) {
      (1 - exp((logLikInterceptOnly - logLik_val) * (2 / n))) / (1 - exp(logLikInterceptOnly * (2 / n)))
    } else {
      NA
    }
    
    summary_df <- rbind(summary_df, data.frame(
      term = term,
      k = length(coef(m_step)),
      logLik = as.numeric(logLik_val),
      aic = extractAIC(m_step)[2],
      r2 = r2,
      r2Dev = r2Dev,
      r2Negel = r2Negel
    ))
  }
  
  # Convert diffs for incremental stats
  summary_df <- within(summary_df, {
    k = c(1, diff(k))
    r2 = c(NA, diff(r2))
    r2Dev = c(NA, diff(r2Dev))
    r2Negel = c(NA, diff(r2Negel))
  })
  
  # Predicted terms and SEs
  preds <- predict(mod, type = "terms", se.fit = TRUE)
  fit <- as.data.frame(preds$fit)
  se.fit <- as.data.frame(preds$se.fit)
  preds_df <- cbind(model_data, fit, se.fit)
  colnames(preds_df) <- c(colnames(model_data),
                          paste0("fit.", colnames(fit)),
                          paste0("se.fit.", colnames(se.fit)))
  
  # Influences overall and trend
  influences <- data.frame(level = levels(model_data[[focus]]))
  overall <- c(NA, NA)
  trend <- c(NA, NA)
  
  for (term in terms_all) {
    if (term != focus) {
      fit_col <- paste0("fit.", term)
      infl <- aggregate(list(value = preds_df[[fit_col]]),
                        list(level = preds_df[[focus]]), mean)
      overall <- c(overall, with(infl, exp(mean(abs(value))) - 1))
      trend <- c(trend, with(infl, exp(cov(seq_along(value), value) / var(seq_along(value))) - 1))
      names(infl) <- c("level", term)
      influences <- merge(influences, infl, all.x = TRUE, by = "level")
    }
  }
  
  summary_df$overall <- overall
  summary_df$trend <- trend
  
  list(
    indices = indices,
    summary = summary_df,
    influences = influences,
    preds = preds_df,
    model = mod,
    focus = focus,
    response = response,
    terms = terms_all
  )
}

res <- influence_calc(mod = mod, focus = "yy", response = "lond")



###########################################
compute_influence_indices_v2 <- function(mod, focus_term) {
  response_name <- names(mod$model)[1]
  observed <- mod$model[[response_name]]
  
  # Check if response is log-transformed (simple check)
  logged <- substr(response_name, 1, 4) == 'log('
  
  # Prepare factor for focus term, ensure consistent levels
  focus_factor <- factor(mod$model[[focus_term]])
  focus_levels <- levels(focus_factor)
  indices <- data.frame(level = focus_levels)
  
  # Compute unstandardised index
  if (logged || all(observed > 0)) {
    log_observed <- if (logged) observed else log(observed)
    unstan <- aggregate(log_observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- exp(unstan$unstan - mean(unstan$unstan))  # Relative index
  } else {
    unstan <- aggregate(observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- unstan$unstan / mean(unstan$unstan)
  }
  
  indices <- merge(indices, unstan, by = "level", all.x = TRUE)
  
  # Add standardised index
  
  coefs <- coef(mod)
  vcov_mat <- vcov(mod)
  
  # Extract coefficients related to focus_term
  term_pattern <- paste0("^", focus_term)
  term_indices <- grep(term_pattern, names(coefs))
  term_coefs <- coefs[term_indices]
  
  # Base level coef = 0 for contrasts
  coeffs <- c(0, term_coefs)
  
  # Calculate standard errors (Francis method)
  V <- vcov_mat[term_indices, term_indices, drop = FALSE]
  n <- length(term_coefs) + 1
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1, ] <- Q[-1, ] + diag(n - 1)
  V0 <- Q %*% V %*% t(Q)
  ses <- sqrt(diag(V0))
  
  base <- mean(coeffs)
  
  indices$stan <- exp(coeffs - base)
  indices$stanLower <- exp(coeffs - base - 2 * ses)
  indices$stanUpper <- exp(coeffs - base + 2 * ses)
  
  # Prepare for incremental models
  terms_vec <- attr(mod$terms, "term.labels")
  
  summary_df <- NULL
  logLikInterceptOnly <- NA
  
  for (termCount in 1:length(terms_vec)) {
    if (termCount == 0) {
      term <- "intercept"
      formula_rhs <- "1"
    } else {
      term <- terms_vec[termCount]
      formula_rhs <- paste(terms_vec[1:termCount], collapse = " + ")
    }
    
    new_formula <- as.formula(paste(response_name, "~", formula_rhs))
    model_update <- update(mod, formula = new_formula)
    
    # Calculate effect index per level of focus_term
    updated_factor <- factor(model_update$model[[focus_term]], levels = focus_levels)
    effect_index <- tapply(fitted(model_update), updated_factor, mean)
    
    indices <- cbind(indices, effect_index)
    colname <- if (termCount == 0) term else if (termCount == 1) term else paste0("+", term)
    names(indices)[ncol(indices)] <- colname
    
    # Summary stats
    logLik_val <- tryCatch(logLik(model_update), error = function(e) NA)
    fitted_vals <- tryCatch(predict(model_update, type = "response"), error = function(e) fitted(model_update))
    
    if (termCount == 0) {
      r2 <- 0
      logLikInterceptOnly <- logLik_val
    } else {
      r2 <- suppressWarnings(cor(log(observed), log(fitted_vals))^2)
    }
    
    r2Dev <- tryCatch({
      (model_update$null.deviance - model_update$deviance) / model_update$null.deviance
    }, error = function(e) NA)
    
    n <- length(observed)
    r2Negel <- if (!is.na(logLikInterceptOnly) && !is.na(logLik_val)) {
      (1 - exp((logLikInterceptOnly - logLik_val) * (2 / n))) / (1 - exp(logLikInterceptOnly * (2 / n)))
    } else {
      NA
    }
    
    summary_df <- rbind(summary_df, data.frame(
      term = term,
      k = length(coef(model_update)),
      logLik = as.numeric(logLik_val),
      aic = extractAIC(model_update)[2],
      r2 = r2,
      r2Dev = r2Dev,
      r2Negel = r2Negel
    ))
  }
  
  # Convert diffs for incremental stats
  summary_df <- within(summary_df, {
    k = c(1, diff(k))
    r2 = c(NA, diff(r2))
    r2Dev = c(NA, diff(r2Dev))
    r2Negel = c(NA, diff(r2Negel))
  })
  
  # Predicted terms and SEs
  preds <- predict(mod, type = "terms", se.fit = TRUE)
  fit <- as.data.frame(preds$fit)
  se.fit <- as.data.frame(preds$se.fit)
  preds_df <- cbind(mod$model, fit, se.fit)
  colnames(preds_df) <- c(colnames(mod$model),
                          paste0("fit.", colnames(fit)),
                          paste0("se.fit.", colnames(se.fit)))
  
  # Influences overall and trend for other terms
  influences <- data.frame(level = focus_levels)
  overall <- numeric(length(terms_vec) + 1)
  trend <- numeric(length(terms_vec) + 1)
  overall[1] <- NA
  trend[1] <- NA
  
  for (i in seq_along(terms_vec)) {
    term <- terms_vec[i]
    if (term != focus_term) {
      fit_col <- paste0("fit.", term)
      infl <- aggregate(preds_df[[fit_col]], list(level = preds_df[[focus_term]]), mean)
      names(infl) <- c("level", term)
      
      overall[i + 1] <- with(infl, exp(mean(abs(x))) - 1)
      trend[i + 1] <- with(infl, {
        ord <- seq_along(x)
        cov(ord, x) / var(ord)
      })
      trend[i + 1] <- exp(trend[i + 1]) - 1
      
      influences <- merge(influences, infl, by = "level", all.x = TRUE)
    } else {
      overall[i + 1] <- NA
      trend[i + 1] <- NA
    }
  }
  
  summary_df$overall <- overall
  summary_df$trend <- trend
  
  return(list(
    indices = indices,
    summary = summary_df,
    influences = influences,
    preds = preds_df,
    model = mod,
    focus = focus_term,
    response = response_name,
    terms = terms_vec
  ))
}
compute_influence_indices_v2(mod = mod, focus_term = 'yy')

calc_bam_influence <- function(bam_mod, response, focus) {
  # Extract observed response
  observed <- bam_mod$model[[response]]
  # Handle potential log-transform flag
  logged <- substr(response, 1, 4) == 'log('
  
  # Prepare indices dataframe with factor levels of focus term
  focus_factor <- factor(bam_mod$model[[focus]])
  focus_levels <- levels(focus_factor)
  indices <- data.frame(level = focus_levels)
  
  # Compute unstandardised index
  if (logged || all(observed > 0)) {
    log_observed <- if (logged) observed else log(observed)
    unstan <- aggregate(log_observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- exp(unstan$unstan - mean(unstan$unstan))  # Relative index
  } else {
    unstan <- aggregate(observed, list(level = focus_factor), mean)
    colnames(unstan)[2] <- "unstan"
    unstan$unstan <- unstan$unstan / mean(unstan$unstan)
  }
  indices <- merge(indices, unstan, by = "level", all.x = TRUE)
  
  # Standardised index using coefficients and SEs
  coefs <- coef(bam_mod)
  vcov_mat <- vcov(bam_mod)
  
  # Extract coefficients related to focus term
  term_pattern <- paste0("^", focus)
  term_indices <- grep(term_pattern, names(coefs))
  term_coefs <- coefs[term_indices]
  
  # Base level coef = 0 for contrasts
  coeffs <- c(0, term_coefs)
  
  # Calculate SEs using Francis method
  V <- vcov_mat[term_indices, term_indices, drop = FALSE]
  n <- length(term_coefs) + 1
  Q <- matrix(-1 / n, nrow = n, ncol = n - 1)
  Q[-1, ] <- Q[-1, ] + diag(n - 1)
  V0 <- Q %*% V %*% t(Q)
  ses <- sqrt(diag(V0))
  
  base <- mean(coeffs)
  
  indices$stan <- exp(coeffs - base)
  indices$stanLower <- exp(coeffs - base - 2 * ses)
  indices$stanUpper <- exp(coeffs - base + 2 * ses)
  
  # Prepare incremental models info
  terms_vec <- attr(bam_mod$terms, "term.labels")
  
  # Loop through terms and add incremental effects columns
  for (termCount in seq_along(terms_vec)) {
    term <- terms_vec[termCount]
    rhs_terms <- paste(terms_vec[1:termCount], collapse = " + ")
    new_formula <- as.formula(paste(response, "~", rhs_terms))
    
    # Update model with subset of terms
    model_update <- update(bam_mod, formula = new_formula, evaluate = TRUE)
    
    # Calculate effect index per level of focus term
    updated_factor <- factor(model_update$model[[focus]], levels = focus_levels)
    effect_index <- tapply(fitted(model_update), updated_factor, mean)
    
    indices <- cbind(indices, effect_index)
    colname <- if (termCount == 1) term else paste("+", term)
    names(indices)[ncol(indices)] <- colname
  }
  
  # Columns with incremental indices start after known fixed columns:
  start_col <- which(names(indices) == terms_vec[1])  # first incremental col
  
  for (col_idx in start_col:ncol(indices)) {
    col_vals <- indices[[col_idx]]
    # Check if all values > 0 to do log centering
    if (all(col_vals > 0)) {
      log_vals <- log(col_vals)
      indices[[col_idx]] <- exp(log_vals - mean(log_vals))
    } else {
      # fallback: scale by mean
      indices[[col_idx]] <- col_vals / mean(col_vals, na.rm = TRUE)
    }
  }
  
  # Summary table with model fit stats
  summary_df <- NULL
  logLikInterceptOnly <- NA  # skipping intercept-only model stats
  
  for (termCount in seq_along(terms_vec)) {
    term <- terms_vec[termCount]
    rhs_terms <- paste(terms_vec[1:termCount], collapse = " + ")
    new_formula <- as.formula(paste(response, "~", rhs_terms))
    
    model_update <- update(bam_mod, formula = new_formula, evaluate = TRUE)
    
    logLik_val <- tryCatch(logLik(model_update), error = function(e) NA)
    fitted_vals <- tryCatch(predict(model_update, type = "response"), error = function(e) fitted(model_update))
    
    r2 <- suppressWarnings(cor(log(observed), log(fitted_vals))^2)
    r2Dev <- tryCatch({
      (model_update$null.deviance - model_update$deviance) / model_update$null.deviance
    }, error = function(e) NA)
    
    n <- length(observed)
    r2Negel <- if (!is.na(logLikInterceptOnly) && !is.na(logLik_val)) {
      (1 - exp((logLikInterceptOnly - logLik_val) * (2 / n))) / (1 - exp(logLikInterceptOnly * (2 / n)))
    } else NA
    
    summary_df <- rbind(summary_df, data.frame(
      term = term,
      k = length(coef(model_update)),
      logLik = as.numeric(logLik_val),
      aic = extractAIC(model_update)[2],
      r2 = r2,
      r2Dev = r2Dev,
      r2Negel = r2Negel
    ))
  }
  
  # Calculate predicted terms and se.fit
  preds <- predict(bam_mod, type = "terms", se.fit = TRUE)
  fit <- as.data.frame(preds$fit)
  se.fit <- as.data.frame(preds$se.fit)
  preds_df <- cbind(fit, se.fit)
  names(preds_df) <- c(paste0("fit.", names(fit)), paste0("se.fit.", names(se.fit)))
  preds_df <- cbind(bam_mod$model, preds_df)
  
  # Calculate influences dataframe
  influences <- data.frame(level = focus_levels)
  overall <- c(NA, NA)  # placeholders for intercept and focus term
  
  for (term in terms_vec) {
    if (term != focus) {
      
      # Handle smooth terms
      fit_colname <- if (paste0("fit.", term) %in% names(preds_df)) {
        paste0("fit.", term)
      } else if (paste0("fit.s(", term, ")") %in% names(preds_df)) {
        paste0("fit.s(", term, ")")
      } else {
        warning(paste("Skipping term:", term, "- no matching column in predictions."))
        next
      }
      
      infl <- aggregate(
        list(value = preds_df[[fit_colname]]),
        list(level = preds_df[[focus]]),
        mean
      )
      
      overall <- c(overall, with(infl, exp(mean(abs(value))) - 1))
      trend <- c(trend, with(infl, exp(cov(seq_along(value), value) / var(seq_along(value))) - 1))
      
      names(infl) <- c("level", term)
      influences <- merge(influences, infl, all.x = TRUE, by = "level")
    }
  }
  influences_scaled <- as.data.frame(lapply(influences[-1], function(col) {
    exp(col - mean(col))
  }))
  influences_scaled$level <- influences$level
  
  
  # Add overall and trend columns for each term (NA for intercept and focus)
  trend <- overall  # for simplicity, assign same as overall here (adjust if needed)
  
  list(
    indices = indices,
    summary = summary_df,
    preds = preds_df,
    influences = influences_scaled,
    overall = overall,
    trend = trend
  )
}
tmp2 <- calc_bam_influence(bam_mod = mod, response = 'lond', focus = 'yy')

# Influences
tmp2$influences |>
  pivot_longer(-level, names_to = 'var', values_to = 'val') |>
  ggplot() +
  aes(as.numeric(as.character(level)), val, col = var, group = 1) +
  geom_line() +
  facet_wrap(~var) + 
  geom_hline(yintercept = 1) +
  labs(x = 'Year', y = 'Influence', col = 'Variable') +
  scale_color_brewer(palette = 'Set1') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'none')

tmp2$indices |>
  pivot_longer(-c(level, contains('stan')), names_to = 'var', values_to = 'val') |>
  ggplot() +
  aes(level, val, col = var, group = var) +
  geom_line() +
  scale_color_brewer(palette = 'Set1') +
  geom_hline(yintercept = 1, linetype = 'dashed') +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'bottom')

tmp2$indices |>
  #dplyr::select(level, unstan,stan,stanLower,stanUpper) |>
  ggplot() +
  aes(level, stan, ymin = stanLower, ymax = stanUpper, group = 1) +
  geom_ribbon(col = 'grey', alpha = 0.2) +
  geom_line() +
  geom_point() +
  gg.theme +
  geom_line(data = tmp2$indices, aes(level, unstan, group = 1), col = 'red') +
  geom_line(aes(level, stan), col = 'blue') +
  geom_hline(yintercept = 1) +
  gg.theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'bottom')
