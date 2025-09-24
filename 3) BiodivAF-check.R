library(ggplot2)
library(dplyr)
library(tidyr)
library(metafor)
library(lme4)

# -------------------------------------------
# Generate predictions by random sampling moderators
# -------------------------------------------
generate_random_rows <- function(data, moderators, n) {
  random_rows <- purrr::map_dfc(moderators, function(var) {
    values <- data[[var]]
    if (is.factor(values)) {
      sampled <- sample(levels(values), n, replace = TRUE)
      out <- factor(sampled, levels = levels(values))
    } else {
      sampled <- sample(values, n, replace = TRUE)
      out <- sampled
    }
    tibble(!!var := out)
  })
  return(random_rows)
}

# -------------------------------------------
# Generate predictions for metafor rma.mv model
# -------------------------------------------
predict_from_saved_rma_model <- function(model_path, inputs = NULL, n_pred = 100, output_dir = "model_outputs") {
  # --- Extract Group, Metric, AF_type from filename ---
  file_name <- basename(model_path)
  meta <- strcapture(
    "^model_([^_]+)_([^_]+)_([^_]+)\\.rds$",
    file_name,
    proto = list(Group = character(), Metric = character(), AF_type = character())
  )
  Group   <- meta$Group
  Metric  <- meta$Metric
  AF_type <- meta$AF_type
  
  # Load model
  model <- readRDS(file.path(output_dir, model_path))
  if (is.null(model)) stop("Model could not be loaded: ", model_path)
  
  # Extract moderators from model
  if (is.null(model$mod_formula)) stop("Model has no moderator formula.")
  
  # Extract training data and moderator formula
  model_data <- model$data
  mod_formula <- model$mod_formula
  moderators <- all.vars(mod_formula)
  
  # Columns we always want to keep if present
  keep_cols <- c("yi", "vi")
  
  # If inputs not provided: generate random rows
  if (is.null(inputs)) {
    inputs <- generate_random_rows(model_data, moderators, n_pred)
  } else {
    inputs <- inputs %>% filter(Group == !!Group, Metric == !!Metric, AF_type == !!AF_type)
    
    inputs <- inputs[, intersect(names(inputs), c(moderators, keep_cols)), drop = FALSE]
    
    # Align factors
    for (mod in moderators) {
      if (mod %in% names(inputs) && mod %in% names(model_data) && is.factor(model_data[[mod]])) {
        inputs[[mod]] <- factor(inputs[[mod]], levels = levels(model_data[[mod]]))
      }
    }
    # Drop invalid rows
    inputs <- inputs %>% dplyr::filter(if_all(all_of(moderators), ~ !is.na(.x)))
  }
  
  if (nrow(inputs) == 0) {
    message("⚠️ No matches for model: ", file_name)
    return(NULL)
  }
  
  # --- Prediction loop ---
  pred_list <- vector("list", nrow(inputs))
  for (i in seq_len(nrow(inputs))) {
    pred_list[[i]] <- predict_rma_mv_manual_biodivAF(
      model = model,
      Group   = Group,
      Metric  = Metric,
      AF_type = AF_type,
      input_values = inputs[i, , drop = FALSE],
      moderators   = moderators
    )
  }
  
  preds <- dplyr::bind_rows(pred_list)
  
  # --- Add observed yi/vi if present ---
  if ("yi" %in% names(inputs)) preds$yi <- inputs$yi
  if ("vi" %in% names(inputs)) preds$vi <- inputs$vi
  
  # --- Transform yi to percent if present ---
  rom_to_percent <- function(x) (exp(x) - 1) * 100
  if ("yi" %in% names(preds)) preds$yi_Percent <- rom_to_percent(preds$yi)
  
  return(preds)
}

# -------------------------------------------
# metafor predictions along moderators - what causes outliers?
# -------------------------------------------
plot_predictions_by_moderator <- function(pred_df, moderator) {
  # Check if moderator exists
  if (!(moderator %in% colnames(pred_df))) {
    stop("Moderator not found in prediction dataframe: ", moderator)
  }
  
  if (is.numeric(pred_df[[moderator]])) {
    # Continuous moderator → scatter + smooth
    p <- ggplot(pred_df, aes(x = .data[[moderator]], y = Percent_Pred)) +
      geom_jitter(width = 0.1, height = 0, alpha = 0.4) +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(x = moderator, y = "Predicted Effect (%)") +
      theme_minimal()
    
  } else {
    # Categorical moderator → boxplot + violin + jitter
    p <- ggplot(pred_df, aes(x = .data[[moderator]], y = Percent_Pred)) +
      geom_violin(fill = "grey80", alpha = 0.6) +
      geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.1, alpha = 0.4, color = "blue") +
      labs(x = moderator, y = "Predicted Effect (%)") +
      theme_minimal()
  }
  
  return(p)
}

# -------------------------------------------
# Generate Root Mean Square Arrors for metafor rma.mv model
# -------------------------------------------
rmse_from_saved_rma_model <- function(model_path, inputs, output_dir = "model_outputs") {
  # --- Extract Group, Metric, AF_type from filename ---
  file_name <- basename(model_path)
  meta <- strcapture(
    "^(?:model|tmp)_([^_]+)_([^_]+)_([^_]+)\\.rds$",
    file_name,
    proto = list(Group = character(), Metric = character(), AF_type = character())
  )
  Group   <- meta$Group
  Metric  <- meta$Metric
  AF_type <- meta$AF_type
  
  # Load model
  if (!file.exists(file.path(output_dir, model_path))) stop("Model file does not exist: ", model_path)
  model <- readRDS(file.path(output_dir, model_path))
  if (is.null(model)) stop("Model could not be loaded: ", model_path)
  
  # Extract moderators
  if (is.null(model$mod_formula)) stop("Model has no moderator formula.")
  moderators <- all.vars(model$mod_formula)
  
  # Columns we always want to keep if present
  keep_cols <- c("yi", "vi")
  
  # Filter inputs to match this model
  inputs <- inputs %>%
    dplyr::filter(Group == !!Group, Metric == !!Metric, AF_type == !!AF_type) 
  inputs <- inputs[, intersect(names(inputs), c(moderators, keep_cols)), drop = FALSE]
  
  # Align factor levels
  for (mod in moderators) {
    if (mod %in% names(inputs) && mod %in% names(model$data) && is.factor(model$data[[mod]])) {
      inputs[[mod]] <- factor(inputs[[mod]], levels = levels(model$data[[mod]]))
    }
  }
  # Drop invalid rows
  inputs <- inputs %>% dplyr::filter(if_all(all_of(moderators), ~ !is.na(.x)))
  
  if (nrow(inputs) == 0) {
    rmse_val <- NA_real_
  } else{
    #  --- Make predictions ---
    pred_list <- lapply(1:nrow(inputs), function(i) {
      predict_rma_mv_manual_biodivAF(
        model = model,
        Group = Group,
        Metric = Metric,
        AF_type = AF_type,
        input_values = inputs[i, , drop = FALSE],
        moderators = moderators
      )
    })
    
    preds <- dplyr::bind_rows(pred_list)
    
    # --- Compute RMSE ---
    if (!("yi" %in% names(inputs)) && !"yi" %in% names(preds)) {
      stop("No observed yi in inputs or predictions to compute RMSE.")
    }
    
    rmse_val <- sqrt(mean((preds$ROM_Pred - inputs$yi)^2, na.rm = TRUE))
  }
  
  
  # --- Return summary ---
  result <- data.frame(
    model = file_name,
    AF_type = AF_type,
    Group = Group,
    Metric = Metric,
    rmse = rmse_val,
    stringsAsFactors = FALSE
  )
  
  return(result)
}

# -------------------------------------------
# Evaluate predictions vs validation data
# -------------------------------------------
evaluate_model <- function(preds) {
  fit <- lm(ROM_Pred ~ yi, data = preds)
  slope_test <- car::linearHypothesis(fit, "yi = 1")$`Pr(>F)`[2]
  intercept_test <- car::linearHypothesis(fit, "(Intercept) = 0")$`Pr(>F)`[2]
  
  # Extract actual slope and intercept
  slope_val <- coef(fit)[2]
  intercept_val <- coef(fit)[1]
  
  # Compute residuals against the 1:1 line directly (smaller is better)
  rmse <- sqrt(mean((preds$ROM_Pred - preds$yi)^2, na.rm = TRUE))
  
  
  preds <- preds %>%
    mutate(se = (ROM_CI_Upper - ROM_CI_Lower) / (2 * 1.96))
  # Predictive log-likelihood (pointwise)
  pll <- sum(dnorm(preds$yi,
                   mean = preds$ROM_Pred,
                   sd   = preds$se,
                   log  = TRUE),
             na.rm = TRUE)
  
  # Information criterion (like WAIC): -2 * PLL
  ic <- -2 * pll
  
  # Check predictions in confidence interval
  pred_check <- preds %>%
    mutate(is_correct = yi >= ROM_CI_Lower & yi <= ROM_CI_Upper)
  mean_CI_width <- mean(preds$ROM_CI_Upper - preds$ROM_CI_Lower, na.rm = TRUE)
  
  correct_count <- sum(pred_check$is_correct, na.rm = TRUE)/nrow(pred_check)
  
  list(
    slope = slope_val,             # actual slope
    intercept = intercept_val,     # actual intercept
    slope_p = slope_test, # Should be > 0.05 to accept H0 (slope = 1)
    intercept_p = intercept_test, # Should be > 0.05 to accept H0 (intercept = 0)
    rmse_1to1 = rmse,
    pll = pll,                     # Predictive log-likelihood (pointwise)
    ic = ic,                       # Information criterion (like WAIC)
    correct_in_CI = correct_count, # Fraction of predictions within the confidence interval
    mean_CI_width = mean_CI_width  # Mean width of the confidence interval
  )
}