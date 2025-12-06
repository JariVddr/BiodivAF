# This script runs predictions from look-up tables in subfolder 'model_outputs' that are needed for the Shiny App interface.

# --------------------------------------------------
# Select the correct model database
# --------------------------------------------------
select_model <- function(Group, Metric, AF_type) {
  
  # Construct clean ID
  id <- paste(Group, Metric, AF_type, sep = "_")
  
  # File paths
  path_norm    <- file.path("model_outputs", paste0("model_", id, ".rds"))
  path_predlow <- file.path("model_outputs", paste0("modelpredlow_", id, ".rds"))
  
  # ---- 1) Prefer the high-quality model_ file ----
  if (file.exists(path_norm)) {
    model <- readRDS(path_norm)
    if (is.null(model)) stop("Loaded model is NULL: ", path_norm)
    model$predlow_flag <- FALSE   # annotate model
    return(model)
  }
  
  # ---- 2) Fallback to modelpredlow_ ----
  if (file.exists(path_predlow)) {
    warning("Using low-prediction-quality model for ", id)
    model <- readRDS(path_predlow)
    if (is.null(model)) stop("Loaded model is NULL: ", path_predlow)
    model$predlow_flag <- TRUE    # annotate model for UI warnings
    return(model)
  }
  
  # ---- 3) Nothing found ----
  warning("Model RDS not found for: ", id)
  return(NULL)
}


# --------------------------------------------------
# Get model moderators
# --------------------------------------------------
get_model_moderators <- function(model) {
  if (is.null(model) || is.null(model$mod_formula)) return(character(0))
  moderators <- all.vars(model$mod_formula)
  return(moderators)
}

# --------------------------------------------------
# Predicting estimates for rma.mv models (manually)
# --------------------------------------------------
predict_rma_mv_manual_biodivAF <- function(model, Group, Metric, AF_type, input_values, moderators) {
  
  # --- Sanity checks ---
  if (is.null(model)) stop("Model is NULL.")
  if (is.null(input_values)) stop("No input data provided.")
  if (is.null(moderators)) stop("Moderator names must be provided.")
  
  # --- Construct model formula ---
  mod_formula <- model$mod_formula
  
  # --- Extract coefficients and variance-covariance matrix ---
  coefs <- as.numeric(model$b)
  names(coefs) <- rownames(model$b)
  vcov_mat <- vcov(model)
  
  # --- Adjust input factor levels to match training data ---
  if (!is.null(model$model_data)) {
    for (mod in moderators) {
      if (mod %in% names(model$model_data)) {
        model_var <- model$model_data[[mod]]
        
        if (is.logical(model_var)) {
          # Convert logicals to factors with levels TRUE/FALSE to match model training
          input_values[[mod]] <- factor(as.logical(input_values[[mod]]), levels = c(FALSE, TRUE))
        } else if (is.factor(model_var) || is.character(model_var)) {
          input_values[[mod]] <- factor(input_values[[mod]],
                                        levels = levels(as.factor(model_var)))
        } else {
          input_values[[mod]] <- as.numeric(input_values[[mod]])
        }
      }
    }
  }
  
  # --- Create model matrix ---
  newmods <- model.matrix(mod_formula, data = input_values)
  colnames(newmods)[colnames(newmods) == "(Intercept)"] <- "intrcpt"
  
  # --- Ensure matrix columns match coefficients ---
  if (!all(names(coefs) %in% colnames(newmods))) {
    missing_cols <- setdiff(names(coefs), colnames(newmods))
    stop("Missing required columns in model matrix: ", paste(missing_cols, collapse = ", "))
  }
  
  X_new <- newmods[, names(coefs), drop = FALSE]
  
  # --- Prediction ---
  pred_rom <- sum(X_new * coefs)
  pred_var <- as.numeric(X_new %*% vcov_mat %*% t(X_new))
  pred_se <- sqrt(pred_var)
  z_val <- qnorm(0.975)
  
  ci_lower_rom <- pred_rom - z_val * pred_se
  ci_upper_rom <- pred_rom + z_val * pred_se
  
  # --- Convert ROM to percent ---
  rom_to_percent <- function(x) (exp(x) - 1) * 100
  pred_pct <- rom_to_percent(pred_rom)
  ci_lower_pct <- rom_to_percent(ci_lower_rom)
  ci_upper_pct <- rom_to_percent(ci_upper_rom)
  
  # --- Final result ---
  result <- data.frame(
    input_values,
    ROM_Pred = pred_rom,
    ROM_CI_Lower = ci_lower_rom,
    ROM_CI_Upper = ci_upper_rom,
    Percent_Pred = pred_pct,
    Percent_CI_Lower = ci_lower_pct,
    Percent_CI_Upper = ci_upper_pct,
    Group = Group,
    Metric = Metric,
    AF_type = AF_type
  )
  
  return(result)
}

