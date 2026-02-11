# Script for generating models for Biodiversity tool for Agroforestry
# This script is designed to create the desired model outputs on which the tool works
# Run this script after adapting the literature databases in order to update the models

# Afore doing so, make sure the exploratory research to add new models or to add moderators is validated

# -------------------------------------------
# Inputs
# -------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(metafor)

# Load dataset
MetaAnalys_data <- readRDS("D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/Meta-analysis full database") %>%
  # Add data transformations:
  mutate(log__forestshrub_frac = log(forestshrub_frac + 0.01),
         sqrt__forestshrub_frac = sqrt(forestshrub_frac),
         exp__forestshrub_frac = exp(forestshrub_frac/100), # Division by 100 to prevent overflow
         expmin__forestshrub_frac = exp(-forestshrub_frac/100), # Division by 100 to prevent overflow
         minlog__forestshrub_frac = -log(forestshrub_frac + 0.01),
         reversed__forestshrub_frac = 1/(1+forestshrub_frac),
         reversedexp__forestshrub_frac = 1/(1+exp(forestshrub_frac/100)), # Division by 100 to prevent overflow
         minexpmin__forestshrub_frac = -exp(-forestshrub_frac/100), # Division by 100 to prevent overflow
         log__crop_frac = log(crop_frac + 0.01),
         sqrt__crop_frac = sqrt(crop_frac),
         exp__crop_frac = exp(crop_frac/100), # Division by 100 to prevent overflow
         expmin__crop_frac = exp(-crop_frac/100), # Division by 100 to prevent overflow
         minlog__crop_frac = -log(crop_frac + 0.01),
         reversed__crop_frac = 1/(1+crop_frac),
         reversedexp__crop_frac = 1/(1+exp(crop_frac/100)), # Division by 100 to prevent overflow
         minexpmin__crop_frac = -exp(-crop_frac/100), # Division by 100 to prevent overflow
         log__Alley.width = log(Alley.width),
         sqrt__Alley.width = sqrt(Alley.width),
         exp__Alley.width = exp(Alley.width/100), # Division by 100 to prevent overflow
         expmin__Alley.width = exp(-Alley.width/100), # Division by 100 to prevent overflow
         minlog__Alley.width = -log(Alley.width),
         reversed__Alley.width = 1/(1+Alley.width),
         reversedexp__Alley.width = 1/(1+exp(Alley.width/100)), # Division by 100 to prevent overflow
         minexpmin__Alley.width = -exp(-Alley.width/100), # Division by 100 to prevent overflow
         log__Age = log(Age),
         sqrt__Age = sqrt(Age),
         exp__Age = exp(Age/100), # Division by 100 to prevent overflow
         expmin__Age = exp(-Age/100), # Division by 100 to prevent overflow
         minlog__Age = -log(Age),
         reversed__Age = 1/(1+Age),
         reversedexp__Age = 1/(1+exp(Age/100)), # Division by 100 to prevent overflow
         minexpmin__Age = -exp(-Age/100), # Division by 100 to prevent overflow
         log__landscape_shannon = log(landscape_shannon + 0.01), # Shannon doesn't exceed large values
         sqrt__landscape_shannon = sqrt(landscape_shannon),
         exp__landscape_shannon = exp(landscape_shannon), 
         expmin__landscape_shannon = exp(-landscape_shannon), 
         minlog__landscape_shannon = -log(landscape_shannon + 0.01),
         reversed__landscape_shannon = 1/(1+landscape_shannon),
         reversedexp__landscape_shannon = 1/(1+exp(landscape_shannon)), 
         minexpmin__landscape_shannon = -exp(-landscape_shannon)
  )

# Create a core dataset and a validation set
if (optie == "Full tool dataset") {
  MetaAnalys_data <- MetaAnalys_data
} else if (optie == "Independent validation set") {
  AF_add <- read.csv("D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/Data/0) Database additions.csv", 
                     header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::select(Title)
  
  Validation_data <- MetaAnalys_data %>%
    semi_join(AF_add %>% distinct(Title), by = "Title")
  saveRDS(Validation_data, "Validation_data")
  
  MetaAnalys_data <- anti_join(MetaAnalys_data, AF_add, by = "Title")
  saveRDS(MetaAnalys_data, "MetaAnalys_core_data")
} else if (optie == "Dependent validation set" & new.validation.needed == TRUE) {
  # Total target size (allow 8.5%–11%)
  total_n <- nrow(MetaAnalys_data)
  min_n <- round(0.085 * total_n)
  max_n <- round(0.11 * total_n)
  
  # Shuffle studies
  #  set.seed(123)  # reproducibility if desired
  unique_titles <- sample(unique(MetaAnalys_data$Title))
  
  # Iteratively add studies until within range
  selected_titles <- c()
  val_n <- 0
  for (t in unique_titles) {
    candidate_n <- val_n + sum(MetaAnalys_data$Title == t)
    if (candidate_n > max_n) next  # skip if it would overshoot too much
    selected_titles <- c(selected_titles, t)
    val_n <- candidate_n
    if (val_n >= min_n) break
  }
  
  # Build validation and training sets
  Validation_data <- MetaAnalys_data %>% filter(Title %in% selected_titles)
  MetaAnalys_data <- MetaAnalys_data %>% filter(!Title %in% selected_titles)
  
  # Save
  saveRDS(Validation_data, "Validation_data")
  saveRDS(MetaAnalys_data, "MetaAnalys_core_data")
} else if (optie == "Dependent validation set" & new.validation.needed == FALSE) {
  # Already exists for previous model structure
  Validation_data <- readRDS("Validation_data")
  MetaAnalys_data <- readRDS("MetaAnalys_core_data")
}

############################################################################################################
# Solve missing sd's
############################################################################################################

# Extract overall meta-analysis data
MetaAnalys_data_overall <- MetaAnalys_data %>%
  filter(Group == "Overall")
MetaAnalys_data_overall

# If you have missing standard deviations (SDs) and want to estimate them using available SDs,
# you can use imputation methods. One common approach is using the coefficient of variation (CV).

# The coefficient of variation (CV) is the ratio of the standard deviation (SD) to the mean (M).
# We calculate it for the data we have:

CV <- MetaAnalys_data_overall %>%
  # ✅ Avoid division by zero or Inf CV values
  filter(!is.na(AF_sd) & !is.na(crtl_sd)) %>%
  filter(AF_sd > 0 & crtl_sd > 0) %>%
  filter(!is.na(AF_mean) & !is.na(crtl_mean)) %>%
  filter(AF_mean > 0 & crtl_mean > 0) %>%
  # ✅ Clean up AF_plottype values safely without overwriting incorrectly
  group_by(AF_type) %>%
  summarize(
    CV_AF = median(AF_sd / AF_mean, na.rm = TRUE),
    CV_crtl = median(crtl_sd / crtl_mean, na.rm = TRUE)
  ) %>%
  ungroup()

# ✅ Add fallback CVs if some AF_plottype categories were missing
CV_defaults <- CV %>%
  summarise(CV_AF = median(CV_AF, na.rm = TRUE),
            CV_crtl = median(CV_crtl, na.rm = TRUE))

# Impute missing SD values
distribute_sd <- function(data, cv_data) {
  # ✅ Join safely on AF_plottype instead of AF_type (for proper mapping)
  data <- left_join(data, cv_data, by = "AF_type")
  
  # ✅ Fill missing CVs with overall medians (prevents NA imputation)
  data <- data %>%
    mutate(
      CV_AF = ifelse(is.na(CV_AF), CV_defaults$CV_AF, CV_AF),
      CV_crtl = ifelse(is.na(CV_crtl), CV_defaults$CV_crtl, CV_crtl)
    )
  
  # ✅ Impute SDs, enforce positive lower bound
  data <- data %>% mutate(
    AF_sd = ifelse(is.na(AF_sd) | AF_sd <= 0 | AF_sd == "", AF_mean * CV_AF, AF_sd),
    crtl_sd = ifelse(is.na(crtl_sd) | crtl_sd <= 0 | crtl_sd == "", crtl_mean * CV_crtl, crtl_sd),
    AF_sd = ifelse(is.na(AF_sd) | AF_sd <= 0, 0.001, AF_sd),
    crtl_sd = ifelse(is.na(crtl_sd) | crtl_sd <= 0, 0.001, crtl_sd)
  )
  return(data)
}

############################################################################################################
# Work with data
############################################################################################################

script_dir <- rstudioapi::getActiveDocumentContext()

data_input <- filter(MetaAnalys_data) %>% filter(System_crtl %in% c("Arable", "Pasture", "Agroecosystem")) %>% mutate(Soil_rocks = Coarse)

# Impute missing SDs
data_input <- distribute_sd(data_input, cv_data = CV)
data_input

# Transformation to avoid zeros
# Back transform: (out * mean(crtl) - 1)/(100 * mean(crtl) - 1) * 100 %
data_input <- data_input %>%
  mutate(crtl_mean = ifelse(Metric %in% c("ab", "SR", "totSR"), crtl_mean + 1, crtl_mean), # Solve 0-values: transform to + 1
         AF_mean = ifelse(Metric %in% c("ab", "SR", "totSR"), AF_mean + 1, AF_mean))

# ----------------------------------
# RR calculation
# ----------------------------------
data_input <- escalc("ROM",  # effect size : (logaritmic) Ratio of Means or Response Ratio
                     m1i = AF_mean, # mean richness at invaded sites
                     n1i = AF_n, # invaded site sample size
                     sd1i = AF_sd, # invaded site SD
                     m2i = crtl_mean,# mean richness at control sites
                     n2i = crtl_n, # control site sample size
                     sd2i = crtl_sd,# control site SD
                     data = data_input)
data_input <- as.data.frame(data_input)

saveRDS(data_input, "D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/MetaAnalys_core_data_used")

# ----------------------------------
# --- Prepare validation set ---
# ----------------------------------
if (optie != "Full tool dataset"){
  validation <- Validation_data %>%
    mutate(crtl_mean = ifelse(Metric %in% c("ab", "SR", "totSR"), crtl_mean + 1, crtl_mean),
           AF_mean   = ifelse(Metric %in% c("ab", "SR", "totSR"), AF_mean + 1, AF_mean))
  validation <- escalc("ROM",  # effect size : (logaritmic) Ratio of Means or Response Ratio
                       m1i = AF_mean, # mean richness at invaded sites
                       n1i = AF_n, # invaded site sample size
                       sd1i = AF_sd, # invaded site SD
                       m2i = crtl_mean,# mean richness at control sites
                       n2i = crtl_n, # control site sample size
                       sd2i = crtl_sd,# control site SD
                       data = validation)
  validation <- as.data.frame(validation)
} else {
  # validation set is internally created during model selection
  validation <- NULL
}

if (!is.null(validation)){
  saveRDS(validation, "D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/Validation_data_used")
}

####################################################
# Functions
####################################################
# --------------------------------------------------
# Model creation - function
# --------------------------------------------------
run_model <- function(data, Group, Metric, AF_type, moderators = c(), interactions = c(), output_dir) {
  data_filtered <- data %>%
    filter(Group == !!Group, Metric == !!Metric, AF_type == !!AF_type) %>% 
    dplyr::select(Group, AF_type, Metric, Study_ID, Site_ID, yi, vi, all_of(moderators)) %>%
    mutate(across(all_of(moderators), ~ {
      if (is.character(.)) {
        factor(trimws(.))
      } else if (is.factor(.)) {
        droplevels(.)
      } else {
        . # keep numeric/logical unchanged
      }
    })) %>%
    filter(if_all(all_of(moderators), ~ !is.na(.))) %>%
    droplevels()
  
  # Remove moderators that have only one unique level in the filtered data
  mod_cut <- c()
  for (mod in moderators) {
    if (length(unique(data_filtered[[mod]])) <= 1) {
      mod_cut <- cbind(mod_cut, mod)
    }
  }
  moderators <- setdiff(moderators, mod_cut)
  
  if (length(moderators) == 0) {
    message("All moderators have only one level — fitting intercept-only model.")
    moderators <- NULL
  }
  
  treshold_sites <- 30 # Effect sizes have to come from at least 30 different sites
  treshold_studies <- 10 # Effect sizes have to come from at least 10 different studies
  
  # Define the models to be fitted (based on tresholds)
  if (length(unique(data_filtered$Study_ID)) < treshold_studies) {
    message("Not enough studies.")
    return(NULL)
  } else if (length(unique(data_filtered$Site_ID)) < treshold_sites) {
    message("Not enough sites.")
    return(NULL)
  }
  
  # --- Build moderator formula ---
  # Base moderators
  mod_terms <- moderators
  # Add interactions
  if (length(interactions) > 0) {
    mod_terms <- c(mod_terms, interactions)
  }
  mod_formula <- as.formula(paste("~", paste(mod_terms, collapse = " + ")))
  
  model <- tryCatch({
    metafor::rma.mv(
      yi, vi,
      mods = mod_formula,
      random = list(~1 | Study_ID/Site_ID
      #           , ~1 | AF_plottype # placed to fixed effects since it has too limited levels to be a random intercept
                    ),
      data = data_filtered,
      method = "REML"
    )
  }, error = function(e) {
    message("Model fitting failed: ", e$message)
    return(NULL)})
  
  if (is.null(model)) {
    message("Model object is NULL after fitting attempt.")
    return(NULL)
  }
  
  model$model_data <- data_filtered  # For later reference in prediction
  model$mod_formula <- mod_formula # For later reference in prediction
  
  # 🔧 Reassign class to preserve S3 methods like vcov()
  class(model) <- c("rma.mv", "rma")
  
  # Create output directory if not exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  # Construct file names
  id <- paste(Group, Metric, AF_type, sep = "_")
  saveRDS(model, file = file.path(output_dir, paste0("tmp_", id, ".rds")))
  
  return(invisible(NULL))# Construct file names
}

# --------------------------------------------------
# Generate all moderator + interaction combinations
# --------------------------------------------------
generate_combinations <- function(moderators, interactions = NULL) {
  # Transformation sets
  transform_map <- list(
    # select transformations from log__, sqrt__, exp__, expmin__, minlog__, reversed__, reversedexp__, minexpmin__
    
    # AGE:
    # Linear baseline, early rapid influence that tapers (sqrt),
    # and diminishing returns with strong plateauing (log).
    Age = c("Age",               # linear effect
            "sqrt__Age",        # early sensitivity that flattens
            "log__Age"),        # diminishing effect with age
    # FOREST SHRUB FRACTION (0–100):
    # Linear baseline, strong early sensitivity at low vegetation cover (log),
    # and smooth saturation / asymptotic decline (reversedexp).
    forestshrub_frac = c("forestshrub_frac",            # linear effect
                         "log__forestshrub_frac",       # strong response at low cover → saturating
                         "reversedexp__forestshrub_frac"),  # smooth diminishing influence
    # CROPLAND FRACTION (0–100):
    # Linear baseline, strong early sensitivity at low vegetation cover (log),
    # and smooth saturation / asymptotic decline (reversedexp).
    forestshrub_frac = c("crop_frac",            # linear effect
                         "log__crop_frac",       # strong response at low cover → saturating
                         "reversedexp__crop_frac"),  # smooth diminishing influence
    # SHANNON LANDSCAPE DIVERSITY (...):
    # Linear baseline, strong early sensitivity at low vegetation cover (log),
    # and smooth saturation / asymptotic decline (reversedexp).
    landscape_shannon = c("landscape_shannon",            # linear effect
                         "log__landscape_shannon",       # strong response at low cover → saturating
                         "reversedexp__landscape_shannon"),  # smooth diminishing influence
    # ALLEY WIDTH:
    # Linear baseline, curvature with more influence at low widths (sqrt),
    # and a transformation that allows a peak / unimodal or "optimal width" shape (minexpmin).
    Alley.width = c("Alley.width",           # linear effect
                    "sqrt__Alley.width",     # early sensitivity → tapering
                    "minexpmin__Alley.width") # allows hump-shaped (optimal) response
  )
  
  # Expand moderators
  expanded <- lapply(moderators, function(mod) {
    if (mod %in% names(transform_map)) transform_map[[mod]] else mod
  })
  names(expanded) <- moderators
  
  # All moderator combinations (each mod gets 1 version)
  moderator_combos <- expand.grid(expanded, stringsAsFactors = FALSE)
  
  results <- list()
  
  for (i in seq_len(nrow(moderator_combos))) {
    chosen_mods <- as.character(moderator_combos[i, ])
    names(chosen_mods) <- names(moderator_combos)
    
    chosen_ints <- c()
    if (!is.null(interactions) && length(interactions) > 0) {
      for (int in interactions) {
        terms <- strsplit(int, ":")[[1]]
        if (all(terms %in% names(chosen_mods))) {
          chosen_ints <- c(chosen_ints, paste(chosen_mods[terms[1]], chosen_mods[terms[2]], sep=":"))
        }
      }
    }
    
    results[[i]] <- list(
      moderators = chosen_mods,
      interactions = chosen_ints
    )
  }
  
  return(results)
}


# --------------------------------------------------
# Select best model from transformations - function
# --------------------------------------------------
# !!!! Check this out because it mixes external and internal validation !!!!
select_best_model <- function(data, Group, Metric, AF_type, moderators, interactions, validation = NULL, output_dir, rmse_threshold) {
  # Generate all combinations of moderators + interactions
  all_combos <- generate_combinations(moderators, interactions)
  
  best_rmse <- Inf
  best_model <- NULL
  validation_rows <- NULL
  
  
  for (idx in 1:length(all_combos)) {
    combo <- all_combos[[idx]]
    combo_mods <- unname(combo$moderators)
    combo_ints <- combo$interactions
    
    # Fit and save temporarily
    run_model(
      data = data,
      Group = Group,
      Metric = Metric,
      AF_type = AF_type,
      moderators = combo_mods,
      interactions = combo_ints,
      output_dir = output_dir
    )
    
    # Canonical temp file name
    tmp_file_name <- paste0("tmp_", Group, "_", Metric, "_", AF_type, ".rds")
    if (!file.exists(file.path(output_dir, tmp_file_name))) {
      next
    }
    best_model <- readRDS(file.path(output_dir, tmp_file_name))
    moddata <- best_model$model_data
    if (is.null(validation_rows)){
      set.seed(123)
      validation_rows <- sample(seq_len(nrow(moddata)), round(1.00*nrow(moddata)), replace = FALSE) # 100% of the model data
    }
    
    # Preference for external validation, but if not possible use internal (with independent training points)
    if (is.null(validation)) {
      validation_base <- moddata[validation_rows, ]
    } else {validation_base <- validation}
    
    # Evaluate RMSE (rmse_from_saved_rma_model expects just the filename)
    if (file.exists(file.path(output_dir, tmp_file_name))) {
      rmse_val <- rmse_from_saved_rma_model(tmp_file_name, validation_base)$rmse
      
      # Keep only if better
      if (!is.na(rmse_val) && rmse_val < best_rmse) {
        best_rmse <- rmse_val
        best_model <- readRDS(file.path(output_dir, tmp_file_name))  # store the actual model object
        message("🔄 New best model found (RMSE = ", round(best_rmse, 3), ") — overwriting.")
      }
    }
  }
  
  # -----------------------------------------
  # --- Abandon outlier-generating models ---
  # -----------------------------------------
  if (is.infinite(best_rmse)) {
    message("⚠️ No valid model for ", Group, "-", Metric, "-", AF_type)
    return(NULL)
  } else if( best_rmse > rmse_threshold) {
    message("⚠️ Best model RMSE (", round(best_rmse, 3), ") exceeds threshold for ", Group, "-", Metric, "-", AF_type)
    return(NULL)
  } else {
    # Overwrite possible worse-fitting models
    saveRDS(best_model, file = file.path(output_dir, tmp_file_name))
    # ----------------------------------
    # --- Abandon bad-fitting models ---
    # ----------------------------------
    preds <- predict_from_saved_rma_model(tmp_file_name, inputs = validation, output_dir = output_dir, random = FALSE)
    results <- evaluate_model(preds)
    res_slope <- results$slope
    res_slope_p <- results$slope_p
    res_intercept_p <- results$intercept_p
    
    id <- paste(Group, Metric, AF_type, sep = "_")
    if (!is.na(res_intercept_p) && res_intercept_p < 0.05) {
      message("⚠️ Best model slope significantly different from 0 (p = ", round(res_intercept_p, 3), ") for ", Group, "-", Metric, "-", AF_type)
      # Model not saved because intercept value is wrongly fitted
    } else if (!is.na(res_slope_p) && res_slope_p < 0.05) {
      message("⚠️ Best model slope significantly different from 1 (p = ", round(res_slope_p, 3), ") for ", Group, "-", Metric, "-", AF_type)
      if (res_slope > 0.15 && res_slope < 1) {
        # Model has troubles with large absolute values of raw data and predicts more narrowly, but not necessarily wrongly
        saveRDS(best_model, file = file.path(output_dir, paste0("modelpredlow_", id, ".rds")))
      }
    } else {
      message("✅ Best model for ", Group, "-", Metric, "-", AF_type, " with RMSE = ", round(best_rmse, 3))
      # Save the best model under canonical name
      saveRDS(best_model, file = file.path(output_dir, paste0("model_", id, ".rds")))
    }
  }
  # Remove temporary file
  suppressWarnings(file.remove(file.path(output_dir, tmp_file_name)))
  
  return(invisible(best_model))
}

####################################################
# --------------------------------------------------
# Run for model pre-allocation
# --------------------------------------------------
####################################################

# Set the output directory and empty it
output_dir <- file.path(dirname(rstudioapi::getActiveDocumentContext()$path), "model_outputs")
old_files <- list.files(output_dir, full.names = TRUE)
file.remove(old_files)

# Input data
data_input <- data_input

if (all_groups == FALSE) {
  data_input <- data_input %>% filter(Group %in% groups_of_focus) %>% filter(Metric %in% metrics_of_focus)
}

# groupmetrics
df_to_test <- expand.grid(
  Group = unique(data_input$Group),
  Metric = unique(data_input$Metric),
  AF_type = unique(data_input$AF_type),
  stringsAsFactors = FALSE
) %>%
  filter(AF_type != "Hedgerows")

# Moderators (preallocated before running)
# mods <- c("Climate", "forestshrub_frac", "Soil_type", "Shrub_layer", "Tree.pol", "Org.Conv", "Tree_SR", "Age", "Alley.width")

for (i in 1:nrow(df_to_test)) {
  Group <- df_to_test$Group[i]
  Metric <- df_to_test$Metric[i]
  aftype <- df_to_test$AF_type[i]
  
  # Add AF_type-specific moderators
  new_mods <- mods # Stock mods, otherwise, it gets overwritten and creates duplicates
  if (aftype == "Silvoarable" && SA.add) {
    new_mods <- c(mods, "Crop_group", "Org.Conv")
  }
  if (aftype == "Silvopastoral" && SP.add) {
    new_mods <- c(mods, "grazing_dens")
  }
  
  model <- select_best_model(
    data = data_input,
    Group = Group,
    Metric = Metric,
    AF_type = aftype,
    moderators = new_mods,
    interactions = interactions,
    validation = validation,
    output_dir = output_dir,
    rmse_threshold = rmse_threshold
  )
  
  if (is.null(model)) next
}
