# Script for generating models for Biodiversity tool for Agroforestry
# This script is designed to create the desired model outputs on which the tool works
# Run this script after adapting the literature databases in order to update the models

# Afore doing so, make sure the explaratory research to add new models or to add moderators is validated

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
         log__Age = log(Age + 0.01),
         sqrt__Age = sqrt(Age),
         exp__Age = exp(Age/100)) # Division by 100 to prevent overflow

# Create a core dataset and a validation set
if (optie == "Full tool dataset") {
  MetaAnalys_data <- MetaAnalys_data
} else if (optie == "Independant validation set") {
  AF_add <- read.csv("D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/Data/0) Database additions.csv", 
                     header = TRUE, stringsAsFactors = FALSE) %>% 
    dplyr::select(Title)
  
  Validation_data <- MetaAnalys_data %>%
    semi_join(AF_add %>% distinct(Title), by = "Title")
  saveRDS(Validation_data, "Validation_data")
  
  MetaAnalys_data <- anti_join(MetaAnalys_data, AF_add, by = "Title")
  saveRDS(MetaAnalys_data, "MetaAnalys_core_data")
} else if (optie == "Dependant validation set") {
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
  filter(!is.na(AF_sd) & !is.na(crtl_sd)) %>%
  mutate(AF_plottype = ifelse(AF_plottype == "Silvoarable tree line", "Silvoarable", AF_type)) %>%
  mutate(AF_plottype = ifelse(AF_plottype == "Silvoarable alley", "Silvoarable", AF_type)) %>%
  mutate(AF_plottype = ifelse(AF_plottype == "Silvopastoral alley", "Silvopasture", AF_type)) %>%
  group_by(AF_plottype) %>%
  summarize(
    CV_AF = median(AF_sd / AF_mean, na.rm = TRUE),
    CV_crtl = median(crtl_sd / crtl_mean, na.rm = TRUE)
  ) %>%
  ungroup()
add1 <- data.frame(AF_plottype = "Silvoarable tree line", CV_AF = CV$CV_AF[CV$AF_plottype == "Silvoarable"], CV_crtl = CV$CV_crtl[CV$AF_plottype == "Silvoarable"])
add2 <- data.frame(AF_plottype = "Silvoarable alley", CV_AF = CV$CV_AF[CV$AF_plottype == "Silvoarable"], CV_crtl = CV$CV_crtl[CV$AF_plottype == "Silvoarable"])
add3 <- data.frame(AF_plottype = "Silvopastoral alley", CV_AF = CV$CV_AF[CV$AF_plottype == "Silvopasture"], CV_crtl = CV$CV_crtl[CV$AF_plottype == "Silvopasture"])
CV <- rbind(CV, add1, add2, add3)

# Impute missing SD values
distribute_sd <- function(data, cv_data) {
  data <- left_join(data, cv_data, by = "AF_plottype")
  data <- data %>% mutate(
    AF_sd = ifelse(is.na(AF_sd) | AF_sd == 0 | AF_sd == "", AF_mean * CV_AF, AF_sd),
    crtl_sd = ifelse(is.na(crtl_sd) | crtl_sd == 0 | crtl_sd == "", crtl_mean * CV_crtl, crtl_sd)
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

# ----------------------------------
# --- Prepare validation set ---
# ----------------------------------
validation <- Validation_data %>%
  mutate(crtl_mean = ifelse(Metric %in% c("ab", "SR", "totSR"), crtl_mean + 1, crtl_mean),
         AF_mean   = ifelse(Metric %in% c("ab", "SR", "totSR"), AF_mean + 1, AF_mean)) %>%
  mutate(yi = log(AF_mean/crtl_mean))

####################################################
# Functions
####################################################
# --------------------------------------------------
# Model creation - function
# --------------------------------------------------
run_model <- function(data, Group, Metric, AF_type, moderators = c(), interactions = c(), output_dir) {
  data_filtered <- data %>%
    filter(Group == !!Group, Metric == !!Metric, AF_type == !!AF_type) %>% 
    dplyr::select(Group, AF_type, Metric, Title, Site_ID, AF_plottype, yi, vi, all_of(moderators)) %>%
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
  
  treshold <- 10*(length(moderators) + 3) # + 3 random effects
  treshold_studies <- 5 # Effect sizes have to come from at least 5 different studies
  
  # Define the moderators to be modelled (based on n) + Remove lines where this does not apply
  if (nrow(data_filtered) < treshold) {
    message("Not enough data points.")
    return(NULL)
  } else if (length(unique(data_filtered$Title)) < treshold_studies) {
    message("Not enough studies.")
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
      random = list(~1 | Title/Site_ID, 
                    ~1 | AF_plottype),
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
    Age = c("Age", "log__Age", "sqrt__Age", "exp__Age"),
    forestshrub_frac = c("forestshrub_frac", "log__forestshrub_frac", "sqrt__forestshrub_frac", "exp__forestshrub_frac")
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
select_best_model <- function(data, Group, Metric, AF_type, moderators, interactions, validation, output_dir, rmse_threshold) {
  # Generate all combinations of moderators + interactions
  all_combos <- generate_combinations(moderators, interactions)
  
  best_rmse <- Inf
  best_model <- NULL
  
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
    
    # Evaluate RMSE (rmse_from_saved_rma_model expects just the filename)
    if (file.exists(file.path(output_dir, tmp_file_name))) {
     rmse_val <- rmse_from_saved_rma_model(tmp_file_name, validation)$rmse
     
     # Keep only if better
     if (!is.na(rmse_val) && rmse_val < best_rmse) {
       best_rmse <- rmse_val
       best_model <- readRDS(file.path(output_dir, tmp_file_name))  # store the actual model object
       message("🔄 New best model found (RMSE = ", round(best_rmse, 3), ") — overwriting.")
     }
     
     # Remove temporary file
     suppressWarnings(file.remove(file.path(output_dir, tmp_file_name)))
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
    message("✅ Best model for ", Group, "-", Metric, "-", AF_type, " with RMSE = ", round(best_rmse, 3))
    # Save the best model under canonical name
    id <- paste(Group, Metric, AF_type, sep = "_")
    saveRDS(best_model, file = file.path(output_dir, paste0("model_", id, ".rds")))
  }
  
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
    new_mods <- c(mods, "Crop_group")
  }
  if (aftype == "Silvopasture" && SP.add) {
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
