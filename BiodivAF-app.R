library(shiny)
library(dplyr)
library(ggplot2)
library(metafor)
library(shinyjs)

# Source your model functions
app_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
source(file.path(app_dir, "2) BiodivAF.R"))



# Default values for all moderators
default_values <- list(
  Climate = "T",
  forestshrub_frac = 50,
  Soil_type = "Loam",
  Shrub_layer = FALSE,
  Tree.pol = TRUE,
  Org.Conv = "Conv",
  Tree_SR = 1,
  Age = 15,
  Alley.width = 10,
  Crop_group = "Cereals",
  grazing_dens = "medium"
)

# --------------------------- UI ---------------------------
ui <- fluidPage(
  useShinyjs(), 
  titlePanel("Biodiversity Gain Estimator in Agroforestry"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("AF_type", "Agroforestry System Type:",
                  choices = c("", "Agroforestry", "Silvoarable", "Silvopastoral"), # "" is placeholder
                  selected = ""),
      
      checkboxGroupInput("group_metric", "Select Biodiversity group and metric of interest:",
                         choices = character(0),
                         selected = character(0)),
      # Placeholder for red warning message if no models are available
      uiOutput("no_model_msg"),
      
      # --- Base moderators ---
      selectInput("Climate", "Climate Type:", choices = c("Temperate", "Mediterranean", "Boreal/Alpine"), selected = default_values$Climate),
      selectInput("Soil_type", "Soil Type:", choices = c("Loam", "Clay", "Sand"), selected = default_values$Soil_type),
      
      sliderInput("forestshrub_frac", "Woody Habitat Fraction (%):", min = 0, max = 100, value = default_values$forestshrub_frac),
      
      selectInput("Tree_SR", "Tree Species Richness:",
                  choices = c("1", "2", "3", "4", "5+"), selected = default_values$Tree_SR),
      sliderInput("Alley.width", "Width between tree rows (m):", min = 0, max = 60, value = default_values$Alley.width),
      checkboxInput("Shrub_layer", "Shrub Layer Present?", value = default_values$Shrub_layer),
      checkboxInput("Tree.pol", "Trees with Pollination Potential?", value = default_values$Tree.pol),
      sliderInput("Age", "Age of the agroforestry system (years):", min = 0.5, max = 60, value = default_values$Age),
      
      selectInput("Org.Conv", "Organic or Conventional:", 
                  choices = c("Org", "Conv"), selected = default_values$Org.Conv),
      # --- Conditional moderators ---
      conditionalPanel(condition = "input.AF_type == 'Silvoarable'",
                       selectInput("Crop_group", "Crop", 
                                   choices = c("Cereals", "Corn", "Vegetables/Mass-flowering crops", 
                                               "Grass/Ley", "Vegetables", "Orchard"), 
                                   selected = default_values$Crop_group)),
      
      conditionalPanel(condition = "input.AF_type == 'Silvopastoral'",
                       selectInput("grazing_dens", "Grazing density", 
                                   choices = c("high", "medium", "low", "very low"), 
                                   selected = default_values$grazing_dens)),
      
      actionButton("run", "Run Model", disabled = TRUE)
    ),
    
    mainPanel(
      tableOutput("result_table"),
      plotOutput("effect_plot")
    )
  )
)

# ------------------------- SERVER -------------------------
server <- function(input, output, session) {
  
  # --------------------- Helper Functions ---------------------
  get_model_moderators <- function(model) {
    if (is.null(model) || is.null(model$mod_formula)) return(character(0))
    moderators <- all.vars(model$mod_formula)
    return(moderators)
  }
  
  update_moderator_inputs <- function(model, session) {
    moderators <- get_model_moderators(model)
    # Base moderators
    shinyjs::toggle("Climate", condition = "Climate" %in% moderators)
    shinyjs::toggle("Soil_type", condition = "Soil_type" %in% moderators)
    shinyjs::toggle("forestshrub_frac", condition = "forestshrub_frac" %in% moderators)
    shinyjs::toggle("Tree_SR", condition = "Tree_SR" %in% moderators)
    shinyjs::toggle("Alley.width", condition = "Alley.width" %in% moderators)
    shinyjs::toggle("Shrub_layer", condition = "Shrub_layer" %in% moderators)
    shinyjs::toggle("Tree.pol", condition = "Tree.pol" %in% moderators)
    shinyjs::toggle("Org.Conv", condition = "Org.Conv" %in% moderators)
    shinyjs::toggle("Age", condition = "Age" %in% moderators)
    # Conditional moderators
    shinyjs::toggle("Crop_group", condition = "Crop_group" %in% moderators)
    shinyjs::toggle("grazing_dens", condition = "grazing_dens" %in% moderators)
  }
  
  ################################################################################
  ### STEP 1: Update group-metric choices based on AF_type
  ################################################################################
  observeEvent(input$AF_type, {
    model_files <- list.files("model_outputs", pattern = "^model_.*\\.rds$", full.names = FALSE)
    
    if(length(model_files) == 0){
      updateCheckboxGroupInput(session, "group_metric", choices = character(0), selected = character(0))
      return()
    }
    
    # Extract Group, Metric, AF_type from filenames
    parsed <- strcapture(
      "^model_([^_]+)_([^_]+)_([^_]+)\\.rds$",
      model_files,
      proto = list(Group = character(), Metric = character(), AF_type = character())
    )
    
    filtered <- parsed %>% filter(AF_type == input$AF_type & Metric %in% c("SR","ab"))
    
    if(nrow(filtered) == 0){
      # No models available for this AF_type → empty choices
      updateCheckboxGroupInput(session, "group_metric", choices = character(0), selected = character(0))
      output$no_model_msg <- renderUI({
        tags$span("No models available for this system type.", style = "color:red; font-weight:bold;")})
      return()
    } else {
      output$no_model_msg <- renderUI({NULL})
    }
    
    # Build display labels
    display_labels <- paste(filtered$Group, ":", 
                            ifelse(filtered$Metric == "SR", "Species richness", "Abundance"))
    names(display_labels) <- display_labels
    
    # Update UI → select all available by default
    updateCheckboxGroupInput(session, "group_metric",
                             choices = names(display_labels),
                             selected = "Overall : Species richness")
  }, ignoreInit = TRUE)
  
  ################################################################################
  ### STEP 2: Update moderators according to model structure
  ################################################################################
  observeEvent(input$group_metric, {
    req(input$group_metric) # ensures user has selected a metric
    
    split_vals <- strsplit(input$group_metric, " : ")
    valid_splits <- split_vals[sapply(split_vals, length) == 2]
    if (length(valid_splits) == 0) return()
    
    gm_split <- do.call(rbind, valid_splits)
    colnames(gm_split) <- c("Group", "Metric")
    gm_df <- as.data.frame(gm_split, stringsAsFactors = FALSE) %>%
      mutate(Metric = ifelse(Metric == "Species richness", "SR", "ab"))
    
    # Extract moderators for the first model (for showing/hiding inputs)
    first_model <- select_model(gm_df$Group[1], gm_df$Metric[1], input$AF_type)
    if (!is.null(first_model)) update_moderator_inputs(first_model, session)
    
    ################################################################################
    ### STEP 3: Update moderator options based on selected group_metrics
    ################################################################################
    
    # For levels of Climate and Soil_type across all selected models
    all_levels <- lapply(1:nrow(gm_df), function(i) {
      model <- select_model(gm_df$Group[i], gm_df$Metric[i], input$AF_type)
      if (is.null(model)) return(NULL)
      
      levels_list <- list()
      if ("Climate" %in% names(model$model_data)) levels_list$Climate <- levels(model$model_data$Climate)
      if ("Soil_type" %in% names(model$model_data)) levels_list$Soil_type <- levels(model$model_data$Soil_type)
      if ("Crop_group" %in% names(model$model_data)) levels_list$Crop_group <- levels(model$model_data$Crop_group)
      if ("grazing_dens" %in% names(model$model_data)) levels_list$grazing_dens <- levels(model$model_data$grazing_dens)
      
      levels_list
    })
    
    all_levels <- Filter(Negate(is.null), all_levels)
    if (length(all_levels) == 0) return()
    
    # Update UI with **common levels** across all selected models
    if ("Climate" %in% names(all_levels[[1]])) {
      common_climate <- Reduce(intersect, lapply(all_levels, `[[`, "Climate"))
      if (length(common_climate) > 0) updateSelectInput(session, "Climate",
                                                        choices = common_climate,
                                                        selected = common_climate[1])
    }
    if ("Soil_type" %in% names(all_levels[[1]])) {
      common_soil <- Reduce(intersect, lapply(all_levels, `[[`, "Soil_type"))
      if (length(common_soil) > 0) updateSelectInput(session, "Soil_type",
                                                     choices = common_soil,
                                                     selected = common_soil[1])
    }
    if ("Crop_group" %in% names(all_levels[[1]])) {
      common_crop <- Reduce(intersect, lapply(all_levels, `[[`, "Crop_group"))
      if (length(common_crop) > 0) updateSelectInput(session, "Crop_group",
                                                     choices = common_crop,
                                                     selected = common_crop[1])
    }
    if ("grazing_dens" %in% names(all_levels[[1]])) {
      common_grazing <- Reduce(intersect, lapply(all_levels, `[[`, "grazing_dens"))
      if (length(common_grazing) > 0) updateSelectInput(session, "grazing_dens",
                                                        choices = common_grazing,
                                                        selected = common_grazing[1])
    }
  }, ignoreInit = TRUE)
  
  ################################################################################
  # Enable/disable the Run button depending on AF_type selection
  # ##############################################################################
  observe({
    shinyjs::toggleState("run", condition = input$AF_type != "")
  })
  
  ################################################################################
  ### STEP 4: Predict when run is clicked
  ################################################################################
  observeEvent(input$run, {
    req(input$group_metric)
    
    split_vals <- strsplit(input$group_metric, ": ")
    valid_splits <- split_vals[sapply(split_vals, length) == 2]
    if (length(valid_splits) == 0) return()
    
    gm_split <- do.call(rbind, valid_splits)
    colnames(gm_split) <- c("Group", "Metric")
    gm_df <- as.data.frame(gm_split, stringsAsFactors = FALSE) %>%
      mutate(Group = trimws(Group),
             Metric = trimws(Metric),
             Metric = ifelse(Metric == "Species richness", "SR", "ab"))
    
    tree_sr_val <- if (input$Tree_SR == "5+") 5 else as.numeric(input$Tree_SR)
    
    # Build input dynamically based on moderators present in first model
    first_model <- select_model(gm_df$Group[1], gm_df$Metric[1], input$AF_type)
    moderators <- get_model_moderators(first_model)
    
    base_input <- list()
    if ("Climate" %in% moderators) base_input$Climate <- input$Climate
    if ("forestshrub_frac" %in% moderators) base_input$forestshrub_frac <- input$forestshrub_frac
    if ("log__forestshrub_frac" %in% moderators) base_input$log__forestshrub_frac <- log(input$forestshrub_frac+0.01) # Transformation
    if ("exp__forestshrub_frac" %in% moderators) base_input$exp__forestshrub_frac <- exp(input$forestshrub_frac/100) # Transformation
    if ("sqrt__forestshrub_frac" %in% moderators) base_input$sqrt__forestshrub_frac <- sqrt(input$forestshrub_frac) # Transformation
    if ("Soil_type" %in% moderators) base_input$Soil_type <- input$Soil_type
    if ("Shrub_layer" %in% moderators) base_input$Shrub_layer <- input$Shrub_layer
    if ("Tree.pol" %in% moderators) base_input$Tree.pol <- input$Tree.pol
    if ("Org.Conv" %in% moderators) base_input$Org.Conv <- input$Org.Conv
    if ("Tree_SR" %in% moderators) base_input$Tree_SR <- tree_sr_val
    if ("Alley.width" %in% moderators) base_input$Alley.width <- input$Alley.width
    if ("Crop_group" %in% moderators) base_input$Crop_group <- input$Crop_group
    if ("grazing_dens" %in% moderators) base_input$grazing_dens <- input$grazing_dens
    # ----------------------------------------------------------------
    # Run prediction for selected input (single age)
    # ----------------------------------------------------------------
    if ("Age" %in% moderators) base_input$Age <- input$Age
    if ("log__Age" %in% moderators) base_input$log__Age <- log(input$Age+0.01) # Transformation
    if ("exp__Age" %in% moderators) base_input$exp__Age <- exp(input$Age/100) # Transformation
    if ("sqrt__Age" %in% moderators) base_input$sqrt__Age <- sqrt(input$Age) # Transformation
  
    input_values <- data.frame(base_input, stringsAsFactors = FALSE)
    
    results_list <- lapply(1:nrow(gm_df), function(i) {
      model <- select_model(gm_df$Group[i], gm_df$Metric[i], input$AF_type)
      if (is.null(model)) return(NULL)
      
      predict_rma_mv_manual_biodivAF(
        model = model,
        Group = gm_df$Group[i],
        Metric = gm_df$Metric[i],
        AF_type = input$AF_type,
        input_values = input_values,
        moderators = names(input_values)
      )
    })
    
    results <- do.call(rbind, Filter(Negate(is.null), results_list))
    
    output$result_table <- renderTable({
      req(nrow(results) > 0)
      results %>%
        mutate(across(c(Percent_Pred, Percent_CI_Lower, Percent_CI_Upper), ~ round(., 1))) %>%
        select(Group, Metric, Model = AF_type, Percent_Pred, Percent_CI_Lower, Percent_CI_Upper)
    })
    
    # ----------------------------------------------------------------
    # For plotting curve along Age
    # ----------------------------------------------------------------
    age_range <- seq(1, 50, by = 1)  # Adjust as needed
    
    results_age_curve <- do.call(rbind, lapply(1:nrow(gm_df), function(i) {
      model <- select_model(gm_df$Group[i], gm_df$Metric[i], input$AF_type)
      if (is.null(model)) return(NULL)
      
      do.call(rbind, lapply(age_range, function(age_val) {
        iv <- input_values
        if ("Age" %in% names(iv)) iv$Age <- age_val # Transformation
        if ("log__Age" %in% names(iv)) iv$log__Age <- log(age_val+0.01) # Transformation
        if ("exp__Age" %in% names(iv)) iv$exp__Age <- exp(age_val/100) # Transformation
        if ("sqrt__Age" %in% names(iv)) iv$sqrt__Age <- sqrt(age_val) # Transformation
        
        pred <- predict_rma_mv_manual_biodivAF(
          model = model,
          Group = gm_df$Group[i],
          Metric = gm_df$Metric[i],
          AF_type = input$AF_type,
          input_values = iv,
          moderators = names(iv)
        )
        pred$Age <- age_val
        pred   # <- return the whole data.frame (needed to complete the second do.call)
      }))
    }))
    
    output$effect_plot <- renderPlot({
      req(nrow(results_age_curve) > 0)
      
      ggplot(results_age_curve, aes(x = Age, y = Percent_Pred, color = Group, fill = Group)) +
        geom_ribbon(aes(ymin = Percent_CI_Lower, ymax = Percent_CI_Upper), alpha = 0.2, color = NA) +
        geom_line(linewidth = 1) +
        geom_point(data = results %>% mutate(Age = input$Age), 
                   aes(x = Age, y = Percent_Pred, color = Group), size = 3, shape = 21, fill = "white") +
        facet_wrap(~Metric, scales = "free_y") +
        labs(title = "Predicted Biodiversity Gain Along Stand Age",
             x = "Stand Age", y = "Percent Biodiversity Gain") +
        theme_minimal() +
        theme(legend.position = "top")
    })
  })
}


# ------------------------- RUN APP -------------------------
shinyApp(ui = ui, server = server)


# Launch the app - CANNOT BE RUN FROM INSIDE
# shiny::runApp("BiodivAF")