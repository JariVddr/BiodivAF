library(shiny)
library(dplyr)
library(ggplot2)
library(metafor)
library(shinyjs)

# Source your model functions
app_dir <- app_dir <- getwd() # dirname(rstudioapi::getActiveDocumentContext()$path)
source(file.path(app_dir, "BiodivAF.R"))

order_groups <- c("Overall", "Arthropods", "Soil-related invertebrates", "Aboveground invertebrates", "Flying vertebrates", "Ground-dwelling vertebrates", 
                  "Autotrophs", "Pollinators", "Natural enemies", "Soil fauna", "Pest species", 
                  "Microbiota", "Aves", "Chiroptera", "Araneae", "Coleoptera", "Hymenoptera", "Gastropoda", "Lepidoptera", "Isopoda", 
                  "Butterflies", "Moths", "Bees", "Lumbricidae", "Hoverflies")
order_groups_new <- c("Overall", "Invertebrates", "Soil-related invertebrates", "Aboveground invertebrates", "Flying vertebrates", "Ground-dwelling vertebrates", 
                  "Plants & lichen", "Pollinators", "Natural enemies", "Soil biodiversity", "Pest species", 
                  "Microbiota", "Birds", "Bats", "Spiders", "Beetles", "Hymenoptera", "Gastropods", "Lepidoptera", "Woodlice", 
                  "Butterflies", "Moths", "Bees", "Earthworms", "Hoverflies")
color_groups <- c("black", "black", "black", "black", "#1D4999", "black", 
                  "#4A8522", "#D29500", "#1D4999", "black", "#BE5108", 
                  "black", "#1D4999", "#1D4999", "#1D4999", "#1D4999", "black", "#BE5108", "#D29500", "black", 
                  "#D29500", "#D29500", "#D29500", "black", "#D29500")

shannon_labels <- c("Extreme Low", "Low", "Medium", "High", "Extreme High")
shannon_values <- log(c(1, 2, 3, 5, 7))  # adjust to scale
# Default values for all moderators
default_values <- list(
  Climate = "Temperate",
  forestshrub_frac = 10,
  crop_frac = 50,
  landscape_shannon = log(shannon_values[2]),  # default at "Low"
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

# Styling intropage
intro_css <- tags$style(HTML("
#intro_page {
  max-width: 900px;
  margin: 40px auto;
  padding: 20px;
  text-align: center;
}

/* Make all normal text justified */
.intro-text p {
  text-align: justify;
  font-size: 14px;
  line-height: 1.6;
  margin-top: 0px;
  margin-bottom: 4px; /* very small spacing */
}

/* Make Intro and Bibliography text italic */
.intro-text .intro-section p,
.intro-text .biblio-section p {
  font-style: italic;
}

/* Adapt whitespace before/after section titles */
.intro-text h3 {
  margin-top: 20px;
  margin-bottom: 10px;
  text-align: left;
}

.logo-container {
  margin-top: 40px;
  display: flex;
  justify-content: center;
  gap: 40px;
  align-items: center;
}

.logo-container img {
  height: 90px;
  width: auto;
}

/* Bigger checkboxes in selection page */
.large-checkbox input[type=checkbox] {
  transform: scale(1.8);
  margin-right: 12px;
}
"))

icon_label <- function(group_name) {
  paste0(
    "<img src='Species_Icons/", group_name, ".png' ",
    "style='height:50px; vertical-align:middle; margin-right:8px;'>",
    "<span>", group_name, "</span>"
  )
}

# ----------------------------------------------------------
# --------------------------- UI ---------------------------
# ----------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  intro_css,
  
  # ================= INTRO PAGE =================
  div(id = "intro_page",
      div(class = "intro-text",
          h1("Agroforestry Biodiversity Gain Estimator (beta)"),
          
          div(class = "intro-section",
            h3("Introduction"),
            p(paste("This web application provides estimated biodiversity responses to agroforestry implementaion", 
                   "in European climatologic context based on existing case-studies and published scientific data.",
                   "More than 100 separate case-studies were included in the predictive modelling approach towards this tool.",
                   "For more information and the scientific background of this tool, please refer to the bibliography section below.",
                   collapse = " ")
              )),
          
          h3("Purpose"),
          p(paste( "The purpose of this tool to inform stakeholders like farmers, policy actors or anyone with interest in agroforestry", 
                   "about potential biodiversity benefits of agroforestry systems in comparison to conventional monoculture agricultural systems.",
                   "It allows users to explore how different agroforestry configurations and landscape contexts may influence biodiversity outcomes.",
                   "Ideally, this tool, in future formats, can support decision-making on agroforestry adoption and design to maximize biodiversity benefits.",
                   collapse = " ")),
          
          h3("Background"),
          p(paste( "During the data assemblage for this tool, biodiversity results were collected on site- and group specific scale", 
                   "to enable predictions on different agroforestry set-ups across different biodiversity groups.", 
                   collapse = " ")),
          p(paste( "The results are derived from published scientific data and reflect predicted", 
                   "percentage changes relative to conventional agricultural systems.", 
                   "These outputs should be interpreted as indicative trends rather than exact forecasts,", 
                   "as ecological responses are context-dependent and influenced by multiple interacting factors.", 
                   collapse = " " )),
          p(paste( "This tool integrates statistical models accounting for landscape structure,", 
                   "management practices, and system age. It is designed to support decision-making,", 
                   "research exploration and scenario comparison.", 
                   collapse = " ")),
          
          h3("Future options of development"),
          p(paste( "The work presented here is far from complete. The current tool, comparing agroforestry with baseline agriculture", 
                   "systems without trees, bases on 62 case-studies, while potentially (much) more data is already published, either internationally",
                   "or nationally. Adding new datasets to the framework behind this tool might enable better predictions, the fit of", 
                   "more model options and the integration of more system-dependent parameters.",
                   collapse = " ")),
          p(paste( "Current restrictions prohibit modelling for silvopastoral systems to large extent, but the integration of more studies,", 
                   "or the data provision of more site-specific results might enhance model requirements. Also the addition of more agroforestry",
                   "systems, like food forests, are potentially possible, if there is sufficient data to be analyzed.", 
                   collapse = " ")),
          p(paste( "Also comparison of agroforestry to other systems, like forests is a future possibility.", 
                   collapse = " ")),
          
          div(class = "biblio-section",
            h3("Bibliography"),
            p(paste("Vandendriessche J, Landuyt D, Eeraerts M, De Frenne P & Verheyen K (2025).", 
                    "Deliverable 3.4: Concept and establishment of a biodiversity tool for agroforestry applications in Europe", 
                    "DOI: (to be added upon publication).", 
                    collapse = " ")
              )),
          
          h3("Contact"),
          p("Jari.vandendriessche@ugent.be"),
          
          br(),
          actionButton("sel_biodiv_model", "Select biodiversity metrics", class = "btn-primary btn-lg")
      ),
      
      div(class = "logo-container",
          tags$img(src = "Logos/DigitAF.png"),
          tags$img(src = "Logos/UGent.png"),
          tags$img(src = "Logos/ForNaLab.png")
      )
  ),
  
  # ================= Model SELECTION PAGE =================
  
  hidden(
    div(id = "selection_page", class = "intro-text",
        titlePanel("Select biodiversity metrics"),
        
        h3("First select the agroforestry system type and the biodiversity indicators you wish to explore."),
        
        selectInput("AF_type", "Agroforestry System Type:",
                    choices = c("", "Agroforestry", "Silvoarable", "Silvopastoral"),
                    selected = ""),
        
        uiOutput("group_metric_ui"),
        uiOutput("predlow_disclaimer"),
        uiOutput("no_model_msg"),
        
        br(),
        
        fluidRow(
          column(6,
                 actionButton("back_intro", "← Back", class = "btn-default")
          ),
          column(6,
                 actionButton("go_tool", "Go to modelling tool →", class = "btn-primary")
          )
        )
    )
  ),
  
  
  
  # ================= TOOL PAGE =================
  hidden(
    div(id = "tool_page",
        titlePanel("Agroforestry Biodiversity Gain Estimator (beta)"),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("Climate", "Climate Type:",
                        choices = c("Temperate", "Mediterranean", "Boreal/Alpine")),
            
            selectInput("Soil_type", "Soil Type:",
                        choices = c("Loam", "Clay", "Sand")),
            
            sliderInput("forestshrub_frac", "Fraction of woody habitat in a 1km radius (%):",
                        min = 0, max = 100, value = default_values$forestshrub_frac),
            
            sliderInput("crop_frac", "Fraction of cropland area in a 1km radius (%):",
                        min = 0, max = 100, value = default_values$crop_frac),
            
            sliderInput("landscape_shannon",
              label = tagList("Landscape diversity:",
                tags$img(src = "Species_Icons/Shannon.png", height = "60px", 
                         style = "width:100%; max-width:400px; height:auto; display:block; margin-left:auto; margin-right:auto;")),
              min = min(shannon_values),
              max = max(shannon_values),
              value = default_values$landscape_shannon,
              step = 0.01, width = "100%",
              ticks = FALSE),
            uiOutput("landscape_shannon_labels"),
            
            selectInput("Tree_SR", "Woody species richness:",
                        choices = c("1", "2", "3", "4", "5+"),
                        selected = "1"),
            
            sliderInput("Alley.width", "Width between tree rows (m):",
                        min = 0, max = 60, value = default_values$Alley.width),
            
            checkboxInput("Shrub_layer", "Are shrub species present in the agroforestry element?", FALSE),
            checkboxInput("Tree.pol", "Are there agroforestry species that need pollinators?", TRUE),
            
            sliderInput("Age", "Age of the agroforestry system (years):",
                        min = 0.5, max = 60, value = default_values$Age),
            
            # --- Conditional moderators ---
            conditionalPanel(condition = "input.AF_type == 'Silvoarable'",
                             selectInput("Crop_group", "Crop type applied:", 
                                         choices = c("Cereals", "Corn", "Vegetables/Mass-flowering crops", 
                                                     "Grass/Ley", "Vegetables", "Orchard"), 
                                         selected = default_values$Crop_group)),
            
            conditionalPanel(condition = "input.AF_type == 'Silvoarable'",
                             selectInput("Org.Conv", "Organic or Conventional management:",
                                         choices = c("Org", "Conv"), 
                                         selected = default_values$Org.Conv)),
            
            conditionalPanel(condition = "input.AF_type == 'Silvopastoral'",
                             selectInput("grazing_dens", "Grazing density on the site:", 
                                         choices = c("high", "medium", "low", "very low"), 
                                         selected = default_values$grazing_dens)),
            
            fluidRow(
              column(6,
                     actionButton("back_selection", "← Back", class = "btn-default")
              )),
            actionButton("run", "Run Model", class = "btn-primary")
          ),
          
          mainPanel(
            tableOutput("result_table"),
            plotOutput("effect_plot", height = "700px")
          )
        )
    )
  )
)

# ----------------------------------------------------------
# ------------------------- SERVER -------------------------
# ----------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$sel_biodiv_model, {
    hide("intro_page")
    show("selection_page")
  })
  
  observeEvent(input$back_intro, {
    hide("selection_page")
    show("intro_page")
    
    # Clear selections when going back
    filtered_models(NULL)
    output$group_metric_ui <- renderUI(NULL)
    output$predlow_disclaimer <- renderUI(NULL)
    output$no_model_msg <- renderUI(NULL)
  })
  
  observeEvent(input$go_tool, {
    hide("selection_page")
    show("tool_page")
  })
  
  observeEvent(input$back_selection, {
    hide("tool_page")
    show("selection_page")
  })
  
  filtered_models <- reactiveVal(NULL)
  
  group_order <- setNames(seq_along(order_groups), order_groups)
  group_order_new <- setNames(seq_along(order_groups_new), order_groups_new)
  group_colors <- setNames(color_groups, order_groups_new)
  
  # --------------------- Helper Functions ---------------------
  update_moderator_inputs <- function(model, session) {
    moderators <- get_model_moderators(model)
    # Base moderators
    shinyjs::toggle("Climate", condition = "Climate" %in% moderators)
    shinyjs::toggle("Soil_type", condition = "Soil_type" %in% moderators)
    shinyjs::toggle("forestshrub_frac", 
                    condition = any(c("forestshrub_frac", "exp__forestshrub_frac", "log__forestshrub_frac", "sqrt__forestshrub_frac",
                                      "expmin__forestshrub_frac", "minlog__forestshrub_frac", "reversed__forestshrub_frac",
                                      "reversedexp__forestshrub_frac", "minexpmin__forestshrub_frac") %in% moderators))
    shinyjs::toggle("crop_frac", 
                    condition = any(c("crop_frac", "exp__crop_frac", "log__crop_frac", "sqrt__crop_frac",
                                      "expmin__crop_frac", "minlog__crop_frac", "reversed__crop_frac",
                                      "reversedexp__crop_frac", "minexpmin__crop_frac") %in% moderators))
    shinyjs::toggle("landscape_shannon", 
                    condition = any(c("landscape_shannon", "exp__landscape_shannon", "log__landscape_shannon", "sqrt__landscape_shannon",
                                      "expmin__landscape_shannon", "minlog__landscape_shannon", "reversed__landscape_shannon",
                                      "reversedexp__landscape_shannon", "minexpmin__landscape_shannon") %in% moderators))
    shinyjs::toggle("Tree_SR", condition = "Tree_SR" %in% moderators)
    shinyjs::toggle("Alley.width", 
                    condition = any(c("Alley.width", "exp__Alley.width", "log__Alley.width", "sqrt__Alley.width",
                                      "expmin__Alley.width", "minlog__Alley.width", "reversed__Alley.width",
                                      "reversedexp__Alley.width", "minexpmin__Alley.width") %in% moderators))
    shinyjs::toggle("Shrub_layer", condition = "Shrub_layer" %in% moderators)
    shinyjs::toggle("Tree.pol", condition = "Tree.pol" %in% moderators)
    shinyjs::toggle("Age",
                    condition = any(c("Age", "exp__Age", "log__Age", "sqrt__Age",
                                      "expmin__Age", "minlog__Age", "reversed__Age",
                                      "reversedexp__Age", "minexpmin__Age") %in% moderators))
    # Conditional moderators
    shinyjs::toggle("Crop_group", condition = "Crop_group" %in% moderators)
    shinyjs::toggle("Org.Conv", condition = "Org.Conv" %in% moderators)
    shinyjs::toggle("grazing_dens", condition = "grazing_dens" %in% moderators)
  }
  
  ################################################################################
  ### STEP 1: Update group-metric choices based on AF_type
  ################################################################################
  observeEvent(input$AF_type, {
    
    # Load model files
    model_base_dir <- "D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/BiodivAF/model_outputs"
    model_files_norm <- list.files(
      model_base_dir, pattern = "^model_.*\\.rds$", full.names = FALSE
    )
    model_files_predlow <- list.files(
      model_base_dir, pattern = "^modelpredlow_.*\\.rds$", full.names = FALSE
    )
    
    # If nothing available
    if (length(model_files_norm) + length(model_files_predlow) == 0) {
      updateCheckboxGroupInput(session, "group_metric",
                               choices = character(0), selected = character(0))
      return()
    }
    
    # ---- Parse regular models ----
    parsed_norm <- strcapture(
      "^model_([^_]+)_([^_]+)_([^_]+)\\.rds$",
      model_files_norm,
      proto = list(Group = character(), Metric = character(), AF_type = character())
    ) %>% mutate(predlow = FALSE)
    
    # ---- Parse predlow models ----
    parsed_predlow <- strcapture(
      "^modelpredlow_([^_]+)_([^_]+)_([^_]+)\\.rds$",
      model_files_predlow,
      proto = list(Group = character(), Metric = character(), AF_type = character())
    ) %>% mutate(predlow = TRUE)
    
    # ---- Combine ----
    parsed <- bind_rows(parsed_norm, parsed_predlow)
    
    # Filter for correct AF_type + correct metrics
    filtered <- parsed %>%
      filter(AF_type == input$AF_type & Metric %in% c("totSR", "SR", "ab"))
    filtered <- filtered[order(group_order[filtered$Group]), ]
    
    # Store filtered in reactiveVal
    filtered_models(filtered)
    
    if (nrow(filtered) == 0) {
      # Clear icon UI
      output$group_metric_ui <- renderUI(NULL)
      # Clear disclaimer
      output$predlow_disclaimer <- renderUI(NULL)
      # Show message
      output$no_model_msg <- renderUI({
        tags$span("No models available for this system type.",
                  style = "color:red; font-weight:bold;")
      })
      return()
    } else {
      output$no_model_msg <- renderUI({ NULL })
    }
    
    # ---- Update checkbox input ----
    output$group_metric_ui <- renderUI({
      
      req(filtered)  # from your observeEvent
      
      filtered <- filtered[order(group_order[filtered$Group]), ]
      
      checkbox_list <- lapply(seq_len(nrow(filtered)), function(i) {
        
        group  <- filtered$Group[i]
        metric <- filtered$Metric[i]
        predlow <- filtered$predlow[i]
        
        # Store label text in hidden span
        label_text <- paste0(group, ": ", metric, ifelse(predlow, " *", ""))
        
        tags$div(
          class = "large-checkbox",
          style = "display:flex; align-items:center; gap:14px; margin-bottom:18px;",
          
          # Icon for group
          tags$img(
            src = paste0("Species_Icons/", group, ".png"),
            height = "150px"
          ),
          
          # Icon for metric
          tags$img(
            src = paste0("Species_Icons/", metric, ".png"),
            height = "150px"
          ),
          
          # Icon for error
          tags$img(
            src = paste0("Species_Icons/", predlow, ".png"),
            height = "150px"
          ),
          
          checkboxInput(
            inputId = paste0("group_metric_", i),
            label = HTML(paste0("<span style='display:none;'>", label_text, "</span>")),
            value = FALSE # or i == 1
          )
          
        )
      })
      
      tagList(checkbox_list)
    })
    
    # ---- Add grey disclaimer if predlow present ----
    if (any(filtered$predlow)) {
      output$predlow_disclaimer <- renderUI({
        tags$div(
          style = "color: grey; font-size: 0.9em; margin-top: 4px;",
          "* Models marked with an asterisk showed low predictive performance and should be interpreted with caution."
        )
      })
    } else {
      output$predlow_disclaimer <- renderUI(NULL)
    }
    
  }, ignoreInit = TRUE)
  
  ################################################################################
  ### STEP 2: Update moderators according to model structure
  ################################################################################
  observeEvent({
    filtered <- filtered_models()    # <- get the latest filtered table
    req(!is.null(filtered), nrow(filtered) > 0)  # only run if models exist
    # safe access to dynamic checkboxes
    sapply(seq_len(nrow(filtered)), function(i) isTRUE(input[[paste0("group_metric_", i)]]))
  }, {
    filtered <- filtered_models()
    
    req(!is.null(filtered), nrow(filtered) > 0)  # only run if models exist
    
    # Get indices of selected checkboxes safely
    selected_idx <- which(sapply(seq_len(nrow(filtered)), function(i) isTRUE(input[[paste0("group_metric_", i)]])))
    if (length(selected_idx) == 0) return()  # exit safely if nothing selected
    
    # Build gm_df from the selected rows
    gm_df <- filtered[selected_idx, c("Group", "Metric")]
    gm_df$Metric <- gsub("\\*", "", gm_df$Metric)
    gm_df$Metric <- trimws(gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "SR", "SR", gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "ab", "ab", gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "totSR", "totSR", gm_df$Metric)
    
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
  
  # Render labels below the slider
  output$landscape_shannon_labels <- renderUI({
    div(
      style = "display:flex; justify-content: space-between; font-size: 0.85em; margin-top: 4px;",
      lapply(shannon_labels, function(lbl) tags$span(lbl))
    )
  })
  
  ################################################################################
  # Enable/disable the go to tool button model checkbox selection
  ################################################################################
  observe({
    filtered <- filtered_models()
    
    # Check if at least one checkbox is TRUE
    checkboxes_selected <- if (!is.null(filtered) && nrow(filtered) > 0) {
      any(sapply(seq_len(nrow(filtered)), function(i) isTRUE(input[[paste0("group_metric_", i)]])))
    } else FALSE
    
    shinyjs::toggleState("go_tool", condition = !is.null(input$AF_type) && input$AF_type != "" && checkboxes_selected)
  })
  
  ################################################################################
  ### STEP 4: Predict when run is clicked
  ################################################################################
  observeEvent(input$run, {
    filtered <- filtered_models()
    
    req(!is.null(filtered), nrow(filtered) > 0)  # only run if models exist
    
    # Get indices of selected checkboxes
    selected_idx <- which(sapply(seq_len(nrow(filtered)), function(i) input[[paste0("group_metric_", i)]]))
    
    # Exit if nothing selected
    if (length(selected_idx) == 0) return()
    
    # Build gm_df from the selected rows
    gm_df <- filtered[selected_idx, c("Group", "Metric")]
    gm_df$Metric <- gsub("\\*", "", gm_df$Metric)
    gm_df$Metric <- trimws(gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "Species richness (plot)", "SR", gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "Abundance", "ab", gm_df$Metric)
    gm_df$Metric <- ifelse(gm_df$Metric == "Species richness (field)", "totSR", gm_df$Metric)
    
    tree_sr_val <- if (input$Tree_SR == "5+") 5 else as.numeric(input$Tree_SR)
    
    # toggle this to TRUE to see debug messages in the console
    verbose_debug <- FALSE
    
    # ---------------------------
    # Predict for the single state (summary table)
    # ---------------------------
    results_list <- lapply(1:nrow(gm_df), function(i) {
      model <- select_model(gm_df$Group[i], gm_df$Metric[i], input$AF_type)
      if (is.null(model)) return(NULL)
      
      # --- model-specific moderators & base input (important) ---
      moderators_model <- get_model_moderators(model)
      
      # Build input dynamically based on moderators
      base_input_model <- list()
      if ("Climate" %in% moderators_model) base_input_model$Climate <- input$Climate
      if (any(c("forestshrub_frac","exp__forestshrub_frac","log__forestshrub_frac","sqrt__forestshrub_frac",
                "expmin__forestshrub_frac","minlog__forestshrub_frac","reversed__forestshrub_frac",
                "reversedexp__forestshrub_frac","minexpmin__forestshrub_frac") %in% moderators_model)) {
        base_input_model$forestshrub_frac <- input$forestshrub_frac
        base_input_model$log__forestshrub_frac <- log(input$forestshrub_frac + 0.01)
        base_input_model$exp__forestshrub_frac <- exp(input$forestshrub_frac / 100)
        base_input_model$sqrt__forestshrub_frac <- sqrt(input$forestshrub_frac)
        base_input_model$expmin__forestshrub_frac <- exp(-input$forestshrub_frac / 100)
        base_input_model$minlog__forestshrub_frac <- -log(input$forestshrub_frac + 0.01)
        base_input_model$reversed__forestshrub_frac <- 1 / (1 + input$forestshrub_frac)
        base_input_model$reversedexp__forestshrub_frac <- 1 / (1 + exp(input$forestshrub_frac))
        base_input_model$minexpmin__forestshrub_frac <- -exp(-input$forestshrub_frac / 100)
      }
      if (any(c("crop_frac","exp__crop_frac","log__crop_frac","sqrt__crop_frac",
                "expmin__crop_frac","minlog__crop_frac","reversed__crop_frac",
                "reversedexp__crop_frac","minexpmin__crop_frac") %in% moderators_model)) {
        base_input_model$crop_frac <- input$crop_frac
        base_input_model$log__crop_frac <- log(input$crop_frac + 0.01)
        base_input_model$exp__crop_frac <- exp(input$crop_frac / 100)
        base_input_model$sqrt__crop_frac <- sqrt(input$crop_frac)
        base_input_model$expmin__crop_frac <- exp(-input$crop_frac / 100)
        base_input_model$minlog__crop_frac <- -log(input$crop_frac + 0.01)
        base_input_model$reversed__crop_frac <- 1 / (1 + input$crop_frac)
        base_input_model$reversedexp__crop_frac <- 1 / (1 + exp(input$crop_frac))
        base_input_model$minexpmin__crop_frac <- -exp(-input$crop_frac / 100)
      }
      if (any(c("landscape_shannon","exp__landscape_shannon","log__landscape_shannon","sqrt__landscape_shannon",
                "expmin__landscape_shannon","minlog__landscape_shannon","reversed__landscape_shannon",
                "reversedexp__landscape_shannon","minexpmin__landscape_shannon") %in% moderators_model)) {
        base_input_model$landscape_shannon <- input$landscape_shannon
        base_input_model$log__landscape_shannon <- log(input$landscape_shannon + 1e-8)
        base_input_model$exp__landscape_shannon <- exp(input$landscape_shannon)
        base_input_model$sqrt__landscape_shannon <- sqrt(input$landscape_shannon)
        base_input_model$expmin__landscape_shannon <- exp(-input$landscape_shannon)
        base_input_model$minlog__landscape_shannon <- -log(input$landscape_shannon + 1e-8)
        base_input_model$reversed__landscape_shannon <- 1 / (1 + input$landscape_shannon)
        base_input_model$reversedexp__landscape_shannon <- 1 / (1 + exp(input$landscape_shannon))
        base_input_model$minexpmin__landscape_shannon <- -exp(-input$landscape_shannon)
      }
      if ("Soil_type" %in% moderators_model) base_input_model$Soil_type <- input$Soil_type
      if ("Shrub_layer" %in% moderators_model) base_input_model$Shrub_layer <- input$Shrub_layer
      if ("Tree.pol" %in% moderators_model) base_input_model$Tree.pol <- input$Tree.pol
      if ("Tree_SR" %in% moderators_model) base_input_model$Tree_SR <- tree_sr_val
      if (any(c("Alley.width","exp__Alley.width","log__Alley.width","sqrt__Alley.width",
                "expmin__Alley.width","minlog__Alley.width","reversed__Alley.width",
                "reversedexp__Alley.width","minexpmin__Alley.width") %in% moderators_model)) {
        base_input_model$Alley.width <- input$Alley.width
        base_input_model$log__Alley.width <- log(input$Alley.width + 0.01)
        base_input_model$exp__Alley.width <- exp(input$Alley.width / 100)
        base_input_model$sqrt__Alley.width <- sqrt(input$Alley.width)
        base_input_model$expmin__Alley.width <- exp(-input$Alley.width / 100)
        base_input_model$minlog__Alley.width <- -log(input$Alley.width + 0.01)
        base_input_model$reversed__Alley.width <- 1 / (1 + input$Alley.width)
        base_input_model$reversedexp__Alley.width <- 1 / (1 + exp(input$Alley.width))
        base_input_model$minexpmin__Alley.width <- -exp(-input$Alley.width / 100)
      }
      if ("Crop_group" %in% moderators_model) base_input_model$Crop_group <- input$Crop_group
      if ("Org.Conv" %in% moderators_model) base_input_model$Org.Conv <- input$Org.Conv
      if ("grazing_dens" %in% moderators_model) base_input_model$grazing_dens <- input$grazing_dens
      # ----------------------------------------------------------------
      # Run prediction for selected input (single age)
      # ----------------------------------------------------------------
      if (any(c("Age","exp__Age","log__Age","sqrt__Age","expmin__Age","minlog__Age",
                "reversed__Age","reversedexp__Age","minexpmin__Age") %in% moderators_model)) {
        base_input_model$Age <- input$Age
        base_input_model$log__Age <- log(input$Age + 0.01)
        base_input_model$exp__Age <- exp(input$Age / 100)
        base_input_model$sqrt__Age <- sqrt(input$Age)
        base_input_model$expmin__Age <- exp(-input$Age / 100)
        base_input_model$minlog__Age <- -log(input$Age + 0.01)
        base_input_model$reversed__Age <- 1 / (1 + input$Age)
        base_input_model$reversedexp__Age <- 1 / (1 + exp(input$Age))
        base_input_model$minexpmin__Age <- -exp(-input$Age / 100)
      }
      # ----------------------------------------------------------------
      # Ensure AF_plottype placeholder if required
      # ----------------------------------------------------------------
      if ("AF_plottype" %in% moderators_model) {
        levs_model <- levels(model$model_data$AF_plottype)
        expanded_inputs_model <- lapply(levs_model, function(lvl) {
          tmp <- as.data.frame(base_input_model, stringsAsFactors = FALSE)
          tmp$AF_plottype <- factor(lvl, levels = levs_model)
          tmp
        })
        w_model <- rep(1 / length(levs_model), length(levs_model))
      } else {
        expanded_inputs_model <- list(as.data.frame(base_input_model, stringsAsFactors = FALSE))
        w_model <- 1
      }
      
      if (verbose_debug) {
        message("Model: ", gm_df$Group[i], "_", gm_df$Metric[i], " moderators: ", paste(moderators_model, collapse = ", "))
        if ("AF_plottype" %in% moderators_model) message("  AF_plottype levels: ", paste(levs_model, collapse = ", "))
      }
      
      # Predict for each expanded input for this model
      pred_each <- Map(function(iv, weight) {
        out <- predict_rma_mv_manual_biodivAF(
          model = model,
          Group = gm_df$Group[i],
          Metric = gm_df$Metric[i],
          AF_type = input$AF_type,
          input_values = iv,
          moderators = names(iv)
        )
        out$weight <- weight
        out
      }, expanded_inputs_model, w_model)
      
      pred_df <- bind_rows(pred_each)
      
      # Weighted average across expanded inputs for this model
      pred_df %>%
        summarise(
          Group = first(Group),
          Metric = first(Metric),
          AF_type = first(AF_type),
          Percent_Pred = sum(Percent_Pred * weight),
          Percent_CI_Lower = sum(Percent_CI_Lower * weight),
          Percent_CI_Upper = sum(Percent_CI_Upper * weight)
        )
    })
    
    results <- do.call(rbind, Filter(Negate(is.null), results_list))
    
    # HTML table
    output$result_table <- renderUI({
      req(nrow(results) > 0)
      
      results_clean <- results %>%
        mutate(
          Group = dplyr::case_when(
            Group == "Araneae" ~ "Spiders",
            Group == "Soil fauna" ~ "Soil biodiversity",
            Group == "Arthropods" ~ "Invertebrates",
            Group == "Autotrophs" ~ "Plants & lichens",
            Group == "Isopoda" ~ "Woodlice",
            Group == "Aves" ~ "Birds",
            Group == "Chiroptera" ~ "Bats",
            Group == "Gastropods" ~ "Gastropods",
            Group == "Coleoptera" ~ "Beetles",
            TRUE ~ Group
          ),
          Metric = dplyr::case_when(
            Metric == "SR" ~ "Mean plot richness",
            Metric == "ab" ~ "Abundance/activity",
            Metric == "totSR" ~ "Total richness",
            TRUE ~ Metric
          )
        )
      
      # ---- ORDER ----
      results_clean <- results_clean[order(group_order_new[results_clean$Group]), ]
      
      # Round values
      results_clean <- results_clean %>%
        mutate(across(c(Percent_Pred, Percent_CI_Lower, Percent_CI_Upper), ~ round(., 1)))
      
      # ---- HTML TABLE WITH COLORS ----
      table_rows <- apply(results_clean, 1, function(row) {
        grp <- row["Group"]
        color <- group_colors[grp]
        
        tags$tr(
          style = paste0("background-color:", color, "20;"),  # transparent tint
          tags$td(row["Group"]),
          tags$td(row["Metric"]),
          tags$td(row["AF_type"]),
          tags$td(row["Percent_Pred"]),
          tags$td(row["Percent_CI_Lower"]),
          tags$td(row["Percent_CI_Upper"])
        )
      })
      
      tags$table(
        style = "width:100%; border-collapse:collapse;",
        tags$thead(
          tags$tr(
            tags$th("Group"), tags$th("Metric"), tags$th("Agroforestry type"),
            tags$th("Prediction (%)"), tags$th("lower confidence (%)"), tags$th("upper confidence (%)")
          )
        ),
        tags$tbody(table_rows)
      )
    })
    
    # ----------------------------------------------------------------
    # For plotting curve along Age
    # ----------------------------------------------------------------
    age_range <- seq(5, 60, by = 1)  # Adjust as needed
    
    results_age_curve <- do.call(rbind, lapply(1:nrow(gm_df), function(i) {
      model <- select_model(gm_df$Group[i], gm_df$Metric[i], input$AF_type)
      if (is.null(model)) return(NULL)
      
      moderators_model <- get_model_moderators(model)
      
      # Build input dynamically based on moderators
      base_template <- list()
      if ("Climate" %in% moderators_model) base_template$Climate <- input$Climate
      if (any(c("forestshrub_frac","exp__forestshrub_frac","log__forestshrub_frac","sqrt__forestshrub_frac",
                "expmin__forestshrub_frac","minlog__forestshrub_frac","reversed__forestshrub_frac",
                "reversedexp__forestshrub_frac","minexpmin__forestshrub_frac") %in% moderators_model)) {
        base_template$forestshrub_frac <- input$forestshrub_frac
        base_template$log__forestshrub_frac <- log(input$forestshrub_frac + 0.01)
        base_template$exp__forestshrub_frac <- exp(input$forestshrub_frac / 100)
        base_template$sqrt__forestshrub_frac <- sqrt(input$forestshrub_frac)
        base_template$expmin__forestshrub_frac <- exp(-input$forestshrub_frac / 100)
        base_template$minlog__forestshrub_frac <- -log(input$forestshrub_frac + 0.01)
        base_template$reversed__forestshrub_frac <- 1 / (1 + input$forestshrub_frac)
        base_template$reversedexp__forestshrub_frac <- 1 / (1 + exp(input$forestshrub_frac))
        base_template$minexpmin__forestshrub_frac <- -exp(-input$forestshrub_frac / 100)
      }
      if (any(c("crop_frac","exp__crop_frac","log__crop_frac","sqrt__crop_frac",
                "expmin__crop_frac","minlog__crop_frac","reversed__crop_frac",
                "reversedexp__crop_frac","minexpmin__crop_frac") %in% moderators_model)) {
        base_template$crop_frac <- input$crop_frac
        base_template$log__crop_frac <- log(input$crop_frac + 0.01)
        base_template$exp__crop_frac <- exp(input$crop_frac / 100)
        base_template$sqrt__crop_frac <- sqrt(input$crop_frac)
        base_template$expmin__crop_frac <- exp(-input$crop_frac / 100)
        base_template$minlog__crop_frac <- -log(input$crop_frac + 0.01)
        base_template$reversed__crop_frac <- 1 / (1 + input$crop_frac)
        base_template$reversedexp__crop_frac <- 1 / (1 + exp(input$crop_frac))
        base_template$minexpmin__crop_frac <- -exp(-input$crop_frac / 100)
      }
      if (any(c("landscape_shannon","exp__landscape_shannon","log__landscape_shannon","sqrt__landscape_shannon",
                "expmin__landscape_shannon","minlog__landscape_shannon","reversed__landscape_shannon",
                "reversedexp__landscape_shannon","minexpmin__landscape_shannon") %in% moderators_model)) {
        base_template$landscape_shannon <- input$landscape_shannon
        base_template$log__landscape_shannon <- log(input$landscape_shannon + 1e-8)
        base_template$exp__landscape_shannon <- exp(input$landscape_shannon)
        base_template$sqrt__landscape_shannon <- sqrt(input$landscape_shannon)
        base_template$expmin__landscape_shannon <- exp(-input$landscape_shannon)
        base_template$minlog__landscape_shannon <- -log(input$landscape_shannon + 1e-8)
        base_template$reversed__landscape_shannon <- 1 / (1 + input$landscape_shannon)
        base_template$reversedexp__landscape_shannon <- 1 / (1 + exp(input$landscape_shannon))
        base_template$minexpmin__landscape_shannon <- -exp(-input$landscape_shannon)
      }
      if ("Soil_type" %in% moderators_model) base_template$Soil_type <- input$Soil_type
      if ("Shrub_layer" %in% moderators_model) base_template$Shrub_layer <- input$Shrub_layer
      if ("Tree.pol" %in% moderators_model) base_template$Tree.pol <- input$Tree.pol
      if ("Tree_SR" %in% moderators_model) base_template$Tree_SR <- tree_sr_val
      if (any(c("Alley.width","exp__Alley.width","log__Alley.width","sqrt__Alley.width",
                "expmin__Alley.width","minlog__Alley.width","reversed__Alley.width",
                "reversedexp__Alley.width","minexpmin__Alley.width") %in% moderators_model)) {
        base_template$Alley.width <- input$Alley.width
        base_template$log__Alley.width <- log(input$Alley.width + 0.01)
        base_template$exp__Alley.width <- exp(input$Alley.width / 100)
        base_template$sqrt__Alley.width <- sqrt(input$Alley.width)
        base_template$expmin__Alley.width <- exp(-input$Alley.width / 100)
        base_template$minlog__Alley.width <- -log(input$Alley.width + 0.01)
        base_template$reversed__Alley.width <- 1 / (1 + input$Alley.width)
        base_template$reversedexp__Alley.width <- 1 / (1 + exp(input$Alley.width))
        base_template$minexpmin__Alley.width <- -exp(-input$Alley.width / 100)
      }
      if ("Crop_group" %in% moderators_model) base_template$Crop_group <- input$Crop_group
      if ("Org.Conv" %in% moderators_model) base_template$Org.Conv <- input$Org.Conv
      if ("grazing_dens" %in% moderators_model) base_template$grazing_dens <- input$grazing_dens
      # ----------------------------------------------------------------
      # Ensure AF_plottype placeholder if required
      # ----------------------------------------------------------------
      if ("AF_plottype" %in% moderators_model) {
        levs_model <- levels(model$model_data$AF_plottype)
        expanded_inputs_model <- lapply(levs_model, function(lvl) {
          tmp <- as.data.frame(base_template, stringsAsFactors = FALSE)
          tmp$AF_plottype <- factor(lvl, levels = levs_model)
          tmp
        })
        w_model <- rep(1 / length(levs_model), length(levs_model))
      } else {
        expanded_inputs_model <- list(as.data.frame(base_template, stringsAsFactors = FALSE))
        w_model <- 1
      }
      
      # For each age, build inputs (with transforms) and predict
      per_model_curve <- do.call(rbind, lapply(age_range, function(age_val) {
        # start from base template and add age transforms only if those exist in the model
        iv_base <- as.data.frame(base_template, stringsAsFactors = FALSE)
        
        if (any(c("Age","log__Age","exp__Age","sqrt__Age","expmin__Age","minlog__Age",
                  "reversed__Age","reversedexp__Age","minexpmin__Age") %in% moderators_model)) {
          iv_base$Age <- age_val
          iv_base$log__Age <- log(age_val + 0.01)
          iv_base$exp__Age <- exp(age_val / 100)
          iv_base$sqrt__Age <- sqrt(age_val)
          iv_base$expmin__Age <- exp(-age_val / 100)
          iv_base$minlog__Age <- -log(age_val + 0.01)
          iv_base$reversed__Age <- 1 / (1 + age_val)
          iv_base$reversedexp__Age <- 1 / (1 + exp(age_val))
          iv_base$minexpmin__Age <- -exp(-age_val / 100)
        }
        
        # expand AF_plottype for this iv if needed
        if (!is.null(levs_model)) {
          expanded_inputs_model <- lapply(levs_model, function(lvl) {
            tmp <- iv_base
            tmp$AF_plottype <- factor(lvl, levels = levs_model)
            tmp
          })
        } else {
          expanded_inputs_model <- list(iv_base)
        }
        
        # Predict for each expanded input (per model)
        pred_each <- Map(function(iv, weight) {
          out <- predict_rma_mv_manual_biodivAF(
            model = model,
            Group = gm_df$Group[i],
            Metric = gm_df$Metric[i],
            AF_type = input$AF_type,
            input_values = iv,
            moderators = names(iv)
          )
          out$weight <- weight
          out
        }, expanded_inputs_model, w_model)
        
        pred <- bind_rows(pred_each)
        
        pred %>%
          summarise(
            Group = first(Group),
            Metric = first(Metric),
            AF_type = first(AF_type),
            Percent_Pred = sum(Percent_Pred * weight),
            Percent_CI_Lower = sum(Percent_CI_Lower * weight),
            Percent_CI_Upper = sum(Percent_CI_Upper * weight)
          ) %>%
          mutate(Age = age_val)
      }))
      
      per_model_curve
    }))
    
    output$effect_plot <- renderPlot({
      req(nrow(results_age_curve) > 0)
      
      # Apply the same naming mutations for plotting
      results_age_curve <- results_age_curve %>%
        mutate(
          Group = dplyr::case_when(
            Group == "Araneae" ~ "Spiders",
            Group == "Soil fauna" ~ "Soil biodiversity",
            Group == "Arthropods" ~ "Invertebrates",
            Group == "Autotrophs" ~ "Plants & lichens",
            Group == "Isopoda" ~ "Woodlice",
            Group == "Aves" ~ "Birds",
            Group == "Chiroptera" ~ "Bats",
            Group == "Gastropoda" ~ "Gastropods",
            Group == "Coleoptera" ~ "Beetles",
            TRUE ~ Group),
          Metric = dplyr::case_when(
            Metric == "SR" ~ "Mean plot richness",
            Metric == "ab" ~ "Abundance/activity",
            Metric == "totSR" ~ "Total richness",
            TRUE ~ Metric))
      
      results_plotpoints <- results %>%
        mutate(
          Group = dplyr::case_when(
            Group == "Araneae" ~ "Spiders",
            Group == "Soil fauna" ~ "Soil biodiversity",
            Group == "Arthropods" ~ "Invertebrates",
            Group == "Autotrophs" ~ "Plants & lichens",
            Group == "Isopoda" ~ "Woodlice",
            Group == "Aves" ~ "Birds",
            Group == "Chiroptera" ~ "Bats",
            Group == "Gastropoda" ~ "Gastropods",
            Group == "Coleoptera" ~ "Beetles",
            TRUE ~ Group),
          Metric = dplyr::case_when(
            Metric == "SR" ~ "Mean plot richness",
            Metric == "ab" ~ "Abundance/activity",
            Metric == "totSR" ~ "Total richness",
            TRUE ~ Metric),
          Age = input$Age)
      
      ggplot(results_age_curve, aes(x = Age, y = Percent_Pred, color = Group, fill = Group)) +
        geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 1.2) +
        geom_ribbon(aes(ymin = Percent_CI_Lower, ymax = Percent_CI_Upper), alpha = 0.2, color = NA) +
        geom_line(linewidth = 1) +
        geom_point(data = results_plotpoints, 
                   aes(x = Age, y = Percent_Pred), size = 3, shape = 21, stroke = 1, 
                   fill = group_colors[results_plotpoints$Group],
                   color = group_colors[results_plotpoints$Group],
                   inherit.aes = FALSE,
                   show.legend = FALSE) +
        coord_cartesian(ylim = c(-100, 400)) +
        facet_wrap(~Metric, ncol = 1, scales = "free_y") +
        labs(title = " ",
             x = "Stand Age (years)", y = "Biodiversity Gain (%)") +
        theme_bw(base_size = 14) +
        theme(legend.position = "top",
              legend.title = element_text(size = 16),
              legend.text  = element_text(size = 14),
              axis.title   = element_text(size = 16),
              axis.text    = element_text(size = 14),
              strip.text   = element_text(size = 16, face = "bold"),   # facet labels
              plot.title   = element_text(size = 18, face = "bold", hjust = 0.5))
    })
  })
}

# -----------------------------------------------------------
# ------------------------- RUN APP -------------------------
# -----------------------------------------------------------
shinyApp(ui = ui, server = server)


# Launch the app - CANNOT BE RUN FROM INSIDE
# shiny::runApp("BiodivAF")