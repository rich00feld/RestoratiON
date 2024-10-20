# Code to install packages ----
# install.packages('rsconnect')
# install.packages("sf")
# install.packages("terra")
# install.packages("geodiv")
# install.packages("raster")
# install.packages("landscapemetrics")
# install.packages("grainscape")
# install.packages("RStoolbox")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("pals")
# install.packages("viridis")
# install.packages("shiny")
# install.packages("shiny.semantic")
# install.packages("shinyjs")
# install.packages("shinythemes")
# install.packages("DT")
# install.packages("rasterVis")
# install.packages("rstudioapi")
# install.packages("ggplot2")
# install.packages("future")
# install.packages("furrr")
# install.packages("stringr")
# install.packages("leaflet")
# install.packages("leaflet.extras")
# install.packages("devtools")
# library(devtools)
# options(download.file.method = "wininet")
# install_github("CRAN/rgdal")
# install.packages("RStoolbox")
# install.packages("leaflet.providers")
# install.packages("stars")


# Loading libraries ----
library(rsconnect)
library(rgdal) # EJN: rgdal will be retired during October 2023. What functions in the script below need to be updated so we can remove this library from the required list?
library(sf)
library(terra)
library(geodiv)
library(raster) # EJN: probably best to convert any raster functions to terra if possible (maybe it's not - just checking.)
library(landscapemetrics)
library(grainscape)
library(RStoolbox)
library(dplyr)
library(tidyr)
library(pals)
library(viridis)
library(shiny)
library(shiny.semantic)
library(shinyjs)
library(shinythemes)
library(DT)
library(rasterVis)
library(rstudioapi)
library(ggplot2)
library(future)
library(furrr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(stars)
library(readr)
library(tidyverse)
library(plotly)
library(tidyterra)
library(cowplot)
library(grid)
library(gridExtra)
library(shinya11y)



# setting the working directory to the location of the script

# script_path <- getActiveDocumentContext()$path
# setwd(dirname(script_path))
# EJN: no need to do this if you package the script as part of a project; this is "best practice" so I've commented these lines out

# Prepare the workspace ----
# This is here to close any multiprocesses that may not have closed properly if the app crashed and is then re-run
plan(sequential)
gc()

# Set the options for non-scientific notation (this can always be removed)
options(scipen = 999)

# Set up reactive values ----
Protected_areas <- reactiveVal()
movement_cost_protected <- reactiveVal()

croppedOntario <- reactiveVal()
cropped_polyrast <- reactiveVal()
croppedAMIS <- reactiveVal()
croppedCHF_mines <- reactiveVal()
croppedCHF_night_lights <- reactiveVal()
croppedCHF_oil_gas <- reactiveVal()
croppedCHF_forestry_harvest <- reactiveVal()
croppedSOLRIS_AggregateExtraction_204 <- reactiveVal()
croppedSOLRIS_TopsoilExtraction_205 <- reactiveVal()
croppedSOLRIS_Undifferentiated_250 <- reactiveVal()
croppedmovement_cost <- reactiveVal()
croppedCLASS_00 <- reactiveVal()
croppedCLASS_05 <- reactiveVal()
croppedCLASS_06 <- reactiveVal()
croppedCLASS_12 <- reactiveVal()
croppedCLASS_13 <- reactiveVal()
croppedCLASS_14 <- reactiveVal()
croppedCLASS_15 <- reactiveVal()
croppedCLASS_16 <- reactiveVal()
croppedCLASS_18 <- reactiveVal()
croppedCLASS_21 <- reactiveVal()
croppedCLASS_23 <- reactiveVal()
croppedCLASS_24 <- reactiveVal()
croppedCLASS_25 <- reactiveVal()
croppedCLASS_26 <- reactiveVal()
croppedmean_temp <- reactiveVal()
croppedprecipitation <- reactiveVal()
croppedElevation <- reactiveVal()
croppedSoil_not <- reactiveVal()
croppedSoil_20 <- reactiveVal()
croppedSoil_75 <- reactiveVal()
croppedSoil_150 <- reactiveVal()
croppedSoil_G150 <- reactiveVal()
croppedProtected_areas <- reactiveVal()
croppedmovement_cost_protected <- reactiveVal()
mapped_val_react <- reactiveVal()
mapped_val_react_pro <- reactiveVal()
selected_polygon <- reactiveVal()

degraded_pixels <- reactiveVal()
restored_land <- reactiveVal()
sample_degraded_land <- reactiveVal()
Deg_PCA <- reactiveVal()
elapsed_time <- reactiveVal()
elapsed_time_env <- reactiveVal()
numDegradedPixels <- reactiveVal()
pca_result <- reactiveVal()
result_habitat_data_updated <- reactiveVal()
landscape_added <- reactiveVal(FALSE)
merged_data_reactive <- reactiveVal()
plot_degraded <- reactiveVal()
degraded_protected_react <- reactiveVal()
best_comb_react <- reactiveVal()
output_extent <- reactiveVal()
combinations_react <- reactiveVal()
values <- reactiveValues(extent = NULL)
landscape_output <- reactiveVal()
export_sf <- reactiveVal(NULL)
KML_output_react <- reactiveVal()
selected_sf_react <- reactiveVal()
temp_dir_react <- reactiveVal()
land_cover_labels_react<- reactiveVal()
land_cover_labels <- data.frame(
  Value = c(
    13, 52, 17, 
    36, 51, 21, 
    31, 53, 23, 
    33, 14, 37, 
    54, 25, 38, 
    15, 22, 32, 
    11, 12, 16, 
    18, 35, 24, 
    34, 55, 99, 
    41),
  Land_Class = c(
    "Alvar", "Anthropogenic", "Barren", 
    "Bog", "Built-up area, pervious", "Coniferous forest", 
    "Coniferous treed swamp", "Cropland", "Deciduous forest", 
    "Deciduous treed swamp", "Dune", "Fen", 
    "Hay/pasture", "Hedge row", "Marsh", 
    "Meadow", "Mixedwood forest", "Mixedwood treed swamp", 
    "Prairie", "Savannah", "Shrubland", 
    "Sparse treed", "Thicket swamp", "Transitional forest", 
    "Transitional treed swamp", "Transportation", "Unclassified", 
    "Water")
) %>% 
  mutate(color = c(
    # The first set are the official colors for the OLC layer from LIO:
    colors <- c(
      "#FDD18D", "#767168", "#DEEFC9",
      "#9ECEC9", "#70834A", "#329700",
      "#009377", "#CDA666", "#B9F500",
      "#6DF1B2", "#EAE312", "#8EB1D4",
      "#BE7F5F", "#357839", "#4D86C4",
      "#B3B300", "#45D000", "#00C595",
      "#D7D69E", "#E7C0F8", "#ECB9B6",
      "#579B36", "#82F1D5", "#AFAF00",
      "#C8FFCB", "#090000", "#FF0000",
      "#185A93"
    ))) %>% 
  # Add our own classes
  bind_rows(data.frame(
    Value = c(
      100, 101, 204, 
      205, 250, 0, 1, 300),
    # These are custom colors we define  (I just threw these in, they are not necessarily "good"):
    color = c(
      "#4B0092", "deeppink", "#FF8333",
      "royalblue", "khaki", "#FFD700", 
      "mediumspringgreen","lightgrey"),
    Land_Class = c("Degraded", "Candidate area", "Aggregate extraction", 
                   "Topsoil/Peat extraction", "Undifferentiated", "Not included", "Areas of conservation concern", "Buffer")
  ))


land_cover_labels_react(land_cover_labels)
polyrast_plot<- reactiveVal()
buffer_value <- reactiveVal()
buffer_value(1200)
sp_plot_polygon <-reactiveVal()
legend_plot1_react <-reactiveVal()
legend_plot2_react <-reactiveVal()
legend_plot3_react <-reactiveVal()
legend_plot4_react <-reactiveVal()
legend_plot5_react <-reactiveVal()
plot_degraded_react <-reactiveVal()
plot_degraded_protected_react <-reactiveVal()
restored_land_plot_react <-reactiveVal()
final_data_table <-reactiveVal()
plot_rds <-reactiveVal()
# BatchloopFinished <- reactiveVal(FALSE)

# Load the rasters ----
Ontario_land_cover <- raster("rasters/habitat/OLC300_SOLRIS_All.tif")

AMIS <- rast("rasters/degraded/AMIS_raster.tif")
CHF_mines <- rast("rasters/degraded/CHF_mines_EPSG.3162.tif")
CHF_night_lights <- rast("rasters/degraded/CHF_night_lights_EPSG.3162.tif")
CHF_oil_gas <- rast("rasters/degraded/CHF_oil_gas_EPSG.3162.tif")
CHF_forestry_harvest <- rast("rasters/degraded/CHF_forestry_harvest_EPSG.3162.tif")
SOLRIS_AggregateExtraction_204 <- rast("rasters/degraded/SOLRIS_AggregateExtraction_204.tif")
SOLRIS_TopsoilExtraction_205 <- rast("rasters/degraded/SOLRIS_TopsoilExtraction_205.tif")
SOLRIS_Undifferentiated_250 <- rast("rasters/degraded/SOLRIS_Undiff_250_AG_4to7.tif")

movement_cost <- rast("rasters/connectivity/movement_cost.tif")

mean_temp <- rast("rasters/environment/mean_temp.tif")
precipitation <- rast("rasters/environment/precipitation.tif")
Elevation <- rast("rasters/environment/Elevation.tif")
Soil_not <- rast("rasters/environment/Depth_not_soil.tif")
Soil_20 <- rast("rasters/environment/Depth_less_20.tif")
Soil_75 <- rast("rasters/environment/Depth_20_75.tif")
Soil_150 <- rast("rasters/environment/Depth_75_150.tif")
Soil_G150 <- rast("rasters/environment/Depth_greater_150.tif")
CLASS_00 <- rast("rasters/environment/CLASS_0.tif")
CLASS_05 <- rast("rasters/environment/CLASS_5.tif")
CLASS_06 <- rast("rasters/environment/CLASS_6.tif")
CLASS_12 <- rast("rasters/environment/CLASS_12.tif")
CLASS_13 <- rast("rasters/environment/CLASS_13.tif")
CLASS_14 <- rast("rasters/environment/CLASS_14.tif")
CLASS_15 <- rast("rasters/environment/CLASS_15.tif")
CLASS_16 <- rast("rasters/environment/CLASS_16.tif")
CLASS_18 <- rast("rasters/environment/CLASS_18.tif")
CLASS_21 <- rast("rasters/environment/CLASS_21.tif")
CLASS_23 <- rast("rasters/environment/CLASS_23.tif")
CLASS_24 <- rast("rasters/environment/CLASS_24.tif")
CLASS_25 <- rast("rasters/environment/CLASS_25.tif")
CLASS_26 <- rast("rasters/environment/CLASS_26.tif")

OLC <- rast("rasters/habitat/OLC300_SOLRIS_All.tif")
CRR_raster <- rast("rasters/protected_area/CRR_raster.tif")
NGO_raster <- rast("rasters/protected_area/NGO_raster.tif")
NHSA_raster <- rast("rasters/protected_area/NHSA_raster.tif")
NHVA_raster <- rast("rasters/protected_area/NHVA_raster.tif")
Parks_raster <- rast("rasters/protected_area/Parks_raster.tif")
National_parks_raster <- rast("rasters/protected_area/National_parks_raster.tif")
CA_raster <- rast("rasters/protected_area/CA_raster.tif")
Far_North_raster <- rast("rasters/protected_area/Far_North_raster.tif")
Municipal_heritage_raster <- rast("rasters/protected_area/Municipal_heritage_raster.tif")
Migratory_bird_raster <- rast("rasters/protected_area/Migratory_bird_raster.tif")
National_wildlife_raster <- rast("rasters/protected_area/National_wildlife_raster.tif")
Wilderness_area_raster <- rast("rasters/protected_area/Wilderness_area_raster.tif")
National_capital_raster <- rast("rasters/protected_area/National_capital_raster.tif")
Provincial_plan_protected_raster <- rast("rasters/protected_area/Provincial_plan_protected_raster.tif")
Crown_plan_protected_raster <- rast("rasters/protected_area/Crown_plan_protected_raster.tif")
OECM_raster <- rast("rasters/protected_area/OECM_raster.tif")

## Load the shapefiles ----
# these are all in CRS 4326 (WGS 84) as that is what the leaflet map uses
Conservation_reserves <- st_read("vectors/Simplified_Conservation_reserve_regulated.shp")
Natural_heritage_value_areas <- st_read("vectors/Simplified_Natural_Heritage_Value_Area.shp")
Natural_heritage_system_areas <- st_read("vectors/Simplified_Natural_Heritage_System_Area.shp")
NGO_reserves <- st_read("vectors/NGO.shp")
Provincial_parks <- st_read("vectors/Simplified_Provincial_park_regulated.shp")
Lower_municipality <- st_read("vectors/Simplified_Mun_Lower_Nowater.shp")
Upper_municipality <- st_read("vectors/Simplified_Mun_Upper_Nowater.shp")
National_parks <- st_read("vectors/National_Park.shp")
Conservation_areas <- st_read("vectors/Simplified_Conservation_area.shp")
Far_North_protected_areas <- st_read("vectors/Far_North_protected_area.shp")
Municipal_Heritage_areas <- st_read("vectors/Municipal_Heritage_Areas.shp")
Migratory_Bird_sanctuaries <- st_read("vectors/Migratory_Bird_Sanctuary.shp")
National_Wildlife_areas <- st_read("vectors/National_Wildlife_Area.shp")
Wilderness_areas <- st_read("vectors/Wilderness_Area.shp")
National_capital_valued_ecosystem_or_habitat <- st_read("vectors/National_Capital_Valued_Ecosystem_or_Habitat.shp")
Provincial_planned_protected_area <- st_read("vectors/Provincial_plan_protected_area.shp")
Crown_plan_protected_area <- st_read("vectors/Crown_plan_protected_area.shp")
Other_effective_area_based_conservation_measures <- st_read("vectors/Other_Effective_area-based_Conservation_Measures.shp")

# _----
# Starting the Shiny server ##########################################
# Set maximum upload size to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

server <- function(input, output, session) {

  # Dynamically generate the splash page with the base64-encoded image
  output$splash <- renderUI({
    # Path to the image
    image_path <- file.path(getwd(), "Splash_RestoratiON.jpg")
    
    # Encode the image as a base64 string
    encoded_image <- base64enc::dataURI(file = image_path, mime = "image/jpeg")
    
    # Use the base64 string in the CSS
    div(class = "splash-container", style = paste0("background-image: url('", encoded_image, "');"),
        
        # Styling for the splash title with white background and black text
        div(class = "splash-title", style = "color: black; background-color: white; padding: 20px; font-size: 36px; text-align: center; border: none; box-shadow: none; border-radius: 10px;",
            "Welcome to the RestoratiON Tool"
        ),
        
        div(class = "splash-text-container", style = "border: none; padding: 0; box-shadow: none;",
            # Styling for the descriptive text with rounded corners
            div(class = "splash-text", style = "color: black; background-color: white; padding: 15px; font-size: 18px; border: none; box-shadow: none; border-radius: 10px;",
                "In this tool, you locate areas of degraded land in a selected 
              geographic area in Ontario, identify candidate areas to restore, 
              and measure the potential benefit that restoring each candidate area has on landscape-scale biodiversity 
              based on changes to patch size, connectivity, and environmental heterogeneity. 
              You can rank candidate areas within and between landscapes by their restoration benefit."
            )
        ),
        
        actionButton("go_button", "Let's go!", class = "go-button")
    )
  })
  # Server logic to handle the button click and proceed to the next section
  observeEvent(input$go_button, {
    # Hide the splash page
    output$splash <- renderUI(NULL) # Hide the splash page content
  })

  # Linked to UI Segment 1, initial choice ---- 
  # hide that UI element after an option is selected

  output$showSingleLandscape <- reactive({
    input$single_landscape > 0
  })
  outputOptions(output, "showSingleLandscape", suspendWhenHidden = FALSE)

  output$showMultipleLandscapes <- reactive({
    input$multiple_landscapes > 0
  })
  outputOptions(output, "showMultipleLandscapes", suspendWhenHidden = FALSE)

  output$showLandscapeVisualizer <- reactive({
    input$landscape_visualizer > 0
  })
  outputOptions(output, "showLandscapeVisualizer", suspendWhenHidden = FALSE)
  
  output$showBatchProcessing <- reactive({
    input$batch_processing > 0
  })
  outputOptions(output, "showBatchProcessing", suspendWhenHidden = FALSE)


  # Linked to UI Segment 2, Calculation type option ----
  # to hide that UI element after an option is selected

  output$showhabitat_based_calculations <- reactive({
    input$habitat_based_calculations > 0
  })
  outputOptions(output, "showhabitat_based_calculations", suspendWhenHidden = FALSE)

  output$showprotected_based_calculations <- reactive({
    input$protected_based_calculations > 0
  })
  outputOptions(output, "showprotected_based_calculations", suspendWhenHidden = FALSE)


  # Linked to UI Segment 3, Defining the 'areas of conservation concern' ----
  # Creating a check-box of features in the UI
  
  output$protected_checkboxes <- renderUI({
    checkbox_data <- c(
      "Provincial parks", "National parks", "Conservation reserves", "Conservation areas", 
      "Non-governmental organization reserves", "Municipal heritage areas", 
      "Natural heritage system areas", "Natural heritage value areas",
      "Far North protected areas", "Wilderness areas", "Migratory bird sanctuaries", 
      "National wildlife areas", "National capital valued ecosystems", 
      "Provincial planned protected areas", "Crown plan protected areas", 
      "Other effective area-based conservation measures"
    )
    
    # Create checkboxes with labels
    checkbox_list <- lapply(checkbox_data, function(feature) {
      tags$div(
        tags$label(
          tags$input(type = "checkbox", 
                     name = "protected_checkboxes", 
                     value = feature, 
                     class = "protected-checkbox",
                     onclick = "updateCheckboxState(); Shiny.setInputValue('protected_checkboxes', getCheckedCheckboxes());" # Update Shiny input
          ),
          feature
        )
      )
    })
    
    do.call(tags$div, checkbox_list)
  })


  # Linked to UI segment 4 - Setting landscape extent, visualizing plots, testing calculation viability ----

  # The code below creates a movement cost and plot raster for the specific 'areas of conservation concern' definition the
  # user selects in the previous check-box segment. This is an observe-event that runs whenever the check-boxes are adjusted.
  
  # Trigger the observeEvent once on initialization to prevent errors
  observe({
    if (length(input$protected_checkboxes) == 0) {
      # Manually call the observeEvent logic
      gc()
      # a notification while the code is run
      notification_id_pro_int <- showNotification("Updating land definitions, please wait...", type = "message", duration = NULL)
      
      # Use local variables to calculate values
      defined_protected_areas <- OLC
      defined_protected_areas[!is.na(defined_protected_areas)] <- 0
      
      if ("Provincial parks" %in% input$protected_checkboxes) {
        defined_protected_areas[Parks_raster == 1] <- 1
      }
      
      defined_protected_areas[OLC == 41] <- 41 # This adds water for mapping purposes
      defined_movement_cost_protected <- (movement_cost * 10) # The basic habitat movement cost raster is multiplied by 10, to create the
      # 'areas of conservation concern' cost raster
      defined_movement_cost_protected[defined_protected_areas == 1] <- 1 # 'areas of conservation concern' pixels are given a value of 1 in this new raster.
      
      # Store values in reactive values
      Protected_areas(defined_protected_areas)
      movement_cost_protected(defined_movement_cost_protected)
      
      removeNotification(notification_id_pro_int)
      gc()
    }})
  
  # Trigger when the user interacts with the checkboxes
  observeEvent(input$protected_checkboxes, {
    gc()
    # a notification while the code is run
    notification_id_pro <- showNotification("Updating land definitions, please wait...", type = "message", duration = NULL)
    
    # Use local variables to calculate values
    defined_protected_areas <- OLC
    defined_protected_areas[!is.na(defined_protected_areas)] <- 0
    
    if ("Conservation reserves" %in% input$protected_checkboxes) {
      defined_protected_areas[CRR_raster == 1] <- 1
    }
    
    if ("Non-governmental organization reserves" %in% input$protected_checkboxes) {
      defined_protected_areas[NGO_raster == 1] <- 1
    }
    
    if ("Natural heritage system areas" %in% input$protected_checkboxes) {
      defined_protected_areas[NHSA_raster == 1] <- 1
    }
    
    if ("Natural heritage value areas" %in% input$protected_checkboxes) {
      defined_protected_areas[NHVA_raster == 1] <- 1
    }
    
    if ("Provincial parks" %in% input$protected_checkboxes) {
      defined_protected_areas[Parks_raster == 1] <- 1
    }
    
    if ("National parks" %in% input$protected_checkboxes) {
      defined_protected_areas[National_parks_raster == 1] <- 1
    }
    
    if ("Conservation areas" %in% input$protected_checkboxes) {
      defined_protected_areas[CA_raster == 1] <- 1
    }
    
    if ("Municipal heritage areas" %in% input$protected_checkboxes) {
      defined_protected_areas[Municipal_heritage_raster == 1] <- 1
    }
    
    if ("Far North protected areas" %in% input$protected_checkboxes) {
      defined_protected_areas[Far_North_raster == 1] <- 1
    }
    
    if ("Wilderness areas" %in% input$protected_checkboxes) {
      defined_protected_areas[Wilderness_area_raster == 1] <- 1
    }
    
    if ("Migratory bird sanctuaries" %in% input$protected_checkboxes) {
      defined_protected_areas[Migratory_bird_raster == 1] <- 1
    }
    
    if ("National wildlife areas" %in% input$protected_checkboxes) {
      defined_protected_areas[National_wildlife_raster == 1] <- 1
    }
    
    if ("National capital valued ecosystems" %in% input$protected_checkboxes) {
      defined_protected_areas[National_capital_raster == 1] <- 1
    }
    
    if ("Provincial planned protected areas" %in% input$protected_checkboxes) {
      defined_protected_areas[Provincial_plan_protected_raster == 1] <- 1
    }
    
    if ("Crown plan protected areas" %in% input$protected_checkboxes) {
      defined_protected_areas[Crown_plan_protected_raster == 1] <- 1
    }
    
    if ("Other effective area-based conservation measures" %in% input$protected_checkboxes) {
      defined_protected_areas[OECM_raster == 1] <- 1
    }
    
    defined_protected_areas[OLC == 41] <- 41 # This adds water for mapping purposes
    defined_movement_cost_protected <- (movement_cost * 10) # The basic habitat movement cost raster is multiplied by 10, to create the
    # 'areas of conservation concern' cost raster
    defined_movement_cost_protected[defined_protected_areas == 1] <- 1 # 'areas of conservation concern' pixels are given a value of 1 in this new raster.
    
    # Store values in reactive values
    Protected_areas(defined_protected_areas)
    movement_cost_protected(defined_movement_cost_protected)
    
    removeNotification(notification_id_pro)
    gc()
  })

# Map to choose target landscape ----
  
  # This code is to load the polygon layers to be visualized via the leaflet map. This is primarily for visualization purposes,
  # but the user also can set the analyzed landscape extent by clicking on any polygon displayed, or by drawing a box on the map.

  # A notification to display while the leaflet map loads
  notification_loading_shapefiles <- showNotification("Loading and preparing map (this may take a few minutes)...", type = "message", duration = NULL)

  # Set up the leaflet map ----
  # Set colors for each polygon layer. IDs for each individual polygon are set with layerId =
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles(group = "Map View") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
      setView(lng = -84.3870, lat = 50.2538, zoom = 5) %>%
      addPolygons(
        data = Conservation_reserves,
        group = "Conservation reserves",
        fillColor = "springgreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~PROTECTE_1,
        label = ~PROTECTE_1
      ) %>%
      addPolygons(
        data = Natural_heritage_value_areas,
        group = "Natural heritage value areas",
        fillColor = "magenta",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~AREA_NAME,
        label = ~AREA_NAME
      ) %>%
      addPolygons(
        data = Natural_heritage_system_areas,
        group = "Natural heritage system areas",
        fillColor = "limegreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OGF_ID,
        label = ~ENABLING_P
      ) %>%
      addPolygons(
        data = NGO_reserves,
        group = "Non-governmental organization<br>reserves",
        fillColor = "purple",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Provincial_parks,
        group = "Provincial parks",
        fillColor = "turquoise",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~PROTECTE_1,
        label = ~PROTECTE_1
      ) %>%
      addPolygons(
        data = Lower_municipality,
        group = "Lower / single-tier municipalities",
        fillColor = "red",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OGF_ID,
        label = ~MUN_NAME
      ) %>%
      addPolygons(
        data = Upper_municipality,
        group = "Upper municipalities / districts",
        fillColor = "orange",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OGF_ID,
        label = ~MUN_NAME
      ) %>%
      addPolygons(
        data = National_parks,
        group = "National parks",
        fillColor = "royalblue",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~NAME_E,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Conservation_areas,
        group = "Conservation areas",
        fillColor = "lawngreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Far_North_protected_areas,
        group = "Far North protected areas",
        fillColor = "deepskyblue",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~NAME_E,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Municipal_Heritage_areas,
        group = "Municipal heritage areas",
        fillColor = "hotpink",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Migratory_Bird_sanctuaries,
        group = "Migratory bird sanctuaries",
        fillColor = "yellow",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = National_Wildlife_areas,
        group = "National wildlife areas",
        fillColor = "orangered",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Wilderness_areas,
        group = "Wilderness areas",
        fillColor = "darkgreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Crown_plan_protected_area,
        group = "Crown plan protected areas",
        fillColor = "lightseagreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Provincial_planned_protected_area,
        group = "Provincial planned protected areas",
        fillColor = "yellowgreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = National_capital_valued_ecosystem_or_habitat,
        group = "National capital valued ecosystem",
        fillColor = "palegreen",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addPolygons(
        data = Other_effective_area_based_conservation_measures,
        group = "Other effective area-based<br>conservation measures",
        fillColor = "maroon",
        fillOpacity = 0.7,
        color = "black",
        weight = 2,
        layerId = ~OBJECTID,
        label = ~NAME_E
      ) %>%
      addDrawToolbar(
        targetGroup = "Your drawn landscape",
        editOptions = FALSE,
        circleOptions = FALSE,
        polygonOptions = TRUE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        polylineOptions = FALSE,
        singleFeature = TRUE
      ) %>%
      # Code to control displayed layers via a leaflet map menu, and set all to not be displayed by default.
      addLayersControl(
        baseGroups = c("Map View", "Satellite View"),
        overlayGroups = c(
          "Your drawn landscape", "Lower / single-tier municipalities", "Upper municipalities / districts", "Provincial parks", "National parks",
          "Conservation reserves", "Conservation areas", "Non-governmental organization<br>reserves", "Natural heritage value areas", "Natural heritage system areas",
          "Far North protected areas", "Municipal heritage areas", "Migratory bird sanctuaries", "National wildlife areas", "Wilderness areas",
          "Crown plan protected areas", "Provincial planned protected areas", "National capital valued ecosystem",
          "Other effective area-based<br>conservation measures"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(group = "Lower / single-tier municipalities") %>%
      hideGroup(group = "Upper municipalities / districts") %>%
      hideGroup(group = "Conservation reserves") %>%
      hideGroup(group = "Natural heritage value areas") %>%
      hideGroup(group = "Natural heritage system areas") %>%
      hideGroup(group = "Non-governmental organization<br>reserves") %>%
      hideGroup(group = "Provincial parks") %>%
      hideGroup(group = "Conservation areas") %>%
      hideGroup(group = "Far North protected areas") %>%
      hideGroup(group = "Municipal heritage areas") %>%
      hideGroup(group = "Migratory bird sanctuaries") %>%
      hideGroup(group = "National wildlife areas") %>%
      hideGroup(group = "Wilderness areas") %>%
      hideGroup(group = "Crown plan protected areas") %>%
      hideGroup(group = "Provincial planned protected areas") %>%
      hideGroup(group = "National capital valued ecosystem") %>%
      hideGroup(group = "Other effective area-based<br>conservation measures") %>%
      hideGroup(group = "National parks") %>%
      hideGroup(group = "Your drawn landscape") %>%
      htmlwidgets::onRender("
      function(el, x) {
        var map = this;

        // Add zoom control to bottom right
        L.control.zoom({ position: 'bottomright' }).addTo(map);

        // Listen for when a shape is drawn
        map.on('draw:created', function (e) {
          var layer = e.layer;

          // Add the drawn layer to the 'Your drawn landscape' group
          layer.addTo(map);
          layer.addTo(map.layerManager.getLayerGroup('Your drawn landscape'));

          // Make sure 'Your drawn landscape' group is shown
          map.addLayer(map.layerManager.getLayerGroup('Your drawn landscape'));

          // Programmatically check the box in the layers control
          var checkbox = document.querySelector('input.leaflet-control-layers-selector');
          if (checkbox && !checkbox.checked) {
            checkbox.click();
          }
        });
      }
    ")
  })

  # Schedule the removal of the loading notification after the rendering is complete
  observe({
    session$onFlush(function() {
      removeNotification(notification_loading_shapefiles)
    })
  })

  # Add a JavaScript listener to capture click events on polygons - so extent can be set via that method.
  session$onFlushed(function() {
    runjs(
      "function onMapClick(e) {
       if (e.layer) {
         Shiny.setInputValue('map_shape_click', { id: e.layer.options.layerId, lng: e.latlng.lng, lat: e.latlng.lat });
       }
     }
     map.on('click', onMapClick);"
    )
  })
  
  # Check and handle clicks for target polygon features----
  
  observeEvent(input$map_shape_click, {
    click_info <- input$map_shape_click
    if (!is.null(click_info)) {
      # Show a notification when a polygon is clicked
      showNotification("Polygon extent selected", type = "message", duration = 3)
      
      # Define the process_clicked_polygon function
      process_clicked_polygon <- function(clicked_polygon, crs_code, OLC) {
        extent_val <- raster::extent(clicked_polygon)
        ext_coords <- cbind(
          c(extent_val@xmin, extent_val@xmax, extent_val@xmax, extent_val@xmin, extent_val@xmin),
          c(extent_val@ymin, extent_val@ymin, extent_val@ymax, extent_val@ymax, extent_val@ymin)
        )
        rownames(ext_coords) <- NULL
        colnames(ext_coords) <- c("x", "y")
        sp_extent <- sp::Polygon(ext_coords)
        sp_extent <- sp::Polygons(list(sp_extent), ID = "1")
        sp_extent <- sp::SpatialPolygons(list(sp_extent))
        sp::proj4string(sp_extent) <- CRS("+proj=longlat +datum=WGS84")
        sp_extent_3162 <- sp::spTransform(sp_extent, CRS("+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +ellps=GRS80 +towgs84=-0.991,1.9072,0.5129,-1.25033e-07,-4.6785e-08,-5.6529e-08,0 +units=m +no_defs +type=crs"))
        extent_val_3162 <- raster::extent(sp_extent_3162)
        values$extent <- extent_val_3162
        
        polygon_geometry <- clicked_polygon$geometry
        polygon_geometry <- st_sf(geometry = polygon_geometry)
        
        sp_polygon_3162 <- sf::st_transform(polygon_geometry, crs = 3162)
        sp_plot_polygon(sf::st_transform(polygon_geometry, crs = st_crs(OLC)))
        ras_resolution <- terra::res(OLC)
        template <- terra::rast(terra::vect(sp_polygon_3162), res = ras_resolution, ext = OLC)
        poly_raster <- terra::rasterize(terra::vect(sp_polygon_3162), template)
        selected_polygon(poly_raster)
      }
      
      # Define the CRS once
      crs_code <- "+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +ellps=GRS80 +towgs84=-0.991,1.9072,0.5129,-1.25033e-07,-4.6785e-08,-5.6529e-08,0 +units=m +no_defs +type=crs"
      
      # Lookup table for polygons
      polygon_types <- list(
        Upper_municipality = "OGF_ID",
        Lower_municipality = "OGF_ID",
        Provincial_parks = "PROTECTE_1",
        Natural_heritage_system_areas = "OGF_ID",
        Natural_heritage_value_areas = "AREA_NAME",
        Conservation_reserves = "PROTECTE_1",
        National_parks = "NAME_E",
        Conservation_areas = "OBJECTID",
        Far_North_protected_areas = "NAME_E",
        Municipal_Heritage_areas = "OBJECTID",
        Migratory_Bird_sanctuaries = "OBJECTID",
        National_Wildlife_areas = "OBJECTID",
        NGO_reserves = "OBJECTID",
        Wilderness_areas = "OBJECTID",
        National_capital_valued_ecosystem_or_habitat = "OBJECTID",
        Provincial_planned_protected_area = "OBJECTID",
        Crown_plan_protected_area = "OBJECTID",
        Other_effective_area_based_conservation_measures = "OBJECTID"
      )
      
      # Find matching polygon 'type' and process
      for (key in names(polygon_types)) {
        if (click_info$id %in% get(key)[[polygon_types[[key]]]]) {
          clicked_polygon <- get(key)[get(key)[[polygon_types[[key]]]] == click_info$id, ]
          process_clicked_polygon(clicked_polygon, crs_code, OLC)
          break
        }
      }
    }
  })
  
  


  # Set the extent if the drawing or polygon creation tools are used. ----
  # It creates a bounding box and sets the extent that way.
  observeEvent(input$map_draw_new_feature, {
    feature <- input$map_draw_new_feature
    if (feature$geometry$type == "Polygon") {
      coords <- matrix(unlist(feature$geometry$coordinates), ncol = 2, byrow = TRUE)
      polygon_sp <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID = "1")))
      polygon_sf <- sf::st_as_sf(polygon_sp)
      polygon_sf <- sf::st_set_crs(polygon_sf, 4326) # Set CRS to WGS 84
      polygon_sf <- sf::st_transform(polygon_sf, sf::st_crs(Ontario_land_cover))
      bbox <- sf::st_bbox(polygon_sf)
      extent_val <- raster::extent(bbox)

      values$extent <- extent_val
      
      # create a polygon raster to work in future steps
      polygon_geometry <- feature$geometry
      # Extract the coordinates
      polygon_coords <- polygon_geometry$coordinates[[1]]
      # Create a matrix from the coordinates
      polygon_matrix <- matrix(unlist(polygon_coords), ncol = 2, byrow = TRUE)
      # Create an sf polygon
      sf_polygon <- sf::st_sfc(sf::st_polygon(list(polygon_matrix)))
      # Set the CRS for the original leaflet polygon 
      sf_polygon <- sf::st_set_crs(sf_polygon, 4326)
      # Set the target CRS
      target_crs <- sf::st_crs(3162)
      
      sf_polygon <- st_sf(geometry = sf_polygon)
      
      sp_polygon_3162 <- sf::st_transform(sf_polygon, crs = target_crs)
      sp_plot_polygon(sf::st_transform(sf_polygon, crs = st_crs(OLC)))
      ras_resolution <- terra::res(OLC)
      template <- terra::rast(terra::vect(sp_polygon_3162), res = ras_resolution, ext = OLC)
      poly_raster <- terra::rasterize(terra::vect(sp_polygon_3162), template)
      # cropped_poly<-crop(poly_raster, extent_val)
      # plot(poly_raster)
      # plot(cropped_poly)
      selected_polygon(poly_raster)
    }
  })


# Buffer for target landscape ----

  # The UI contains the option for the user to set a buffer value in metres around their selected extent via a text input.
  # This value can be from 0 up, with 0 as a default. The code below is just a simple check to prevent errors that would result
  # if non-valid values are entered, e.g. the input box is cleared but no replacement value is given
  
  
  input_use_counter <- reactiveVal(0)
  
  # Increment the counter each time the buffer input changes
  observeEvent(input$buffer_unit_value, {
    input_use_counter(input_use_counter() + 1)
  })
  
  # Code to ensure a valid buffer value always exists to prevent crashing
  observeEvent(input$buffer_unit_value, {
    if (input_use_counter() > 1) {
    if (!is.null(input$buffer_unit_value) && input$buffer_unit_value >= 0) {
      buffer_value(input$buffer_unit_value)
    } else {
      buffer_value(1200)
    }
    }
  })



  # This code runs when the 'set extent' button is clicked.
  observeEvent(input$set_extent, {
    extent_coord <- values$extent
    # an message is displayed in the UI if no extent has been selected before clicking the set extent button
    if (is.null(extent_coord)) {
      showNotification("Please draw a rectangle on the map or click on a polygon to set the extent", type = "warning")
      return()
    }
    # A notification to display until this code is finished running
    notification_id_extent <- showNotification("Setting extent and testing calculation viability...", type = "message", duration = NULL)
    # Create a buffer around the extent_coord, 0 if no value was input
    extent_coord <- raster::extend(extent_coord, buffer_value())
    output_extent(extent_coord)

    # Crop rasters to target landscape extent ----
    # All rasters used in subsequent steps in the app are cropped to selected landscape extent
    # Cropped layers are reactive values, which have already been defined 
    pre_cropped_polyrast <- selected_polygon()
    cropped_Ontario_land_cover <- raster::crop(Ontario_land_cover, extent_coord)
    cropped_Ontario_land_cover <- rast(cropped_Ontario_land_cover)
    croppedOntario(cropped_Ontario_land_cover)
    cropped_polyrast(crop(pre_cropped_polyrast, cropped_Ontario_land_cover))
    croppedAMIS(crop(AMIS, cropped_Ontario_land_cover))
    croppedCHF_mines(crop(CHF_mines, cropped_Ontario_land_cover))
    croppedCHF_night_lights(crop(CHF_night_lights, cropped_Ontario_land_cover))
    croppedCHF_oil_gas(crop(CHF_oil_gas, cropped_Ontario_land_cover))
    croppedCHF_forestry_harvest(crop(CHF_forestry_harvest, cropped_Ontario_land_cover))
    croppedmovement_cost(crop(movement_cost, cropped_Ontario_land_cover))
    croppedCLASS_00(crop(CLASS_00, cropped_Ontario_land_cover))
    croppedCLASS_05(crop(CLASS_05, cropped_Ontario_land_cover))
    croppedCLASS_06(crop(CLASS_06, cropped_Ontario_land_cover))
    croppedCLASS_12(crop(CLASS_12, cropped_Ontario_land_cover))
    croppedCLASS_13(crop(CLASS_13, cropped_Ontario_land_cover))
    croppedCLASS_14(crop(CLASS_14, cropped_Ontario_land_cover))
    croppedCLASS_15(crop(CLASS_15, cropped_Ontario_land_cover))
    croppedCLASS_16(crop(CLASS_16, cropped_Ontario_land_cover))
    croppedCLASS_18(crop(CLASS_18, cropped_Ontario_land_cover))
    croppedCLASS_21(crop(CLASS_21, cropped_Ontario_land_cover))
    croppedCLASS_23(crop(CLASS_23, cropped_Ontario_land_cover))
    croppedCLASS_24(crop(CLASS_24, cropped_Ontario_land_cover))
    croppedCLASS_25(crop(CLASS_25, cropped_Ontario_land_cover))
    croppedCLASS_26(crop(CLASS_26, cropped_Ontario_land_cover))
    croppedmean_temp(crop(mean_temp, cropped_Ontario_land_cover))
    croppedprecipitation(crop(precipitation, cropped_Ontario_land_cover))
    croppedElevation(crop(Elevation, cropped_Ontario_land_cover))
    croppedSoil_not(crop(Soil_not, cropped_Ontario_land_cover))
    croppedSoil_20(crop(Soil_20, cropped_Ontario_land_cover))
    croppedSoil_75(crop(Soil_75, cropped_Ontario_land_cover))
    croppedSoil_150(crop(Soil_150, cropped_Ontario_land_cover))
    croppedSoil_G150(crop(Soil_G150, cropped_Ontario_land_cover))
    croppedSOLRIS_AggregateExtraction_204(crop(SOLRIS_AggregateExtraction_204, cropped_Ontario_land_cover))
    croppedSOLRIS_TopsoilExtraction_205(crop(SOLRIS_TopsoilExtraction_205, cropped_Ontario_land_cover))
    croppedSOLRIS_Undifferentiated_250(crop(SOLRIS_Undifferentiated_250, cropped_Ontario_land_cover))
    croppedProtected_areas(crop(Protected_areas(), cropped_Ontario_land_cover))
    croppedmovement_cost_protected(crop(movement_cost_protected(), cropped_Ontario_land_cover))
    

    # buffer the raster for plotting
    sp_polygon_3162_buffered <- st_buffer(sp_plot_polygon(), buffer_value())
    plot_buffered_raster <- crop(croppedOntario(), sp_polygon_3162_buffered, mask = TRUE)
    polyrast_plot(plot_buffered_raster)
    

    
    # This plots the cropped landscape with habitat classes
    
    #create the target landscape object for the plots
    target_landscape <- sp_plot_polygon()
    
    output$rasterPlot <-  renderPlotly({
      #Creates a table to map land class labels to numeric values in 'Ontario_land_cover' (for the plot legend essentially)
      land_cover_labels <- land_cover_labels_react()
      # Determine percentage of each land cover class in the *buffered* landscape, since this is what readers will see
      percent_landcover <- plot_buffered_raster %>% 
        data.frame() %>% 
        rename(value = 1) %>% 
        group_by(value) %>% 
        tally() %>% 
        mutate(percentage = round(n/sum(n)*100, 2)) %>% 
        dplyr::select(-n)
      
      #Extract unique values from the SpatRaster for the legend of the plot
      unique_values <- as.vector(unique(values(plot_buffered_raster)))
      
      # Make a map of colors and labels to raster values in cropped landscape
      df_joined <- data.frame(value = unique_values) %>%
        left_join(land_cover_labels, by = c("value" = "Value"))
      
      # Make a sf points feature (I previously used it to create labels for the map - so this code is likely overkill for what I end up doing...)
      label_points_sf <- as.data.frame(plot_buffered_raster, xy = TRUE, cells = TRUE) %>% 
        rename(value = 4) %>% 
        st_as_sf(coords = c("x", "y"), 
                 crs = crs(plot_buffered_raster)) %>% 
        left_join(df_joined, by = "value") %>% 
        # Add percentage of each Land Class in the map
        left_join(percent_landcover, by = "value") %>% 
        # some land classes have 0 percent coverage in the TARGET landscape
        # but are present in the buffer.
        mutate(percentage = if_else(is.na(percentage), 0, percentage)) %>% 
        # Create a new label for each class
        mutate(Label = paste0("              ", Land_Class, " (", percentage, "%)"),
               # arrange the Land Class labels by percentage
               Label = fct_infreq(Label))
      
      # get colors for plot
      plot_colors <- label_points_sf %>% 
        st_drop_geometry() %>% 
        arrange(Label) %>% 
        dplyr::select(value, color, Label, percentage) %>%
        arrange(desc(percentage)) %>% 
        distinct()
      
      # Ready the raster for plotting - assign colors
      plot_buffered_raster <- as.factor(plot_buffered_raster)
      
      levels(plot_buffered_raster) = data.frame(value = plot_colors$value, 
                                          Label = plot_colors$Label)
      
      # plot in ggplot first
      my_ggplot <- ggplot() +
        geom_spatraster(data = plot_buffered_raster) +
        scale_fill_manual(name = "\nLand class and percent cover\n",
                          values = plot_colors$color,
                          na.value = "white") +
        geom_sf(data = sp_polygon_3162_buffered,
                aes(color = "Buffer extent"), fill = NA, linewidth = 1) +
        geom_sf(data = target_landscape, aes(color = "Target landscape"),
                fill = NA, linewidth = 1) +
        scale_color_manual(name = "", values = c("lightgrey", "black")) +
        # Old line of code to get labels - not needed anymore
        # geom_sf(data = label_points_sf,
        #         aes(text = Label, color = Label), alpha = 0) +
        theme_minimal() + 
        labs(title = "\nTarget landscape for analysis\n") +
        theme(panel.grid = element_blank(), legend.margin = margin(c(0,0,0,0)),
              legend.key.size = unit(1.5, "cm"), # Adjust legend key size
              legend.text = element_text(size = 12, face = "bold"), # Adjust legend text size
              legend.title = element_text(size = 16, face = "bold"), # Adjust legend title size
              legend.box.spacing = unit(0.5, "cm")) +
        guides(fill = guide_legend(
          ncol = 2,
          label.position = "left",
          label.hjust = 1,
          order = 2),
          color = guide_legend(
            direction = "horizontal",
            label.position = "right",
            label.hjust = 0,
            order = 1))
      
      # strip out the legend - this makes it easier to format and put it where you might want it - whether above, below, or beside plotly interactive plot
      legend_plot1 <- cowplot::get_plot_component(my_ggplot, 'guide-box-right', return_all = TRUE)
      legend_plot1_react(legend_plot1)
      
      
      plotly_object<-ggplotly(my_ggplot + theme(legend.position = "none",
                                  axis.text = element_blank()),
                tooltip = "value")
      
      # Combine plot objects into a list
      plot_objects <- list(
        legend = legend_plot1,
        plotly = plotly_object
      )
      plot_rds(plot_objects)
      
      # Users can hover over the map output below and the raster value will appear.
      # I'm not totally happy with this - sometimes hover doesn't work.
     ggplotly(my_ggplot + theme(legend.position = "none",
                                axis.text = element_blank()), 
              tooltip = "value")
     
    })
    
    output$rasterPlot1legend <- renderPlot({
      legend_plot1<-legend_plot1_react()
      grid.newpage()
      grid.draw(legend_plot1)
    })
    
    

    
  
    
    # If the 'areas of conservation concern' option is selected in the previous UI Segment 2,
    # then an 'areas of conservation concern' plot is also created using the definitions the user selected previously
    
    # Get the target landscape
    target_landscape <- sp_plot_polygon()
    
    output$protectedPlot <- renderPlotly({
      req(input$protected_based_calculations > 0)
      Protected_areas_plot <- crop(croppedProtected_areas(), sp_polygon_3162_buffered, mask = TRUE)
        #Creates a table to map land class labels to numeric values in 'Ontario_land_cover' (for the plot legend essentially)
        land_cover_labels <- land_cover_labels_react()
        
        # Determine percentage of each land cover class in the *buffered* landscape, since this is what readers will see
        percent_landcover <- Protected_areas_plot %>% 
          data.frame() %>% 
          rename(value = 1) %>% 
          group_by(value) %>% 
          tally() %>% 
          mutate(percentage = round(n/sum(n)*100, 2)) %>% 
          dplyr::select(-n)
        
        #Extract unique values from the SpatRaster for the legend of the plot
        unique_values <- as.vector(unique(values(Protected_areas_plot)))
        
        # Make a map of colors and labels to raster values in cropped landscape
        df_joined <- data.frame(value = unique_values) %>%
          left_join(land_cover_labels, by = c("value" = "Value"))
        
        # Make a sf points feature (I previously used it to create labels for the map - so this code is likely overkill for what I end up doing...)
        label_points_sf <- as.data.frame(Protected_areas_plot, xy = TRUE, cells = TRUE) %>% 
          rename(value = 4) %>% 
          st_as_sf(coords = c("x", "y"), 
                   crs = crs(Protected_areas_plot)) %>% 
          left_join(df_joined, by = "value") %>% 
          # Add percentage of each Land Class in the map
          left_join(percent_landcover, by = "value") %>% 
          # some land classes have 0 percent coverage in the TARGET landscape
          # but are present in the buffer.
          mutate(percentage = if_else(is.na(percentage), 0, percentage)) %>% 
          # Create a new label for each class
          mutate(Label = paste0("              ", Land_Class, " (", percentage, "%)"),
                 # arrange the Land Class labels by percentage
                 Label = fct_infreq(Label))
        
        # get colors for plot
        plot_colors <- label_points_sf %>% 
          st_drop_geometry() %>% 
          arrange(Label) %>% 
          dplyr::select(value, color, Label, percentage) %>%
          arrange(desc(percentage)) %>% 
          distinct()
        
        # Ready the raster for plotting - assign colors
        Protected_areas_plot <- as.factor(Protected_areas_plot)
        
        levels(Protected_areas_plot) = data.frame(value = plot_colors$value, 
                                                  Label = plot_colors$Label)
        
        # plot in ggplot first
        my_ggplot <- ggplot() +
          geom_spatraster(data = Protected_areas_plot) +
          scale_fill_manual(name = "\nAreas of conservation concern percent cover\n",
                            values = plot_colors$color,
                            na.value = "white") +
          geom_sf(data = sp_polygon_3162_buffered,
                  aes(color = "Buffer extent"), fill = NA, linewidth = 1) +
          geom_sf(data = target_landscape, aes(color = "Target landscape"),
                  fill = NA, linewidth = 1) +
          scale_color_manual(name = "", values = c("lightgrey", "black")) +
          # Old line of code to get labels - not needed anymore
          # geom_sf(data = label_points_sf,
          #         aes(text = Label, color = Label), alpha = 0) +
          theme_minimal() + 
          labs(title = "\nAreas of conservation concern within target landscape\n") +
          theme(panel.grid = element_blank(), legend.margin = margin(c(0,0,0,0)),
                legend.key.size = unit(1.5, "cm"), # Adjust legend key size
                legend.text = element_text(size = 12, face = "bold"), # Adjust legend text size
                legend.title = element_text(size = 16, face = "bold"), # Adjust legend title size
                legend.box.spacing = unit(0.5, "cm")) + # Adjust spacing
          guides(fill = guide_legend(
            ncol = 2,
            label.position = "left",
            label.hjust = 1,
            order = 2),
            color = guide_legend(
              direction = "horizontal",
              label.position = "right",
              label.hjust = 0,
              order = 1))
        
        # strip out the legend - this makes it easier to format and put it where you might want it - whether above, below, or beside plotly interactive plot
        legend_plot2 <- cowplot::get_plot_component(my_ggplot, 'guide-box-right', return_all = TRUE)
        legend_plot2_react(legend_plot2)
        
        
        # Users can hover over the map output below and the raster value will appear.
        # I'm not totally happy with this - sometimes hover doesn't work.
        ggplotly(my_ggplot + theme(legend.position = "none",
                                   axis.text = element_blank()), 
                 tooltip = "value")
        
      })
      
      output$rasterPlot2legend <- renderPlot({
        req(input$protected_based_calculations > 0)
        legend_plot2<-legend_plot2_react()
        grid.newpage()
        grid.draw(legend_plot2)
      })
      

#_----
    #Tests for viability of landscape for connectivity calculations ----
    # The purpose of these tests are to check to see if a selected landscape extent provides sufficient
    # nodes for the connectivity calculations at later steps (assuming default settings), with the aim
    # of saving the users time and letting them know if they need to adjust their extent selection.
    # Essentially, this runs though future steps in a quick way to see if they will fail or not.

    # This creates degraded land condition layers
    test_cropped_mines <- croppedCHF_mines()
    test_cropped_AMIS <- croppedAMIS()
    test_cropped_night_lights <- croppedCHF_night_lights()
    test_cropped_oil_gas <- croppedCHF_oil_gas()
    test_cropped_forestry_harvest <- croppedCHF_forestry_harvest()
    test_cropped_aggregate_extraction <- croppedSOLRIS_AggregateExtraction_204()
    test_cropped_topsoil_extraction <- croppedSOLRIS_TopsoilExtraction_205()
    test_cropped_undifferentiated <- croppedSOLRIS_Undifferentiated_250()

    # This establishes a degraded pixels raster using default settings
    test_condition_mines <- test_cropped_mines >= 2
    test_condition_AMIS <- test_cropped_AMIS >= 1
    test_condition_night_lights <- test_cropped_night_lights >= 1
    test_condition_oil_gas <- test_cropped_oil_gas >= 6
    test_condition_forestry_harvest <- test_cropped_forestry_harvest >= 4
    test_condition_aggregate_extraction <- test_cropped_aggregate_extraction >= 1
    test_condition_topsoil_extraction <- test_cropped_topsoil_extraction >= 1
    test_condition_undifferentiated <- test_cropped_undifferentiated >= 1

    test_final_condition <- test_condition_mines | test_condition_night_lights | test_condition_oil_gas | test_condition_forestry_harvest |
      test_condition_AMIS | test_condition_aggregate_extraction | test_condition_topsoil_extraction | test_condition_undifferentiated

    test_degraded_pixels_temp <- croppedOntario()
    test_unfit_values <- c(41, 52, 53, 55, 54, 51, 99) # Land classes unfit for restoration
    test_mask <- test_degraded_pixels_temp %in% test_unfit_values
    test_unfit_raster <- test_degraded_pixels_temp * test_mask
    test_unfit_raster[!test_mask] <- NA

    test_degraded_pixels_temp[test_final_condition] <- 100 # value for degraded land 
    test_degraded_pixels_temp[!is.na(test_unfit_raster)] <- NA
    test_degraded_pixels_temp[test_degraded_pixels_temp != 100] <- NA

    # This is a function used below to convert the pixel combinations to the right format
    convert_to_list <- function(mat) {
      list_of_matrices <- list()
      for (i in 1:ncol(mat)) {
        col_values <- mat[, i]
        col_values <- col_values[!is.na(col_values)]
        dim(col_values) <- c(length(col_values), 1)
        list_of_matrices[[i]] <- col_values
      }
      return(list_of_matrices)
    }

    # This sets the combination of degraded to restored pixels in the landscape to calculate metrics for
    num_combined_pixels <- 1
    valid_locations <- which(!is.na(values(test_degraded_pixels_temp)))
    if (length(valid_locations) >= num_combined_pixels) {
      combinations <- combn(valid_locations, num_combined_pixels)
      combinations <- convert_to_list(combinations)
    } else {
      # Handle the case when there are not enough valid locations - some landscapes will not have sufficient degraded land for calculations to succeed.
      showNotification(paste("Insufficient degraded land. Calculations for this landscape are likely to fail due to lack of degraded land."), type = "error", duration = NULL)
      combinations <- NULL
    }

    # Habitat connectivity test for sufficient nodes ---- 
    # this is a test to see if there are sufficient nodes in the habitat raster layer
    # to calculate connectivity metrics successfully.
    movement_cost_con <- croppedmovement_cost()
    movement_cost_con[!is.na(test_degraded_pixels_temp)] <- 1000
    movement_cost_raster <- raster(movement_cost_con)
    tryCatch(
      {
        patches <- (movement_cost_raster == 1)
        mpg <- MPG(movement_cost_raster, patch = patches)
        mc_neighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
        if (nrow(mc_neighbours) < 5) {
          stop("Error: Insufficient nodes for habitat connectivity metrics. A minimum of 5 nodes are required.")
        }
      },
      error = function(e) {
        # Handle errors by displaying a notification to the user
        showNotification(paste("Connectivity is measured between distinct patches, or “nodes” of suitable habitat for wildlife and plants. Connectivity metrics may fail to be calculated for the selected landscape due to insufficient nodes (< 5 nodes triggers this warning). Depending on options you choose in the next few steps – e.g., what defines degraded land – a successful calculation may still be possible. Alternatively, please add a buffer around the selected area, select or draw a larger extent, or re-define areas of conservation concern with additional features to produce sufficient nodes. You may disregard this warning if you are only interested in connectivity between nodes of areas of conservation concern."), type = "error", duration = NULL)
      }
    )
    removeNotification(notification_id_extent)
    rm(movement_cost_con, patches)

    # Areas of conservation concern test for sufficient nodes ---- 
    # this is basically the same connectivity test as above, but for the 'areas of conservation concern'
    # raster layer instead (if that option was selected in UI segment 2).
    if (req(input$protected_based_calculations > 0)) {
      notification_id_extent <- showNotification("Setting extent and testing calculation viability...", type = "message", duration = NULL)

      movement_cost_con <- croppedmovement_cost_protected()
      movement_cost_con[!is.na(test_degraded_pixels_temp)] <- 10000
      movement_cost_raster <- raster(movement_cost_con)
      tryCatch(
        {
          patches <- (movement_cost_raster == 1)
          mpg <- MPG(movement_cost_raster, patch = patches)
          mc_neighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
          if (nrow(mc_neighbours) < 5) {
            stop("Error: Insufficient nodes for areas of conservation concern connectivity metrics. A minimum of 5 nodes are required.")
          }
        },
        error = function(e) {
          # Handle errors by displaying a notification to the user
          showNotification(paste("Connectivity metrics for areas of conservation concern may fail to be calculated for the selected landscape due to insufficient nodes (< 5 nodes triggers this warning).
                             Depending on chosen options (degraded land definition and combination type) a successful calculation may still be possible.
                             Alternatively, please add a buffer, select/draw a larger extent, or re-define areas of conservation concern with additional features so as to produce sufficient nodes.
                             You may disregard this warning if only interested in connectivity between habitat nodes."), type = "error", duration = NULL)
        }
      )
      removeNotification(notification_id_extent)
      rm(movement_cost_con, patches)
    }
  })


  # Linked to UI Segment 5, defining degraded land ----

  # A slider panel that allows users to set the definition for degraded land
  output$sliderPanel <- renderUI({
    if (!is.null(croppedOntario())) {
      tagList(
        div(
          tags$label(`for` = "mine_slider", HTML("<b>Active mines</b><br><i>1 = Any active mine (open pit or underground), and areas within 10 km of any active mine are 
              degraded.<br>8 = Only land within 500 m of an open pit mine is degraded. For more detail on how active mine thresholds are defined, 
              see table 3, <a href='https://www.sciencedirect.com/science/article/abs/pii/S0169204608000637' target='_blank'>Woolmer et al. 2008</a>,
              also accessible as table 5 in <a href='https://www.facetsjournal.com/doi/full/10.1139/facets-2021-0063#tab5' target='_blank'>Hirsh-Pearson et al. 2022</a>.</i><br><br>")),
          sliderInput("mine_slider", NULL, min = 1, max = 9, value = 2, step = 1),
          tags$p(id = "mine_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "amis_slider", HTML("<b>Abandoned mines</b><br><i>1 = Pixels with 1 or more abandoned mines are degraded.<br>3 = Only pixels with 3 or more abandoned 
              mines are degraded.</i><br><br>")),
          sliderInput("amis_slider", NULL, min = 1, max = 4, value = 1, step = 1),
          tags$p(id = "amis_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "night_lights_slider", HTML("<b>Night lights</b><br><i>1 = Pixels with any amount of night light pollution are degraded.<br>10 = Only pixels with the most severe night light pollution
              are degraded.</i><br><br>")),
          sliderInput("night_lights_slider", NULL, min = 1, max = 11, value = 1, step = 1),
          tags$p(id = "night_lights_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "oil_gas_slider", HTML("<b>Oil and gas</b><br><i>1 = Any area within 5 km of an active oil and gas field is degraded.<br>10 = Only areas within 300 m of active oil and gas activity
              are degraded.</i><br><br>")),
          sliderInput("oil_gas_slider", NULL, min = 1, max = 11, value = 6, step = 1),
          tags$p(id = "oil_gas_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "forestry_harvest_slider", HTML("<b>Forestry harvest</b><br><i>2 = Forest land that has been disturbed, but is past the early stages of regrowth following clearing<br>4 = Forest in early
          regeneration, within 0-12 years of cutting. For more detail on how forestry thresholds are defined, 
              see <a href='https://www.sciencedirect.com/science/article/abs/pii/S0169204608000637?via%3Dihub' target='_blank'>Woolmer et al. 2008</a>,
              and <a href='https://www.facetsjournal.com/doi/full/10.1139/facets-2021-0063#tab5' target='_blank'>Hirsh-Pearson et al. 2022</a>.</i><br><br>")),
          sliderInput("forestry_harvest_slider", NULL, min = 2, max = 5, value = 4, step = 1),
          tags$p(id = "forestry_harvest_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "aggregate_extraction_slider", HTML("<b>Aggregate extraction</b><br><i>1 = Any area affected by aggregate extraction is degraded.</i><br><br>")),
          sliderInput("aggregate_extraction_slider", NULL, min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "aggregate_extraction_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "topsoil_extraction_slider", HTML("<b>Topsoil extraction</b><br><i>1 = Any area affected by topsoil extraction is degraded.</i><br><br>")),
          sliderInput("topsoil_extraction_slider", NULL, min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "topsoil_extraction_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "undifferentiated_slider", HTML("<b>Undifferentiated land</b><br><i>1 = Brownfields and marginal farmland are considered degraded land. Note that any pixels classified as 'undifferentiated' 
              by the Southern Ontario Land Resource Information System, SOLRIS, that are also classified as 'good farmland' in the Canada Land Inventory will not be classed 
              as degraded here.</i><br><br>")),
          sliderInput("undifferentiated_slider", NULL, min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "undifferentiated_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        )
      )
    }
  })

  # Bring in the degraded land rasters ----
  observeEvent(input$preview, {
    cropped_mines <- croppedCHF_mines()
    cropped_AMIS <- croppedAMIS()
    cropped_night_lights <- croppedCHF_night_lights()
    cropped_oil_gas <- croppedCHF_oil_gas()
    cropped_forestry_harvest <- croppedCHF_forestry_harvest()
    cropped_aggregate_extraction <- croppedSOLRIS_AggregateExtraction_204()
    cropped_topsoil_extraction <- croppedSOLRIS_TopsoilExtraction_205()
    cropped_undifferentiated <- croppedSOLRIS_Undifferentiated_250()

    ## Filter rasters to definitions the user has set via the sliders ----
    condition_mines <- cropped_mines >= input$mine_slider
    condition_AMIS <- cropped_AMIS >= input$amis_slider
    condition_night_lights <- cropped_night_lights >= input$night_lights_slider
    condition_oil_gas <- cropped_oil_gas >= input$oil_gas_slider
    condition_forestry_harvest <- cropped_forestry_harvest >= input$forestry_harvest_slider
    condition_aggregate_extraction <- cropped_aggregate_extraction >= input$aggregate_extraction_slider
    condition_topsoil_extraction <- cropped_topsoil_extraction >= input$topsoil_extraction_slider
    condition_undifferentiated <- cropped_undifferentiated >= input$undifferentiated_slider

    # A final cumulative condition (encompassing all the conditions above) is then used to define degraded pixels
    final_condition <- condition_mines | condition_night_lights | condition_oil_gas | condition_forestry_harvest | condition_AMIS | condition_aggregate_extraction | condition_topsoil_extraction | condition_undifferentiated

    degraded_pixels_temp <- croppedOntario()
    degraded_protected <- croppedProtected_areas()

    ## Remove values unfit for restoration ----
    # Water (41), Built up area-pervious (51),
    # Anthropogenic (52), Cropland (53), Hay/pasture (54), 
    # Transportation (55), Unclassified (99)
    unfit_values <- c(41, 51, 52, 53, 54, 55, 99) 

    mask <- degraded_pixels_temp %in% unfit_values
    unfit_raster <- degraded_pixels_temp * mask
    unfit_raster[!mask] <- NA


    degraded_pixels_temp[final_condition] <- 100 # Pixels that meet the cumulative condition are set to degraded
    degraded_pixels_temp[!is.na(unfit_raster)] <- NA

    degraded_protected[final_condition] <- 100 # an areas of conservation concern version of this is done as well (mainly for plotting)
    degraded_protected[is.na(degraded_pixels_temp)] <- NA
    degraded_protected_react(degraded_protected)
    
    sp_polygon_3162_buffered <- st_buffer(sp_plot_polygon(), buffer_value())
    
    plot_degraded_protected <- degraded_protected
    plot_degraded_protected <- crop(plot_degraded_protected, sp_polygon_3162_buffered, mask = TRUE)
    plot_degraded_protected[!(plot_degraded_protected[] %in% c(1, 100))] <- NA
    
    plot_degraded_pixels <- degraded_pixels_temp
    plot_degraded_pixels <- crop(plot_degraded_pixels, sp_polygon_3162_buffered, mask = TRUE)

    # These reactive values are used to store the degraded pixels for plotting later
    plot_degraded_react(plot_degraded_pixels)
    plot_degraded_protected_react(plot_degraded_protected)

    degraded_pixels_temp[degraded_pixels_temp != 100] <- NA

    # This is used to get a count of the number of degraded pixels
    deg_pixel_count <- raster(plot_degraded_pixels)
    degraded_array <- matrix(raster::extract(
      deg_pixel_count,
      raster::extent(deg_pixel_count)
    ), ncol = 1, nrow = ncell(deg_pixel_count))
    num_degraded <- sum(degraded_array == 100, na.rm = TRUE)
    numDegradedPixels(num_degraded) # Save the number of degraded pixels for use in later steps


    # The number of degraded pixels is output in the UI
    output$numDegradedPixels <- renderText({
      paste("Number of degraded pixels:", numDegradedPixels())
    })

    degraded_pixels(degraded_pixels_temp) # a reactive values stores the degraded pixels raster for use later

    # Plot target landscape with degraded pixels ----
    # a plot is output showing the landscape with non-tolerable pixels excluded and degraded pixels in orange
    
    # Get the target landscape
    target_landscape <- sp_plot_polygon()
    
    output$degradedPlot <- renderPlotly({
      plot_degraded_pixels <- plot_degraded_react()
      
      #Creates a table to map land class labels to numeric values in 'Ontario_land_cover' (for the plot legend essentially)
      land_cover_labels <- land_cover_labels_react()
      
      # Determine percentage of each land cover class in the *buffered* landscape, since this is what readers will see
      percent_landcover <- plot_degraded_pixels %>% 
        data.frame() %>% 
        rename(value = 1) %>% 
        group_by(value) %>% 
        tally() %>% 
        mutate(percentage = round(n/sum(n)*100, 2)) %>% 
        dplyr::select(-n)
      
      #Extract unique values from the SpatRaster for the legend of the plot
      unique_values <- as.vector(unique(values(plot_degraded_pixels)))
      
      # Make a map of colors and labels to raster values in cropped landscape
      df_joined <- data.frame(value = unique_values) %>%
        left_join(land_cover_labels, by = c("value" = "Value"))
      
      # Make a sf points feature (I previously used it to create labels for the map - so this code is likely overkill for what I end up doing...)
      label_points_sf <- as.data.frame(plot_degraded_pixels, xy = TRUE, cells = TRUE) %>% 
        rename(value = 4) %>% 
        st_as_sf(coords = c("x", "y"), 
                 crs = crs(plot_degraded_pixels)) %>% 
        left_join(df_joined, by = "value") %>% 
        # Add percentage of each Land Class in the map
        left_join(percent_landcover, by = "value") %>% 
        # some land classes have 0 percent coverage in the TARGET landscape
        # but are present in the buffer.
        mutate(percentage = if_else(is.na(percentage), 0, percentage)) %>% 
        # Create a new label for each class
        mutate(Label = paste0("              ", Land_Class, " (", percentage, "%)"),
               # arrange the Land Class labels by percentage
               Label = fct_infreq(Label))
      
      # get colors for plot
      plot_colors <- label_points_sf %>% 
        st_drop_geometry() %>% 
        arrange(Label) %>% 
        dplyr::select(value, color, Label, percentage) %>%
        arrange(desc(percentage)) %>% 
        distinct()
      
      # Ready the raster for plotting - assign colors
      plot_degraded_pixels <- as.factor(plot_degraded_pixels)
      
      levels(plot_degraded_pixels) = data.frame(value = plot_colors$value, 
                                                Label = plot_colors$Label)
      
      # plot in ggplot first
      my_ggplot <- ggplot() +
        geom_spatraster(data = plot_degraded_pixels) +
        scale_fill_manual(name = "\nHabitat type and percent cover\n",
                          values = plot_colors$color,
                          na.value = "white") +
        geom_sf(data = sp_polygon_3162_buffered,
                aes(color = "Buffer extent"), fill = NA, linewidth = 1) +
        geom_sf(data = target_landscape, aes(color = "Target landscape"),
                fill = NA, linewidth = 1) +
        scale_color_manual(name = "", values = c("lightgrey", "black")) +
        # Old line of code to get labels - not needed anymore
        # geom_sf(data = label_points_sf,
        #         aes(text = Label, color = Label), alpha = 0) +
        theme_minimal() + 
        labs(title = "\nTarget landscape showing degraded pixels and\nhabitat types considered suitable for restoration\n") +
        theme(panel.grid = element_blank(), legend.margin = margin(c(0,0,0,0)),
              legend.key.size = unit(1.5, "cm"), # Adjust legend key size
              legend.text = element_text(size = 12, face = "bold"), # Adjust legend text size
              legend.title = element_text(size = 16, face = "bold"), # Adjust legend title size
              legend.box.spacing = unit(0.5, "cm")) + # Adjust spacing
        guides(fill = guide_legend(
          ncol = 2,
          label.position = "left",
          label.hjust = 1,
          order = 2),
          color = guide_legend(
            direction = "horizontal",
            label.position = "right",
            label.hjust = 0,
            order = 1))
      
      # strip out the legend - this makes it easier to format and put it where you might want it - whether above, below, or beside plotly interactive plot
      legend_plot3 <- cowplot::get_plot_component(my_ggplot, 'guide-box-right', return_all = TRUE)
      grid.newpage()
      grid.draw(legend_plot3)
      legend_plot3_react(legend_plot3)
      
      
      # Users can hover over the map output below and the raster value will appear.
      # I'm not totally happy with this - sometimes hover doesn't work.
      ggplotly(my_ggplot + theme(legend.position = "none",
                                 axis.text = element_blank()), 
               tooltip = "value")
      
    })
    
    output$rasterPlot3legend <- renderPlot({
      legend_plot3<-legend_plot3_react()
      grid.newpage()
      grid.draw(legend_plot3)
    })
    
    
    # A plot for 'areas of conservation concern' if that option is selected in UI segment 2
    
    # Get the target landscape
    target_landscape <- sp_plot_polygon()

    output$degradedprotectedPlot <- renderPlotly({
      req(input$protected_based_calculations > 0)
      plot_degraded_protected <- plot_degraded_protected_react()
        
        #Creates a table to map land class labels to numeric values in 'Ontario_land_cover' (for the plot legend essentially)
        land_cover_labels <- land_cover_labels_react()
        
        # Determine percentage of each land cover class in the *buffered* landscape, since this is what readers will see
        percent_landcover <- plot_degraded_protected %>% 
          data.frame() %>% 
          rename(value = 1) %>% 
          group_by(value) %>% 
          tally() %>% 
          mutate(percentage = round(n/sum(n)*100, 2)) %>% 
          dplyr::select(-n)
        
        #Extract unique values from the SpatRaster for the legend of the plot
        unique_values <- as.vector(unique(values(plot_degraded_protected)))
        
        # Make a map of colors and labels to raster values in cropped landscape
        df_joined <- data.frame(value = unique_values) %>%
          left_join(land_cover_labels, by = c("value" = "Value"))
        
        # Make a sf points feature (I previously used it to create labels for the map - so this code is likely overkill for what I end up doing...)
        label_points_sf <- as.data.frame(plot_degraded_protected, xy = TRUE, cells = TRUE) %>% 
          rename(value = 4) %>% 
          st_as_sf(coords = c("x", "y"), 
                   crs = crs(plot_degraded_protected)) %>% 
          left_join(df_joined, by = "value") %>% 
          # Add percentage of each Land Class in the map
          left_join(percent_landcover, by = "value") %>% 
          # some land classes have 0 percent coverage in the TARGET landscape
          # but are present in the buffer.
          mutate(percentage = if_else(is.na(percentage), 0, percentage)) %>% 
          # Create a new label for each class
          mutate(Label = paste0("              ", Land_Class, " (", percentage, "%)"),
                 # arrange the Land Class labels by percentage
                 Label = fct_infreq(Label))
        
        # get colors for plot
        plot_colors <- label_points_sf %>% 
          st_drop_geometry() %>% 
          arrange(Label) %>% 
          dplyr::select(value, color, Label, percentage) %>%
          arrange(desc(percentage)) %>% 
          distinct()
        
        # Ready the raster for plotting - assign colors
        plot_degraded_protected <- as.factor(plot_degraded_protected)
        
        levels(plot_degraded_protected) = data.frame(value = plot_colors$value, 
                                                  Label = plot_colors$Label)
        
        # plot in ggplot first
        my_ggplot <- ggplot() +
          geom_spatraster(data = plot_degraded_protected) +
          scale_fill_manual(name = "\nDegraded land with percent cover\n",
                            values = plot_colors$color,
                            na.value = "white") +
          geom_sf(data = sp_polygon_3162_buffered,
                  aes(color = "Buffer extent"), fill = NA, linewidth = 1) +
          geom_sf(data = target_landscape, aes(color = "Target landscape"),
                  fill = NA, linewidth = 1) +
          scale_color_manual(name = "", values = c("lightgrey", "black")) +
          # Old line of code to get labels - not needed anymore
          # geom_sf(data = label_points_sf,
          #         aes(text = Label, color = Label), alpha = 0) +
          theme_minimal() + 
          labs(title = "\nDegraded land and areas of conservation concern\n") +
          theme(panel.grid = element_blank(), legend.margin = margin(c(0,0,0,0)),
                legend.key.size = unit(1.5, "cm"), # Adjust legend key size
                legend.text = element_text(size = 12, face = "bold"), # Adjust legend text size
                legend.title = element_text(size = 16, face = "bold"), # Adjust legend title size
                legend.box.spacing = unit(0.5, "cm")) + # Adjust spacing
          guides(fill = guide_legend(
            ncol = 2,
            label.position = "left",
            label.hjust = 1,
            order = 2),
            color = guide_legend(
              direction = "horizontal",
              label.position = "right",
              label.hjust = 0,
              order = 1))
        
        # strip out the legend - this makes it easier to format and put it where you might want it - whether above, below, or beside plotly interactive plot
        legend_plot4 <- cowplot::get_plot_component(my_ggplot, 'guide-box-right', return_all = TRUE)
        grid.newpage()
        grid.draw(legend_plot4)
        legend_plot4_react(legend_plot4)
        
        
        # Users can hover over the map output below and the raster value will appear.
        # I'm not totally happy with this - sometimes hover doesn't work.
        ggplotly(my_ggplot + theme(legend.position = "none",
                                   axis.text = element_blank()), 
                 tooltip = "value")
        
      })
      
      output$rasterPlot4legend <- renderPlot({
        req(input$protected_based_calculations > 0)
        legend_plot4<-legend_plot4_react()
        grid.newpage()
        grid.draw(legend_plot4)
      })
    })



  # Linked to UI Segment 6, simulating restoration of degraded pixels ----

  observeEvent(input$simulate_restoration, {
    # This locks out buttons while the calculation is running
    shinyjs::disable("simulate_restoration")
    shinyjs::disable("automatic_combination")
    shinyjs::disable("manual_combination")
    notification_id_res <- showNotification("Simulating restored land...", type = "message", duration = NULL)


    Ontario_degraded_land <- ifel(is.na(degraded_pixels()), croppedOntario(), degraded_pixels())
    sample_degraded_land <- raster(Ontario_degraded_land)
    restored_land <- raster(Ontario_degraded_land)

    # Process sample_degraded_land
    remove_values <- c(100, 41, 52, 53, 55, 54, 51, 99)
    for (val in remove_values) {
      sample_degraded_land[sample_degraded_land == val] <- NA
      sample_degraded_land(sample_degraded_land)
    }

    # Process restored_land
    remove_values <- c(41, 52, 53, 55, 54, 51, 99) # , 250
    for (val in remove_values) {
      restored_land[restored_land == val] <- NA
    }

    # Initialize kernel (i.e neighborhood) size and iteration counter
    kernel_size <- 3
    iteration <- 1
    # Create a custom function to calculate the mode
    custom_mode <- function(x) {
      x <- na.omit(x)
      if (length(x) == 0) {
        return(NA)
      }
      table_x <- table(x)

      # Find the mode value
      mode_val <- as.integer(names(sort(table_x, decreasing = TRUE)[1]))
      # If '100' is present and there are other values, prioritize the next prominent value - this is so pixels aren't 'restored' to the degraded class
      if ("100" %in% names(table_x) && length(table_x) > 1) {
        other_vals <- names(table_x)[names(table_x) != "100"]
        next_prominent_val <- other_vals[which.max(table_x[other_vals])]
        mode_val <- as.integer(next_prominent_val)
      }
      return(mode_val)
    }


    # Initialize a flag to track whether any replacements were made
    replacements_made <- TRUE
    # Continue iterating until no more 100 values (degraded class pixels) are found
    while (replacements_made) {
      # Use the focal function to calculate the mode with the current kernel size
      kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)
      restored_land_mode <- focal(restored_land, w = kernel, fun = custom_mode, pad = TRUE)
      # Find the indices of cells with values equal to 100
      cells_to_replace <- which(restored_land[] == 100)
      if (length(cells_to_replace) > 0) {
        # Replace 100 values with the mode values
        restored_land[cells_to_replace] <- restored_land_mode[cells_to_replace]
        # Check if any 100 values remain
        replacements_made <- any(na.omit(restored_land[]) == 100)
      } else {
        # If no more 100 values to replace, exit the loop
        replacements_made <- FALSE
      }
      # Plot the modified raster for this iteration - maybe not necessary anymore? Originally I used this for diagnostic purposes
      output$restorationPlot <- renderPlot({
        plot(restored_land, main = paste("Iteration:", iteration), col = viridis(20, direction = -1))
      })
      iteration <- iteration + 1 # Increment iteration counter
      # Increase the kernel size by 2 every iteration. This significantly speeds up the procees for
      # 'islands' of pixels that are surrounded by NAs
      kernel_size <- kernel_size + 2
    }

    restored_land_plot <- rast(restored_land)
    sp_polygon_3162_buffered <- st_buffer(sp_plot_polygon(), buffer_value())
    restored_land_plot <- crop(restored_land_plot, sp_polygon_3162_buffered, mask = TRUE)
    restored_land_plot_react(restored_land_plot)
    
    restored_land(restored_land)

    removeNotification(notification_id_res)

    # Plot the restored landscape
    
    # Get the target landscape
    target_landscape <- sp_plot_polygon()
    
     output$restorationPlot <- renderPlotly({
      restored_land_plot <- restored_land_plot_react()
      
      #Creates a table to map land class labels to numeric values in 'Ontario_land_cover' (for the plot legend essentially)
      land_cover_labels <- land_cover_labels_react()
      
      # Determine percentage of each land cover class in the *buffered* landscape, since this is what readers will see
      percent_landcover <- restored_land_plot %>% 
        data.frame() %>% 
        rename(value = 1) %>% 
        group_by(value) %>% 
        tally() %>% 
        mutate(percentage = round(n/sum(n)*100, 2)) %>% 
        dplyr::select(-n)
      
      #Extract unique values from the SpatRaster for the legend of the plot
      unique_values <- as.vector(unique(values(restored_land_plot)))
      
      # Make a map of colors and labels to raster values in cropped landscape
      df_joined <- data.frame(value = unique_values) %>%
        left_join(land_cover_labels, by = c("value" = "Value"))
      
      # Make a sf points feature (I previously used it to create labels for the map - so this code is likely overkill for what I end up doing...)
      label_points_sf <- as.data.frame(restored_land_plot, xy = TRUE, cells = TRUE) %>% 
        rename(value = 4) %>% 
        st_as_sf(coords = c("x", "y"), 
                 crs = crs(restored_land_plot)) %>% 
        left_join(df_joined, by = "value") %>% 
        # Add percentage of each Land Class in the map
        left_join(percent_landcover, by = "value") %>% 
        # some land classes have 0 percent coverage in the TARGET landscape
        # but are present in the buffer.
        mutate(percentage = if_else(is.na(percentage), 0, percentage)) %>% 
        # Create a new label for each class
        mutate(Label = paste0("              ", Land_Class, " (", percentage, "%)"),
               # arrange the Land Class labels by percentage
               Label = fct_infreq(Label))
      
      # get colors for plot
      plot_colors <- label_points_sf %>% 
        st_drop_geometry() %>% 
        arrange(Label) %>% 
        dplyr::select(value, color, Label, percentage) %>%
        arrange(desc(percentage)) %>% 
        distinct()
      
      # Ready the raster for plotting - assign colors
      restored_land_plot <- as.factor(restored_land_plot)
      
      levels(restored_land_plot) = data.frame(value = plot_colors$value, 
                                                   Label = plot_colors$Label)
      
      # plot in ggplot first
      my_ggplot <- ggplot() +
        geom_spatraster(data = restored_land_plot) +
        scale_fill_manual(name = "\nRestored land composition\n",
                          values = plot_colors$color,
                          na.value = "white") +
        geom_sf(data = sp_polygon_3162_buffered,
                aes(color = "Buffer extent"), fill = NA, linewidth = 1) +
        geom_sf(data = target_landscape, aes(color = "Target landscape"),
                fill = NA, linewidth = 1) +
        scale_color_manual(name = "", values = c("lightgrey", "black")) +
        # Old line of code to get labels - not needed anymore
        # geom_sf(data = label_points_sf,
        #         aes(text = Label, color = Label), alpha = 0) +
        theme_minimal() + 
        labs(title = "\nTarget landscape with all degraded land restored\n") +
        theme(panel.grid = element_blank(), legend.margin = margin(c(0,0,0,0)),
              legend.key.size = unit(1.5, "cm"), # Adjust legend key size
              legend.text = element_text(size = 12, face = "bold"), # Adjust legend text size
              legend.title = element_text(size = 16, face = "bold"), # Adjust legend title size
              legend.box.spacing = unit(0.5, "cm")) + # Adjust spacing
        guides(fill = guide_legend(
          ncol = 2,
          label.position = "left",
          label.hjust = 1,
          order = 2),
          color = guide_legend(
            direction = "horizontal",
            label.position = "right",
            label.hjust = 0,
            order = 1))
      
      # strip out the legend - this makes it easier to format and put it where you might want it - whether above, below, or beside plotly interactive plot
      legend_plot5 <- cowplot::get_plot_component(my_ggplot, 'guide-box-right', return_all = TRUE)
      grid.newpage()
      grid.draw(legend_plot5)
      legend_plot5_react(legend_plot5)
      
      
      # Users can hover over the map output below and the raster value will appear.
      # I'm not totally happy with this - sometimes hover doesn't work.
      ggplotly(my_ggplot + theme(legend.position = "none",
                                 axis.text = element_blank()), 
               tooltip = "value")
      
    })
    
    output$rasterPlot5legend <- renderPlot({
      legend_plot5<-legend_plot5_react()
      grid.newpage()
      grid.draw(legend_plot5)
    })
    
    
    shinyjs::enable("simulate_restoration")
    shinyjs::enable("automatic_combination")
    shinyjs::enable("manual_combination")
  })

  # Linked to UI Segment 7, choosing how to set the combinations of restored pixels for metric calculations ----

  # Code to hide the UI element for combination type choice, after an option is selected.
  output$showManualCombo <- reactive({
    input$manual_combination > 0
  })
  outputOptions(output, "showManualCombo", suspendWhenHidden = FALSE)

  output$showAutoCombo <- reactive({
    input$automatic_combination > 0
  })
  outputOptions(output, "showAutoCombo", suspendWhenHidden = FALSE)


  # Linked to UI Segment 8, set and display the number of combinations if the manual option is selected ----

  output$numDegradedPixels_Seg8 <- renderText({
    paste("Number of degraded pixels:", numDegradedPixels())
  })


  output$numCombinations <- renderText({
    req(input$calculate_combinations)
    req(input$num_combined_pixels)
    n <- numDegradedPixels()
    r <- input$num_combined_pixels
    choose(n, r)
  })


  # Linked to UI Segment 9, Calculating habitat metrics ----

  # Code to create the dynamic checkbox for excluding specific habitat classes contained
  # in a given landscape from the habitat metrics calculation
  land_class_mapping <- setNames(
    c(11, 12, 13, 14, 15, 16, 17, 18, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 36, 37, 38, 41, 51, 52, 53, 54, 55, 99, 100, 204, 205, 250),
    c(
      "Prairie", "Savannah", "Alvar", "Dune", "Meadow", "Shrubland", "Barren", "Sparse treed", "Coniferous forest", "Mixedwood forest",
      "Deciduous forest", "Transitional forest", "Hedge row", "Coniferous treed swamp", "Mixedwood treed swamp", "Deciduous treed swamp",
      "Transitional treed swamp", "Thicket swamp", "Bog", "Fen", "Marsh", "Water", "Built up area-pervious", "Anthropogenic", "Cropland",
      "Hay/pasture", "Transportation", "Unclassified", "Degraded", "Aggregate extraction", "Topsoil/Peat extraction", "Undifferentiated"
    )
  )
  
  choices_reactive <- reactive({
    if (is.null(restored_land())) {
      return(NULL)
    }
    na.omit(unique(values(restored_land())))
  })
  
  output$dynamic_checkboxes <- renderUI({
    if (is.null(choices_reactive())) {
      return(NULL)
    }
    
    # Get the names of the selected habitat classes to exclude
    selected_labels <- names(land_class_mapping)[land_class_mapping %in% choices_reactive()]
    
    # Generate dynamic checkboxes with labels for accessibility
    tagList(
      HTML("You may be interested in calculating the benefit of restoring certain habitat types only. 
    If you would like to exclude habitat classes from the patch size calculations, please select them below:<br><br>"),
      
      div(
        id = "dynamic_checkboxes",
        lapply(selected_labels, function(label) {
          div(
            class = "field",
            div(class = "ui checkbox",
                tags$input(
                  type = "checkbox",
                  id = paste0("habitat_", gsub(" ", "_", label)),  # Create a unique ID for each checkbox
                  name = "selected_habitats",
                  value = label
                ),
                tags$label(`for` = paste0("habitat_", gsub(" ", "_", label)), label)  # Associate label with the checkbox
            )
          )
        })
      )
    )
  })


  result_habitat_data <- reactiveVal(data.frame()) # a reactive value to store results

  # Code that runs when the calculate_metrics button is clicked in the UI
  observeEvent(input$calculate_metrics, {
    # Buttons are disabled while the calculation is running
    shinyjs::disable("calculate_metrics")
    shinyjs::disable("perform_merge")
    # a notification is printed in the UI while the code runs
    notification_id_hab <- showNotification("Calculating habitat metrics...", type = "message", duration = NULL)


    selected_habitats <- land_class_mapping[names(land_class_mapping) %in% input$selected_habitats]

    # Extract the pixel values from restored_land and sample_degraded_land
    restored_land_values <- restored_land()
    restored_land_values <- rast(restored_land_values)

    sample_degraded_land_values <- restored_land_values
    sample_degraded_land_values[!is.na(degraded_pixels())] <- NA
    sample_degraded_land_values <- raster(sample_degraded_land_values)
    
    cropped_polygon<-cropped_polyrast()


    # Find the locations where restored_land has non-NA values, sample_degraded_land has NA values, and are within the selected polygon
    valid_locations <- which(!is.na(values(restored_land_values)) & is.na(values(sample_degraded_land_values)) & !is.na(values(cropped_polygon)))


    ### This generates combinations of restorable pixels, by grouping contiguous pixels that share the same habitat class into lists
    # This is the 'automatic' combination setting in the UI. If the manual setting is chosen then this code is not used.
    if (input$automatic_combination > 0) {
      # Define a function to find contiguous regions of pixels with the same habitat class
      find_contiguous <- function(valid_locations, raster_values, nrows, ncols) {
        # Initialize a list to store the regions found
        regions <- vector("list")
        # Initialize a logical vector to keep track of 'visited' locations
        visited <- logical(length(valid_locations))

        # Define a function to find neighbors of a given index in a raster
        neighbours <- function(idx) {
          row <- (idx - 1) %/% ncols + 1
          col <- idx - (row - 1) * ncols

          # Define possible neighbor offsets
          possible_neighbours <- expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1))
          possible_neighbours <- possible_neighbours[-5, ] # Remove the center (0,0) as it is the current pixel

          # Calculate row and column offsets for neighbors
          row_offsets <- possible_neighbours$x + row
          col_offsets <- possible_neighbours$y + col

          # Ensure the neighbors are within the bounds of the raster
          row_offsets <- pmin(pmax(row_offsets, 1), nrows)
          col_offsets <- pmin(pmax(col_offsets, 1), ncols)

          # Calculate indices of neighbors
          neighbour_indices <- (row_offsets - 1) * ncols + col_offsets
          neighbour_indices <- neighbour_indices[neighbour_indices != idx] # Exclude the current pixel
          neighbour_indices
        }

        # Loop through each valid location
        for (i in seq_along(valid_locations)) {
          idx <- valid_locations[i]
          if (!visited[i]) {
            val <- raster_values[idx]
            region_indices <- idx
            visited[i] <- TRUE

            to_explore <- region_indices

            # 'Explore' the region by finding contiguous pixels
            while (length(to_explore) > 0) {
              current_idx <- to_explore[1]
              to_explore <- to_explore[-1]

              current_neighbours <- neighbours(current_idx)
              new_neighbours <- current_neighbours[!(current_neighbours %in% region_indices)]

              # Check and add new neighbors with the same habitat class
              for (neighbour in new_neighbours) {
                if (neighbour %in% valid_locations && !visited[which(valid_locations == neighbour)] &&
                  !is.na(raster_values[neighbour]) && raster_values[neighbour] == val) {
                  region_indices <- c(region_indices, neighbour)
                  visited[which(valid_locations == neighbour)] <- TRUE
                  to_explore <- c(to_explore, neighbour)
                }
              }
            }
            # Store the found region/combination in the list
            regions[[length(regions) + 1]] <- region_indices
          }
        }
        # Return the list of regions
        regions
      }

      # Use the above function to find contiguous regions based on valid locations and their corresponding raster values
      contiguous_regions <- find_contiguous(valid_locations, restored_land_values[], nrow(restored_land_values), ncol(restored_land_values))

      # Create a matrix to store the indices for each group (as integers)
      max_length <- max(lengths(contiguous_regions))
      result_matrix <- matrix(NA_integer_, nrow = max_length, ncol = length(contiguous_regions))

      # Fill the matrix with indices for each group as integers
      for (i in seq_along(contiguous_regions)) {
        result_matrix[1:length(contiguous_regions[[i]]), i] <- as.integer(contiguous_regions[[i]])
      }

      combinations <- result_matrix

      # Function to convert columns to list elements and remove NA values (so they can be used properly in future steps)
      convert_to_list <- function(mat) {
        list_of_matrices <- list()
        for (i in 1:ncol(mat)) {
          col_values <- mat[, i]
          col_values <- col_values[!is.na(col_values)]
          dim(col_values) <- c(length(col_values), 1)
          list_of_matrices[[i]] <- col_values
        }
        return(list_of_matrices)
      }

      combinations <- convert_to_list(combinations)
      combinations_react(combinations)
      # saveRDS(combinations, file = "combinations.rds") - for troubleshooting
    } else {
      convert_to_list <- function(mat) {
        list_of_matrices <- list()
        for (i in 1:ncol(mat)) {
          col_values <- mat[, i]
          col_values <- col_values[!is.na(col_values)]
          dim(col_values) <- c(length(col_values), 1)
          list_of_matrices[[i]] <- col_values
        }
        return(list_of_matrices)
      }

      # Get the number of combinations from the input
      num_combined_pixels <- as.numeric(input$num_combined_pixels)
      combinations <- combn(valid_locations, num_combined_pixels)
      combinations <- convert_to_list(combinations)
      combinations_react(combinations)
      # saveRDS(combinations, file = "combinations.rds") - for troubleshooting
    }


    # Calculate the habitat metrics ----

    # to calculate the elapsed time
    start_time <- Sys.time()

    # Precompute values to save on overhead in the parallel process
    restored_values <- values(restored_land_values)
    degraded_patch_size <- lsm_c_area_mn(sample_degraded_land_values)

    calculate_metrics <- function(combination, degraded_raster, excluded_classes) {
      # Create a copy of the degraded raster and add the values from restored_land
      modified_raster <- degraded_raster
      modified_raster[combination] <- restored_values[combination]

      # Filter out the excluded classes
      included_classes <- setdiff(unique(modified_raster), excluded_classes)

      # Here, calculate the metrics for the modified raster
      modified_patch_size <- lsm_c_area_mn(modified_raster)

      # Calculate the differences for each class
      patch_size_diffs <- setNames(modified_patch_size$value - degraded_patch_size$value, paste0("patch_size_diff_class", modified_patch_size$class))
      
      # Combine the results
      c(
        list(combination = paste(combination, collapse = "-")),
        patch_size_diffs
      )
    }


    ## Parallel processing with furrr ----
    library(furrr)
    plan(multisession(workers = 2)) # Set the number of workers - 3 was most stable on the systems we tested on, but this could be set to anything

    calculate_metrics_parallel <- function(combination) {
      excluded_classes <- as.numeric(selected_habitats)
      metrics <- calculate_metrics(combination, sample_degraded_land_values, excluded_classes)

      # Extract patch size and cohesion differences
      patch_size_diffs <- metrics[names(metrics) %in% paste0("patch_size_diff_class", degraded_patch_size$class)]

      # Combine results into a data frame
      data.frame(combination = metrics$combination, patch_size_diffs)
    }

    # Use future_map_dfr to parallelize the computation for all combinations
    calculated_habitat_data <- future_map_dfr(
      seq_along(combinations),
      .options = furrr_options(seed = TRUE),
      ~ calculate_metrics_parallel(combinations[[.x]])
    )

    # Close parallel processing
    plan(sequential)

    # Filter out the selected classes
    if (length(selected_habitats) > 0) {
      cols_to_exclude <- c()
      for (class in selected_habitats) {
        cols_to_exclude <- c(cols_to_exclude, grep(paste0("class", class, "$"), colnames(calculated_habitat_data)))
      }
      calculated_habitat_data <- calculated_habitat_data[-cols_to_exclude]
    }

    # Mapping from class numbers to class names
    class_mapping <- c(
      `11` = "Prairie", `12` = "Savannah", `13` = "Alvar", `14` = "Dune", `15` = "Meadow",
      `16` = "Shrubland", `17` = "Barren", `18` = "Sparse Treed", `21` = "Coniferous Forest",
      `22` = "Mixedwood Forest", `23` = "Deciduous Forest", `24` = "Transitional Forest",
      `25` = "Hedge Row", `31` = "Coniferous Treed Swamp", `32` = "Mixedwood Treed Swamp",
      `33` = "Deciduous Treed Swamp", `34` = "Transitional Treed Swamp", `35` = "Thicket Swamp",
      `36` = "Bog", `37` = "Fen", `38` = "Marsh", `41` = "Water", `51` = "Built Up Area-Pervious",
      `52` = "Anthropogenic", `53` = "Cropland", `54` = "Hay/Pasture", `55` = "Transportation",
      `99` = "Unclassified", `100` = "Degraded"
    )


    # Function to rename the columns
    rename_columns <- function(df, class_mapping) {
      colnames(df) <- sapply(colnames(df), function(col) {
        new_name <- col
        for (class_num in names(class_mapping)) {
          class_name <- class_mapping[class_num]
          pattern <- paste0("class", class_num)
          replacement <- paste0(class_name, " (Class ", class_num, ")")
          new_name <- gsub(pattern, replacement, new_name)
        }
        new_name
      })
      df
    }

    # Rename the columns
    calculated_habitat_data <- rename_columns(calculated_habitat_data, class_mapping)

    # Update the reactiveVal with the computed data
    result_habitat_data(calculated_habitat_data)


    if (input$protected_based_calculations > 0) { # if the 'areas of conservation concern' option is selected in segment 2,
      # then patch size differences for these areas are calculated as well

      combinations <- combinations_react()
      degraded_protected <- degraded_protected_react()

      protected_restored <- croppedProtected_areas()
      protected_restored[protected_restored == 0] <- NA
      protected_restored[protected_restored == 41] <- NA

      degraded_protected[degraded_protected == 0] <- NA
      degraded_protected[degraded_protected == 100] <- NA
      degraded_protected <- raster(degraded_protected)

      # Precompute values
      restored_values <- values(protected_restored)
      degraded_patch_size <- lsm_c_area_mn(degraded_protected)$value

      # Create a function to calculate metrics for a given combination of 4 pixels added to Sample_degraded_land
      calculate_metrics <- function(combination, degraded_raster) {
        # Create a copy of the degraded raster
        modified_raster <- degraded_raster
        # Add the values from Restored_land at the specified combination locations
        modified_raster[combination] <- restored_values[combination]
        # Calculate the difference in patch size metrics for all classes
        patch_size_metrics <- lsm_c_area_mn(modified_raster)
        Protected_patch_size_difference <- patch_size_metrics$value - degraded_patch_size
        return(list(patch_size = Protected_patch_size_difference, raster = modified_raster))
      }

      library(furrr)

      plan(multisession(workers = 2)) # Adjust the number of workers as needed

      # Define a function for parallel computation
      calculate_metrics_parallel <- function(combination, degraded_raster) {
        metrics <- calculate_metrics(combination, degraded_raster)
        patch_size_difference <- sum(abs(metrics$patch_size))
        return(data.frame(combination = paste(combination, collapse = "-"), patch_size_difference))
      }


      # Use future_map_dfr to parallelize the computation for all combinations
      protected_result <- future_map_dfr(
        seq_along(combinations),
        .options = furrr_options(seed = TRUE),
        ~ calculate_metrics_parallel(combinations[[.x]], degraded_protected)
      )

      protected_result <- protected_result %>%
        rename(protected_patch_size_difference = patch_size_difference)

      # Close parallel processing
      plan(sequential)

      # combine this patch size metric with the other results table
      hab_result_combined <- merge(result_habitat_data(), protected_result, by = "combination")


      result_habitat_data(hab_result_combined)
    }

    # Calculate the elapsed time
    end_time <- Sys.time()
    elapsed_time(end_time - start_time)
    
    format_elapsed_time <- function(time_diff) {
      # Convert the difftime object to numeric seconds
      total_seconds <- as.numeric(time_diff, units = "secs")
      
      # Round the time to the nearest second
      rounded_seconds <- round(total_seconds)
      
      # Calculate minutes and remaining seconds
      minutes <- rounded_seconds %/% 60
      remaining_seconds <- rounded_seconds %% 60
      
      # Create the output string
      if (minutes > 0) {
        return(paste(minutes, "minute(s),", remaining_seconds, "seconds"))
      } else {
        return(paste(rounded_seconds, "seconds"))
      }
    }

    # Display the elapsed time in the UI
    output$elapsedTime <- renderText({
      paste("Elapsed time:", format_elapsed_time(elapsed_time()))
    })
    
    removeNotification(notification_id_hab)
    shinyjs::enable("perform_merge")
    shinyjs::enable("calculate_metrics")
  })



  # Linked to UI Segment 10, Merging habitat metrics results ----

  # Observer to handle merging of metrics or displaying un-merged results (whether to merge patch size differences by
  # habitat class or output them separately)
  observeEvent(input$perform_merge, {
    result_habitat_data_updated(result_habitat_data())
    
    # Safely checking the value of input$merge_metrics
    if (!is.null(input$merge_metrics)) {
      if (input$merge_metrics == "Merge") {
        updated_data <- result_habitat_data() %>%
          mutate(
            sum_habitat_patch_size_diff = rowSums(select(., starts_with("patch_size_diff")), na.rm = TRUE)
          ) %>%
          select(-starts_with("patch_size_diff"))
        
        result_habitat_data_updated(updated_data)
      } else if (input$merge_metrics == "Do not merge") {
        # Handle 'Do not merge' logic here (if different)
        result_habitat_data_updated(result_habitat_data())
      }
    } else {
      # Handle cases where merge_metrics is NULL
      showNotification("No option selected for merge metrics", type = "error")
    }
    
    
    
    # Display the results in the UI, merged or un-merged
    output$habitatTable <- renderDataTable({
      req(result_habitat_data_updated())
      # Round the values to 3 decimal places
      rounded_data <- result_habitat_data_updated()
      rounded_data[] <- lapply(rounded_data, function(x) if (is.numeric(x)) round(x, 3) else x)
      # rename 'protected_patch_size_difference' to 'Protected area patch size'
      if("protected_patch_size_difference" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'protected_patch_size_difference'] <- 'Change in protected area patch size'
      }
      # rename 'sum_habitat_patch_size_diff' to 'Sum patch size' if present
      if("sum_habitat_patch_size_diff" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'sum_habitat_patch_size_diff'] <- 'Sum change in habitat patch size'
      }
      # Use a pattern matching approach to rename the 'patch_size_diff' columns if they are present
      new_names <- names(rounded_data)
      new_names <- gsub("^patch_size_diff_(.+?) \\(Class \\d+\\)$", "Change in \\1 patch size", new_names)
      # Assign the new names back to the data frame
      names(rounded_data) <- new_names
      
      # Rename 'combination' column to 'Candidate area'
      names(rounded_data)[names(rounded_data) == 'combination'] <- 'Candidate area'
      # Get the total number of rows
      total_rows <- nrow(rounded_data)
      # Determine the number of zeros needed based on the total number of rows
      num_zeros <- nchar(as.character(total_rows))
      # Generate the new values for the 'Candidate area' column
      rounded_data$`Candidate area` <- sapply(1:total_rows, function(i) {
        # Count the number of '-' in the original entry
        original_value <- rounded_data$`Candidate area`[i]
        num_pixels <- str_count(original_value, "-") + 1
        # Generate the CA number with leading zeros
        ca_number <- sprintf(paste0("CA_%0", num_zeros, "d"), i)
        # Combine CA number with pixel information
        paste0(ca_number, " (", num_pixels, " pixels)")
      })
      datatable(rounded_data, rownames = FALSE, 
                options = list(scrollX = TRUE),         
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                  HTML("<br><strong style='font-size: 16px;'>Difference in average patch size (hectares) between original and restored landscape for each candidate area. Positive values indicate that patch size is greater in the restored landscape.</strong>")))
    })
    
    output$subtitleText1 <- renderUI({
      req(result_habitat_data_updated())  # Ensure that the table data is ready
      HTML("<p style='font-size: 14px; text-align: left;'>**Each candidate area has a unique ID: CA_001 (3 pixels) means candidate area 1, containing 3 pixels of degraded land.</p>")
    })
    
  })

  # Linked to UI Segment 11, Calculating landscape connectivity metrics ----

  # reactive value to store results for use later
  result_connectivity <- reactiveVal(data.frame())

  observeEvent(input$calculate_connectivity, {
    # lockout buttons
    shinyjs::disable("calculate_pca")
    shinyjs::disable("calculate_connectivity")
    notification_id_con <- showNotification("Calculating connectivity metrics...", type = "message", duration = NULL)
    start_time <- Sys.time()

    # Define the initial state of movement_cost with 1000 where degraded_pixels are not NA
    if (input$habitat_based_calculations > 0) {
      movement_cost_con <- croppedmovement_cost()
      movement_cost_con[!is.na(degraded_pixels())] <- 1000
    } else {
      # this is the alternative if the 'areas of conservation concern' option is selected in UI segment 2
      movement_cost_con <- croppedmovement_cost_protected()
      movement_cost_con[!is.na(degraded_pixels())] <- 10000
    }

    combinations <- combinations_react()

    movement_cost_raster <- raster(movement_cost_con)
    # Initialize a variable to store the result
    calculation_result <- NULL

    # Wrapping in a tryCatch block to capture errors and prevent crashes
    tryCatch(
      {
        # Function to calculate mean path resistance
        calculate_mean_resistance <- function(raster) {
          patches <- (raster == 1)
          mpg <- MPG(raster, patch = patches)
          mc_neighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
          mc_neighbours_df <- as.data.frame(mc_neighbours)
          colnames(mc_neighbours_df) <- c("Node 1", "Node 2", "Path distance (Resistance)")
          mean(mc_neighbours_df$`Path distance (Resistance)`)
        }

        # Calculate baseline mean resistance
        baseline_mean_res <- calculate_mean_resistance(movement_cost_raster)

        # Initialize a dataframe to store the results
        results <- data.frame()

        # Set up parallel processing
        plan(multisession(workers = 2))

        # Function to apply a combination and calculate the difference in resistance
        calculate_combination_resistance <- function(combination) {
          modified_raster <- movement_cost_raster
          modified_raster[combination] <- 1
          new_mean_res <- calculate_mean_resistance(modified_raster)
          reduced_resistance <- baseline_mean_res - new_mean_res
          c(list(combination = paste(combination, collapse = "-")),
            reduced_resistance = reduced_resistance
          )
        }

        # Calculate the differences using parallel processing for all combinations
        results <- future_map_dfr(
          .options = furrr_options(seed = TRUE),
          seq_along(combinations),
          ~ calculate_combination_resistance(combinations[[.x]])
        )

        # Store the result
        calculation_result <- results

        # Close parallel processing
        plan(sequential)
      },
      error = function(e) {
        # Handle errors by displaying a notification to the user
        showNotification(paste("An error occurred during calculation: ", e$message), type = "error", duration = NULL)
        plan(sequential)
      }
    ) # Close tryCatch block here

    # Report the result outside the tryCatch block (if no errors or warnings occurred)
    if (!is.null(calculation_result)) {
      result_connectivity(calculation_result)
    }

    removeNotification(notification_id_con)

    # Calculate the elapsed time
    end_time <- Sys.time()
    elapsed_time(end_time - start_time)

    format_elapsed_time <- function(time_diff) {
      # Convert the difftime object to numeric seconds
      total_seconds <- as.numeric(time_diff, units = "secs")
      
      # Round the time to the nearest second
      rounded_seconds <- round(total_seconds)
      
      # Calculate minutes and remaining seconds
      minutes <- rounded_seconds %/% 60
      remaining_seconds <- rounded_seconds %% 60
      
      # Create the output string
      if (minutes > 0) {
        return(paste(minutes, "minute(s),", remaining_seconds, "seconds"))
      } else {
        return(paste(rounded_seconds, "seconds"))
      }
    }
    

    # Display the elapsed time in the UI
    output$conTime <- renderText({
      paste("Elapsed time:", format_elapsed_time(elapsed_time()))
    })

    # Display the results in the UI
    output$connectivityTable <- renderDataTable({
      req(result_connectivity())
      
      # Round the values to 3 decimal places
      rounded_data <- result_connectivity()
      rounded_data[] <- lapply(rounded_data, function(x) if (is.numeric(x)) round(x, 3) else x)
      
      # rename 'reduced_resistance' to 'Change in path resistance'
      if("reduced_resistance" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'reduced_resistance'] <- 'Change in path resistance'
      }
        
        # Rename 'combination' column to 'Candidate area'
        names(rounded_data)[names(rounded_data) == 'combination'] <- 'Candidate area'
        # Get the total number of rows
        total_rows <- nrow(rounded_data)
        # Determine the number of zeros needed based on the total number of rows
        num_zeros <- nchar(as.character(total_rows))
        # Generate the new values for the 'Candidate area' column
        rounded_data$`Candidate area` <- sapply(1:total_rows, function(i) {
          # Count the number of '-' in the original entry
          original_value <- rounded_data$`Candidate area`[i]
          num_pixels <- str_count(original_value, "-") + 1
          # Generate the CA number with leading zeros
          ca_number <- sprintf(paste0("CA_%0", num_zeros, "d"), i)
          # Combine CA number with pixel information
          paste0(ca_number, " (", num_pixels, " pixels)")
        })
      
      datatable(rounded_data, rownames = FALSE, options = list(scrollX = TRUE),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                  HTML("<br><strong style='font-size: 16px;'>Difference in mean path resistance between original and restored landscape for each candidate area. Positive values indicate less resistance in the restored landscape.</strong>")))
    })

     output$subtitleText2 <- renderUI({
      req(result_connectivity())  # Ensure that the table data is ready
      HTML("<p style='font-size: 14px; text-align: left;'>**Each candidate area has a unique ID (e.g, CA-001 (3 pixels) means candidate area 1, containing 3 pixels of degraded land).</p>")
    })
    
    
    shinyjs::enable("calculate_pca")
    shinyjs::enable("calculate_connectivity")
  })

  # Linked to UI Segment 12, generating the environmental PCA raster ----

  observeEvent(input$calculate_pca, {
    # Lockout buttons
    shinyjs::disable("calculate_env_metrics")
    shinyjs::disable("calculate_pca")
    notification_id_PCA <- showNotification("PCA calculation is running...", type = "message", duration = NULL)
    # Get all the environmental rasters from the reactive values
    CLASS_00_env <- croppedCLASS_00()
    CLASS_05_env <- croppedCLASS_05()
    CLASS_06_env <- croppedCLASS_06()
    CLASS_12_env <- croppedCLASS_12()
    CLASS_13_env <- croppedCLASS_13()
    CLASS_14_env <- croppedCLASS_14()
    CLASS_15_env <- croppedCLASS_15()
    CLASS_16_env <- croppedCLASS_16()
    CLASS_18_env <- croppedCLASS_18()
    CLASS_21_env <- croppedCLASS_21()
    CLASS_23_env <- croppedCLASS_23()
    CLASS_24_env <- croppedCLASS_24()
    CLASS_25_env <- croppedCLASS_25()
    CLASS_26_env <- croppedCLASS_26()
    mean_temp_env <- croppedmean_temp()
    precipitation_env <- croppedprecipitation()
    Elevation_env <- croppedElevation()
    Soil_not_env <- croppedSoil_not()
    Soil_20_env <- croppedSoil_20()
    Soil_75_env <- croppedSoil_75()
    Soil_150_env <- croppedSoil_150()
    Soil_G150_env <- croppedSoil_G150()
    
    # Create a rasterstack
    raster_stack <- c(
      mean_temp_env, precipitation_env, Elevation_env, Soil_not_env, Soil_20_env, Soil_75_env,
      Soil_150_env, Soil_G150_env, CLASS_00_env, CLASS_05_env, CLASS_06_env, CLASS_12_env,
      CLASS_13_env, CLASS_14_env, CLASS_15_env, CLASS_16_env, CLASS_18_env, CLASS_21_env,
      CLASS_23_env, CLASS_24_env, CLASS_25_env, CLASS_26_env
    )

    # create a rasterPCA with the raster stack (RStoolbox function)
    pca_result_temp <- rasterPCA(raster_stack, maskCheck=FALSE)
    # Store the PCA result in the reactive value
    pca_result(pca_result_temp)

    # Print summary of the PCA to the UI
    output$pcaSummary <- renderDataTable({
      # Extract the components from the PCA summary
      sdev <- pca_result_temp$model$sdev
      loadings <- pca_result_temp$model$loadings

      # Calculate proportion of variance and cumulative proportion
      prop_var <- (sdev^2) / sum(sdev^2)
      cum_prop_var <- cumsum(prop_var)

      pca_table <- data.frame(
        `Principal component` = paste("PC", seq_along(sdev)),
        `Standard deviation` = sdev,
        `Proportion of variance` = prop_var,
        `Cumulative proportion` = cum_prop_var,
        check.names = FALSE
      )
      
      # Round the values in the table to 3 decimal places
      pca_table[] <- lapply(pca_table, function(x) if (is.numeric(x)) round(x, 3) else x)

      row.names(pca_table) <- NULL
      datatable(pca_table, rownames = FALSE, options = list(scrollX = TRUE),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                  HTML("<br><strong style='font-size: 16px;'>Summary of each environmental principal component.</strong>")))
    })
    
    removeNotification(notification_id_PCA)
    shinyjs::enable("calculate_env_metrics")
    shinyjs::enable("calculate_pca")
  })


  # Linked to UI Segment 13, environmental heterogeneity metrics calculation ----

  result_env_data <- reactiveVal(data.frame()) # to store results
  observeEvent(input$calculate_env_metrics, {
    # lockout buttons
    shinyjs::disable("merge_and_display")
    shinyjs::disable("calculate_env_metrics")
    notification_id_env <- showNotification("Calculating environmental metrics...", type = "message", duration = NULL)
    req(pca_result())
    num_combined_pixels <- as.numeric(input$num_combined_pixels)
    num_pcs <- input$num_pcs # This is the number of principal components the user chooses to use via the UI

    # Extracting the PCA rasters based on user input
    pca_rasters <- lapply(1:num_pcs, function(i) {
      pca_result()$map[[i]]
    })

    start_time_env <- Sys.time()
    calculated_env_data_list <- vector("list", length = num_pcs)

    # Define a function to calculate metrics for a given combination of pixels
    calculate_metrics <- function(combination, degraded_raster, env_values) {
      # Create a copy of the degraded raster
      modified_raster <- degraded_raster
      # Add the values from env_values at the specified combination locations
      modified_raster[combination] <- env_values[combination]
      # Calculate heterogeneity for the raster
      sa_metric <- sa(modified_raster)
      # Return the metrics as a list
      return(list(sa = sa_metric))
    }

    # Load the furrr library for parallel processing
    library(furrr)
    # Set up parallel processing with 3 workers
    plan(multisession(workers = 2))

    # Loop over for the number of principal components selected
    for (i in 1:num_pcs) {
      # Begin to set up the comparison of the degraded vs the restored raster
      Env_PCA <- pca_rasters[[i]]
      Deg_PCA <- pca_rasters[[i]]

      Deg_PCA[!is.na(degraded_pixels())] <- NA # We are treating degraded pixels as having no environmental heterogeneity

      # Extract pixel values from each raster
      env_values <- values(Env_PCA)
      Deg_PCA_values <- values(Deg_PCA)
      # Calculate environmental heterogeneity for the degraded landscape
      degraded_sa <- sa(Deg_PCA_values)

      # Get the pixel combinations from storage
      combinations <- combinations_react()

      # function to calculate metrics for each combination
      calculate_metrics_parallel <- function(combination) {
        metrics <- calculate_metrics(combination, Deg_PCA_values, env_values)
        # Calculate the difference in environmental heterogeneity between the degraded and restored landscapes
        sa_diff <- as.numeric(metrics$sa) - as.numeric(degraded_sa)
        # Return a data frame with the principal component, given combination, and heterogeneity difference
        return(data.frame(PC = paste0("Env_PC", i), combination = paste(combination, collapse = "-"), sa_diff = sa_diff))
      }

      # Use future_map_dfr for parallelization
      calculated_env_data <- future_map_dfr(seq_along(combinations), ~ calculate_metrics_parallel(combinations[[.x]]))
      # Store the results
      calculated_env_data_list[[i]] <- calculated_env_data
    }

    # Close parallel processing
    plan(sequential)


    # Combine the results into one data frame
    combined_calculated_env_data <- do.call(rbind, calculated_env_data_list)

    combined_calculated_env_data <- combined_calculated_env_data %>%
      spread(key = PC, value = sa_diff) %>%
      rename_with(~ paste0(., "_sa_diff"), -combination)


    # Update the reactiveVal with the computed data
    result_env_data(combined_calculated_env_data)

    # Calculate the elapsed time
    end_time_env <- Sys.time()
    elapsed_time_env(end_time_env - start_time_env)

    # Display the results in the UI
    output$environmentTable <- renderDataTable({
      req(result_env_data())
      # Round the values to 3 decimal places
      rounded_data <- result_env_data()
      rounded_data[] <- lapply(rounded_data, function(x) if (is.numeric(x)) round(x, 3) else x)
      
      
      # Extract the current column names
      new_names <- names(rounded_data)
      # Use gsub to replace the pattern
      new_names <- gsub("Env_PC(\\d+)_sa_diff", "Change in environmental heterogeneity: PC\\1", new_names)
      # Assign the new names back to the data frame
      names(rounded_data) <- new_names
      
      extract_first_number <- function(x) {
        as.numeric(sub("-.*", "", x))
      }
      # Apply the function to the 'combination' column
      first_numbers <- sapply(rounded_data$combination, extract_first_number)
      # Sort the dataframe based on the first pixel index
      rounded_data <- rounded_data[order(first_numbers), ]
      
      # Rename 'combination' column to 'Candidate area'
      names(rounded_data)[names(rounded_data) == 'combination'] <- 'Candidate area'
      # Get the total number of rows
      total_rows <- nrow(rounded_data)
      # Determine the number of zeros needed based on the total number of rows
      num_zeros <- nchar(as.character(total_rows))
      # Generate the new values for the 'Candidate area' column
      rounded_data$`Candidate area` <- sapply(1:total_rows, function(i) {
        # Count the number of '-' in the original entry
        original_value <- rounded_data$`Candidate area`[i]
        num_pixels <- str_count(original_value, "-") + 1
        # Generate the CA number with leading zeros
        ca_number <- sprintf(paste0("CA_%0", num_zeros, "d"), i)
        # Combine CA number with pixel information
        paste0(ca_number, " (", num_pixels, " pixels)")
      })
      
      datatable(rounded_data, rownames = FALSE, options = list(scrollX = TRUE),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                  HTML("<br><strong style='font-size: 16px;'>Difference in environmental heterogeneity between original and restored landscape for each candidate area and each principal component. Positive values indicate that environmental heterogeneity is greater in the restored landscape.</strong>")))
    })

    output$subtitleText3 <- renderUI({
      req(result_env_data()) # Ensure that the table data is ready
      HTML("<p style='font-size: 14px; text-align: left;'><em>In some cases, restoring degraded land will reduce overall heterogeneity of the landscape. Consider a situation where the landscape has only 10 total pixels; 3 pixels sample very different environments, and 7 are degraded but cover very similar environments. Restoring the 7 degraded pixels to one minimally varying environmental condition would reduce overall heterogeneity.</em><br>
       <br>**Each candidate area has a unique ID (e.g., CA-001 (3 pixels) means candidate area 1, containing 3 pixels of degraded land).</p>")
    })
    
    
    format_elapsed_time <- function(time_diff) {
      # Convert the difftime object to numeric seconds
      total_seconds <- as.numeric(time_diff, units = "secs")
      
      # Round the time to the nearest second
      rounded_seconds <- round(total_seconds)
      
      # Calculate minutes and remaining seconds
      minutes <- rounded_seconds %/% 60
      remaining_seconds <- rounded_seconds %% 60
      
      # Create the output string
      if (minutes > 0) {
        return(paste(minutes, "minute(s),", remaining_seconds, "seconds"))
      } else {
        return(paste(rounded_seconds, "seconds"))
      }
    }
    
    # Display the elapsed time in the UI
    output$elapsedTimeEnv <- renderText({
      paste("Elapsed time:", format_elapsed_time(elapsed_time_env()))
    })
    
    removeNotification(notification_id_env)
    shinyjs::enable("merge_and_display")
    shinyjs::enable("calculate_env_metrics")
  })



  # Linked to UI Segment 14,  Merging all results/metrics together in preparation for weighting ----

  # Display the results in the UI after the merge button is clicked
  observeEvent(input$merge_and_display, {
    # Merge all data sets
    merged_data <- merge(result_habitat_data_updated(), result_env_data(), by = "combination")
    merged_data <- merge(result_connectivity(), merged_data, by = "combination")
    # Scale the columns except 'combination'
    columns_to_scale <- setdiff(names(merged_data), "combination")
    scaled_data <- as.data.frame(lapply(merged_data[columns_to_scale], scale))
    colnames(scaled_data) <- colnames(merged_data[columns_to_scale])
    # Replace NAs with 0s after scaling (NAs were produced when the scale was applied to values of 0)
    scaled_data[is.na(scaled_data)] <- 0
    # Combine scaled columns with 'combination'
    merged_data <- cbind(merged_data["combination"], scaled_data)

    # Output the table in the UI
    output$mergedResultsTable <- renderDataTable({
      
      # Round the values to 3 decimal places
      rounded_data <- merged_data
      rounded_data[] <- lapply(rounded_data, function(x) if (is.numeric(x)) round(x, 3) else x)
      # rename 'protected_patch_size_difference' to 'Protected area patch size'
      if("protected_patch_size_difference" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'protected_patch_size_difference'] <- 'Change in protected area patch size'
      }
      # rename 'sum_habitat_patch_size_diff' to 'Sum patch size' if present
      if("sum_habitat_patch_size_diff" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'sum_habitat_patch_size_diff'] <- 'Sum change in habitat patch size'
      }
      # rename 'reduced_resistance' to 'Change in path resistance'
      if("reduced_resistance" %in% names(rounded_data)) {
        names(rounded_data)[names(rounded_data) == 'reduced_resistance'] <- 'Change in path resistance'
      }
      # Use a pattern matching approach to rename the 'patch_size_diff' and 'ENV_PC' columns if they are present
      new_names <- names(rounded_data)
      new_names <- gsub("^patch_size_diff_(.+?) \\(Class \\d+\\)$", "Change in \\1 patch size", new_names)
      new_names <- gsub("Env_PC(\\d+)_sa_diff", "Change in environmental heterogeneity: PC\\1", new_names)
      names(rounded_data) <- new_names
      
      extract_first_number <- function(x) {
        as.numeric(sub("-.*", "", x))
      }
      # Apply the function to the 'combination' column
      first_numbers <- sapply(rounded_data$combination, extract_first_number)
      # Sort the dataframe based on the first pixel index
      rounded_data <- rounded_data[order(first_numbers), ]
      
      # Add pixels column
      rounded_data$Pixels<-rounded_data$combination
      
      
      # Rename 'combination' column to 'Candidate area'
      names(rounded_data)[names(rounded_data) == 'combination'] <- 'Candidate area'
      # Get the total number of rows
      total_rows <- nrow(rounded_data)
      # Determine the number of zeros needed based on the total number of rows
      num_zeros <- nchar(as.character(total_rows))
      # Generate the new values for the 'Candidate area' column
      rounded_data$`Candidate area` <- sapply(1:total_rows, function(i) {
        # Count the number of '-' in the original entry
        original_value <- rounded_data$`Candidate area`[i]
        num_pixels <- str_count(original_value, "-") + 1
        # Generate the CA number with leading zeros
        ca_number <- sprintf(paste0("CA_%0", num_zeros, "d"), i)
        # Combine CA number with pixel information
        paste0(ca_number, " (", num_pixels, " pixels)")
      })
      final_data_table(rounded_data)
      rounded_data <- rounded_data[ , !names(rounded_data) %in% "Pixels"]
      datatable(rounded_data, rownames = FALSE, options = list(scrollX = TRUE),
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                  HTML("<br><strong style='font-size: 16px;'>Summary of scaled landscape metrics.</strong>")))
    })
      output$subtitleText4 <- renderUI({
      HTML("<p style='font-size: 14px; text-align: left;'>**Each candidate area has a unique ID (e.g, CA-001 (3 pixels) means candidate area 1, containing 3 pixels of degraded land).</p>")
    })
  })

  # Linked to UI Segment 15,  weighting and best combination plotting ----

  # Code to create the metrics weighting UI element
  output$weights_ui <- renderUI({
    merged_data <- final_data_table()
    
    # Exclude some columns to get variable names
    vars <- setdiff(names(merged_data), c("Pixels", "Candidate area"))
    
    # Identify the variables with "patch size" in their name
    patch_size_vars <- grep("patch size", vars, value = TRUE, ignore.case = TRUE)
    n_patch_size_vars <- length(patch_size_vars)
    
    # Set the default value for patch size variables and round to 3 decimal places
    patch_size_value <- if (n_patch_size_vars > 0) round(1 / n_patch_size_vars, 3) else 1
    
    # Generate UI inputs for all variables
    lapply(vars, function(var) {
      # Determine the value based on whether the variable is a patch size or not
      default_value <- if (var %in% patch_size_vars) patch_size_value else 1
      
      numericInput(
        inputId = paste0("weight_", gsub("[^a-zA-Z0-9]", "_", var)),
        label = paste("Weight for", var, ":"),
        value = default_value
      )
    })
  })
  
  # Code to ensure a valid top combinations input value always exists to prevent crashing
  input_num_top_combinations_counter <- reactiveVal(0)
  num_top_combinations_react<-reactiveVal(1)
  
  # Increment the counter each time the top combinations input changes
  observeEvent(input$num_top_combinations, {
    input_num_top_combinations_counter(input_num_top_combinations_counter() + 1)
  })
  
  observeEvent(input$num_top_combinations, {
    if (input_num_top_combinations_counter() >= 1) {
      if (!is.null(input$num_top_combinations) && input$num_top_combinations >= 1) {
        num_top_combinations_react(input$num_top_combinations)
      } else {
        num_top_combinations_react(1)
      }
    }
  })

  observeEvent(input$find_best_comb, {
    # Merging the data sets
    merged_data <- final_data_table()
    
    # Exclude some columns to get variable names
    vars <- setdiff(names(merged_data), c("Pixels", "Candidate area"))
    
    # Adjust the values of each variable based on user-provided weights
    for (var in vars) {
      weight_input_id <- paste0("weight_", gsub("[^a-zA-Z0-9]", "_", var))
      merged_data[[var]] <- merged_data[[var]] * input[[weight_input_id]]
    }
    # Calculate the sum of weighted values for each combination
    merged_data$`Sum weighted` <- rowSums(merged_data[, vars, drop = FALSE])
    merged_data_reactive(merged_data)
    
    # Identify the top N combinations with the maximum sum of weighted values
    num_top_combinations<-num_top_combinations_react()
    top_combinations <- merged_data[order(merged_data$`Sum weighted`, decreasing = TRUE), ][1:num_top_combinations, "Pixels"]
    top_combinations_text <-merged_data[order(merged_data$`Sum weighted`, decreasing = TRUE), ][1:num_top_combinations, c("Pixels", "Candidate area")]
    
    # Convert top_combinations to a data frame
    top_combinations <- data.frame(Pixels = top_combinations)
    
    # Store the top combinations for use in the plots
    best_comb_react(top_combinations)
    
    # Render the best combination index as text in the UI
    output$bestCombinationName <- renderUI({
      formatted_output <- mapply(function(i, CA) {
        paste0(
          "<div>",
          paste0("<b>#", i, ":&nbsp;&nbsp;&nbsp;", CA, "</b>"),
          "</div>\n",
          ifelse(i < num_top_combinations, "<br>", "")
        )
      }, 1:num_top_combinations, top_combinations_text$`Candidate area`)
      
      HTML(paste(formatted_output, collapse = ""))
    })
    
    # Create polygons for the top combinations
    plot_pixels <- degraded_pixels()
    all_indices <- 1:length(values(degraded_pixels()))
    
    plot_polygons <- list()
    for (i in 1:nrow(top_combinations)) {
      comb <- unlist(strsplit(as.character(top_combinations[i, "Pixels"]), "-"))
      # Subset the pixels for the current combination
      comb_pixels <- plot_pixels
      comb_pixels[-as.integer(comb)] <- NA
      crs(comb_pixels) <- "EPSG:3162"
      
      # Convert raster pixels to polygons
      comb_pixels <- as.polygons(comb_pixels, dissolve = TRUE)
      # Convert Polygons to sf object
      comb_pixels_sf <- st_as_sf(comb_pixels)
      
      # Re-project the sf object to EPSG:4326
      comb_pixels_sf <- st_transform(comb_pixels_sf, crs = 4326)
      
      # Assign labels to each polygon
      comb_pixels_sf$label <- as.character(i)
      
      # Store polygons in the list
      plot_polygons[[i]] <- comb_pixels_sf
    }
    
    # Combine all polygons into a single sf object
    plot_pixels_sf <- do.call(rbind, plot_polygons)
    export_sf(plot_pixels_sf)
    
    # Calculate centroids of each polygon
    centroids <- st_centroid(plot_pixels_sf)
    
    # Extract the coordinates of centroids
    centroid_coords <- st_coordinates(centroids)
    
    # Ensure centroids keep the label information
    centroids$label <- plot_pixels_sf$label
    
    output$map_best_comb <- renderLeaflet({
      
      # Convert the extent to an sf object
      extent_sf <- st_as_sfc(st_bbox(output_extent(), crs = st_crs(3162)))
      
      # Transform the extent to WGS84
      extent_wgs84 <- st_transform(extent_sf, crs = 4326)
      
      # Extract the coordinates for the bounding box in WGS84 as a list
      bbox <- as.list(st_bbox(extent_wgs84))
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addControl(
          html = "<h4 style='text-align: center; margin: 0;'>Top candidate areas for restoration</h4>", 
          position = "topleft"
        ) %>%
        addTiles(group = "Map View") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
        fitBounds(lng1 = bbox$xmin, lat1 = bbox$ymin, 
                  lng2 = bbox$xmax, lat2 = bbox$ymax) %>%
        addPolygons(
          data = plot_pixels_sf,
          fillColor = "deeppink",
          fillOpacity = 0.5,
          color = "black",
          weight = 0.5,
          group = "Top Candidate Areas"  # Specify the group name for the polygons
        ) %>%
        addLabelOnlyMarkers(
          lng = centroid_coords[, 1],  # Longitude of the centroid
          lat = centroid_coords[, 2],  # Latitude of the centroid
          label = centroids$label,  # Use the labels from the centroids
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, style = list(
            "font-weight" = "bold",
            "font-size" = "14px"
          ))
        ) %>%
        addLayersControl(
          baseGroups = c("Map View", "Satellite View"),
          overlayGroups = c("Top Candidate Areas"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        htmlwidgets::onRender("
    function(el, x) {
      var map = this;
      L.control.zoom({ position: 'bottomright' }).addTo(map);
    }
  ")
    })
    
    # Create a list to store individual plot outputs
    plot_outputs <- lapply(seq_len(nrow(top_combinations)), function(i) {
      comb <- top_combinations$Pixels[i]
      # Crop the 'Ontario_land_cover' raster to the specified extent
      cropped_raster <- cropped_polyrast()
      cropped_raster <- ifel(!is.na(cropped_raster), croppedOntario(), cropped_raster)
      
      # Here add the buffer
      buffered_landscape <- polyrast_plot()
      buffered_landscape <- resample(buffered_landscape, cropped_raster, method = "near")
      # Identify pixels that are non-NA in target_landscape but NA in cropped_raster
      mask_raster <- !is.na(buffered_landscape) & is.na(cropped_raster)
      # Assign a value of 300 to those pixels in cropped_raster
      cropped_raster[mask_raster] <- 300
      
      # Generate unique output ID for each plot
      output_id <- paste0("plot_", i)
      
      land_cover_labels <- land_cover_labels_react()
      
      # Extract pixel indices from the combination string
      best_combination_indices <- as.numeric(strsplit(comb, "-")[[1]])
      
      values(cropped_raster)[best_combination_indices] <- 101
      
      # Match raster values to land cover classes and convert to factor
      mapped_values <- factor(land_cover_labels$Land_Class[match(values(cropped_raster), land_cover_labels$Value)])
      
      # Create a categorical raster and set its categories
      cat_raster <- rast(cropped_raster)
      values(cat_raster) <- mapped_values
      levels(cat_raster) <- data.frame(ID = 1:length(levels(mapped_values)), LC = levels(mapped_values))
      
      # Define colors based on the reactive land_cover_labels
      num_classes <- length(levels(mapped_values))
      color_palette <- land_cover_labels$color[match(levels(mapped_values), land_cover_labels$Land_Class)]
      
      # Plot the cropped raster with the custom colors
      output[[output_id]] <- renderPlot({
        plot(cat_raster, main = paste("Candidate area for restoration #", i, ":", top_combinations_text$`Candidate area`[i]), col = color_palette, 
             axes = FALSE, 
             box = FALSE)
        # box(lty = "blank")  # Remove the plot border
      })
      
      plotOutput(output_id)
    })
    
    # Output the list of plot outputs
    output$plotsContainer <- renderUI({
      plot_outputs
    })
  })
  
  
  
  # Linked to UI Segment 16, exporting the landscape results to a .CSV ----

  # Observer to add landscape column/name based on user input
  observeEvent(input$add_column, {
    landscape_name <- input$landscape_name
    # Check to make sure the landscape_name is not null or blank
    if (!is.null(landscape_name) && landscape_name != "") {
      merged_data <- merged_data_reactive() # bring it out of the reactive value

      # Add a new column 'landscape' with the provided landscape name to the merged data
      merged_data$Landscape <- landscape_name
      merged_data <- merged_data[c("Landscape", names(merged_data)[names(merged_data) != "Landscape"])]
      merged_data <- merged_data[order(-merged_data$`Sum weighted`), ]
      merged_data_reactive(merged_data)
      landscape_added(TRUE) # After the landscape name is added, trigger the switch to make the download UI appear
    }
  })

  # The switch for the download button
  output$landscape_added <- reactive({
    landscape_added()
  })
  outputOptions(output, "landscape_added", suspendWhenHidden = FALSE)



  # Linked to UI Segment 17, downloading results to a CSV ----

  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$landscape_name, "_", Sys.Date(), ".csv", sep = "") # code for the output name - it includes the date,
      # which could be changed based on preference
    },
    content = function(file) {
      # Get the reactive data for the main content of the CSV
      data <- merged_data_reactive()

      # Get the extent object from the reactive value - this is used in the other app functions when CSVs are imported
      extent <- output_extent()

      # Convert the extent object to a numeric vector
      extent_values <- c(extent@xmin, extent@ymin, extent@xmax, extent@ymax)

      # Create a file connection to write the CSV
      con <- file(file, open = "wt")

      # Write the extent as a separate header in the CSV
      cat("Extent_Xmin,Extent_Ymin,Extent_Xmax,Extent_Ymax\n", file = con)
      cat(paste(extent_values, collapse = ","), "\n", file = con)

      # Write the data frame, appending to the file connection
      write.csv(data, con, row.names = FALSE, quote = FALSE)

      # Close the file connection
      close(con)
    }
  )
  
  # Download KML button
  output$downloadKML <- downloadHandler(
    filename = function() {
      if (!is.null(export_sf())) {
        paste0(input$landscape_name, "_top_combination", Sys.Date(), ".kml", sep = "")
      }
    },
    content = function(file) {
      if (!is.null(export_sf())) {
        # Write sf object to KML file
        sf::st_write(export_sf(), dsn = file, driver = "kml")
      }
    }
  )
  
  output$downloadplotRDS <- downloadHandler(
    filename = function() {
      # Define the name of the downloaded file
      paste(input$landscape_name, "_", Sys.Date(), ".RDS", sep = "")
    },
    content = function(file) {
      plot_objects<-plot_rds()
      # Save the list as an RDS file to the provided file path
      saveRDS(plot_objects, file = file, compress = "xz")
    }
  )
  
  # End of the single landscape analysis


  # Linked to UI Segment 18, Loading and merging multiple landscapes for analysis ----
  # (if that initial choice is selected in segment 1)

  # Create a reactiveValues object to store compared extents
  compared_extents <- reactiveValues(extents = list())

  # Reactive expression to read, join .csv files, and extract extents for mapping
  landscape_merged <- reactive({
    req(input$fileInput)
    files <- input$fileInput$datapath

    output$fileSelected <- reactive({
      result <- (length(files))
      returnedValue <- result
      return(returnedValue)
    })

    outputOptions(output, "fileSelected", suspendWhenHidden = FALSE)


    data_list <- list()
    polygons_list <- list()

    for (file in files) {
      # Read the extents from the CSV file
      extent_df <- read.csv(file, nrows = 1, header = FALSE, skip = 1, sep = ",")

      # Extract extent values using indices and ensure they are numeric
      xmin <- as.numeric(extent_df[1, 1])
      ymin <- as.numeric(extent_df[1, 2])
      xmax <- as.numeric(extent_df[1, 3])
      ymax <- as.numeric(extent_df[1, 4])

      # Extract the landscape name from row 4, column 1
      landscape <- read.csv(file, nrows = 1, header = FALSE, skip = 3)[1, 1]

      # Store extent values in compared_extents reactiveValues
      compared_extents$extents[[landscape]] <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)


      # Create the bounding box and sfc_polygon (for the leaflet map)
      bbox <- st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
      sfc_polygon <- st_as_sfc(bbox)

      # Set the CRS for the sfc_polygon
      st_crs(sfc_polygon) <- 3161

      # Now transform the CRS of the sfc_polygon to WGS 84 (that's what the leaflet map uses)
      sfc_polygon <- st_transform(sfc_polygon, 4326)

      # Add the transformed sfc_polygon to list
      polygons_list[[length(polygons_list) + 1]] <- sfc_polygon

      # Read and store data, skipping the first two lines (the extent information)
      df <- read.csv(file, skip = 2, stringsAsFactors = FALSE, sep = ",", check.names = FALSE)
      df <- df[, names(df) != "Sum weighted"]
      if (nrow(df) == 0) {
        stop("One of the files is empty: ", file)
      }
      data_list[[length(data_list) + 1]] <- df
    }

    joined_df <- dplyr::bind_rows(data_list)

    # Combine all polygons into a single sf object so their coverage can be plotted in the leaflet map
    combined_polygons <- NULL
    if (length(polygons_list) > 0) {
      combined_polygons <- Reduce(st_union, polygons_list)
    } else {
      stop("No polygons were created.")
    }

    # Return a list containing the data and the polygons
    list(data = joined_df, polygons = combined_polygons)
  })


  # Output for the Joined Data Table
  output$landscapeMergedTable <- renderDataTable({
    req(landscape_merged()$data)
    
    # Round the values to 3 decimal places
    rounded_data <- landscape_merged()$data
    rounded_data[] <- lapply(rounded_data, function(x) if (is.numeric(x)) round(x, 3) else x)
    
    datatable(rounded_data, rownames = FALSE, options = list(scrollX = TRUE),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left; white-space: pre-wrap; width: auto;',
                HTML("<br><strong style='font-size: 16px;'>Previously calculated metrics for candidate areas within the selected landscapes.</strong>")))
  })

  output$subtitleText5 <- renderUI({
    req(landscape_merged()$data)  # Ensure that the table data is ready
    HTML("<p style='font-size: 14px; text-align: left;'>**Each candidate area has a unique ID (e.g, CA-001 (3 pixels) means candidate area 1, containing 3 pixels of degraded land).</p>")
  })


  # Output for the Leaflet Map with a polygon indicating the area the CSVs cover
  output$map_lands <- renderLeaflet({
    req(landscape_merged()$polygons)
    leaflet() %>%
      addTiles(group = "Map View") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
      addPolygons(data = landscape_merged()$polygons) %>%
      addLayersControl(
        baseGroups = c("Map View", "Satellite View"))
  })


  # Linked to UI Segment 19, Ranking the best combinations across multiple landscapes ----


  # The weighting UI, similar to the single landscape analysis
  output$landscape_weights_ui <- renderUI({
    req(landscape_merged()$data)
    vars <- setdiff(names(landscape_merged()$data), c("Candidate area", "Landscape", "Pixels"))

    lapply(vars, function(var) {
      numericInput(
        inputId = paste0("weight_", gsub("[^a-zA-Z0-9]", "_", var)),
        label = paste("Weight for", var, ":"),
        value = 1
      )
    })
  })


  # This finds the best pixel combination, as in the single landscape analysis, but the table now includes multiple landscapes
  observeEvent(input$lands_best_comb, {
    req(landscape_merged()$data)
    landscape_data <- landscape_merged()$data
    vars <- setdiff(names(landscape_data), c("Candidate area", "Landscape", "Pixels"))

    for (var in vars) {
      weight_input_id <- paste0("weight_", gsub("[^a-zA-Z0-9]", "_", var))
      landscape_data[[var]] <- as.numeric(landscape_data[[var]]) * input[[weight_input_id]]
    }

    landscape_data$`Sum weighted` <- rowSums(landscape_data[, vars, drop = FALSE])


    # Get the top n combinations
    top_n <- landscape_data[order(landscape_data$`Sum weighted`, decreasing = TRUE), ][1:input$topNCombinations, ] # input.topNCombinations set via the UI
    
    # update the landscape_merged reactive value
    landscape_data <- landscape_data[order(-landscape_data$`Sum weighted`), ]
    landscape_output(landscape_data)

    # Output the top n combinations landscape names and indices in  HTML (this was the only way I found to prevent the text getting cut off?)
    output$topCombinations <- renderUI({
      formatted_output <- mapply(function(i, comb, land) {
        paste0(
          "<div>",
          paste0("<b>#", i, "&nbsp;&nbsp;&nbsp;", land, "<br>Index:&nbsp;&nbsp;&nbsp;", comb, "</b>"),
          "</div>\n",
          ifelse(i < input$topNCombinations, "<br>", "")
        )
      }, 1:input$topNCombinations, top_n$Pixels, top_n$Landscape)

      HTML(paste(formatted_output, collapse = ""))
    })

    
    # Create a list to store polygons for leaflet output
    plot_polygons <- list()
    
    # Create a list to store KML output
    KML_output <- list()
    
    for (i in seq_len(nrow(top_n))) {
      comb <- top_n$Pixels[i]
      land <- top_n$Landscape[i]
      
      # Retrieve extent values from compared_extents reactiveValues
      extent_values <- compared_extents$extents[[land]]
      
      # Create an Extent object
      extent_object <- extent(extent_values[1], extent_values[3], extent_values[2], extent_values[4])
      
      # Crop the OLC raster to the specified extent
      cropped_raster <- crop(OLC, extent_object)
      
      # Extract pixel indices from the combination string
      best_combination_indices <- as.numeric(strsplit(top_n$Pixels[i], "-")[[1]])
      
      values(cropped_raster)[best_combination_indices] <- 101
      cropped_raster[cropped_raster != 101] <- NA
      
      
      # Create a new, finer resolution raster template to minimize re-sampling distortions
      finer_raster <- rast(ext(cropped_raster), nrow = nrow(cropped_raster) * 15, ncol = ncol(cropped_raster) * 15)
      # Resample to the finer resolution
      comb_pixels_resampled <- resample(cropped_raster, finer_raster, method = "near")
      # Now re-project to WGS84
      comb_pixels_reproj <- project(comb_pixels_resampled, "EPSG:4326", method = "near")
      comb_pixels_stars <- st_as_stars(comb_pixels_reproj)
      
      # Convert the reprojected stars object to polygons
      plot_pixels_sf <- st_as_sf(comb_pixels_stars, merge = TRUE, as_points = FALSE, na.rm = TRUE)
      
      # Store the plot_pixels_sf in the KML_output list
      KML_output[[i]] <- plot_pixels_sf
      
      # Assign labels to each polygon
      plot_pixels_sf$label <- as.character(i)
      
      # Store polygons in the list
      plot_polygons[[i]] <- plot_pixels_sf
    }
    
    KML_output_react(KML_output)
    
    # Combine all polygons into a single sf object
    plot_pixels_sf <- do.call(rbind, plot_polygons)
    
    # Calculate centroids of each polygon
    centroids <- st_centroid(plot_pixels_sf)
    
    # Extract the coordinates of centroids
    centroid_coords <- st_coordinates(centroids)
    
    # Ensure centroids keep the label information
    centroids$label <- plot_pixels_sf$label
    
    # Use the centroids and labels from plot_pixels_sf when adding labels to the map
    output$map_best_comb_multi <- renderLeaflet({
      leaflet() %>%
        addTiles(group = "Map View") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite View") %>%
        setView(lng = -84.3870, lat = 50.2538, zoom = 5) %>%
        addPolygons(
          data = plot_pixels_sf,
          fillColor = "deeppink",
          fillOpacity = 0.5,
          color = "black",
          weight = 0.5
        ) %>%
        addLabelOnlyMarkers(
          lng = centroid_coords[, 1],  # Longitude of the centroid
          lat = centroid_coords[, 2],  # Latitude of the centroid
          label = centroids$label,  # Use the labels from the centroids
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
        ) %>%
        addLayersControl(
          baseGroups = c("Map View", "Satellite View"))
    })
    
    
    # I also wanted to create priority pixel plots for each of the top n landscapes

    # Create a list to store individual plot outputs
    plot_outputs <- lapply(seq_len(nrow(top_n)), function(i) {
      comb <- top_n$Pixels[i]
      land <- top_n$Landscape[i]

      # Retrieve extent values from compared_extents reactiveValues
      extent_values <- compared_extents$extents[[land]]

      # Create an Extent object
      extent_object <- extent(extent_values[1], extent_values[3], extent_values[2], extent_values[4])

      # Crop the 'Ontario_land_cover' raster to the specified extent
      cropped_raster <- crop(Ontario_land_cover, extent_object)

      # Generate unique output ID for each plot
      output_id <- paste0("plot_", i)
	    
	land_cover_labels <- land_cover_labels_react()

      # Extract pixel indices from the combination string
      best_combination_indices <- as.numeric(strsplit(comb, "-")[[1]])


      values(cropped_raster)[best_combination_indices] <- 101

      # Match raster values to land cover classes and convert to factor
      mapped_values <- factor(land_cover_labels$Land_Class[match(values(cropped_raster), land_cover_labels$Value)])

      # Create a categorical raster and set its categories
      cat_raster <- rast(cropped_raster)
      values(cat_raster) <- mapped_values
      levels(cat_raster) <- data.frame(ID = 1:length(levels(mapped_values)), LC = levels(mapped_values))

      num_classes <- length(levels(mapped_values))
      color_palette <- viridis(num_classes, direction = -1)

      priority_idx <- which(levels(mapped_values) == "Priority pixels")
      if (length(priority_idx) > 0) color_palette[priority_idx] <- "turquoise1"

      # Plot the cropped raster
      output[[output_id]] <- renderPlot({
        plot(cat_raster, main = paste("#", i, " ", land), col = color_palette)
      })

      plotOutput(output_id)
    })
    # Output the list of plot outputs
    output$dynamicPlots <- renderUI({
      plot_outputs
    })
  })

  # Linked to UI Segment 20, downloading results to a CSV ----
  
  output$download_multiple_data <- downloadHandler(
    filename = function() {
      paste("Multiple_landscapes_", Sys.Date(), ".csv", sep = "") # code for the output name - it includes the date,
      # which could be changed based on preference
    },
    content = function(file) {
      # Get the reactive data for the main content of the CSV
      data <- landscape_output()
      
      # Create a file connection to write the CSV
      con <- file(file, open = "wt")
      
      # Write the data frame, appending to the file connection
      write.csv(data, con, row.names = FALSE, quote = FALSE)
      
      # Close the file connection
      close(con)
    }
  )
  
  
  # Download KML button for multiple sf objects
  output$download_multiple_KML <- downloadHandler(
    filename = function() {
      paste0("Mutiple_comparison_KMLs_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      KML_output <- KML_output_react()
      
      # Create a temporary directory to store KML files, and remove previous files and zip folders (prevents "Error: Dataset already exists.")
      temp_dir <- tempdir()
      files <- list.files(temp_dir, full.names = TRUE, pattern = ".*\\.kml$")
      file.remove(files)
      files <- list.files(temp_dir, full.names = TRUE, pattern = ".*\\.zip$")
      file.remove(files)
      
      # Write each sf object to a separate KML file in the temporary directory
      purrr::walk2(KML_output, seq_along(KML_output), function(sf_obj, index) {
        kml_filename <- file.path(temp_dir, paste0("top_combination_", index, ".kml"))
        st_write(sf_obj, dsn = kml_filename, driver = "kml")
      })
      
      # Create a zip file containing all KML files in the temporary directory
      zip_filename <- file.path(temp_dir, "Mutiple_comparison_KMLs.zip")
      zip(zip_filename, files = list.files(temp_dir, pattern = ".*\\.kml$", full.names = TRUE), flags = '-r9Xj') # the flag argument specifies to ignore the folder directories and just take the files
      
      # Copy the zip file to the specified download location
      file.copy(zip_filename, file)
    }
  )
  
  
  # Linked to UI Segment 21, Visualizing landscapes based on RDS files ----
  
  library(tools)  # For file_path_sans_ext
  loaded_plots <- reactiveVal(list())
  
  observeEvent(input$view_landscapes, {
    req(input$vis_fileInput)
    
    # Initialize a list to store plot objects
    plot_list <- list()
    
    # Retrieve file names
    file_names <- input$vis_fileInput$name
    
    # Loop through each uploaded file
    for (i in seq_along(input$vis_fileInput$datapath)) {
      file <- input$vis_fileInput$datapath[i]
      file_name <- tools::file_path_sans_ext(file_names[i])
      
      # Read the RDS file
      plot_objects <- readRDS(file)
      
      # Print the file name for debugging
      print(paste("Processing file:", file_name))
      
      # Ensure the RDS file contains the expected plot elements
      if (is.list(plot_objects) && all(c("plotly", "legend") %in% names(plot_objects))) {
        # Update the Plotly plot title
        plot_objects$plotly <- plot_objects$plotly %>%
          layout(title = file_name)
        
        # Append the plots to the list
        plot_list[[file_name]] <- plot_objects
      } else {
        showNotification("Invalid RDS file format. Expected a list with plotly and legend elements.", type = "error")
      }
    }
    
    # Update the reactive value with the loaded plots
    loaded_plots(plot_list)
    
 
    output$vis_dynamicPlots <- renderUI({
      plot_output_list <- lapply(seq_along(plot_list), function(i) {
        file_name <- names(plot_list)[i]
        plot_objects <- plot_list[[file_name]]
        
        # Create UI for each plotly plot and legend
        fluidRow(
          column(width = 6, plotlyOutput(outputId = paste0("plotly_", i), height = "800px")),
          column(width = 6, plotOutput(outputId = paste0("legend_", i), height = "800px"))
        )
      })
      do.call(tagList, plot_output_list)
    })
    
    for (i in seq_along(plot_list)) {
      local({
        my_i <- i
        file_name <- names(plot_list)[my_i]
        plot_objects <- plot_list[[file_name]]
        
        output[[paste0("plotly_", my_i)]] <- renderPlotly({
          plot_objects$plotly
        })
        
        output[[paste0("legend_", my_i)]] <- renderPlot({
          grid.newpage()
          grid.draw(plot_objects$legend)
        })
      })
    }
  })
  
  
#Linked to UI Segment 22, Batch Processing-----
  
  # Create a reactive value to store a default batch_buffer_unit_value of 1200
  
  batch_input_use_counter <- reactiveVal(0)
  
  # Increment the counter each time the buffer input changes
  observeEvent(input$batch_buffer_unit_value, {
    batch_input_use_counter(batch_input_use_counter() + 1)
  })
  
  observeEvent(input$batch_buffer_unit_value, {
    if (batch_input_use_counter() > 1) {
    # Code to ensure a valid buffer value always exists to prevent crashing
    if (!is.null(input$batch_buffer_unit_value) && input$batch_buffer_unit_value >= 0) {
      buffer_value(input$batch_buffer_unit_value)
    } else {
      buffer_value(1200)
    }
    }
  })
  
  output$BatchsliderPanel <- renderUI({
    {
      tagList(
        div(
          sliderInput("mine_slider", HTML("<br><b>Active mines</b><br><i>1 = Any active mine (open pit or underground), 
		and areas within 10 km of any active mine are degraded.<br>8 = Only land within 500 m of an open pit mine
		 is degraded. For more detail on how active mine thresholds are defined, 
                      see table 3, <a href='https://www.sciencedirect.com/science/article/abs/pii/S0169204608000637' target='_blank'>Woolmer et al. 2008</a>,
                                          also accessible as table 5 in <a href='https://www.facetsjournal.com/doi/full/10.1139/facets-2021-0063#tab5' target='_blank'>Hirsh-Pearson et al. 2022</a>.
                                          </i><br><br>"), min = 1, max = 9, value = 2, step = 1),
          tags$p(id = "mine_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("amis_slider", HTML("<br><b>Abandoned mines</b><br><i>1 = Pixels with 1 or more abandoned mines are
		 degraded.<br>3 = Only pixels with 3 abandoned mines are degraded.</i><br><br>"), 
                      min = 1, max = 4, value = 1, step = 1),
          tags$p(id = "amis_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("night_lights_slider", HTML("<br><b>Night lights</b><br><i>1 = Pixels with any amount of night
		 light pollution are degraded.<br>10 = Only pixels with the most severe night light pollution are degraded.
		</i><br><br>"), min = 1, max = 11, value = 1, step = 1),
          tags$p(id = "night_lights_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("oil_gas_slider", HTML("<br><b>Oil and gas</b><br><i>1 = Any area within 5 km of an active oil
		 and gas field is degraded.<br>10 = Only areas within 300 m of active oil and gas activity are degraded.</i><br><br>"),
                      min = 1, max = 11, value = 6, step = 1),
          tags$p(id = "oil_gas_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          tags$label(`for` = "forestry_harvest_slider", HTML("<b>Forestry harvest</b><br><i>2 = Forest land that has been disturbed but is not in the early stages of regrowth following clearing<br>4 = early
          regeneration, within 0-12 years of cutting. For more detail on how forestry thresholds are defined, 
              see <a href='https://www.sciencedirect.com/science/article/abs/pii/S0169204608000637?via%3Dihub' target='_blank'>Woolmer et al. 2008</a>,
              and <a href='https://www.facetsjournal.com/doi/full/10.1139/facets-2021-0063#tab5' target='_blank'>Hirsh-Pearson et al. 2022</a>.</i><br><br>")),
          sliderInput("forestry_harvest_slider", NULL, min = 2, max = 5, value = 4, step = 1),
          tags$p(id = "forestry_harvest_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("aggregate_extraction_slider", HTML("<br><b>Aggregate extraction</b><br><i>1 = Any area affected
		 by aggregate extraction is degraded.</i><br><br>"),, min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "aggregate_extraction_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("topsoil_extraction_slider", HTML("<br><b>Topsoil extraction</b><br><i>1 = Any area affected by
		 topsoil extraction is degraded.</i><br><br>"), min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "topsoil_extraction_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        ),
        div(
          sliderInput("undifferentiated_slider", HTML("<br><b>Undifferentiated land</b><br><i>1 = Brownfields and marginal 
	farmland are considered degraded land. Note that any pixels classified as 'undifferentiated' by the Southern Ontario
	 Land Resource Information System, SOLRIS, that were also classified as 'good farmland' in the Canada Land Inventory 
	will not be classed as degraded here.</i><br><br>"), min = 1, max = 2, value = 1, step = 1),
          tags$p(id = "undifferentiated_max_label", "Not included", style = "text-align: right; margin-top: -20px;")
        )
      )
    }
  })
  
  sf_objects <- list(
    Conservation_reserves = st_read("vectors/Simplified_Conservation_reserve_regulated.shp"),
    Natural_heritage_value_areas = st_read("vectors/Simplified_Natural_Heritage_Value_Area.shp"),
    Natural_heritage_system_areas = st_read("vectors/Simplified_Natural_Heritage_System_Area.shp"),
    NGO_reserves = st_read("vectors/NGO.shp"),
    Provincial_parks = st_read("vectors/Simplified_Provincial_park_regulated.shp"),
    Lower_municipality = st_read("vectors/Simplified_Mun_Lower_Nowater.shp"),
    Upper_municipality = st_read("vectors/Simplified_Mun_Upper_Nowater.shp"),
    National_parks = st_read("vectors/National_Park.shp"),
    Conservation_areas = st_read("vectors/Simplified_Conservation_area.shp"),
    Far_North_protected_areas = st_read("vectors/Far_North_protected_area.shp"),
    Municipal_Heritage_areas = st_read("vectors/Municipal_Heritage_Areas.shp"),
    Migratory_Bird_sanctuaries = st_read("vectors/Migratory_Bird_Sanctuary.shp"),
    National_Wildlife_areas = st_read("vectors/National_Wildlife_Area.shp"),
    Wilderness_areas = st_read("vectors/Wilderness_Area.shp"),
    National_capital_valued_ecosystem_or_habitat = st_read("vectors/National_Capital_Valued_Ecosystem_or_Habitat.shp"),
    Provincial_planned_protected_area = st_read("vectors/Provincial_plan_protected_area.shp"),
    Crown_plan_protected_area = st_read("vectors/Crown_plan_protected_area.shp"),
    Other_effective_area_based_conservation_measures = st_read("vectors/Other_Effective_area-based_Conservation_Measures.shp")
  )
  
  # Key for ID column by polygon type
  polygon_name <- list(
    Upper_municipality = "MUN_NAME",
    Lower_municipality = "MUN_NAME",
    Provincial_parks = "PROTECTE_1",
    Natural_heritage_system_areas = "ENABLING_P",
    Natural_heritage_value_areas = "AREA_NAME",
    Conservation_reserves = "PROTECTE_1",
    National_parks = "NAME_E",
    Conservation_areas = "NAME_E",
    Far_North_protected_areas = "NAME_E",
    Municipal_Heritage_areas = "NAME_E",
    Migratory_Bird_sanctuaries = "NAME_E",
    National_Wildlife_areas = "NAME_E",
    NGO_reserves = "NAME_E",
    Wilderness_areas = "NAME_E",
    National_capital_valued_ecosystem_or_habitat = "NAME_E",
    Provincial_planned_protected_area = "NAME_E",
    Crown_plan_protected_area = "NAME_E",
    Other_effective_area_based_conservation_measures = "NAME_E"
  )
  
  # Dynamically generate the list of features based on the selected polygon type
  observeEvent(input$sf_object_selection, {
    
    selected_sf <- sf_objects[[input$sf_object_selection]]
    
    # Get the correct column name for the selected polygon type
    selected_column <- polygon_name[[input$sf_object_selection]]
    
    # Check if the column exists in the selected sf object
    if (!is.null(selected_column) && selected_column %in% colnames(selected_sf)) {
      
      # Create a list of features by their names (or any identifier, like "Name" or "ID")
      feature_choices <- as.character(selected_sf[[selected_column]])
      
      # Generate dynamic UI for feature selection
      output$feature_selection_ui <- renderUI({
        tagList(
          tags$style(HTML("h4.custom-header { font-size: 13px; }")),
          h4(class = "custom-header", "Select individual features:"),
          
          # Add action buttons for select and unselect all
          actionButton("select_all", "Select All"),
          actionButton("unselect_all", "Unselect All"),
          br(),
          br(),
          
          # Explicitly creating a label for each checkbox in the dynamic checklist
          div(
            id = "checkboxes",
            lapply(feature_choices, function(feature) {
              # Replace or remove apostrophes in the feature name for the ID
              safe_feature_id <- gsub("[^A-Za-z0-9]", "_", feature)  # Replace non-alphanumeric characters with underscores
              
              div(
                class = "field",
                div(class = "ui checkbox",
                    tags$input(
                      type = "checkbox",
                      id = paste0("feature_", safe_feature_id),  # Create a unique ID with cleaned feature name
                      name = "feature_checkboxes",
                      value = feature
                    ),
                    tags$label(`for` = paste0("feature_", safe_feature_id), feature)  # Associate the label with the checkbox
                )
              )
            })
          ),
          
          # Include JavaScript to collect checkbox values and bind to `input$selected_features`
          tags$script(HTML("
        $('input[name=\"feature_checkboxes\"]').change(function() {
          var selected_features = [];
          $('input[name=\"feature_checkboxes\"]:checked').each(function() {
            selected_features.push($(this).val());
          });
          Shiny.setInputValue('selected_features', selected_features);
        });
      "))
        )
      })
    } else {
      output$feature_selection_ui <- renderUI({
        p("No valid features found for the selected type.")
      })
    }
    
    # Observe the "Select All" button
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(session, "selected_features", selected = feature_choices)
      shinyjs::runjs("$('input[name=\"feature_checkboxes\"]').prop('checked', true).trigger('change');")
    })
    
    # Observe the "Unselect All" button
    observeEvent(input$unselect_all, {
      updateCheckboxGroupInput(session, "selected_features", selected = character(0))
      shinyjs::runjs("$('input[name=\"feature_checkboxes\"]').prop('checked', false).trigger('change');")
    })
  })
  
  # Filter the selected sf objects based on the user's feature selection
  observe({
    #req(input$selected_features)
    if (!is.null(input$selected_features)) {
      selected_sf <- sf_objects[[input$sf_object_selection]]
      
      # Get the correct column name for the selected polygon type
      selected_column <- polygon_name[[input$sf_object_selection]]
      
      if (!is.null(selected_column) && selected_column %in% colnames(selected_sf)) {
        # Subset the sf object based on the selected features
        filtered_sf <- selected_sf[selected_sf[[selected_column]] %in% input$selected_features, ]
        
        selected_sf_react(filtered_sf)  # Update the reactive object with the filtered sf
      }
    }
  })
  
  
  # Define the CRS once
  crs_code <- "+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +ellps=GRS80 +towgs84=-0.991,1.9072,0.5129,-1.25033e-07,-4.6785e-08,-5.6529e-08,0 +units=m +no_defs +type=crs"
  
  # Define the process_polygon function
  process_polygon <- function(polygon, crs_code, OLC) {
    extent_val <- raster::extent(polygon)
    ext_coords <- cbind(
      c(extent_val@xmin, extent_val@xmax, extent_val@xmax, extent_val@xmin, extent_val@xmin),
      c(extent_val@ymin, extent_val@ymin, extent_val@ymax, extent_val@ymax, extent_val@ymin)
    )
    rownames(ext_coords) <- NULL
    colnames(ext_coords) <- c("x", "y")
    sp_extent <- sp::Polygon(ext_coords)
    sp_extent <- sp::Polygons(list(sp_extent), ID = "1")
    sp_extent <- sp::SpatialPolygons(list(sp_extent))
    sp::proj4string(sp_extent) <- CRS("+proj=longlat +datum=WGS84")
    sp_extent_3162 <- sp::spTransform(sp_extent, CRS(crs_code))
    extent_val_3162 <- raster::extent(sp_extent_3162)
    values$extent <- extent_val_3162
    
    polygon_geometry <- polygon$geometry
    sp_polygon_3162 <- sf::st_transform(polygon_geometry, crs = 3162)
    ras_resolution <- terra::res(OLC)
    template <- terra::rast(terra::vect(sp_polygon_3162), res = ras_resolution, ext = OLC)
    poly_raster <- terra::rasterize(terra::vect(sp_polygon_3162), template)
    selected_polygon(poly_raster)
  }
  
  
  
  
  
  
  # Process each polygon in selected_sf
  observeEvent(input$run_batch, {
    # Create a unique temporary directory to store csv files
    temp_path <- normalizePath(tempdir(), winslash = "/")
    # Format the timestamp
    timestamp <- gsub("[:/]", "_", format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"))
    # Construct the full directory path
    temp_dir <- file.path(temp_path, timestamp)
    dir.create(temp_dir, recursive = TRUE)
    temp_dir_react(temp_dir)
    
    shinyjs::disable("run_batch")
    shinyjs::disable("download_data_batch")
    
    selected_sf<-selected_sf_react()
    total_i <- nrow(selected_sf)
    
    
    log_file <- file.path(temp_dir, "Error_Log.csv")  # Define the path to the log file
    
    # Ensure the log file exists and create a header if it doesn't
    if (!file.exists(log_file)) {
      write_csv(data.frame(timestamp = character(), landscape_name = character(), error_message = character()), log_file)
    }
    
    
    # The batch loop 
    for (i in 1:nrow(selected_sf)) {
      tryCatch({
    polygon <- selected_sf[i, ]
    
    # Progress notification
    batch_notification<-showNotification(paste("Running landscape", i, "of", total_i, "..."), type = "message", duration = NULL, closeButton = FALSE)
    
    # Key for ID column by polygon type
    polygon_name <- list(
      Upper_municipality = "MUN_NAME",
      Lower_municipality = "MUN_NAME",
      Provincial_parks = "PROTECTE_1",
      Natural_heritage_system_areas = "ENABLING_P",
      Natural_heritage_value_areas = "AREA_NAME",
      Conservation_reserves = "PROTECTE_1",
      National_parks = "NAME_E",
      Conservation_areas = "NAME_E",
      Far_North_protected_areas = "NAME_E",
      Municipal_Heritage_areas = "NAME_E",
      Migratory_Bird_sanctuaries = "NAME_E",
      National_Wildlife_areas = "NAME_E",
      NGO_reserves = "NAME_E",
      Wilderness_areas = "NAME_E",
      National_capital_valued_ecosystem_or_habitat = "NAME_E",
      Provincial_planned_protected_area = "NAME_E",
      Crown_plan_protected_area = "NAME_E",
      Other_effective_area_based_conservation_measures = "NAME_E"
    )
    
    selected_polygon_type <- input$sf_object_selection
    id_column_name <- polygon_name[[selected_polygon_type]]
    landscape_name <- polygon[[id_column_name]]
    
    
    process_polygon(polygon, crs_code, OLC)
 
    extent_coord <- values$extent
  
    # Create a buffer around the extent_coord, 0 if no value was input
    extent_coord <- raster::extend(extent_coord, buffer_value())
    output_extent(extent_coord)
    
    # Crop rasters to target landscape extent
    # All rasters used in subsequent steps in the app are cropped to selected landscape extent
    # Cropped layers are reactive values, which have already been defined 
    pre_cropped_polyrast<-selected_polygon()
    cropped_Ontario_land_cover <- raster::crop(Ontario_land_cover, extent_coord)
    cropped_Ontario_land_cover <- rast(cropped_Ontario_land_cover)
    croppedOntario(cropped_Ontario_land_cover)
    cropped_polyrast(crop(pre_cropped_polyrast, cropped_Ontario_land_cover))
    croppedAMIS(crop(AMIS, cropped_Ontario_land_cover))
    croppedCHF_mines(crop(CHF_mines, cropped_Ontario_land_cover))
    croppedCHF_night_lights(crop(CHF_night_lights, cropped_Ontario_land_cover))
    croppedCHF_oil_gas(crop(CHF_oil_gas, cropped_Ontario_land_cover))
    croppedCHF_forestry_harvest(crop(CHF_forestry_harvest, cropped_Ontario_land_cover))
    croppedmovement_cost(crop(movement_cost, cropped_Ontario_land_cover))
    croppedCLASS_00(crop(CLASS_00, cropped_Ontario_land_cover))
    croppedCLASS_05(crop(CLASS_05, cropped_Ontario_land_cover))
    croppedCLASS_06(crop(CLASS_06, cropped_Ontario_land_cover))
    croppedCLASS_12(crop(CLASS_12, cropped_Ontario_land_cover))
    croppedCLASS_13(crop(CLASS_13, cropped_Ontario_land_cover))
    croppedCLASS_14(crop(CLASS_14, cropped_Ontario_land_cover))
    croppedCLASS_15(crop(CLASS_15, cropped_Ontario_land_cover))
    croppedCLASS_16(crop(CLASS_16, cropped_Ontario_land_cover))
    croppedCLASS_18(crop(CLASS_18, cropped_Ontario_land_cover))
    croppedCLASS_21(crop(CLASS_21, cropped_Ontario_land_cover))
    croppedCLASS_23(crop(CLASS_23, cropped_Ontario_land_cover))
    croppedCLASS_24(crop(CLASS_24, cropped_Ontario_land_cover))
    croppedCLASS_25(crop(CLASS_25, cropped_Ontario_land_cover))
    croppedCLASS_26(crop(CLASS_26, cropped_Ontario_land_cover))
    croppedmean_temp(crop(mean_temp, cropped_Ontario_land_cover))
    croppedprecipitation(crop(precipitation, cropped_Ontario_land_cover))
    croppedElevation(crop(Elevation, cropped_Ontario_land_cover))
    croppedSoil_not(crop(Soil_not, cropped_Ontario_land_cover))
    croppedSoil_20(crop(Soil_20, cropped_Ontario_land_cover))
    croppedSoil_75(crop(Soil_75, cropped_Ontario_land_cover))
    croppedSoil_150(crop(Soil_150, cropped_Ontario_land_cover))
    croppedSoil_G150(crop(Soil_G150, cropped_Ontario_land_cover))
    croppedSOLRIS_AggregateExtraction_204(crop(SOLRIS_AggregateExtraction_204, cropped_Ontario_land_cover))
    croppedSOLRIS_TopsoilExtraction_205(crop(SOLRIS_TopsoilExtraction_205, cropped_Ontario_land_cover))
    croppedSOLRIS_Undifferentiated_250(crop(SOLRIS_Undifferentiated_250, cropped_Ontario_land_cover))
    croppedProtected_areas(crop(Protected_areas(), cropped_Ontario_land_cover))
    croppedmovement_cost_protected(crop(movement_cost_protected(), cropped_Ontario_land_cover))
    
    cropped_mines <- croppedCHF_mines()
    cropped_AMIS <- croppedAMIS()
    cropped_night_lights <- croppedCHF_night_lights()
    cropped_oil_gas <- croppedCHF_oil_gas()
    cropped_forestry_harvest <- croppedCHF_forestry_harvest()
    cropped_aggregate_extraction <- croppedSOLRIS_AggregateExtraction_204()
    cropped_topsoil_extraction <- croppedSOLRIS_TopsoilExtraction_205()
    cropped_undifferentiated <- croppedSOLRIS_Undifferentiated_250()
    
    ## Filter rasters to definitions the user has set via the sliders
    condition_mines <- cropped_mines >= input$mine_slider
    condition_AMIS <- cropped_AMIS >= input$amis_slider
    condition_night_lights <- cropped_night_lights >= input$night_lights_slider
    condition_oil_gas <- cropped_oil_gas >= input$oil_gas_slider
    condition_forestry_harvest <- cropped_forestry_harvest >= input$forestry_harvest_slider
    condition_aggregate_extraction <- cropped_aggregate_extraction >= input$aggregate_extraction_slider
    condition_topsoil_extraction <- cropped_topsoil_extraction >= input$topsoil_extraction_slider
    condition_undifferentiated <- cropped_undifferentiated >= input$undifferentiated_slider
    
    # A final cumulative condition (encompassing all the conditions above) is then used to define degraded pixels
    final_condition <- condition_mines | condition_night_lights | condition_oil_gas | condition_forestry_harvest | condition_AMIS | condition_aggregate_extraction | condition_topsoil_extraction | condition_undifferentiated
    
    degraded_pixels_temp <- croppedOntario()
    degraded_protected <- croppedProtected_areas()
    
    ## Remove values unfit for restoration
    # Water (41), Built up area-pervious (51),
    # Anthropogenic (52), Cropland (53), Hay/pasture (54), 
    # Transportation (55), Unclassified (99)
    unfit_values <- c(41, 51, 52, 53, 54, 55, 99) 
    
    mask <- degraded_pixels_temp %in% unfit_values
    unfit_raster <- degraded_pixels_temp * mask
    unfit_raster[!mask] <- NA
    
    
    degraded_pixels_temp[final_condition] <- 100 # Pixels that meet the cumulative condition are set to degraded
    degraded_pixels_temp[!is.na(unfit_raster)] <- NA
    
    degraded_protected[final_condition] <- 100 # an areas of conservation concern version of this is done as well (mainly for plotting)
    degraded_protected[is.na(degraded_pixels_temp)] <- NA
    degraded_protected_react(degraded_protected)
    
    degraded_pixels_temp[degraded_pixels_temp != 100] <- NA
    
    # This is used to get a count of the number of degraded pixels
    degraded_pixels_temp_raster <- raster(degraded_pixels_temp)
    degraded_array <- matrix(raster::extract(
      degraded_pixels_temp_raster,
      raster::extent(degraded_pixels_temp_raster)
    ), ncol = 1, nrow = ncell(degraded_pixels_temp_raster))
    num_degraded <- sum(degraded_array == 100, na.rm = TRUE)
    numDegradedPixels(num_degraded) # Save the number of degraded pixels for use in later steps
    
    degraded_pixels(degraded_pixels_temp) # a reactive values stores the degraded pixels raster for use later
    
   
      # Simulate restoration
      Ontario_degraded_land <- ifel(is.na(degraded_pixels()), croppedOntario(), degraded_pixels())
      sample_degraded_land <- raster(Ontario_degraded_land)
      restored_land <- raster(Ontario_degraded_land)
      
      # Process sample_degraded_land
      remove_values <- c(100, 41, 52, 53, 55, 54, 51, 99)
      for (val in remove_values) {
        sample_degraded_land[sample_degraded_land == val] <- NA
        sample_degraded_land(sample_degraded_land)
      }
      
      # Process restored_land
      remove_values <- c(41, 52, 53, 55, 54, 51, 99) # , 250
      for (val in remove_values) {
        restored_land[restored_land == val] <- NA
      }
      
      # Initialize kernel (i.e neighborhood) size and iteration counter
      kernel_size <- 3
      iteration <- 1
      # Create a custom function to calculate the mode
      custom_mode <- function(x) {
        x <- na.omit(x)
        if (length(x) == 0) {
          return(NA)
        }
        table_x <- table(x)
        
        # Find the mode value
        mode_val <- as.integer(names(sort(table_x, decreasing = TRUE)[1]))
        # If '100' is present and there are other values, prioritize the next prominent value - this is so pixels aren't 'restored' to the degraded class
        if ("100" %in% names(table_x) && length(table_x) > 1) {
          other_vals <- names(table_x)[names(table_x) != "100"]
          next_prominent_val <- other_vals[which.max(table_x[other_vals])]
          mode_val <- as.integer(next_prominent_val)
        }
        return(mode_val)
      }
      
      
      # Initialize a flag to track whether any replacements were made
      replacements_made <- TRUE
      # Continue iterating until no more 100 values (degraded class pixels) are found
      while (replacements_made) {
        # Use the focal function to calculate the mode with the current kernel size
        kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)
        restored_land_mode <- focal(restored_land, w = kernel, fun = custom_mode, pad = TRUE)
        # Find the indices of cells with values equal to 100
        cells_to_replace <- which(restored_land[] == 100)
        if (length(cells_to_replace) > 0) {
          # Replace 100 values with the mode values
          restored_land[cells_to_replace] <- restored_land_mode[cells_to_replace]
          # Check if any 100 values remain
          replacements_made <- any(na.omit(restored_land[]) == 100)
        } else {
          # If no more 100 values to replace, exit the loop
          replacements_made <- FALSE
        }
        # Plot the modified raster for this iteration - maybe no necessary anymore? Originally I used this for diagnostic purposes
          plot(restored_land, main = paste("Iteration:", iteration), col = viridis(20, direction = -1))
        iteration <- iteration + 1 # Increment iteration counter
        # Increase the kernel size by 2 every iteration. This significantly speeds up the procees for
        # 'islands' of pixels that are surrounded by NAs
        kernel_size <- kernel_size + 2
      }
      
      restored_land(restored_land)
      
      
    
      # # Code to create the dynamic checkbox for excluding specific habitat classes contained
      # # in a given landscape from the habitat metrics calculation
      # land_class_mapping <- setNames(
      #   c(11, 12, 13, 14, 15, 16, 17, 18, 21, 22, 23, 24, 25, 31, 32, 33, 34, 35, 36, 37, 38, 41, 51, 52, 53, 54, 55, 99, 100, 204, 205, 250),
      #   c(
      #     "Prairie", "Savannah", "Alvar", "Dune", "Meadow", "Shrubland", "Barren", "Sparse treed", "Coniferous forest", "Mixedwood forest",
      #     "Deciduous forest", "Transitional forest", "Hedge row", "Coniferous treed swamp", "Mixedwood treed swamp", "Deciduous treed swamp",
      #     "Transitional treed swamp", "Thicket swamp", "Bog", "Fen", "Marsh", "Water", "Built up area-pervious", "Anthropogenic", "Cropland",
      #     "Hay/pasture", "Transportation", "Unclassified", "Degraded", "Aggregate extraction", "Topsoil/Peat extraction", "Undifferentiated"
      #   )
      # )
      # 
      # choices_reactive <- reactive({
      #   if (is.null(restored_land())) {
      #     return(NULL)
      #   }
      #   na.omit(unique(values(restored_land())))
      # })
      # 
      # output$dynamic_checkboxes <- renderUI({
      #   if (is.null(choices_reactive())) {
      #     return(NULL)
      #   }
      # 
      #   # Get the names of the selected habitat classes to exclude
      #   selected_labels <- names(land_class_mapping)[land_class_mapping %in% choices_reactive()]
      # 
      #   multiple_checkbox("selected_habitats", "Select habitat classes to *exclude* from patch size calculations:", choices = selected_labels)
      # })
      
      
      result_habitat_data <- reactiveVal(data.frame()) # a reactive value to store results
      
        
        
        # selected_habitats <- land_class_mapping[names(land_class_mapping) %in% input$selected_habitats]
        
        # Extract the pixel values from restored_land and sample_degraded_land
        restored_land_values <- restored_land()
        restored_land_values <- rast(restored_land_values)
        
        sample_degraded_land_values <- restored_land_values
        sample_degraded_land_values[!is.na(degraded_pixels())] <- NA
        sample_degraded_land_values <- raster(sample_degraded_land_values)
        
        cropped_polygon<-cropped_polyrast()
        
        
        # Find the locations where restored_land has non-NA values, sample_degraded_land has NA values, and are within the selected polygon
        valid_locations <- which(!is.na(values(restored_land_values)) & is.na(values(sample_degraded_land_values)) & !is.na(values(cropped_polygon)))
        
        
        ### This generates combinations of restorable pixels, by grouping contiguous pixels that share the same habitat class into lists
        # This is the 'automatic' combination setting in the UI. If the manual setting is chosen then this code is not used.
        # if (input$automatic_combination > 0) {
        
          # Define a function to find contiguous regions of pixels with the same habitat class
          find_contiguous <- function(valid_locations, raster_values, nrows, ncols) {
            # Initialize a list to store the regions found
            regions <- vector("list")
            # Initialize a logical vector to keep track of 'visited' locations
            visited <- logical(length(valid_locations))
            
            # Define a function to find neighbors of a given index in a raster
            neighbours <- function(idx) {
              row <- (idx - 1) %/% ncols + 1
              col <- idx - (row - 1) * ncols
              
              # Define possible neighbor offsets
              possible_neighbours <- expand.grid(x = c(-1, 0, 1), y = c(-1, 0, 1))
              possible_neighbours <- possible_neighbours[-5, ] # Remove the center (0,0) as it is the current pixel
              
              # Calculate row and column offsets for neighbors
              row_offsets <- possible_neighbours$x + row
              col_offsets <- possible_neighbours$y + col
              
              # Ensure the neighbors are within the bounds of the raster
              row_offsets <- pmin(pmax(row_offsets, 1), nrows)
              col_offsets <- pmin(pmax(col_offsets, 1), ncols)
              
              # Calculate indices of neighbors
              neighbour_indices <- (row_offsets - 1) * ncols + col_offsets
              neighbour_indices <- neighbour_indices[neighbour_indices != idx] # Exclude the current pixel
              neighbour_indices
            }
            
            # Loop through each valid location
            for (i in seq_along(valid_locations)) {
              idx <- valid_locations[i]
              if (!visited[i]) {
                val <- raster_values[idx]
                region_indices <- idx
                visited[i] <- TRUE
                
                to_explore <- region_indices
                
                # 'Explore' the region by finding contiguous pixels
                while (length(to_explore) > 0) {
                  current_idx <- to_explore[1]
                  to_explore <- to_explore[-1]
                  
                  current_neighbours <- neighbours(current_idx)
                  new_neighbours <- current_neighbours[!(current_neighbours %in% region_indices)]
                  
                  # Check and add new neighbors with the same habitat class
                  for (neighbour in new_neighbours) {
                    if (neighbour %in% valid_locations && !visited[which(valid_locations == neighbour)] &&
                        !is.na(raster_values[neighbour]) && raster_values[neighbour] == val) {
                      region_indices <- c(region_indices, neighbour)
                      visited[which(valid_locations == neighbour)] <- TRUE
                      to_explore <- c(to_explore, neighbour)
                    }
                  }
                }
                # Store the found region/combination in the list
                regions[[length(regions) + 1]] <- region_indices
              }
            }
            # Return the list of regions
            regions
          }
          
          # Use the above function to find contiguous regions based on valid locations and their corresponding raster values
          contiguous_regions <- find_contiguous(valid_locations, restored_land_values[], nrow(restored_land_values), ncol(restored_land_values))
          
          # Create a matrix to store the indices for each group (as integers)
          max_length <- max(lengths(contiguous_regions))
          result_matrix <- matrix(NA_integer_, nrow = max_length, ncol = length(contiguous_regions))
          
          # Fill the matrix with indices for each group as integers
          for (i in seq_along(contiguous_regions)) {
            result_matrix[1:length(contiguous_regions[[i]]), i] <- as.integer(contiguous_regions[[i]])
          }
          
          combinations <- result_matrix
          
          # Function to convert columns to list elements and remove NA values (so they can be used properly in future steps)
          convert_to_list <- function(mat) {
            list_of_matrices <- list()
            for (i in 1:ncol(mat)) {
              col_values <- mat[, i]
              col_values <- col_values[!is.na(col_values)]
              dim(col_values) <- c(length(col_values), 1)
              list_of_matrices[[i]] <- col_values
            }
            return(list_of_matrices)
          }
          
          combinations <- convert_to_list(combinations)
          combinations_react(combinations)

        # Calculate the habitat metrics
        
        # Precompute values to save on overhead in the parallel process
        restored_values <- values(restored_land_values)
        degraded_patch_size <- lsm_c_area_mn(sample_degraded_land_values)
        
        calculate_metrics <- function(combination, degraded_raster) {
          # Create a copy of the degraded raster and add the values from restored_land
          modified_raster <- degraded_raster
          modified_raster[combination] <- restored_values[combination]

          
          # Here, calculate the metrics for the modified raster
          modified_patch_size <- lsm_c_area_mn(modified_raster)
          
          # Calculate the differences for each class
          patch_size_diffs <- setNames(modified_patch_size$value - degraded_patch_size$value, paste0("patch_size_diff_class", modified_patch_size$class))
          
          # Combine the results
          c(
            list(combination = paste(combination, collapse = "-")),
            patch_size_diffs
          )
        }
        
        library(furrr)
        plan(multisession(workers = 2)) # Set the number of workers - 3 was most stable on the systems we tested on, but this could be set to anything
        
        calculate_metrics_parallel <- function(combination) {
          
          metrics <- calculate_metrics(combination, sample_degraded_land_values)
          
          # Extract patch size and cohesion differences
          patch_size_diffs <- metrics[names(metrics) %in% paste0("patch_size_diff_class", degraded_patch_size$class)]
          
          # Combine results into a data frame
          data.frame(combination = metrics$combination, patch_size_diffs)
        }
        
        # Use future_map_dfr to parallelize the computation for all combinations
        calculated_habitat_data <- future_map_dfr(
          seq_along(combinations),
          .options = furrr_options(seed = TRUE),
          ~ calculate_metrics_parallel(combinations[[.x]])
        )
        
        # Close parallel processing
        plan(sequential)
        
        
        # Mapping from class numbers to class names
        class_mapping <- c(
          `11` = "Prairie", `12` = "Savannah", `13` = "Alvar", `14` = "Dune", `15` = "Meadow",
          `16` = "Shrubland", `17` = "Barren", `18` = "Sparse Treed", `21` = "Coniferous Forest",
          `22` = "Mixedwood Forest", `23` = "Deciduous Forest", `24` = "Transitional Forest",
          `25` = "Hedge Row", `31` = "Coniferous Treed Swamp", `32` = "Mixedwood Treed Swamp",
          `33` = "Deciduous Treed Swamp", `34` = "Transitional Treed Swamp", `35` = "Thicket Swamp",
          `36` = "Bog", `37` = "Fen", `38` = "Marsh", `41` = "Water", `51` = "Built Up Area-Pervious",
          `52` = "Anthropogenic", `53` = "Cropland", `54` = "Hay/Pasture", `55` = "Transportation",
          `99` = "Unclassified", `100` = "Degraded"
        )
        
        
        # Function to rename the columns
        rename_columns <- function(df, class_mapping) {
          colnames(df) <- sapply(colnames(df), function(col) {
            new_name <- col
            for (class_num in names(class_mapping)) {
              class_name <- class_mapping[class_num]
              pattern <- paste0("class", class_num)
              replacement <- paste0(class_name, " (Class ", class_num, ")")
              new_name <- gsub(pattern, replacement, new_name)
            }
            new_name
          })
          df
        }
        
        # Rename the columns
        calculated_habitat_data <- rename_columns(calculated_habitat_data, class_mapping)
        
        # Update the reactiveVal with the computed data
        result_habitat_data(calculated_habitat_data)
        
        
        if (input$protected_based_calculations > 0) { # if the 'areas of conservation concern' option is selected in segment 2,
          # then patch size differences for these areas are calculated as well
          
          combinations <- combinations_react()
          degraded_protected <- degraded_protected_react()
          
          protected_restored <- croppedProtected_areas()
          protected_restored[protected_restored == 0] <- NA
          protected_restored[protected_restored == 41] <- NA
          
          degraded_protected[degraded_protected == 0] <- NA
          degraded_protected[degraded_protected == 100] <- NA
          degraded_protected <- raster(degraded_protected)
          
          # Precompute values
          restored_values <- values(protected_restored)
          degraded_patch_size <- lsm_c_area_mn(degraded_protected)$value
          
          # Create a function to calculate metrics for a given combination of 4 pixels added to Sample_degraded_land
          calculate_metrics <- function(combination, degraded_raster) {
            # Create a copy of the degraded raster
            modified_raster <- degraded_raster
            # Add the values from Restored_land at the specified combination locations
            modified_raster[combination] <- restored_values[combination]
            # Calculate the difference in patch size metrics for all classes
            patch_size_metrics <- lsm_c_area_mn(modified_raster)
            Protected_patch_size_difference <- patch_size_metrics$value - degraded_patch_size
            return(list(patch_size = Protected_patch_size_difference, raster = modified_raster))
          }
          
          library(furrr)
          
          plan(multisession(workers = 2)) # Adjust the number of workers as needed
          
          # Define a function for parallel computation
          calculate_metrics_parallel <- function(combination, degraded_raster) {
            metrics <- calculate_metrics(combination, degraded_raster)
            patch_size_difference <- sum(abs(metrics$patch_size))
            return(data.frame(combination = paste(combination, collapse = "-"), patch_size_difference))
          }
          
          
          # Use future_map_dfr to parallelize the computation for all combinations
          protected_result <- future_map_dfr(
            seq_along(combinations),
            .options = furrr_options(seed = TRUE),
            ~ calculate_metrics_parallel(combinations[[.x]], degraded_protected)
          )
          
          protected_result <- protected_result %>%
            rename(protected_patch_size_difference = patch_size_difference)
          
          # Close parallel processing
          plan(sequential)
          
          
          # combine this patch size metric with the other results table
          hab_result_combined <- merge(result_habitat_data(), protected_result, by = "combination")
          
          
          result_habitat_data(hab_result_combined)
        }
        
        result_habitat_data_updated(result_habitat_data())
        # # Check the selected option
        if (input$merge_metrics == "Merge") {
          # Perform the merging and updating of the data
          updated_data <- result_habitat_data() %>%
            mutate(
              sum_habitat_patch_size_diff = rowSums(select(., starts_with("patch_size_diff")), na.rm = TRUE)
            ) %>%
            select(-starts_with("patch_size_diff"))
          # Update the reactive value
          result_habitat_data_updated(updated_data)
      }

          # reactive value to store results for use later
          result_connectivity <- reactiveVal(data.frame())
            
            # Define the initial state of movement_cost with 1000 where degraded_pixels are not NA
            if (input$habitat_based_calculations > 0) {
              movement_cost_calc <- croppedmovement_cost()
              movement_cost_calc[!is.na(degraded_pixels())] <- 1000
            } else {
              # this is the alternative if the 'areas of conservation concern' option is selected in UI segment 2
              movement_cost_calc <- croppedmovement_cost_protected()
              movement_cost_calc[!is.na(degraded_pixels())] <- 10000
            }
            
            combinations <- combinations_react()
            
            movement_cost_raster <- raster(movement_cost_calc)
            # Initialize a variable to store the result
            calculation_result <- NULL
            
     
                # Function to calculate mean path resistance
                calculate_mean_resistance <- function(raster) {
                  patches <- (raster == 1)
                  mpg <- MPG(raster, patch = patches)
                  mc_neighbours <- graphdf(mpg)[[1]]$e[, c(1, 2, 4)]
                  mc_neighbours_df <- as.data.frame(mc_neighbours)
                  colnames(mc_neighbours_df) <- c("Node 1", "Node 2", "Path distance (Resistance)")
                  mean(mc_neighbours_df$`Path distance (Resistance)`)
                }
                
                # Calculate baseline mean resistance
                baseline_mean_res <- calculate_mean_resistance(movement_cost_raster)
                
                # Initialize a data frame to store the results
                results <- data.frame()
                
                # Set up parallel processing
                plan(multisession(workers = 2))
                
                # Function to apply a combination and calculate the difference in resistance
                calculate_combination_resistance <- function(combination) {
                  modified_raster <- movement_cost_raster
                  modified_raster[combination] <- 1
                  new_mean_res <- calculate_mean_resistance(modified_raster)
                  reduced_resistance <- baseline_mean_res - new_mean_res
                  c(list(combination = paste(combination, collapse = "-")),
                    reduced_resistance = reduced_resistance
                  )
                }
                
                # Calculate the differences using parallel processing for all combinations
                results <- future_map_dfr(
                  .options = furrr_options(seed = TRUE),
                  seq_along(combinations),
                  ~ calculate_combination_resistance(combinations[[.x]])
                )
                
                # Store the result
                calculation_result <- results
                
                # Close parallel processing
                plan(sequential)
                
              result_connectivity(calculation_result)
            
          
              # Linked to UI Segment 12, generating the environmental PCA raster 
                
              # Get all the environmental rasters from the reactive values
              CLASS_00_env <- croppedCLASS_00()
              CLASS_05_env <- croppedCLASS_05()
              CLASS_06_env <- croppedCLASS_06()
              CLASS_12_env <- croppedCLASS_12()
              CLASS_13_env <- croppedCLASS_13()
              CLASS_14_env <- croppedCLASS_14()
              CLASS_15_env <- croppedCLASS_15()
              CLASS_16_env <- croppedCLASS_16()
              CLASS_18_env <- croppedCLASS_18()
              CLASS_21_env <- croppedCLASS_21()
              CLASS_23_env <- croppedCLASS_23()
              CLASS_24_env <- croppedCLASS_24()
              CLASS_25_env <- croppedCLASS_25()
              CLASS_26_env <- croppedCLASS_26()
              mean_temp_env <- croppedmean_temp()
              precipitation_env <- croppedprecipitation()
              Elevation_env <- croppedElevation()
              Soil_not_env <- croppedSoil_not()
              Soil_20_env <- croppedSoil_20()
              Soil_75_env <- croppedSoil_75()
              Soil_150_env <- croppedSoil_150()
              Soil_G150_env <- croppedSoil_G150()
              
              # Create a rasterstack
              raster_stack <- c(
                mean_temp_env, precipitation_env, Elevation_env, Soil_not_env, Soil_20_env, Soil_75_env,
                Soil_150_env, Soil_G150_env, CLASS_00_env, CLASS_05_env, CLASS_06_env, CLASS_12_env,
                CLASS_13_env, CLASS_14_env, CLASS_15_env, CLASS_16_env, CLASS_18_env, CLASS_21_env,
                CLASS_23_env, CLASS_24_env, CLASS_25_env, CLASS_26_env
              )
              
              # create a rasterPCA with the raster stack (RStoolbox function)
              pca_result_temp <- rasterPCA(raster_stack, maskCheck=FALSE)
              # Store the PCA result in the reactive value
              pca_result(pca_result_temp)
              
              
              # environmental heterogeneity metrics calculation 
              
              result_env_data <- reactiveVal(data.frame()) # to store results
                req(pca_result())
                num_pcs <- 2
                
                # Extracting the PCA rasters based on user input
                pca_rasters <- lapply(1:num_pcs, function(i) {
                  pca_result()$map[[i]]
                })
                
                calculated_env_data_list <- vector("list", length = num_pcs)
                
                # Define a function to calculate metrics for a given combination of pixels
                calculate_metrics <- function(combination, degraded_raster, env_values) {
                  # Create a copy of the degraded raster
                  modified_raster <- degraded_raster
                  # Add the values from env_values at the specified combination locations
                  modified_raster[combination] <- env_values[combination]
                  # Calculate heterogeneity for the raster
                  sa_metric <- sa(modified_raster)
                  # Return the metrics as a list
                  return(list(sa = sa_metric))
                }
                
                # Load the furrr library for parallel processing
                library(furrr)
                # Set up parallel processing with 3 workers
                plan(multisession(workers = 2))
                
                # Loop over for the number of principal components selected
                for (i in 1:num_pcs) {
                  # Begin to set up the comparison of the degraded vs the restored raster
                  Env_PCA <- pca_rasters[[i]]
                  Deg_PCA <- pca_rasters[[i]]
                  
                  Deg_PCA[!is.na(degraded_pixels())] <- NA # We are treating degraded pixels as having no environmental heterogeneity
                  
                  # Extract pixel values from each raster
                  env_values <- values(Env_PCA)
                  Deg_PCA_values <- values(Deg_PCA)
                  # Calculate environmental heterogeneity for the degraded landscape
                  degraded_sa <- sa(Deg_PCA_values)
                  
                  # Get the pixel combinations from storage
                  combinations <- combinations_react()
                  
                  # function to calculate metrics for each combination
                  calculate_metrics_parallel <- function(combination) {
                    metrics <- calculate_metrics(combination, Deg_PCA_values, env_values)
                    # Calculate the difference in environmental heterogeneity between the degraded and restored landscapes
                    sa_diff <- as.numeric(metrics$sa) - as.numeric(degraded_sa)
                    # Return a data frame with the principal component, given combination, and heterogeneity difference
                    return(data.frame(PC = paste0("Env_PC", i), combination = paste(combination, collapse = "-"), sa_diff = sa_diff))
                  }
                  
                  # Use future_map_dfr for parallelization
                  calculated_env_data <- future_map_dfr(seq_along(combinations), ~ calculate_metrics_parallel(combinations[[.x]]))
                  # Store the results
                  calculated_env_data_list[[i]] <- calculated_env_data
                }
                
                # Close parallel processing
                plan(sequential)
                
                
                # Combine the results into one data frame
                combined_calculated_env_data <- do.call(rbind, calculated_env_data_list)
                
                combined_calculated_env_data <- combined_calculated_env_data %>%
                  spread(key = PC, value = sa_diff) %>%
                  rename_with(~ paste0(., "_sa_diff"), -combination)
                
                
                # Update the reactiveVal with the computed data
                result_env_data(combined_calculated_env_data)
                
                
                
              # Linked to UI Segment 14,  Merging all results/metrics together in preparation for weighting
              
                habitat_data <- result_habitat_data_updated()
                env_data <- result_env_data()
                connectivity_data <- result_connectivity()
                
                # Check if "combination" is present in all data sets
                if ("combination" %in% colnames(habitat_data) &&
                    "combination" %in% colnames(env_data) &&
                    "combination" %in% colnames(connectivity_data)) {
                  # Merge all data sets
                  merged_data <- merge(result_habitat_data_updated(), result_env_data(), by = "combination")
                  merged_data <- merge(result_connectivity(), merged_data, by = "combination")
                  # Scale the columns except 'combination'
                  columns_to_scale <- setdiff(names(merged_data), "combination")
                  scaled_data <- as.data.frame(lapply(merged_data[columns_to_scale], scale))
		              colnames(scaled_data) <- colnames(merged_data[columns_to_scale])
                  # Replace NAs with 0s after scaling
                  scaled_data[is.na(scaled_data)] <- 0
                  # Combine scaled columns with 'combination'
                  merged_data <- cbind(merged_data["combination"], scaled_data)
                }
                merged_data_reactive(merged_data)
        
                    merged_data <- merged_data_reactive() # bring it out of the reactive value
                    
                    # Add a new column 'landscape' with the provided landscape name to the merged data
                    merged_data$Landscape <- landscape_name
                    merged_data <- merged_data[c("Landscape", names(merged_data)[names(merged_data) != "Landscape"])]
                    # rename 'protected_patch_size_difference' to 'Protected area patch size'
                    if("protected_patch_size_difference" %in% names(merged_data)) {
                      names(merged_data)[names(merged_data) == 'protected_patch_size_difference'] <- 'Change in protected area patch size'
                    }
                    # rename 'sum_habitat_patch_size_diff' to 'Sum patch size' if present
                    if("sum_habitat_patch_size_diff" %in% names(merged_data)) {
                      names(merged_data)[names(merged_data) == 'sum_habitat_patch_size_diff'] <- 'Sum change in habitat patch size'
                    }
                    # rename 'reduced_resistance' to 'Change in path resistance'
                    if("reduced_resistance" %in% names(merged_data)) {
                      names(merged_data)[names(merged_data) == 'reduced_resistance'] <- 'Change in path resistance'
                    }
                    # Use a pattern matching approach to rename the 'patch_size_diff' and 'ENV_PC' columns if they are present
                    new_names <- names(merged_data)
                    new_names <- gsub("^patch_size_diff_(.+?) \\(Class \\d+\\)$", "Change in \\1 patch size", new_names)
                    new_names <- gsub("Env_PC(\\d+)_sa_diff", "Change in environmental heterogeneity: PC\\1", new_names)
                    names(merged_data) <- new_names
                    
                    extract_first_number <- function(x) {
                      as.numeric(sub("-.*", "", x))
                    }
                    # Apply the function to the 'combination' column
                    first_numbers <- sapply(merged_data$combination, extract_first_number)
                    # Sort the dataframe based on the first pixel index
                    merged_data <- merged_data[order(first_numbers), ]
                    
                    # Add pixels column
                    merged_data$Pixels<-merged_data$combination
                    
                    
                    # Rename 'combination' column to 'Candidate area'
                    names(merged_data)[names(merged_data) == 'combination'] <- 'Candidate area'
                    # Get the total number of rows
                    total_rows <- nrow(merged_data)
                    # Determine the number of zeros needed based on the total number of rows
                    num_zeros <- nchar(as.character(total_rows))
                    # Generate the new values for the 'Candidate area' column
                    merged_data$`Candidate area` <- sapply(1:total_rows, function(i) {
                      # Count the number of '-' in the original entry
                      original_value <- merged_data$`Candidate area`[i]
                      num_pixels <- str_count(original_value, "-") + 1
                      # Generate the CA number with leading zeros
                      ca_number <- sprintf(paste0("CA_%0", num_zeros, "d"), i)
                      # Combine CA number with pixel information
                      paste0(ca_number, " (", num_pixels, " pixels)")
                    })
                    
                    
                    
                    merged_data_reactive(merged_data)
                    
                    

                    #Generate a filename based on landscape_name and current date/time
                    filename <- paste(landscape_name, "_", Sys.Date(), ".csv", sep = "")
                    csv_filename <- file.path(temp_dir, filename)

                    # Get the extent object from the reactive value
                    extent <- output_extent()
                    
                    # Convert the extent object to a numeric vector
                    extent_values <- c(extent@xmin, extent@ymin, extent@xmax, extent@ymax)
                    
                    ## Create the CSV file
                    con <- file(csv_filename, open = "wt")
                    cat("Extent_Xmin,Extent_Ymin,Extent_Xmax,Extent_Ymax\n", file = con)
                    cat(paste(extent_values, collapse = ","), "\n", file = con)
                    write.csv(merged_data, con, row.names = FALSE, quote = FALSE)
                    close(con)
                    
                    removeNotification(batch_notification)
                    
      }, error = function(e) {
        # Log the error with landscape_name and error message
        log_entry <- data.frame(
          timestamp = Sys.time(),
          landscape_name = as.character(landscape_name),
          error_message = e$message,
          stringsAsFactors = FALSE
        )
        write_csv(log_entry, log_file, append = TRUE)
        removeNotification(batch_notification)
      })
      
      
      
                    }
                    
    #End of loop
    showNotification("Processing has finished! Continute to download", type = "message", duration = NULL)
    # shinyjs::enable("run_batch")
    shinyjs::enable("download_data_batch")
    })
  
    
  
  
  # Download CSV button for multiple CSV objects
  output$download_data_batch <- downloadHandler(
    filename = function() {
      paste0("Batch_Results_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_zip<-temp_dir_react()
      # Create a zip file containing all KML files in the temporary directory
      zip_filename <- file.path(temp_zip, "Batch_Results.zip")
      zip(zip_filename, files = list.files(temp_zip, pattern = ".*\\.csv$", full.names = TRUE), flags = '-r9Xj') # the flag argument specifies to ignore the folder directories and just take the files
      
      # Copy the zip file to the specified download location
      file.copy(zip_filename, file)
    }
  )

}
  
  





# The UI segments linked to the server code #############
ui <- shinyUI(semanticPage(
  title = "Restoration Prioritization",
  useShinyjs(),
  tags$html(lang = "en"),

# Custom CSS for styling the splash page
  tags$head(
    tags$style(HTML("
      .splash-container {
        position: relative;
        width: 100%;
        height: 100vh;
        background-size: cover;
        background-position: center;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        text-align: center;
        color: white;
      }
      .splash-title {
        font-family: 'Arial', sans-serif;
        font-size: 3em;
        font-weight: bold;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.5);
        margin-bottom: 20px;
      }
      .splash-text-container {
        background-color: rgba(255, 255, 255, 0.8);
        padding: 20px;
        border-radius: 10px;
        max-width: 600px;
        margin-bottom: 30px;
      }
      .splash-text {
        font-family: 'Arial', sans-serif;
        font-size: 1.2em;
        color: black;
      }
      .go-button {
        background-color: #4CAF50; /* Green */
        border: none;
        color: white;
        padding: 15px 32px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 1.2em;
        font-weight: bold;
        border-radius: 8px;
        cursor: pointer;
        transition: background-color 0.3s;
      }
      .go-button:hover {
        background-color: #45a049;
      }
    "))
  ),

tags$head(
  tags$script(HTML("
      document.addEventListener('keydown', function(event) {
        if (event.key === 'Enter') {
          var activeElement = document.activeElement;
          // If the focused element can be clicked (e.g., checkbox, button)
          if (activeElement && (activeElement.tagName === 'BUTTON' || activeElement.tagName === 'A' || activeElement.type === 'checkbox' || activeElement.type === 'radio')) {
            activeElement.click();
          }
        }
      });
    "))
),

  # Splash page content
  uiOutput("splash"),

# Add a Fomantic-UI menu at the top for the feedback button
div(class = "ui top fixed menu",
    div(class = "item", ""),  # Placeholder for title
    div(class = "right menu",
        a(class = "item", 
          href = "https://forms.office.com/Pages/ResponsePage.aspx?id=KRLczSqsl0u3ig5crLWGXNRxs3qV961EgSoYMvAfZJ5UODdBUFlaNVpLU0wyRVRBQURPMFNGR0U3Sy4u",
          target = "_blank",
          icon("edit"), "Leave Feedback")
    )
),

  ## Segment 1: Initial Choice for app use ----
  conditionalPanel(
        condition = "input.go_button > 0 && !output.showSingleLandscape && !output.showMultipleLandscapes && !output.showLandscapeVisualizer && !output.showBatchProcessing",
    segment(
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", HTML("<br><br>Choosing an analysis type")),
      p(HTML("The province of Ontario is divided into 300 by 300 metre units, called 'pixels'. Each pixel contains information on habitat type, environment, potential degradation sources (e.g., light pollution, mining activity), and how easily it can be used by organisms crossing the landscape ('movement cost'). Analyzing the entire province at once is impractical due to high computational costs. Instead, select one or more specific landscapes to analyze or compare across previously analyzed landscapes. <br><br>
<b>Choose an option to continue:</b><br>")),
      actionButton("single_landscape", "Calculate metrics for a single landscape"),
      br(),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this if it’s your first time using the tool or if you want to analyze a single landscape. This option will output results in a single .csv file for review.</p></i>")),
      actionButton("batch_processing", "Calculate metrics for all landscapes of a given category"),
      br(),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this to calculate metrics for multiple landscapes belonging to a particular category (e.g., muncipalities, provincial parks).</p></i>")),
      actionButton("multiple_landscapes", "Compare metrics across multiple landscapes"),
      br(),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this if you want to compare restoration priorities across multiple landscapes. You must first create at least two results .csv files using the single landscape or category options.</p></i>")),
      actionButton("landscape_visualizer", "Visualize landscape(s)"),
      br(),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this to visualize the habitat types of one or more landscapes based on results .csv files.</p></i>"))
       )
  ),

  ## Segment 2:  Calculation type (any habitat or conservation areas) choice ----
  conditionalPanel(
    condition = "(output.showSingleLandscape > 0 || output.showBatchProcessing > 0) && !output.showhabitat_based_calculations && !output.showprotected_based_calculations",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Select what areas to consider when measuring the benefit of restoration"),
      p(HTML("Here you have the option of highlighting the benefits that restoration may have on areas of conservation concern.<br><br> 
<b>Choose an option below to continue:</b><br>
")),
      actionButton("habitat_based_calculations", "Focus on entire landscape"),
      br(),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this to measure the effect of restoring degraded land on the entire landscape.</p></i>")),
      actionButton("protected_based_calculations", "Focus on areas of conservation concern"),
      p(HTML("<i><p style='margin-left: 25px;'>Choose this to measure the effect of restoring degraded land on the size and connectivity of patches belonging to one or more categories of conservation concern,  such as provincial parks, conservation reserves, or municipal heritage areas.</p></i>"))
    )
  ),

  ## Segment 3: Check-box for defining what features are used to define 'areas of conservation concern'.----

# Script to prevent user from un-checking the last 'areas of conservation concern' check-box
# (this would result in errors - and this is one way to handle it)
tags$head(
  tags$script(HTML("
    function updateCheckboxState() {
      var checkboxes = $('.protected-checkbox');
      var checked = checkboxes.filter(':checked');
      checkboxes.prop('disabled', false);
      if (checked.length === 1) {
        checked.prop('disabled', true);
      }
    }

    function getCheckedCheckboxes() {
      var checkboxes = $('.protected-checkbox');
      var checkedValues = [];
      checkboxes.each(function() {
        if ($(this).is(':checked')) {
          checkedValues.push($(this).val());
        }
      });
      return checkedValues;
    }

    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'protected_checkboxes') {
        updateCheckboxState();
      }
    });

    $(document).ready(function() {
      updateCheckboxState();
    });
  "))
),
conditionalPanel(
  condition = "input.protected_based_calculations > 0",
  segment(
    br(),
    br(),
    br(),
    div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Defining areas of conservation concern"),
    p("Select what features should be included in areas of conservation concern. A minimum of 1 feature must be selected."),
    br(),
    uiOutput("protected_checkboxes"),
  )
),

  ## Segment 4: The leaflet map, setting landscape extent, and outputting plots ----
  conditionalPanel(
    condition = "(input.habitat_based_calculations > 0 || input.protected_based_calculations > 0) && output.showSingleLandscape > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Choosing a landscape"),
      p(HTML("You are ready to select a target landscape to analyze. You can choose to draw a landscape or select a landscape from pre-defined categories:<br><br>
  <b>Option 1: Choose landscape extent by drawing a shape</b><br>
  Ensure the “Your drawn landscape” box is checked in the panel on the righthand side of the map. Set a custom extent using the
  draw tools located at the top left of the map. Create a custom shape by first selecting the pentagon icon, then 
  clicking the map to drop points that define a target landscape. Alternatively, create a rectangle by clicking the square icon, then dropping a single
  point and dragging to define a target landscape.<br><br>
  <b>Option 2: Choose landscape extent by selecting a map feature</b><br>
  Make different features visible by clicking the checkboxes on the panel on the righthand
             side of the map. Hover over the map to view the names of individual features. Then, select a single feature by clicking
             on it (e.g., a single provincial park).<br>"
)),
      leafletOutput("map", height = 600),
      br(),
      br(),
      p(HTML("<b>Add a buffer around the target landscape</b><br>
      We recommend adding a buffer to the selected landscape. A buffer is an area that extends outward from your target landscape by a specified number of metres. Buffers are useful in reducing the influence of distortions or artefacts that may arise because features outside but near your selected landscape are not included when calculating restoration benefit. If you are considering restoration projects that may benefit more mobile species, your buffer size should be larger. Since the pixel units are 300 by 300 metres, any number entered here will be rounded down to the nearest multiple of 300.<br>")),
      numericInput("buffer_unit_value", "Enter an extent buffer value in metres (optional but recommended):", value = 1200),
      br(),
      br(),
      p(HTML("<b>Set the final extent</b><br>
               Finally, press “Set extent” to confirm the final target landscape to analyze. An image of the selected landscape, including the buffer area, is displayed for you to review, along with the proportion of total area covered by different habitat types.<br><br>
             When the landscape extent is set, a test is run on that landscape to see if it is large enough to calculate connectivity metrics. This will be explained in more detail as you progress through the tool.<br>
               ")),
      actionButton("set_extent", "Set extent"),
      br(),
      verbatimTextOutput("extent_values"),
      br(),
fluidRow(
  column(width = 6, plotlyOutput("rasterPlot", height = "800px")),
  column(width = 6, plotOutput("rasterPlot1legend", height = "800px"))),
        conditionalPanel(
          condition = "input.protected_based_calculations > 0",
            fluidRow(
             column(width = 6, plotlyOutput("protectedPlot", height = "800px")),
               column(width = 6, plotOutput("rasterPlot2legend", height = "800px"))),
    )
  )),

  ## Segment 5: Define degraded land, output plots ----
  conditionalPanel(
    condition = "input.set_extent > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Defining degraded pixels"),
      p(HTML("The next step is defining which pixels qualify as “degraded land” based on user-defined thresholds. Using the sliders below, you can customize the criteria under
      which pixels are considered degraded; these pixels are eligible for restoration. Conditions that could influence whether a pixel is considered degraded land include presence of active
      mines, abandoned mines, night light pollution, oil and gas extraction, aggregate extraction, topsoil extraction, and marginal farmland.<br><br>
      The slider values indicate the severity of each degradation condition, and the slider position indicates the minimum threshold. For example, setting “Night lights = 1” would mean that 
      land with the lowest levels of night light pollution would be considered degraded, and all areas with values above 1 would be considered degraded as well. Conversely, if the slider is
      set to 10, only land with the highest levels of light pollution would be considered degraded. To exclude or ignore a condition entirely, set the slider to “Not included”. For night 
      lights, “Night lights = 11” means that light pollution is not considered a factor defining degraded land.<br><br> 
      For a given pixel, only one condition needs to be met to be considered degraded. For example, if a pixel meets the threshold you set for night lights, but not for aggregate extraction,
      it will still be considered degraded.<br><br>
      After setting the thresholds, you can preview the pixels defined as “degraded” in a map.")),
      uiOutput("sliderPanel"),
      br(),
      p(HTML("Click the “preview” button to see which pixels in your target landscape are defined as degraded based on the conditions you chose. In the preview map, habitat types considered unrestorable (open water, urban areas, urban parks, transportation, good farmland, and hay/ pasture land) are excluded and not mapped (shown in white below). Feel free to re-adjust the sliders and try again until you're happy with which areas are defined as degraded.")),
      br(),
      actionButton("preview", "Preview"),
      br(),
      br(),
      div(style = "font-size: 20px;", textOutput("numDegradedPixels")),
      fluidRow(
        column(width = 6, plotlyOutput("degradedPlot", height = "800px")),
        column(width = 6, plotOutput("rasterPlot3legend", height = "800px"))),
      conditionalPanel(
        condition = "input.protected_based_calculations > 0",
        fluidRow(
          column(width = 6, plotlyOutput("degradedprotectedPlot", height = "800px")),
          column(width = 6, plotOutput("rasterPlot4legend", height = "800px"))))
    )
  ),

  ## Segment 6: Simulate restoration, output plots ----
  conditionalPanel(
    condition = "input.preview > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Simulating restoration"),
      p(HTML("The next step is to simulate the restoration of pixels that are defined as degraded. When 'simulate restoration' is clicked, each individual degraded pixel in the target landscape is replaced with the most prevalent natural habitat in the 3x3 pixel neighbourhood (8 pixels total) that surround it. <br><br>
     If a degraded pixel is not surrounded by any natural habitat in the 3x3 neighborhood, which may occur when the degraded land is completely surrounded by water, urban areas, or farmland, the tool will attempt to replace the degraded pixel using successively larger neighborhoods, which increase by 2 pixels each time (e.g., 5x5, 7x7, 9x9, etc.).<br><br> 
             The process continues until all degraded pixels are 'restored' to natural habitat. Click 'simulate restoration' to view the restored landscape, with degraded pixels replaced by natural habitat.")),
      actionButton("simulate_restoration", "Simulate restoration"),
      fluidRow(
        column(width = 6, plotlyOutput("restorationPlot", height = "800px")),
        column(width = 6, plotOutput("rasterPlot5legend", height = "800px"))),
    )
  ),



  ## Segment 7: Choosing how to set the combinations of restored pixels for metric calculations ----
  conditionalPanel(
    condition = "input.simulate_restoration > 0 && !output.showAutoCombo && !output.showManualCombo",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Choosing how to define candidate areas for restoration"),
      p(HTML("Your target landscape likely has multiple areas with one or more degraded pixels that can be restored. Eventually, the tool will rank these 'candidate areas' to determine which, once restored, has the greatest potential biodiversity benefit. However, first you must choose how to <i>define</i> candidate areas for restoration. <br><br>
<b>There are two options:</b>"
	    )),
      br(),
      actionButton("automatic_combination", "Define candidate areas based on contiguous habitat type"),
      p(HTML("<i><p style='margin-left: 25px;'>This option creates candidate areas by automatically dividing sections of degraded land into one or more groups. The pixels in each group share the same habitat type and are connected to each other. 'Connected' means that at least one corner of a pixel touches the corner of another pixel. In general, this option will produce fewer candidate areas.</p></i>")),
      br(),
      actionButton("manual_combination", "Define candidate areas based on a set number of pixels"),
      p(HTML("<i><p style='margin-left: 25px;'>If your restoration efforts must be focused on a small area within a target landscape – e.g., only one or two 300 x 300 m pixels (9 – 18 hectares), you may wish to define candidate areas based on a set number of pixels, e.g., 1, 2, or 3. This option <b>does not constrain candidate areas to be contiguous</b>; instead, it groups all possible combinations of degraded pixels for restoration based on a set number of pixels.</p></i>"))
    )
  ),

  ## Segment 8: Calculate number of combinations (only occurs if manual_combination choice is selected) ----
  conditionalPanel(
    condition = "input.manual_combination > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Set number of pixels"),
      p(HTML("Choose the number of pixels that will define candidate areas for restoration. The higher the number of candidate areas, the longer the computation time will be. For example, if a landscape has 100 degraded pixels, setting the number to 1, where each pixel is assessed on its own, will result in 100 candidate areas. Setting the number to 2 will result in 4950 candidate areas, and setting the number to 3 will result in 161700 candidate areas. On the other hand, if a target landscape has only 10 degraded pixels, then setting the number to 3 results in only 120 candidate areas. The number of candidate areas to analyze can quickly become very large (and take a very long time to assess) so you must consider the total number of degraded pixels in your target landscape when determining a reasonable number of pixels to include in a single candidate area.<br><br>You can test different scenarios by setting the number of pixels, then clicking 'Calculate number of candidate areas'. We recommend limiting the number of candidate areas to less than 15000.")),
      textOutput("numDegradedPixels_Seg8"),
      br(),
      numericInput("num_combined_pixels", "Set number of pixels:", value = 1, min = 1),
      br(),
      actionButton("calculate_combinations", "Calculate number of candidate areas"),
      br(),
      br(),
      div(style = "font-size: 20px;", textOutput("numCombinations"))
    )
  ),

  ## Segment 9: Calculate habitat metrics ----
  conditionalPanel(
    condition = "input.calculate_combinations > 0 || input.automatic_combination > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Calculating patch size"),
      p(HTML("Now, we can calculate the effect of restoring candidate areas on habitat patch size.<br><br>
Patch size is calculated as the average size of contiguous areas of a particular habitat type. For each habitat type, the effect of restoration is measured by comparing average patch size of the original landscape against the set of landscapes with each candidate area restored.  
The tool computes the difference in patch size between the original and each restored landscape. Additionally, if you selected the option to focus on ‘areas of conservation concern’, the change in the size of the conservation area is shown, regardless of habitat type. 
")),
      br(),
      uiOutput("dynamic_checkboxes"),
      br(),
      p(HTML("Click 'Calculate habitat metrics' to view results of patch size calculations.")),
      br(),
      actionButton("calculate_metrics", "Calculate habitat metrics"),
      br(),
      br(),
      textOutput("elapsedTime")
    )
  ),


# Segment 10: Merging habitat metrics in a table ----
conditionalPanel(
  condition = "input.calculate_metrics > 0",
  segment(
    br(),
    br(),
    br(),
    div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Merge patch size results?"),
    p("You may choose to merge patch size calculations across all habitat types, or keep results separate for each habitat type. If patch size results are merged, the change in average patch size for each habitat type are summed across all habitat types. If patch size for one habitat type is considered more valuable for the purposes of restoration than another, patch size metrics should be left separate so they can be weighted accordingly at a later step. You can try both options ('Merge' vs 'Do not merge') before making a final choice."),
    
    # Manually create radio buttons with correct labels
    div(
      tags$label("Merge patch size calculations:"),
      div(class = "radio",
          tags$label(
            tags$input(type = "radio", id = "merge_metrics_merge", name = "merge_metrics", value = "Merge", 
                       onclick = "Shiny.setInputValue('merge_metrics', this.value)"),
            "Merge"
          )),
      div(class = "radio",
          tags$label(
            tags$input(type = "radio", id = "merge_metrics_do_not_merge", name = "merge_metrics", value = "Do not merge", 
                       onclick = "Shiny.setInputValue('merge_metrics', this.value)"),
            "Do not merge"
          )),
    ),
    
    actionButton("perform_merge", "Proceed", style = "margin-top: 15px;"),
    br(),
    br(),
    dataTableOutput("habitatTable"),
    uiOutput("subtitleText1")
  )
),



  ## Segment 11: Calculate Connectivity Metrics ----
  conditionalPanel(
    condition = "input.perform_merge > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Calculating connectivity metrics"),
      p(HTML("Next, we will measure connectivity metrics for each candidate area. Connectivity can be defined as the ease of movement across a landscape due to connectedness of habitat types. Another way to view this is the amount of resistance a landscape has to movement. We assigned a movement cost value to each 300 x 300 metre pixel in the province, following Pither et al. 2023 (a full breakdown of the cost values for each pixel type are given
      <a href='https://figshare.com/articles/journal_contribution/Land_cover_layers_and_their_sources_used_to_construct_a_movement_cost_layer_for_Canada_/22143033' target='_blank'>here</a>). We modify cost values for some pixels: any degraded pixels are given a high cost value (1000), whereas restored pixels and natural habitat are given the lowest cost value (1). 
      <br><br> 
     If the option to focus on areas of conservation concern is selected, these areas, and any pixels restored within them, are given a cost value of 1, with other natural  habitat given a cost value of 10, and costs for all other pixel types scaled up by a magnitude of 10 (e.g. degraded pixels = 10000).<br><br>
             To get an idea of connectivity within each restored landscape, we measure the movement cost of many potential movement paths across the landscape and take an average of the cost of those paths as an indication of the resistance of that landscape; this is referred to as ‘mean path resistance’. These resistance paths are measured between all ‘nodes’ or ‘clusters’ of either contiguous areas of natural habitat, or areas of conservation concern (depending on the focus of the analysis) that occur in the landscape.<br><br>
             The mean path resistance  for each restored landscape is then subtracted from the mean path resistance of the degraded landscape – to get a measure of how much resistance was reduced (or connectivity increased) by restoring each candidate area. Positive values denote reduced resistance and better movement in a landscape, whereas negative values denote increased resistance and more difficult movement. Results for each combination are output in a table.")),
      actionButton("calculate_connectivity", "Calculate connectivity", style = "margin-top: 15px;"),
      br(),
      br(),
      textOutput("conTime"),
      br(),
      br(),
      dataTableOutput("connectivityTable"),
      uiOutput("subtitleText2")
    )
  ),


  ## Segment 12: Generating the environmental PCA ----
  conditionalPanel(
    condition = "input.calculate_connectivity > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Calculating environmental heterogeneity: creating principal component data layers"),
      p(HTML("If different species thrive in different combinations of environmental conditions, then a more environmentally heterogeneous landscape shoud be able to support more species. Here, we measure environmental heterogeneity in the original landscape, excluding degraded areas, and compare that value against each restored landscape, where the restored candidate areas contribute their environmental conditions to overall heterogeneity.
      <br><br>
	    There is a large set of variables to consider when measuring environmental heterogeneity, such as temperature, precipitation, and soil types. Principal Component Analysis (PCA) is a statistical method that uses one variable, called a principal component, to summarize variation in multiple variables. Sites - pixels in our case - that share similar environmental conditions will receive a similar principal component score. Because there can be multiple axes of environmental variation - e.g., hot vs cold sites, wet vs dry sites - there are multiple principal components.<br><br>
	    Here, we use PCA to summarize several environmental layers (temperature, precipitation, elevation, soil type, soil depth) and assign scores from each principal component to each pixel. Each principal component explains a proportion of the total environmental variation. For example, if almost all pixel-to-pixel variation is due to temperature variation, then one component might capture most of the total environmental variation. If there are strong temperature and precipitaton gradients, and temperature and precipitation are uncorrelated, then there might be two gradients each explaining half of the pixel-to-pixel environmental variation. The contribution of each principal component to overall environmental variation is shown as standard deviation and proportion of variance in a summary table.")),
      actionButton("calculate_pca", "Calculate PCA and print summary"),
      br(),
      br(),
      dataTableOutput("pcaSummary"),
    )
  ),


  ## Segment 13: Environmental heterogeneity calculation ----
  conditionalPanel(
    condition = "input.calculate_pca > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Calculating environmental heterogeneity: comparing degraded and restored landscapes"),
      p(HTML("Here, we measure the difference in environmental heterogeneity between the original landscape against the set of landscapes with each candidate area restored, based on the scores from the principal component analysis. In the original landscape, degraded pixels are treated as if they have no value, whereas after restoration pixels are given their principal component scores.
      <br><br>
      Heterogeneity, or variation in the landscape, is calculated by measuring the “average surface roughness” of the landscape – essentially a measure of how the environmental values for each pixel differ from the mean environmental value of all pixels in the landscape. Therefore, surface roughness indicates whether pixels are mostly similar to or different from each other. If restoration adds pixels to the landscape that have relatively novel environmental conditions, surface roughness increases from pre- to post-restoration. 
      <br><br> 
      You have the option to select how many of the principal component layers to include in the analysis. The default is to use the first two principal components, as they typically contain > 99% of the environmental variation. Results are output in a table for each candidate area.")),
      numericInput("num_pcs", "Number of principal components:",
        value = 2, min = 1, max = 22
      ),
      br(),
      actionButton("calculate_env_metrics", "Calculate environmental metrics"),
      br(),
      br(),
      textOutput("elapsedTimeEnv"),
      br(),
      br(),
      dataTableOutput("environmentTable"),
      uiOutput("subtitleText3")
    )
  ),

  ## Segment 14:  Merging all results/metrics together in preparation for weighting ----
  conditionalPanel(
    condition = "input.calculate_env_metrics > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Merging and scaling results"),
      p("To identify the best candidate area(s) to restore, results from all previous calculations are merged into a single table. The values for each metric (patch size, connectivity, heterogeneity) for each candidate area are scaled to be comparable. For each metric, the value for each candidate area is subtracted from the mean value for all candidate areas and the result is divided by the standard deviation. After scaling, values in the table below express the number of standard deviations a candidate area's value is from the mean for that metric."),
      actionButton("merge_and_display", "Merge and scale results"),
      br(),
      br(),
      dataTableOutput("mergedResultsTable"),
      uiOutput("subtitleText4")
    )
  ),

  ## Segment 15: Weighting and prioritization ----
  conditionalPanel(
    condition = "input.merge_and_display > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Weighting and finding the best candidate areas"),
      p(HTML("The final step is to determine and visualize the best candidate area(s) for restoration. Here, you have the option to adjust the weight given to each metric calculated in the tool. Each metric starts out with a default weight of 1. Different weights can be assigned with the goal of prioritizing some metrics in the selection process over others.")),
      uiOutput("weights_ui"),
      br(),
      p(HTML("<br><br>Now, set the number of candidate areas you wish to return. For example, if you set the number of top candidate areas to display to 5, the best 5 candidate areas will be displayed in descending order of importance, with the best candidate area displayed first.")),
      numericInput("num_top_combinations", "Number of top candidate areas to display:", value = 1, min = 1, step = 1),
      br(),
      p(HTML("When the 'Find best candidate areas' button is clicked, values for each metric are adjusted based on the weights you provide. The sum of weighted values is calculated for each candidate area, and the area with the maximum sum is identified as the best candidate area for restoration. Two figures are created: an interactive map displaying the best candidate areas, and another showing habitat types and candidate areas in the target landscape. The latter figure includes an option for displaying the best candidate area in conjunction with areas of conservation concern, if this option was selected.<br><br>
             <strong style='font-size: 14px;'>In the resulting map below, there is the option to visualize candidate areas alongside satellite imagery by toggling 'satellite view' in the map legend. Visualizing where candidate areas are located in real landscapes is a critical step to determine restoration feasibility, as original data layers may have misclassified habitat and other features and may not reflect recent landscape changes. Later on, you will also have the option of exporting the results in .kml format to visualize candidate areas in Google Earth or another mapping tool of your choice.")),
      actionButton("find_best_comb", "Find best candidate areas"),
      uiOutput("bestCombinationName", class = "ui message"),
      leafletOutput("map_best_comb", height = 600),
      uiOutput("plotsContainer")
    )
  ),

  ## Segment 16:  Saving the metrics to files ----
  conditionalPanel(
    condition = "input.find_best_comb > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Saving candidate areas and associated metrics"),
      p(HTML("Finally, to conclude the single landscape analysis, you can save the table containing the final, summed values for all environmental metrics for each candidate area. Enter a unique name for your landscape below. A .csv file containing the metrics for each combination (before weighting) is created. This results .csv file can be used to compare candidate areas in this landscape to other landscapes in the 'Multiple Landscapes' section of the tool.<br><br>Click 'Save landscape metrics and candidate area performance .CSV' below to save the results file.<br>")),
      br(),
      textInput("landscape_name", "Enter a unique name for your landscape:"),
      br(),
      actionButton("add_column", "Set unique landscape name")
    )
  ),

  ## Segment 17: The download button ----
  conditionalPanel(
    condition = "output.landscape_added",
    segment(
      downloadButton("download_data", "Save landscape metrics and candidate area performance .CSV", style = "height:60px; width:300px; font-size:25px;")),
      segment(
        p(HTML("If you would like to export a KML file to view the top candidate areas chosen in the analysis outside this tool (e.g., in Google Earth or ArcGIS), click 'Export top candidate area(s) to .KML'.<br>")),
        downloadButton("downloadKML", "Export top candidate area(s) to .KML", style = "height:60px; width:300px; font-size:25px;")),
      segment(
        p(HTML("If you would like to view this landscape later, you can save an .RDS file to import back into the tool to visualize.<br>")),
      downloadButton("downloadplotRDS", "Save landscape visualization", 
                     style = "height:60px; width:300px; font-size:25px;")),
    br(),
  ),

  ## Segment 18: Loading and merging multiple landscapes (if that initial choice is selected in segment 1) ----

conditionalPanel(
  condition = "output.showMultipleLandscapes > 0",
  segment(
    br(),
    br(),
    br(),
    div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", 
        "Load and merge combination metrics from multiple landscapes"),
    p("Here multiple .CSV files containing landscape data (created at the conclusion of the single landscape analysis) can be uploaded, 
       with each file representing a distinct landscape. The code reads these .CSV files, extracts information such as the extent 
       (i.e. spatial boundaries) and landscape name, and joins data from these files together to form a unified dataset for multi-landscape analysis. 
       The spatial extents of each landscape are visualized on a map, providing an overview of the coverage of the uploaded landscapes."),
    
    # Add CSS for screen-reader-only elements
    tags$style(HTML("
    .sr-only {
      position: absolute;
      width: 1px;
      height: 1px;
      padding: 0;
      margin: -1px;
      overflow: hidden;
      clip: rect(0, 0, 0, 0);
      border: 0;
    }
  ")),
    
    # Create the file input with an associated label
    div(
      tags$label("Choose .CSV files:", `for` = "fileInput"),
      fileInput("fileInput", label = NULL, multiple = TRUE, 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      # Add a hidden label for the hidden status input
      tags$label("File selection status", `for` = "fileInput-status", class = "sr-only")
    ),
    
    # JavaScript to manage the display of selected file status
    tags$script(HTML("
    $(document).ready(function() {
      var fileInputStatus = $('input[readonly][placeholder=\"No file selected\"]');
      if (fileInputStatus.length > 0) {
        fileInputStatus.attr('id', 'fileInput-status'); // Ensure the status input has an ID
      }
    });
  ")),
    
    # Leaflet map and table outputs
    leafletOutput("map_lands", height = 600),
    dataTableOutput("landscapeMergedTable"),
    uiOutput("subtitleText5")
  )),


  ## Segment 19: Comparing multiple landscapes to find top combinations ----

  conditionalPanel(
    condition = "output.fileSelected > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Weighting and ranking"),
      p("Similar to single landscape analysis, weights can be assigned to different metrics for the multi-landscape analysis.
        The best restored candidate areas across all landscapes are ranked using the weighted metrics, and displayed showing the candidate area name and landscape name. Individual plots for each of the top 
        combinations are also generated, highlighting candidate areas in the landscapes alongside habitat classes."),
      uiOutput("landscape_weights_ui"),
      br(),
      br(),
      numericInput("topNCombinations", "Number of top candidate areas to display:", value = 1, min = 1),
      br(),
      br(),
      actionButton("lands_best_comb", "Find best candidate areas"),
      br(),
      br(),
      uiOutput("topCombinations", class = "ui message"),
      leafletOutput("map_best_comb_multi", height = 600),
      br(),
      br(),
      uiOutput("dynamicPlots")
    )
  ),
  
  ## Segment 20: The download buttons for the 'multiple landscapes' results----
  conditionalPanel(
    condition = "input.lands_best_comb > 0",
    segment(
      downloadButton("download_multiple_data", "Save landscape metrics and candidate area performance csv", style = "height:60px; width:300px; font-size:25px;")),
      segment(
        downloadButton("download_multiple_KML", "Export top candidate area(s) to KML", style = "height:60px; width:300px; font-size:25px;")),
    br(),
  ),

## Segment 21: visualizing landscapes based on RDS files ----
# JavaScript to set an upload limit client-side
tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      // Attach event listener to the file input
      document.getElementById('vis_fileInput').addEventListener('change', function(event) {
        let maxCollectiveSize = 3 * 1024 * 1024 * 1024; // 3 GB in bytes
        let totalSize = 0;
        
        // Calculate the total size of all selected files
        for (let i = 0; i < event.target.files.length; i++) {
          totalSize += event.target.files[i].size;
        }
        
        // If the total size exceeds the limit, show an alert and reset the file input
        if (totalSize > maxCollectiveSize) {
          alert('The total size of selected files exceeds 3 GB. Please select smaller files.');
          event.target.value = '';  // Reset the file input element
        }
      });
    });
  ")),

# UI for landscape visualizer
conditionalPanel(
  condition = "output.showLandscapeVisualizer > 0",
  segment(
    br(),
    br(),
    br(),
    div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Select landscapes to visualize"),
    p("This is a simple visualizer. Here users can select one or multiple results .RDS files from previously analyzed landscapes to visualize.
        Individual plots containing habitat types in those landscapes are then displayed, and a breakdown of habitat type as a proportion (in percent) of pixels is 
        output for each landscape."),
    fileInput("vis_fileInput", "Choose .RDS files", multiple = TRUE, accept = c(".rds")),
    br(),
    br(),
    actionButton("view_landscapes", "View landscapes"),
    br(),
    br(),
    uiOutput("vis_dynamicPlots")
  )
),
  
  
  ## Segment 22: Batch Processing Category Selection ----
  conditionalPanel(
    condition = "(input.habitat_based_calculations > 0 || input.protected_based_calculations > 0) && output.showBatchProcessing > 0",
    segment(
      br(),
      br(),
      br(),
      div(style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;", "Batch processing for all landscapes of a given type"),
      br(),
      selectInput(
        inputId = "sf_object_selection",
        label = "Select a group of landscapes to analyze:",
        choices = list(
          "Conservation reserves" = "Conservation_reserves",
          "Natural heritage value areas" = "Natural_heritage_value_areas",
          "Natural heritage system areas" = "Natural_heritage_system_areas",
          "Non-governmental organization reserves" = "NGO_reserves",
          "Provincial parks" = "Provincial_parks",
          "Lower municipalities" = "Lower_municipality",
          "Upper municipalities" = "Upper_municipality",
          "National parks" = "National_parks",
          "Conservation areas" = "Conservation_areas",
          "Far North protected areas" = "Far_North_protected_areas",
          "Municipal heritage areas" = "Municipal_Heritage_areas",
          "Migratory bird sanctuaries" = "Migratory_Bird_sanctuaries",
          "National wildlife areas" = "National_Wildlife_areas",
          "Wilderness areas" = "Wilderness_areas",
          "National capital valued ecosystems or habitats" = "National_capital_valued_ecosystem_or_habitat",
          "Provincial planned protected areas" = "Provincial_planned_protected_area",
          "Crown plan protected areas" = "Crown_plan_protected_area",
          "Other effective area-based conservation measures" = "Other_effective_area_based_conservation_measures"
        )
      ),
      uiOutput("feature_selection_ui"),  # Dynamic input for individual features
      br(),
      br(),
      p(HTML("Below, a landscape buffer can be set by entering a number in the text box. Since the pixel 
	    units are 300 by 300 metres, any number entered gets rounded down to the nearest multiple of 300.")),
      numericInput("batch_buffer_unit_value", "(Optional) Enter a landscape buffer value in metres:", value = 1200),
      br(),
      br(),
      p(HTML("Below, set a combination of conditions defining which pixels are degraded using sliders. Conditions include active mines, abandoned mines (AMIS),
      night lights, oil/gas extraction, aggregate extraction, topsoil extraction, and marginal farmland. Slider values indicate the level of
      the particular condition: e.g. Night Lights = 1 would mean the lowest level of light pollution, whereas 
      Night Lights = 10 would mean the highest. 
      <br><br>
      The slider position indicates the miniumum level (or threshold) of that degradation 
      condition required for pixels to considered degraded - so an 8 would mean only pixels with an 8 or higher for that condition would be considered degraded,
      whereas a 1 would mean all pixels with values greater than 1 would be considered degraded. For a given pixel, determination of whether or not it is degraded 
      is inclusive of all slider thresholds, and only one condition needs to be met to be considered degraded (e.g. if a pixel meets the set threshold for Aggregate Extraction,
      but not for Topsoil Extraction, it will still be defined as degraded). Any condition can also be excluded/ignored by setting its slider to the 'Not Included'
      value.")),
      uiOutput("BatchsliderPanel"),
      br(),
      p("Below is an option to merge patch size results across all habitat types, or keep results separate for each habitat type.
        If patch size results are merged, the sum of patch size results are combined. If they are not merged, they are kept as calculated.
        If patch size for one habitat type is considered more valuable for the purposes of restoration than another, 
        patch size metrics should be left seperate so they can be weighted accordingly at a later step."),
      # Manually create radio buttons with correct labels
      div(
        tags$label("Merge patch size calculations:"),
        div(class = "radio",
            tags$label(
              tags$input(type = "radio", id = "merge_metrics_merge", name = "merge_metrics", value = "Merge", 
                         onclick = "Shiny.setInputValue('merge_metrics', this.value)"),
              "Merge"
            )),
        div(class = "radio",
            tags$label(
              tags$input(type = "radio", id = "merge_metrics_do_not_merge", name = "merge_metrics", value = "Do not merge", 
                         onclick = "Shiny.setInputValue('merge_metrics', this.value)"),
              "Do not merge"
            )),
      ),
      br(),
      actionButton("run_batch", "Calculate metrics for all landscapes of selected type"),
    )
  ),


## Segment 23:  Saving the metrics to files ----
conditionalPanel(
  condition = "input.run_batch > 0",
    segment(
      downloadButton("download_data_batch", "Save landscape metics and combinaton performance", style = "height:60px; width:300px; font-size:25px;")),
  br(),
    )
))


# Run the app
shinyApp(ui, server)
