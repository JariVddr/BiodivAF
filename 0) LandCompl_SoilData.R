#usethis::edit_r_environ()
#TMPDIR=E:/rtemp 
#TMP=E:/rtemp 
#TEMP=E:/rtemp
tempdir()

#####################

# removeTmpFiles when disk is full
# Warning: problem writing to connectionWarning: size of values file does not match the number of cells (given the data type)

#packages
library(sf)
#install.packages("terra")
library(terra)
library(dplyr)
library(tidyr)
library(raster)
library(sp)
library(rgdal)
library(rgeos)  # Buffering the spatial points 
library(maptools)
library(ggplot2)

# ----------------------------------------------------------------------------
# Metadata on raster
# ----------------------------------------------------------------------------
# raster layer with landcovers: source Vito
# https://lcviewer.vito.be/2019
# https://worldcover2021.esa.int/download 
# https://soilgrids.org/ 

#legend #land cover classification according to FAO
#10 tree cover
#20 shrubland
#30 grassland
#40 cropland
#50 built-up
#60 sparse vegetation
#70 snow and ice
#80 permanent water bodies
#90 herbaceous wetland
#95 mangroves
#100 moss and lichen

# create matrix
class<-c(10,20,30,40,50,60,70,80,90,95,100)
forest<-c(1,0,0,0,0,0,0,0,0,0,0)
shrubland<-c(0,1,0,0,0,0,0,0,0,1,0) # + mangroves
grassland<-c(0,0,1,0,0,0,0,0,0,0,0)
cropland<-c(0,0,0,1,0,0,0,0,0,0,0)
builtup<-c(0,0,0,0,1,0,0,0,0,0,0)
water<-c(0,0,0,0,0,0,0,1,0,0,0)     # + ice
other<-c(0,0,0,0,0,1,0,0,1,0,1)     # Sparse vegetation, herbaceous wetland, moss and lichen
df_ESA<-cbind(class,forest, shrubland,grassland, cropland, builtup,water, other)


# -----------------------------------------------------------------------------
# Functions for landscape analysis
# -----------------------------------------------------------------------------

#########################
# Helper function to safely extract
#########################
safe_extract <- function(cover, xy, buffer) {
  result <- raster::extract(cover, xy, buffer = buffer, fun = mean, weights = TRUE) # %
  # result <- raster::extract(cover, xy, buffer = buffer, fun = sum, na.rm = TRUE) # Number of pixels
  if (length(result) == 0) {
    return(rep(0, nrow(cluster))) 
  } else {
      result[is.na(result)] <- 0
    }
  return(result)
}

#########################
# ESA world cover
#########################
landscape_analysis_ESA <- function(cluster, ESA, df_ESA, xy, buffer) {
  
  #-----------------------------------------
  # Reclassify raster
  #-----------------------------------------
  cover_forest <- reclassify(ESA, as.matrix(df_ESA)[, 1:2]) 
  cover_shrub<- reclassify(ESA, as.matrix(df_ESA)[, c(1,3)]) 
  cover_grass<- reclassify(ESA, as.matrix(df_ESA)[, c(1,4)]) 
  cover_crop<- reclassify(ESA, as.matrix(df_ESA)[, c(1,5)]) 
  cover_bu<- reclassify(ESA, as.matrix(df_ESA)[, c(1,6)]) 
  cover_water<- reclassify(ESA, as.matrix(df_ESA)[, c(1,7)]) 
  cover_other<- reclassify(ESA, as.matrix(df_ESA)[, c(1,8)]) 
  
  #-----------------------------------------
  # Adjusted version needs preallocation of dataset
  #-----------------------------------------
  #mean weighted by area gives proportional cover
  #buffer is in meters
  cluster$forest_frac<-safe_extract(cover_forest, xy, buffer=buffer)

  
  cluster$shrub_frac<-safe_extract(cover_shrub, xy, buffer=buffer)
  
  cluster$grass_frac<-safe_extract(cover_grass, xy, buffer=buffer)
  
  cluster$crop_frac<-safe_extract(cover_crop, xy, buffer=buffer)
  
  cluster$builtup_frac<-safe_extract(cover_bu, xy, buffer=buffer)
  
  cluster$water_frac<-safe_extract(cover_water, xy, buffer=buffer)
  
  cluster$other_frac<-safe_extract(cover_other, xy, buffer=buffer)
  
  return(cluster)
}

#########################
# Batch processing
#########################
run_over_rasters_ESA <- function(coord_list, file.names, df_ESA) {
  
  #-----------------------------------------
  # Preallocate 0-values
  #-----------------------------------------
  coord_list <- coord_list %>%
    mutate(forest_frac = 0, shrub_frac = 0, 
           grass_frac = 0, crop_frac = 0, 
           builtup_frac = 0, water_frac = 0, 
           other_frac = 0)
  
  #-----------------------------------------
  # Loop through each point
  #-----------------------------------------
  for (j in 1:nrow(coord_list)) {
    cat('\n \n This is coordinate', j, 'of', nrow(coord_list), '\n \n')
    coord <- coord_list[j,]
    buffer <- coord$buffer
    
    # -------------------------------------------------------------
    # Grid coordinates for solving edge effects
    # -------------------------------------------------------------
    grid <- calculate_grid(x = coord$Lng, y = coord$Lat, ID = coord$Loc, distance_km = coord$buffer/1000, directions = c("N", "E", "S", "W"))
    
    
    #-----------------------------------------
    # Turn grid dataframe into spatial points
    #-----------------------------------------
    x<-grid$Lng
    y<-grid$Lat
    xy_grid<-data.frame(x , y)
    xy_grid
    coordinates(xy_grid) <- ~ x + y
    proj4string(xy_grid) <- "+proj=longlat +datum=WGS84 +no_defs" #projection of raster
    
    #-----------------------------------------
    # Initialize an empty list to store raster paths with points
    #-----------------------------------------
    valid_rasters <- list()
    
    #-----------------------------------------
    # Check each raster to see if it contains any points
    #-----------------------------------------
    for (k in 1:length(file.names)) {
      
      cover <- raster(file.names[k])
      
      # Check if any points belong to the raster
      if (!is.null(grid) && any(!is.na(extract(cover, xy_grid)))) {
        # cat('\n \n Checking raster', k, 'of', length(file.names), '\n \n')
        cat('Points found in raster:', file.names[k], '\n\n')
        valid_rasters <- c(valid_rasters, file.names[k])
      } else {
        # cat('\n \n Checking raster', k, 'of', length(file.names), '\n \n')
        # cat('No points found in raster:', file.names[k], '\n')
      }
    }
    
    # Process only the rasters that contain points
    if (length(valid_rasters) == 1) {
      cover_worldcover <- raster(valid_rasters[[1]])
    } else if( length(valid_rasters) == 2) {
      cover_worldcover <- raster::merge(raster(valid_rasters[[1]]), raster(valid_rasters[[2]]))
    } else if (length(valid_rasters) == 3) {
      cover_worldcover <- raster::merge(raster(valid_rasters[[1]]), raster(valid_rasters[[2]]), raster(valid_rasters[[3]]))
    } else if (length(valid_rasters) == 4) {
      cover_worldcover <- raster::merge(raster(valid_rasters[[1]]), raster(valid_rasters[[2]]), raster(valid_rasters[[3]]), raster(valid_rasters[[4]]))
    } else {
      cat('No valid rasters found for study:', coord_list$Loc[j], '\n')
      next}
    
    #-----------------------------------------
    # Turn coord into spatial point- for analysis
    #-----------------------------------------
    x<-coord$Lng
    y<-coord$Lat
    xy<-data.frame(x , y)
    xy
    coordinates(xy) <- ~ x + y
    proj4string(xy) <- "+proj=longlat +datum=WGS84 +no_defs" #projection of raster
    
    # Crop raster to the extent of points, adding a small buffer
    e <- as(extent(min(coord$Lng) - 0.15, max(coord$Lng) + 0.15, 
                   min(coord$Lat) - 0.15, max(coord$Lat) + 0.15), 'SpatialPolygons')
    crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
    
    # Crop the raster to a smaller area around the points
    r <- raster::crop(cover_worldcover, e)
    
    #-----------------------------------------
    # Run landscape analysis (assuming the landscape_analysis_ESA function is defined)
    #-----------------------------------------
    grid_coordinates_new <- landscape_analysis_ESA(cluster = coord, 
                                                   ESA = r, df_ESA, xy, buffer = buffer)
    coord_list[j,] <- grid_coordinates_new
  }
  
    # -------------------------------------------------------------
    # Normalize values to % of total land cover
    # -------------------------------------------------------------
    coord_list <- coord_list %>% 
      mutate(tot_frac = forest_frac + shrub_frac + grass_frac + crop_frac + builtup_frac + water_frac + other_frac,
             forest_frac = forest_frac/tot_frac,
             shrub_frac = shrub_frac/tot_frac,
             grass_frac = grass_frac/tot_frac,
             crop_frac = crop_frac/tot_frac,
             builtup_frac = builtup_frac/tot_frac,
             water_frac = water_frac/tot_frac,
             other_frac = other_frac/tot_frac)
 
  return(coord_list)
}

#########################
# SoilGrids values
#########################
# install.packages(c("jsonlite", "httr"))
library(jsonlite)
library(httr)

get_soilgrids <- function(locs, retries = 3) {
  lat <- locs$Lat
  lon <- locs$Lng
  
  base_url <- "https://rest.isric.org/soilgrids/v2.0/properties/query"
  
  # Manually construct the query string for multiple properties & depths
  query_string <- paste0(
    "?lon=", lon,
    "&lat=", lat,
    "&property=cfvo&property=clay&property=sand&property=silt",
    "&depth=0-5cm&depth=5-15cm&depth=15-30cm",
    "&value=mean")
  
  full_url <- paste0(base_url, query_string)
  Sys.sleep(3)  # Wait 3 seconds between requests
  
  for (attempt in 1:retries) {
    response <- GET(full_url, add_headers(accept = "application/json"))
    
    if (status_code(response) == 200) {
      content_text <- content(response, "text", encoding = "UTF-8")
      
      # Define depth labels and fractions
      depths <- c("0-5cm", "5-15cm", "15-30cm")
      fractions <- c("cfvo", "clay", "sand", "silt")
      
      # Regular expression to capture mean values in the form {"mean":XXX}
      mean_values <- unlist(regmatches(content_text, gregexpr('\\{"mean":(\\d+)\\}', content_text)))
      mean_values <- gsub('^\\{"mean":|\\}$', '', mean_values)
      mean_values <- as.numeric(mean_values)
      cfvo_vec <- mean_values[1:3]
      clay_vec <- mean_values[4:6]
      sand_vec <- mean_values[7:9]
      silt_vec <- mean_values[10:12]
      
      # Extract weighted means (g/kg or cm³/dm³)
      clay   <- (clay_vec[1]*5 + clay_vec[2]*10 + clay_vec[3]*15)/30 # Weighted mean
      silt   <- (silt_vec[1]*5 + silt_vec[2]*10 + silt_vec[3]*15)/30 # Weighted mean
      sand   <- (sand_vec[1]*5 + sand_vec[2]*10 + sand_vec[3]*15)/30 # Weighted mean
      coarse <- (cfvo_vec[1]*5 + cfvo_vec[2]*10 + cfvo_vec[3]*15)/30 # Weighted mean
      # Extract weighted means (%)
      clay   <- clay/10
      silt   <- silt/10
      sand   <- sand/10
      coarse <- coarse/10
      
      return(data.frame(Lat = lat, Lng = lon, Clay = clay, Silt = silt, Sand = sand, Coarse = coarse))
    } 
    
    else if (status_code(response) == 429) {
      warning("API rate limit exceeded. Retrying after 180 seconds...")
      Sys.sleep(180)
    } 
    
    else {
      warning(paste("Request failed with status:", status_code(response)))
      return(data.frame(Lat = lat, Lng = lon, Clay = NA, Silt = NA, Sand = NA, Coarse = NA))
    }
  }
  stop("Failed after ", retries, " retries")
}

#########################
# Soiltype classification
#########################
classify_soil_usda <- function(sand, silt, clay) {
  # Check if any inputs are missing
  if (is.na(sand) | is.na(silt) | is.na(clay)) {
    return(NA)  # Return NA instead of causing an error
  }
  
  if (clay >= 40) {
    if (silt >= 40) return("Clay") # Silty Clay
    if (sand >= 45) return("Clay") # Sandy Clay
    return("Clay")
  }
  
  if (clay >= 27 & clay < 40) {
    if (silt >= 40) return("Clay") # Silty Clay Loam
    if (sand >= 45) return("Clay") # Sandy Clay Loam
    if (sand >= 45 & clay >= 35) return("Clay") # Sandy Clay
    return("Clay") # Clay Loam
  }
  
  if (clay < 27 & clay > 20 & silt < 28) return("Clay") # Sandy Clay Loam
  
  if (sand <= 52 & silt >= 28 & silt <= 50 & clay >= 7 & clay <= 27) return("Loam") # Loam
  
  if (silt >= 80 & clay < 15) return("Loam") # Silt
  
  if (silt >= 50 & silt < 85 & clay < 27) return("Loam") # Silt Loam
  
  if (sand >= 85 & clay <= 10 & silt + clay <= 15) return("Sand") # Sand
  
  if (sand >= 70 & clay <= 15 & silt + clay <= 30) return("Sand") # Loamy Sand
  
  if (silt < 40 & sand > 52 & clay < 20) return("Sand") # Sandy Loam
  
  if (silt < 50 & clay < 7) return("Sand") # Sandy Loam
  
  return("")
  
}

##############################################################################
##############################################################################
##############################################################################

# ----------------------------------------------------------------------------
# Find separate coordinates for each study
# ----------------------------------------------------------------------------
locs <- all_data %>% 
  dplyr::select(lng_exact, lat_exact, lng_region, lat_region) %>%
  distinct() %>% mutate(buffer = 1000, Lat = lat_exact, Lng = lng_exact) %>% # 1 km buffer
  mutate(buffer = ifelse(is.na(lng_exact), 3000, buffer)) %>% # 3km buffer when we have region coordinates
  mutate(Lng = ifelse(buffer == 3000, lng_region, Lng)) %>%
  mutate(Lat = ifelse(buffer == 3000, lat_region, Lat)) %>%
  filter(!is.na(Lat) & !is.na(Lng)) %>%
  mutate(Loc = row_number())

# ----------------------------------------------------------------------------
# Run functions
# ----------------------------------------------------------------------------
setwd("E:/Users/jjovdndr/Raster-files/International maps/ESA Worldcover/")
file.names <- list.files("E:/Users/jjovdndr/Raster-files/International maps/ESA Worldcover/", 
                         pattern = "*.tif", full.names = TRUE)

coords <- run_over_rasters_ESA(coord_list = locs, file.names, df_ESA)

for (i in 1: nrow(locs)) {
  if (i == 1) {
    soil_data <- get_soilgrids(locs[i, ])
  } else {
    soil_data <- rbind(soil_data, get_soilgrids(locs[i, ]))
  }
}

plotdata <- coords %>%
  arrange(tot_frac) 

coords <- left_join(coords, soil_data, by = c("Lat", "Lng")) %>% 
  dplyr::select(-tot_frac, -buffer, -Loc, -Lat, -Lng)

setwd("D:/Users/jjovdndr/OneDrive - UGent/PhD DigitAf/Papers/Rproj/Paper 4/")
# ----------------------------------------------------------------------------
# Reprocess values
# ----------------------------------------------------------------------------
coords <- coords %>%
  mutate(forestshrub_frac = forest_frac + shrub_frac,
         grasscrop_frac = grass_frac + crop_frac,
         Soil_type = mapply(classify_soil_usda, Sand, Silt, Clay)) %>%
  mutate(forest_frac = forest_frac*100,
         shrub_frac = shrub_frac*100,
         grass_frac = grass_frac*100,
         crop_frac = crop_frac*100,
         builtup_frac = builtup_frac*100,
         water_frac = water_frac*100,
         other_frac = other_frac*100,
         forestshrub_frac = forestshrub_frac*100,
         grasscrop_frac = grasscrop_frac*100)
  
  
