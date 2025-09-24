#######################################
# Function to convert kilometers to degrees
#######################################
km_to_degrees <- function(km) {
  degrees_per_km = km / 111.32 # Approximation: 1 degree latitude ~ 111.32 km
  return(degrees_per_km)
}

#######################################
# Function to calculate new coordinates
#######################################
calculate_coordinates <- function(x, y, angle_rad, distance) {
  tmp_lng = x + distance * cos(angle_rad)
  tmp_lat = y + distance * sin(angle_rad)
  return(data.frame(Lng = tmp_lng, Lat = tmp_lat))
}

#######################################
# Function to calculate grid coordinates
#######################################
calculate_grid <- function(x, y, ID, distance_km, directions) {
  # x: longitude
  # y: latitude
  # distance_km: a single digit or a vector (e.g., seq(5, 40, by=5))
  # directions: a vector of directions
  
  # Convert the distance from kilometers to degrees
  distance_degrees <- km_to_degrees(distance_km)
  
  # Define all possible directions and their corresponding angles in radians
  directions_tot <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  angles_tot <- seq(0, 315, by = 45) * (pi / 180) # Convert angles to radians
  dir_angle_df <- data.frame(directions_tot, angles_tot)
  
  # Filter the directions and angles we need
  angles <- dir_angle_df[dir_angle_df$directions_tot %in% directions, "angles_tot"]
  
  # Initialize a data frame to store all grid points
  grid_coordinates <- data.frame(Lng = numeric(0), Lat = numeric(0), OriginalID = integer(0))
  
  # Iterate over each coordinate in x and y
  for (i in 1:length(x)) {
    current_x <- x[i]
    current_y <- y[i]
    
    # Calculate new coordinates for each direction
    for (j in seq_along(angles)) {
      angle_rad <- angles[j]
      new_coords <- calculate_coordinates(current_x, current_y, angle_rad, distance_degrees)
      new_coords$OriginalID <- ID[i] # Track which original point these coordinates come from
      grid_coordinates <- rbind(grid_coordinates, new_coords)
    }
    
    # Add the original point as well
    grid_coordinates <- rbind(grid_coordinates, data.frame(Lng = current_x, Lat = current_y, OriginalID = ID[i]))
  }
  
  return(grid_coordinates)
}