#Function to calculate tree height from clinometer angles and distance to tree
#Using trigonometric relationships
#Handles different cases of tree position relative to observer (uphill, downhill, same level)
calculate_tree_height <- function(plotID, plant_nr, dist_to_tree, eye_height, angle_canopy, angle_base, tree_position, height_pos, verbose = TRUE) { #for bottom or top height/angles
  
  # Convert angles from degrees to radians
  angle_canopy_rad <- angle_canopy * (pi / 180)
  angle_base_rad <- angle_base * (pi / 180)
  
  # --- 1. PRIORITY: invalid / warning cases first ---
  if (is.na(angle_canopy_rad) | is.na(angle_base_rad)) {
    #NA values in angles
    if (verbose) {
      print('Warning! NA value in angle measurements! Setting height to NA!')
      print(paste0("plotID: ", plotID, ", plant_nr: ", plant_nr, ", angle_canopy: ", angle_canopy, ", angle_base: ", angle_base, ", tree_position: ", tree_position, ", height_position: ", height_pos))
    }
    return(list(height_canopy = NA, height_canopy_eye = NA))
  }
  
  if (angle_canopy_rad < angle_base_rad) {
    if (verbose) {
      print('Warning! Angle to canopy is less than angle to base! Check measurements! Setting height to NA!')
      print(paste0("plotID: ", plotID, ", plant_nr: ", plant_nr, ", angle_canopy: ", angle_canopy, ", angle_base: ", angle_base, ", tree_position: ", tree_position, ", height_position: ", height_pos))
    }
    return(list(height_canopy = NA, height_canopy_eye = NA))
  }
  
  if ((angle_canopy_rad == angle_base_rad) & (height_pos == 'top')) {
    # Only a problem for top canopy angle, since the bottom of the canopy could be at ground level.
    if (verbose) {
      print('Warning! Angle to top canopy is equal to angle to base! Check measurements! Setting height to NA!')
      print(paste0("plotID: ", plotID, ", plant_nr: ", plant_nr, ", angle_canopy: ", angle_canopy, ", angle_base: ", angle_base, ", tree_position: ", tree_position, ", height_position: ", height_pos))
    }
    return(list(height_canopy = NA, height_canopy_eye = NA))
  }
  
  # --- 2. Normal geometric cases after all warning checks ---  
  if (angle_canopy_rad > 0 & angle_base_rad > 0) {
    # Tree should be uphill if angle to base is bigger than zero!
    # Angle to canopy cannot be equal to zero otherwise smaller than angle of base
    if (!is.na(tree_position) && tree_position != 'uphill' && verbose) {
      print('Warning! Angles indicate tree is uphill but tree_position is not set to uphill!')
      print(paste0("plotID: ", plotID, ", plant_nr: ", plant_nr, ", angle_canopy: ", angle_canopy, ", angle_base: ", angle_base, ", tree_position: ", tree_position, ", height_position: ", height_pos))
    }
    height_canopy <- dist_to_tree * tan(angle_canopy_rad) - dist_to_tree * tan(angle_base_rad)
    height_canopy_eye <- NA
    
  } else if (angle_canopy_rad >= 0 & angle_base_rad < 0) {
    # Can happen with tree being uphill, downhill or at the same level as the observer
    # Equation is the same.
    # In this case (angle to base < 0), angle to canopy can be equal to zero
    height_canopy <- dist_to_tree * tan(angle_canopy_rad) + dist_to_tree * tan(-angle_base_rad)
    height_canopy_eye <- case_when(
      tree_position == "same" ~ eye_height + dist_to_tree * tan(angle_canopy_rad),
      TRUE ~ NA_real_
    )
    
  } else if (angle_canopy_rad < 0 & angle_base_rad < 0) {
    # Can happen with tree being uphill, downhill or at the same level as the observer
    # Equation is the same.
    height_canopy <- dist_to_tree * tan(-angle_base_rad) - dist_to_tree * tan(-angle_canopy_rad)
    height_canopy_eye <- case_when(
      tree_position == "same" ~ eye_height - dist_to_tree * tan(-angle_canopy_rad),
      TRUE ~ NA_real_
    )
  } else {
    if (verbose) {
      print('Warning! Unhandled case in height calculation! Setting height to NA!')
      print(paste0("plotID: ", plotID, ", plant_nr: ", plant_nr, ", angle_canopy: ", angle_canopy, ", angle_base: ", angle_base, ", tree_position: ", tree_position, ", height_position: ", height_pos))
    }
    return(list(height_canopy = NA, height_canopy_eye = NA))
  }
  
  return(list(height_canopy = height_canopy, height_canopy_eye = height_canopy_eye))
}