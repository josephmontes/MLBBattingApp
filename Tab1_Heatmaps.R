# Use the full Statcast data ('alldat') to mutate, clean, and isolate the data needed for heat maps ('heatmap_data')

  heatmap_data <- alldat %>% mutate(pfx_x_in_pv = -12*pfx_x, 
                                    pfx_z_in = 12*pfx_z,
                                    barrel = ifelse((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                                                    & launch_angle > 4 & launch_angle < 50, 1, 0),
                                    pitch_type = case_when(
                                      pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper", "Slow Curve") ~ "Breaking Ball",
                                      pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter") ~ "Fastball",
                                      pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus") ~ "Offspeed",
                                      TRUE ~ "Unknown"  # Default value if none of the conditions are met
                                    )) %>%  
    select(pitch_name, p_throws, release_speed, pfx_x_in_pv, pfx_z_in, release_spin_rate, plate_x, 
           plate_z, player_name, game_year, description, bb_type, barrel, pitch_type) %>% 
    filter(!is.na(pitch_name), !is.na(p_throws),
           !is.na(release_speed), !is.na(pfx_x_in_pv), !is.na(pfx_z_in), 
           !is.na(release_spin_rate),!is.na(plate_x), !is.na(plate_z),
           !is.na(player_name))
  
  
# Create 'find_plots()' function used to filter the data for each heat map stat-type
  
  find_plots <- function(data, column, value) {
    subset(data, data[[column]] == value)
  }


# The heat maps are individually built in the server of the Rshiny app
 # See 'Shiny_UIServer.R' lines 403-629
 # Or see 'Full_MLBBattingApp.R' lines 2535 - 2740
