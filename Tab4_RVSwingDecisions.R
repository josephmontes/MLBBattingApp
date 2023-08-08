# This fourth tab is centered around dividing and analyzing the strike zone by 'heart' of the zone, 'shadow' or edge of the zone, 'chase' part of the zone, and 'waste' pitches
 # Use 'alldat' to mutate, clean, and isolate the data needed for the Swing Decision data visualization 

  swing_dat <- alldat %>% 
    filter(!is.na(plate_x)) %>% 
    mutate(count= paste(balls, strikes),
           pfx_x_in_pv = pfx_x*-12, 
           pfx_z_in = pfx_z*12) %>% 
    select(p_throws, game_year, pfx_x_in_pv, pfx_z_in, count, plate_x, plate_z, pitch_name, 
           pitch_type, sz_top, sz_bot, description, delta_run_exp, player_name, zone, balls, strikes)
  
  
  # I created a second get_years() function to execute this data visualization
   # Each argument in the function is an input in the Rshiny app that will filter the data
  
  get_years2 <- function(df, years, p_throws, pitch_type, counts) {
    filtered_data <- df %>%
      filter(game_year %in% years,
             count %in% counts)
    
    if(p_throws == "R"){
      pidf <- filtered_data %>%  filter(p_throws=="R")
    } else if (p_throws == "L"){
      pidf <- filtered_data %>%  filter(p_throws== "L")
    } else if (p_throws == "B"){
      pidf <- filtered_data
    }
    
    if(pitch_type == "Breaking Ball") {
      pitch_df <- pidf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                                     "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      pitch_df <- pidf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      pitch_df <- pidf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      pitch_df <- pidf
    }
    
    return(pitch_df)
  }
  
 # CREATE PLAYER TABLE
  # Create the get_loc_table() function that will output the info needed for the Swing Decision data visualizations
  # 'df' comes from the data frame filtered by previously created get_years2()
  
  
  get_loc_table <- function(df, player){
    
    # Filter the player that will be prompted by user input
    
    pdf <- filter(df, player_name == player)
    
    
    # Create the table that will display the necessary data
    
    # Multiplying the strike zone coordinates by 12 (inches) made it easier to define the parts of the zone
    
    table <- pdf %>% mutate(plate_x = plate_x *12, 
                            plate_z = plate_z*12,
                            sz_top = sz_top*12,
                            sz_bot = sz_bot*12,
                            
                            
                            # Mutate a category column that uses the plate coordinates and the player's strike zone top and bottom to determine what part of the zone the pitch was in
                            # The proportions come from Baseball Savant's strike zone breakdown image (can be found here) https://www.si.com/mlb/2021/03/08/baseball-preview-breakout-pitchers-corbin-burnes
                            
                            category = case_when(
                              plate_x <= 6.7 & plate_x >= -6.7 & plate_z >= ((11/9)*sz_bot) & plate_z <= ((19/21)*sz_top) ~ "heart",
                              (plate_x >= 6.7 & plate_x <= 13.3 & plate_z >= ((7/9)*sz_bot) & plate_z <= ((23/21)*sz_top)) |
                                (plate_x >= -13.3 & plate_x <= -6.7 & plate_z >= ((7/9)*sz_bot) & plate_z <= ((23/21)*sz_top)) |
                                (plate_x >= -6.7 & plate_x <= 6.7 & ((plate_z >= ((7/9)*sz_bot) & plate_z <= ((11/9)*sz_bot)) | 
                                                                       (plate_z >= ((19/21)*sz_top) & plate_z <= ((23/21)*sz_top)))) ~ "shadow",
                              (plate_x >= 13.3 & plate_x <= 20 & plate_z >= ((1/3)*sz_bot) & plate_z <= ((9/7)*sz_top)) |
                                (plate_x >= -20 & plate_x <= -13.3 & plate_z >= ((1/3)*sz_bot) & plate_z <= ((9/7)*sz_top)) |
                                (plate_x >= -13.3 & plate_x <= 13.3 & ((plate_z >= ((1/3)*sz_bot) & plate_z <= ((7/9)*sz_bot)) | 
                                                                         (plate_z >= ((23/21)*sz_top) & plate_z <= ((9/7)*sz_top)))) ~ "chase",
                              plate_x > 20 | plate_x < -20 | plate_z > ((9/7)*sz_top) | plate_z < ((1/3)*sz_bot) ~ "waste" ),
                            
                            
                            # Mutate a 'swing' and 'take' column, which will be necessary to evaluate swing decisions
                            
                            swing = ifelse(description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked", "foul_bunt", "missed_bunt", "bunt_foul"),1,0),
                            take = ifelse(description %in% c("ball","called_strike", "blocked_ball", "pitchout", "hit_by_pitch"),1,0)) %>% 
      
      
      # Group by previously defined categories so that the player's performance in each part of the zone can be individually evaluated
      
       group_by(category) %>%
      
      
      # This mutation says that if 'take' is FALSE, then print the value in the 'delta_run_exp' column in a new column called 'swing_runs'
      # If 'take' is TRUE, then print the value in the 'delta_run_exp' column in a new column called 'take_runs'
      # This will allow you to calculate the change in run expectancy for a batter's swings vs. their takes by summing the totals in each column
      
      mutate(swing_runs = ifelse(take == FALSE, delta_run_exp, 0),
             take_runs = ifelse(take == TRUE, delta_run_exp, 0)) %>%
       summarise(count = n(),
                       swing = sum(swing),
                       take = sum(take),
                       value = sum(delta_run_exp, na.rm = TRUE),
                       s_value = sum(swing_runs, na.rm = TRUE),
                       t_value = sum(take_runs, na.rm = TRUE)) %>%
       ungroup() %>%
       mutate(
        take_pct = 100*round(take/count,3),
        swing_pct = 100*round(swing/count,3),
        usage = 100* round(count/sum(count),3),
        run_value = round(value,1),
        swing_value = round(s_value,1),
        take_value = round(t_value,1)) %>%
       select(category, count, usage, swing_pct, take_pct, run_value, swing_value, take_value) %>% 
      
      
      # arrange() will help visually display the zones in order from the 'heart' to 'waste'
      
      arrange(factor(category, levels = c("heart", "shadow", "chase", "waste")))
    
    table
  }
  
# CREATE LEAGUE TABLE 
  # Create the get_lg_table() function, which does the same as get_loc_table(), but finds the league average rather than being player-specific
  
  get_lg_table <- function(df){
    table <- df %>% mutate(plate_x = plate_x *12, 
                           plate_z = plate_z*12,
                           sz_top = sz_top*12,
                           sz_bot = sz_bot*12,
                           category = case_when(
                             plate_x <= 6.7 & plate_x >= -6.7 & plate_z >= ((11/9)*sz_bot) & plate_z <= ((19/21)*sz_top) ~ "heart",
                             (plate_x >= 6.7 & plate_x <= 13.3 & plate_z >= ((7/9)*sz_bot) & plate_z <= ((23/21)*sz_top)) |
                               (plate_x >= -13.3 & plate_x <= -6.7 & plate_z >= ((7/9)*sz_bot) & plate_z <= ((23/21)*sz_top)) |
                               (plate_x >= -6.7 & plate_x <= 6.7 & ((plate_z >= ((7/9)*sz_bot) & plate_z <= ((11/9)*sz_bot)) | 
                                                                      (plate_z >= ((19/21)*sz_top) & plate_z <= ((23/21)*sz_top)))) ~ "shadow",
                             (plate_x >= 13.3 & plate_x <= 20 & plate_z >= ((1/3)*sz_bot) & plate_z <= ((9/7)*sz_top)) |
                               (plate_x >= -20 & plate_x <= -13.3 & plate_z >= ((1/3)*sz_bot) & plate_z <= ((9/7)*sz_top)) |
                               (plate_x >= -13.3 & plate_x <= 13.3 & ((plate_z >= ((1/3)*sz_bot) & plate_z <= ((7/9)*sz_bot)) | 
                                                                        (plate_z >= ((23/21)*sz_top) & plate_z <= ((9/7)*sz_top)))) ~ "chase",
                             plate_x > 20 | plate_x < -20 | plate_z > ((9/7)*sz_top) | plate_z < ((1/3)*sz_bot) ~ "waste" ),
                           swing = ifelse(description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked", "foul_bunt", "missed_bunt", "bunt_foul_"),1,0),
                           take = ifelse(description %in% c("ball","called_strike", "blocked_ball", "pitchout", "hit_by_pitch"),1,0)) %>% 
       group_by(category) %>%
       mutate(swing_runs = ifelse(take == FALSE, delta_run_exp, 0),
                    take_runs = ifelse(take == TRUE, delta_run_exp, 0)) %>%
       summarise(count = n(),
                       swing = sum(swing),
                       take = sum(take),
                       value = sum(delta_run_exp, na.rm = TRUE),
                       s_value = sum(swing_runs, na.rm = TRUE),
                       t_value = sum(take_runs, na.rm = TRUE)) %>%
       ungroup() %>%
       mutate(take_pct = 100*round(take/count,3),
                    swing_pct = 100*round(swing/count,3),
                    usage = 100* round(count/sum(count),3),
                    run_value = round(value,1),
                    swing_value = round(s_value,1),
                    take_value = round(t_value,1)) %>%
       select(category, count, usage, swing_pct, take_pct, run_value, swing_value, take_value) %>% 
      arrange(factor(category, levels = c("heart", "shadow", "chase", "waste")))
    
    table
    
  }
  
# CREATE THE 4 ZONE DATA VISUALIZATION ('heart', 'shadow', 'chase', 'waste')
  # Create the 'get_rv_plot()' function that will create the 4-Zone Run Value plot visualization
  
  get_rv_plot <- function(df, player_name) {
    
    
    # Create the data frame that will contain the 4 zones
    
    rect_data <- data.frame(x = 1, y = 1)
    
    
    # Create the 'heart' zone shape
    
    rect1 <- ggplot(rect_data) +
      geom_rect(aes(xmin = x - 2, xmax = x + 2, ymin = y - 2, ymax = y + 2), 
                fill = "#E1AFDD", color = NA) + 
      theme_void() +
      
      
      # Use the get_loc_table() function to get the data for the 'heart' of the zone
      
      geom_text(data = filter(get_loc_table(df = df, player = player_name), category == "heart"),
                aes(x = 1, y = 2.25, label = paste(run_value, "runs")),
                color = "black", size = 8)
    
    
    # Create the 'shadow' zone shape
    
    rect2 <- ggplot(rect_data) +
      geom_rect(aes(xmin = x - 2, xmax = x + 2, ymin = y - 2, ymax = y + 2),
                fill = "#E9A4A7", color = NA) + 
      
      
      # This second geom_rect() arguments in the 'shadow' zone displays the strike zone as a dashed line
      
      geom_rect(aes(xmin = x - 1.65, xmax = x + 1.65, ymin = y - 1.65, ymax = y + 1.475),
                fill = NA, color = "black", linetype = "dashed")+ 
      theme_void() +
      
      
      # Use the get_loc_table() function to get the data for the 'shadow' of the zone
      
      geom_text(data = filter(get_loc_table(df = df, player = player_name), category == "shadow"),
                aes(x = 1, y = 2.75, label = paste(run_value, "runs")),
                color = "black", size = 8)
    
    
    # Repeat for 'chase' and 'waste' parts of the zone
    
    rect3 <- ggplot(rect_data) +
      geom_rect(aes(xmin = x - 2, xmax = x + 2, ymin = y - 2, ymax = y + 2),
                fill = "#ffff7f", color = NA) + theme_void() + 
      geom_text(data = filter(get_loc_table(df = df, player = player_name), category == "chase"),
                aes(x = 1, y = 2.8, label = paste(run_value, "runs")),
                color = "black", size = 8)
    
    
    rect4 <- ggplot(rect_data) +
      geom_rect(aes(xmin = x - 2, xmax = x + 2, ymin = y - 2, ymax = y + 2),
                fill = "#E0DFDA", color = NA) +
      theme_void() +
      geom_text(data = filter(get_loc_table(df = df, player = player_name), category == "waste"),
                aes(x = 1, y = 2.8, label = paste(run_value, "runs")),
                color = "black", size = 8)+
      
      
      # The title for the plot can go above the 'waste' zone since it is the outermost zone
      
      labs(title = paste("Run Value by Zone:", player_name)) +
      theme(plot.margin = margin(t = 1, unit = "mm"))
    
    
    # Use cowplot::ggdraw() to visually combine the plots 
    # Use the 'width =' and 'height =' arguments to size the plots properly
    
    combined_plot <- ggdraw() +
      draw_plot(rect4, x = 0.2, y = 0.2, width = .675, height = .82) +
      draw_plot(rect3, x = 0.275, y = 0.275, width = 0.525, height = 0.63) +
      draw_plot(rect2, x = .34, y = .34, width = 0.4, height = 0.505) +
      draw_plot(rect1, x = .425, y = .425, width = .225, height = 0.33)
    
    return(combined_plot)
  }


# The other 2 data visualizations displayed in this fourth tab are built in the RShiny server
 # See 'Shiny_UIServer.R' lines 759 - 820 & 823 - 870
 # Or see 'FullMLBBattingApp.R' lines 2870 - 2931 & 2934 - 2981
