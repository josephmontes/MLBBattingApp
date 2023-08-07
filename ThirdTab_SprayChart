# Use 'alldat' to isolate the spray chart data 
  
  spray_dat <- alldat %>% select(pitch_name, pitch_type, player_name, des, hc_x, hc_y, p_throws, game_pk, at_bat_number,
                                 events, bb_type, launch_speed, launch_angle, game_year, zone, description, strikes, balls)
  
# CREATE THE SPRAY CHART
  # Create the color palettes for the spray chart 'hit_type' options
  
  hit_palette <- c('Single' = "blue",
                   'Double' = "yellow",
                   'Triple' = "green",
                   'Homerun' = "red")
  
  bb_palette <- c('Groundball' = "#506cf6",
                  'Linedrive' = "#f1f650",
                  'Popup' = "#73ff67",
                  'Flyball' = "#ff6f55")
  
  
  # Create the main spray chart function, get_spraychart(), by using baseballr::ggspraychart()
  # 'df' argument will come from 'spray_dat' once it is filtered through the get_years() function prompted by the user in the app
  # The rest of the arguments in this function are inputs selected by the user in the app
  
  get_spraychart <- function(df, Player, pitch_type, hit_type){
    
    
    # Filter the data by pitch type
    
    if(pitch_type == "Breaking Ball") {
      pitch_df <- df %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                                   "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      pitch_df <- df %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      pitch_df <- df %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      pitch_df <- df
      
    }
    
    
    # Create a new data frame that labels all the batted ball and hit types
    
    ndf <- pitch_df %>% 
      filter(player_name == Player) %>%  
      mutate(bb_type = str_replace(bb_type, "ground_ball", "Groundball"),
             bb_type = str_replace(bb_type, "line_drive", "Linedrive"),
             bb_type = str_replace(bb_type, "fly_ball", "Flyball"),
             bb_type = str_replace(bb_type, "popup", "Popup"),
             result = case_when(
               grepl("double", des, ignore.case = TRUE) ~ "Double",
               grepl("singles", des, ignore.case = TRUE) ~ "Single",
               grepl("triples", des, ignore.case=TRUE) ~ "Triple",
               grepl("homers", des, ignore.case = TRUE) ~ "Homerun",
               TRUE ~ des), 
             
             
             # Mutate the y hit coordinate so ggspraychart() can use it to create the spray chart
             
             hc_y = hc_y*-1) %>%
      
      
      # Isolate the columns needed to create the spray chart
      
      select(player_name, hc_x, hc_y, result, bb_type, pitch_name, p_throws) 
    
    
    # Use ifelse() function to filter 'ndf' by the 'hit_type' argument options in this function
    
    if(hit_type == "Single") {
      plot_df <- filter(ndf, result == "Single")
    } else if (hit_type == "Double") {
      plot_df <- filter(ndf, result == "Double")
    } else if (hit_type == "Triple") {
      plot_df <- filter(ndf, result == "Triple")
    } else if (hit_type == "Homerun") {
      plot_df <- filter(ndf, result == "Homerun")
    } else if (hit_type == "All hits") {
      plot_df <- filter(ndf, result %in% c("Single", "Double", "Triple", "Homerun"))
    } else if(hit_type == "Groundball") {
      plot_df <- filter(ndf, bb_type == "Groundball")
    } else if (hit_type == "Linedrive") {
      plot_df <- filter(ndf, bb_type == "Linedrive")
    } else if (hit_type == "Popup") {
      plot_df <- filter(ndf, bb_type == "Popup")
    } else if (hit_type == "Flyball") {
      plot_df <- filter(ndf, bb_type == "Flyball")
    } else if (hit_type == "All batted balls") {
      plot_df <- ndf
    }
    
    
    # Use an ifelse() function to create the plots depending on the 'hit_type' input
    # An ifelse() function is used because the 'fill_value' argument inside ggspraychart() is different for hits vs. batted balls
    
    if(hit_type %in% c("Single", "Double", "Triple", "Homerun", "All hits")){
      p <- ggspraychart(plot_df, x_value = "hc_x", y_value = "hc_y", fill_value = "result", fill_palette= hit_palette, point_size = 3)+
        facet_wrap(~p_throws)+ 
        ggtitle(paste("Spray Chart:", Player))
    } else if (hit_type %in% c("Groundball", "Linedrive", "Flyball", "Popup", "All batted balls")){
      p <- ggspraychart(plot_df, x_value = "hc_x", y_value = "hc_y", fill_value = "bb_type", fill_palette= bb_palette, point_size = 3)+
        facet_wrap(~p_throws)+ 
        ggtitle(paste("Spray Chart:", Player))
    }
    
    
    return(p)
  }
  
 # CREATE TABLE 1
  # Create the get_player_data1() function that will get player data such as BA, OBP, SLG, K%, GB%, Barrel% and more (Third tab)
  
  get_player_data1 <- function(df, player){
    
    
    # Filter data by the inputted player
    
    bdf <- filter(df, player_name == player)
    
    
    # Use distinct() to filter the data down to the resultant pitch of each at bat
    
    bbdf <- distinct(bdf, game_pk, at_bat_number, .keep_all = TRUE)
    
    
    # Create the table that contains AB rated and batted ball rated statistics
    
    table <- bbdf %>% 
       group_by(player_name)  %>% 
       summarise(PAs = nrow(bbdf),
                       ABs = PAs - sum(events %in% c("walk", "field_error", "hit_by_pitch", "catcher_interf", "fielders_choice")),
                       hits = sum(events %in% c("single", "double", "triple", "home_run")),
                       on_base = sum(events %in% c("single", "double", "triple", "home_run", "walk", "hit_by_pitch")),
                       BBs = sum(grepl("walk",des, ignore.case = TRUE)),
                       Ks=sum(events %in% "strikeout"),
                       GB=sum(bb_type == "ground_ball"),
                       FB= sum(bb_type == "fly_ball"),
                       LD = sum(bb_type == "line_drive"),
                       PU = sum(bb_type == "popup"),
                       TB = sum(events == "single")+ (2*sum(events == "double"))+(3*sum(events == "triple"))+(4*sum(events == "home_run")),
                       in_play = sum(description == "hit_into_play"),
                       HH = sum(launch_speed >= 95, na.rm = T),
                       MH = sum(launch_speed >= 80 & launch_speed < 95, na.rm = T),
                       SH = sum(launch_speed < 80, na.rm = T),
                       SwSpot = sum(launch_angle > 8 & launch_angle < 32, na.rm = T),
                       barrels = sum((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                                     & launch_angle > 4 & launch_angle < 50, na.rm = T)
                       ) %>%
       ungroup() %>%
       mutate(Batting_Average = round(hits/ABs,3),
                    OBP = round(on_base/PAs,3),
                    SLG = round(TB/(ABs),3),
                    K_Rate = 100*round(Ks/ABs,3),
                    BB_Rate= 100*round(BBs/PAs,3),
                    GB_pct= 100*round(GB/in_play,3),
                    LD_pct = 100*round(LD/in_play,3),
                    FB_pct = 100*round(FB/in_play,3),
                    PU_pct = 100*round(PU/in_play,3),
                    HH_pct = 100*round(HH/in_play,3),
                    MH_pct = 100*round(MH/in_play,3),
                    SH_pct = 100*round(SH/in_play,3),
                    SwSpot_pct = 100*round(SwSpot/in_play,3),
                    b_pct = 100*round(barrels/in_play,3)
              ) %>%
       select("Player" = player_name, "BA" = Batting_Average, "OBP" = OBP, "SLG" = SLG, "K %" = K_Rate, "BB %" = BB_Rate, "GB %" = GB_pct, 
            "LD %" = LD_pct, "FB %" = FB_pct, "PU %" = PU_pct, "Hard %" = HH_pct, "Med %" = MH_pct, "Soft %" = SH_pct, "SwSpot %" = SwSpot_pct, 
            "Barrel %" = b_pct)
    table
  }
  
  
  # Create a similar function called get_lg_data1() that will get the league average for each of the previously attained stats
  
  get_lg_data1 <- function(df){
    bbdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    table <- bbdf %>% 
       summarise(PAs = nrow(bbdf),
                       ABs = PAs - sum(events %in% c("walk", "field_error", "hit_by_pitch", "catcher_interf", "fielders_choice")),
                       hits = sum(events %in% c("single", "double", "triple", "home_run")),
                       on_base = sum(events %in% c("single", "double", "triple", "home_run", "walk", "hit_by_pitch")),
                       BBs = sum(grepl("walk",des, ignore.case = TRUE)),
                       Ks=sum(events %in% "strikeout"),
                       GB=sum(bb_type == "ground_ball"),
                       FB= sum(bb_type == "fly_ball"),
                       LD = sum(bb_type == "line_drive"),
                       PU = sum(bb_type == "popup"),
                       TB = sum(events == "single")+ (2*sum(events == "double"))+(3*sum(events == "triple"))+(4*sum(events == "home_run")),
                       in_play = sum(description == "hit_into_play"),
                       HH = sum(launch_speed >= 95, na.rm = T),
                       MH = sum(launch_speed >= 80 & launch_speed < 95, na.rm = T),
                       SH = sum(launch_speed < 80, na.rm = T),
                       SwSpot = sum(launch_angle > 8 & launch_angle < 32, na.rm = T),
                       barrels = sum((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                                     & launch_angle > 4 & launch_angle < 50, na.rm = T)) %>%
       ungroup() %>%
       mutate(Batting_Average = round(hits/ABs,3),
                    OBP = round(on_base/PAs,3),
                    SLG = round(TB/(ABs),3),
                    K_Rate = 100*round(Ks/ABs,3),
                    BB_Rate= 100*round(BBs/PAs,3),
                    GB_pct= 100*round(GB/in_play,3),
                    LD_pct = 100*round(LD/in_play,3),
                    FB_pct = 100*round(FB/in_play,3),
                    PU_pct = 100*round(PU/in_play,3),
                    HH_pct = 100*round(HH/in_play,3),
                    MH_pct = 100*round(MH/in_play,3),
                    SH_pct = 100*round(SH/in_play,3),
                    SwSpot_pct = 100*round(SwSpot/in_play,3),
                    b_pct = 100*round(barrels/in_play,3)
              ) %>%
       select("BA" = Batting_Average, "OBP" = OBP, "SLG" = SLG, "K %" = K_Rate, "BB %" = BB_Rate, "GB %" = GB_pct, "LD %" = LD_pct, 
                    "FB %" = FB_pct, "PU %" = PU_pct, "Hard %" = HH_pct, "Med %" = MH_pct, "Soft %" = SH_pct, "SwSpot %" = SwSpot_pct, "Barrel %" = b_pct) %>% 
       mutate("Player" = "League Average") 
    
    table <- table %>%
       select("Player", everything())
    
    print(table)
  }
  
  
  # Create the get_all_batter_data1() function that will combine the get_player_data() and get_lg_data() in one table 
  
  get_all_batter_data1 <- function(df, player){
    table1 <- get_player_data1(df, player)
    table2 <- get_lg_data1(df)
    return(rbind(table1, table2))
  }

# CREATE TABLE 2
  # Create the get_player_data2() function that will gather a set of statistics regarding plate discipline (Third tab)

    get_player_data2 <- function(df, player){
      df <- filter(df, player_name == player)
      table <- df %>% 
         group_by(player_name)  %>% 
         summarise(out_of_zone = sum(zone %in% c(11:14)),
                         in_the_zone = sum(zone %in% c(1:9)),
                         tp = nrow(df),
                         swings = sum(description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked"), na.rm = T),
                         z_swing = sum(zone %in% c(1:9) & description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         o_swing = sum(zone %in% c(11:14) & description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         whiff = sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         contact = sum(description %in% c("hit_into_play", "foul")),
                         o_con = sum(zone %in% c(11:14) & description %in% c("hit_into_play", "foul")),
                         z_con = sum(zone %in% c(1:9) & description %in% c("hit_into_play", "foul")),
                         zone = sum(zone %in% c(1:9)),
                         first_pitch = sum(balls==0 & strikes==0),
                         first_pitch_swing = sum(balls==0 & strikes==0 & description%in% c("foul","hit_into_play","swinging_strike", 
                                                                                           "foul_tip", "swinging_strike_blocked"), na.rm = T)
                   ) %>%
         ungroup() %>%
         mutate(o_swings = 100*round(o_swing/swings,3),
                      z_swings = 100*round(z_swing/swings,3),
                      o_cons = 100*round(o_con/o_swing,3),
                      z_cons = 100*round(z_con/z_swing,3),
                      whiff_pct = 100*round(whiff/swings,3),
                      contact_pct = 100*round(contact/swings,3),
                      zone_pct = 100*round(zone/tp,3),
                      fp_swing = 100*round(first_pitch_swing/first_pitch,3),
                      swing_pct = 100*round(swings/tp,3)
                ) %>%
         select("Player" = player_name, "Zone %" = zone_pct, "O-Swing %" = o_swings, "Z-Swing %" = z_swings, "O-Contact %" = o_cons, 
                      "Z-Contact %" = z_cons, "Whiff %" = whiff_pct, "Contact %" = contact_pct, "Swing %" = swing_pct, "1st Pitch Swing %" = fp_swing)
      table
    }
    
    
  # Create the get_lg_data2() function that will get the league average for the same set of statistics
    
    get_lg_data2 <- function(df){
      table <- df %>% 
         summarise(out_of_zone = sum(zone %in% c(11:14)),
                         in_the_zone = sum(zone %in% c(1:9)),
                         tp = nrow(df),
                         swings = sum(description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked"), na.rm = T),
                         z_swing = sum(zone %in% c(1:9) & description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         o_swing = sum(zone %in% c(11:14) & description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         whiff = sum(description %in% c("swinging_strike", "foul_tip", "swinging_strike_blocked")),
                         contact = sum(description %in% c("hit_into_play", "foul")),
                         o_con = sum(zone %in% c(11:14) & description %in% c("hit_into_play", "foul")),
                         z_con = sum(zone %in% c(1:9) & description %in% c("hit_into_play", "foul")),
                         zone = sum(zone %in% c(1:9)),
                         first_pitch = sum(balls==0 & strikes==0),
                         first_pitch_swing = sum(balls==0 & strikes==0 & description%in% c("foul","hit_into_play","swinging_strike", 
                                                                                           "foul_tip", "swinging_strike_blocked"), na.rm = T)
                   ) %>%
         ungroup() %>%
         mutate(o_swings = 100*round(o_swing/swings,3),
                      z_swings = 100*round(z_swing/swings,3),
                      o_cons = 100*round(o_con/o_swing,3),
                      z_cons = 100*round(z_con/z_swing,3),
                      whiff_pct = 100*round(whiff/swings,3),
                      contact_pct = 100*round(contact/swings,3),
                      zone_pct = 100*round(zone/tp,3),
                      fp_swing = 100*round(first_pitch_swing/first_pitch,3),
                      swing_pct = 100*round(swings/tp,3)) %>%
         select("Zone %" = zone_pct, "O-Swing %" = o_swings, "Z-Swing %" = z_swings, "O-Contact %" = o_cons, 
                      "Z-Contact %" = z_cons, "Whiff %" = whiff_pct, "Contact %" = contact_pct, "Swing %" = swing_pct, 
                "1st Pitch Swing %" = fp_swing) %>% 
         mutate("Player" = "League Average")
      
      table <- table %>%
         select("Player", everything())
      
      table
    }
    
    
  # Create the get_all_batter_data2() function that will display the two tables as one table
  # This table is also used in the fourth tab, but in that tab the table updates with the inputs
    
    get_all_batter_data2 <- function(df, player){
      table1 <- get_player_data2(df, player)
      table2 <- get_lg_data2(df)
      return(rbind(table1, table2))
    }
    
 # CREATE THE LAUNCH MAP PLOTLY
  # Color palette for the Exit Velo/Launch Angle map graphic (Third tab continued)
    
  launch_palette <- c(
    'Single' = "#506cf6",
    'Double' = "#f1f650",
    'Triple' = "#73ff67",
    'Homerun' = "#ff6f55",
    'Groundball' = "blue",
    'Linedrive' = "yellow",
    'Popup' = "green",
    'Flyball' = "red")
  
  
  # Create get_launch_map() function that will display a plotly map showing the Launch Angle & Exit Velos of a player's hits
  # This graphic is 2 layers: first layer is a league average texture map showing what LA & EV combos result in hits
  #                         : second layer is a geom_point() graphic of the player's hits over the league average texture map
  
  get_launch_map <- function(df, name, pitch_type, hit_type) {
    
    
    # Create the 'result' column so the launch angle and exit velos can be identified for batted balls that fall for hits
    # 'hits' is used to create the layered background in the plotly, displaying the range of exit velo and launch angle combos that are most frequently hits 
    
    hits <- df %>%  mutate(result = case_when(
      grepl("double", des, ignore.case = TRUE) ~ "Double",
      grepl("singles", des, ignore.case = TRUE) ~ "Single",
      grepl("triples", des, ignore.case=TRUE) ~ "Triple",
      grepl("homers", des, ignore.case = TRUE) ~ "Homerun",
      TRUE ~ des),
      launch_angle = round(launch_angle,3),
      launch_speed = round(launch_speed,3)) %>%  
      filter(result %in% c("Single", "Double", "Triple", "Homerun"), description == "hit_into_play") %>% 
      select(launch_speed, launch_angle, result, description)
    
    
    # Filter the data by pitch type (the data is the Third tab's 'spray_dat' filtered by the web app's inputs)
    
    if(pitch_type == "Breaking Ball") {
      pitch_df <- df %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                                   "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      pitch_df <- df %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      pitch_df <- df %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      pitch_df <- df
      
    }
    
    
    # Filter the data by hit type
    
    if(hit_type == "Single") {
      plot_df <- pitch_df %>% mutate(result = case_when(
        grepl("singles", des, ignore.case = TRUE) ~ "Single",
        TRUE ~ des)) %>% filter(result == "Single", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Double") {
      plot_df <- pitch_df %>% mutate(result = case_when(
        grepl("double", des, ignore.case = TRUE) ~ "Double",
        TRUE ~ des)) %>% filter(result == "Double", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Triple") {
      plot_df <- pitch_df %>% mutate(result = case_when(
        grepl("triples", des, ignore.case = TRUE) ~ "Triple",
        TRUE ~ des)) %>% filter(result == "Triple", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Homerun") {
      plot_df <- pitch_df %>% mutate(result = case_when(
        grepl("homers", des, ignore.case = TRUE) ~ "Homerun",
        TRUE ~ des)) %>% filter(result == "Homerun", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "All hits") {
      plot_df <- pitch_df %>%  mutate(result = case_when(
        grepl("double", des, ignore.case = TRUE) ~ "Double",
        grepl("singles", des, ignore.case = TRUE) ~ "Single",
        grepl("triples", des, ignore.case = TRUE) ~ "Triple",
        grepl("homers", des, ignore.case = TRUE) ~ "Homerun",
        TRUE ~ des)) %>% filter(result %in% c("Single", "Double", "Triple", "Homerun"), description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if(hit_type == "Groundball") {
      plot_df <- pitch_df %>% mutate(result = str_replace(bb_type, "ground_ball", "Groundball")) %>% 
        filter(result == "Groundball", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Linedrive") {
      plot_df <- pitch_df %>% mutate(result = str_replace(bb_type, "line_drive", "Linedrive")) %>% 
        filter(result == "Linedrive", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Popup") {
      plot_df <- pitch_df %>% mutate(result = str_replace(bb_type, "popup", "Popup")) %>% 
        filter(result == "Popup", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "Flyball") {
      plot_df <- pitch_df %>% mutate(result = str_replace(bb_type, "fly_ball", "Flyball")) %>% 
        filter(result == "Flyball", description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    } 
      else if (hit_type == "All batted balls") {
      plot_df <- pitch_df %>% mutate(result = ifelse(bb_type == "ground_ball", "Groundball",
                                      ifelse(bb_type == "line_drive", "Linedrive",
                                      ifelse(bb_type == "fly_ball", "Flyball",
                                      ifelse(bb_type == "popup", "Popup", bb_type))))) %>%  
        filter(description == "hit_into_play") %>% 
        select(result, player_name, launch_speed, launch_angle)
    }
    
    
    # Filter 'plot_df' to include only the selected player
    
    player <- plot_df %>% filter(player_name == name) %>% select(launch_speed, launch_angle, player_name, result)
    
    
    # Use ggplot() to create the plot
    
    plot <- ggplot(hits, aes(x = launch_speed, y = launch_angle)) +
      
     # This is similar to the heat map creation from the first tab
      geom_density_2d(aes(fill = stat(level)), alpha = 0.85, color = "grey") +
      scale_fill_distiller(type = "qual", limits = c(0, 0.0005), oob = scales::squish) +
      coord_fixed() +
      xlim(c(15, 120)) +
      ylim(c(-80, 90)) +
      theme_classic() +
      
      # Format the plot
      theme(plot.background = element_rect(fill = "white", color = "black", size = 1),  
            panel.background = element_rect(fill = "white", color = "black", size = 1),
            panel.grid = element_line(color = "gray"),
            legend.position = "right",
            plot.margin = margin(2, 2, 2, 2, "cm")) +
      
      # Use geom_point() to layer the player's hits over the league average texture map
      geom_point(data = player, aes(color = result), size = 1) +
      
      # Use scale_color_manual() to format the geom_points
      scale_color_manual(values = launch_palette, breaks = c('Single', 'Double', 'Triple', 'Homerun', 'Groundball', 
                                                             'Linedrive', 'Flyball', 'Popup')) +
      xlab("Exit Velocity") +
      ylab("Launch Angle") +
      
      # Use geom_hline() to create 2 horizontal lines through the chart that mark the Sweet Spot
      geom_hline(yintercept = c(8, 32), color = "maroon", linetype = "solid", size = 0.25) +
      
      # Use geom_vline() to create 3 vertical lines that mark hard hit (98mph - 110mph) and the singles threshold (65 mph)
      geom_vline(xintercept = c(65, 98, 110), color = c("navy", "maroon", "maroon"), linetype = "solid", size = 0.25) +
      ggtitle(paste("Hit profile:", name)) +
      
      # Remove the key for the league texture map, which is understood as the 'fill'
      guides(fill = "none") +
    
      labs(color = "Hit Types")
    
    # Convert ggplot to Plotly 
    
      ggplotly(plot, tooltip = c("y", "x"))
    
  }
