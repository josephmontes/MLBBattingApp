#Load packages

library(shiny)
library(tidyverse)
library(baseballr)
library(cowplot)
library(plotly)

# Load the Statcast data from 2019-2023

s19 <- read.csv("season2019.csv")
s20 <- read.csv("season2020.csv")
s21 <- read.csv("season2021.csv")
s22 <- read.csv("season2022.csv")
rs23 <- read.csv("season231h.csv")
  
alldat <- rbind(s19, s20, s21, s22, rs23)


# Create the get_years() function that is used to filter seasons (First, Second & Third tabs)

  get_years <- function(df, years){
    filtered_data <- df %>% filter(game_year %in% years)
    return(filtered_data)
    
  }


# Use the raw Statcast data ('alldat') to mutate, clean, and isolate the data needed for heat maps (First Tab)

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
  
  
  # Create the find_plots() function that is used to filter the data for each heat map type (First Tab)
  
  find_plots <- function(data, column, value) {
    subset(data, data[[column]] == value)
  }


# Use 'alldat' to isolate the data needed for the 13 zones data visualization (Second Tab)

  zonedat <- alldat %>% select(p_throws, pitch_name, pitch_type, description, launch_speed, launch_angle, delta_run_exp, zone, player_name, game_year)
  
  
  # Create the get_lg_table2() function that calculates the league average value for each zone and each 'type' filter in the 13 zone data viz
  # The values in the table will be used to set the midpoint value for the color scale in the plot
  
  get_lg_table2 <- function(df) {
    table <- df %>%
      mutate(
        whiff = ifelse(description %in% c("swinging_strike", "foul_tip"),1, 0),
        swing = ifelse(description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked", "foul_bunt", "missed_bunt", "bunt_foul_"), 1, 0),
        take = ifelse(description %in% c("ball", "called_strike", "blocked_ball", "pitchout", "hit_by_pitch"), 1, 0),
        hard_hit = ifelse(launch_speed >= 95, 1, 0),
        sw_spot = ifelse(launch_angle >= 8 & launch_angle <= 32, 1, 0),
        CsW = ifelse(description %in% c("swinging_strike", "called_strike"),1,0),
        barrel = ifelse((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                        & launch_angle > 4 & launch_angle < 50, 1, 0),
        zone_group = ifelse(zone %in% 1:9, "Zone 1-9", "Zone 11-14")
        ) %>%
      group_by(zone_group) %>%
      summarize(
        count = n(),
        whiff = sum(whiff),
        swing = sum(swing),
        take = sum(take),
        avg_ev = mean(launch_speed, na.rm = TRUE),
        avg_la = mean(launch_angle, na.rm = TRUE),
        run_value = sum(delta_run_exp, na.rm = TRUE),
        hard_hit = sum(hard_hit, na.rm = TRUE),
        sw_spot = sum(sw_spot, na.rm = TRUE),
        CsW = sum(CsW, na.rm =TRUE),
        barrel = sum(barrel, na.rm =TRUE)
      ) %>%
      mutate(
        whiff_pct = 100 * round(whiff / swing, 3),
        take_pct = 100 * round(take / count, 3),
        swing_pct = 100 * round(swing / count, 3),
        usage = 100 * round(count / sum(count), 3),
        avg_ev = round(avg_ev, 1),
        avg_la = round(avg_la, 1),
        run_value = round(run_value, 1),
        hard_hit = 100 * round(hard_hit / swing, 3),
        sw_spot = 100 * round(sw_spot / swing, 3),
        CsW = 100 * round(CsW / count, 3),
        barrel_rate = 100*round(barrel/swing,3)
      ) %>%
      select(zone_group, count, usage, swing_pct, take_pct, whiff_pct, avg_ev, avg_la, run_value, hard_hit, sw_spot, CsW, barrel_rate)
    
    table
  }
  
  
  # Create the get_zone_data() function that outputs the full 13 zone visualization (Second Tab)
  # 'df' argument will come from 'zonedat' once it is filtered through the get_years() function prompted by the user in the app
  # The rest of the arguments in this function will be inputs that are selected by the user in the app
  
  get_zone_data <- function(df, Player, type = "whiff", pitch_type, p_throws) {
    
    
    # Filter the data by pitcher handedness
    
    if(p_throws == "R"){
      pidf <- df %>%  filter(p_throws=="R")
    } else if (p_throws == "L"){
      pidf <- df %>%  filter(p_throws== "L")
    } else if (p_throws == "B"){
      pidf <- df
    }
    
    
    # Filter the data by pitch type
    
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
    
    
    # Create the midpoint values needed for the color scale argument in the plot (below average will be blue, above average will be red)
    
    lg_dat <- get_lg_table2(pitch_df)
    whiff_mid_zone <- lg_dat$whiff_pct[lg_dat$zone_group == "Zone 1-9" & lg_dat$whiff_pct != ""]
    whiff_mid_quad <- lg_dat$whiff_pct[lg_dat$zone_group == "Zone 11-14" & lg_dat$whiff_pct != ""]
    swing_mid_zone<- lg_dat$swing_pct[lg_dat$zone_group == "Zone 1-9" & lg_dat$swing_pct != ""]
    swing_mid_quad<- lg_dat$swing_pct[lg_dat$zone_group == "Zone 11-14" & lg_dat$swing_pct != ""]
    take_mid_zone<- lg_dat$take_pct[lg_dat$zone_group == "Zone 1-9" & lg_dat$take_pct != ""]
    take_mid_quad<- lg_dat$take_pct[lg_dat$zone_group == "Zone 11-14" & lg_dat$take_pct != ""]
    HH_mid_zone<- lg_dat$hard_hit[lg_dat$zone_group == "Zone 1-9" & lg_dat$hard_hit != ""]
    HH_mid_quad<- lg_dat$hard_hit[lg_dat$zone_group == "Zone 11-14" & lg_dat$hard_hit != ""]
    CsW_mid_zone<- lg_dat$CsW[lg_dat$zone_group == "Zone 1-9" & lg_dat$CsW != ""]
    CsW_mid_quad<- lg_dat$CsW[lg_dat$zone_group == "Zone 11-14" & lg_dat$CsW != ""]
    SS_mid_zone<- lg_dat$sw_spot[lg_dat$zone_group == "Zone 1-9" & lg_dat$sw_spot != ""]
    SS_mid_quad<- lg_dat$sw_spot[lg_dat$zone_group == "Zone 11-14" & lg_dat$sw_spot != ""]
    EV_mid_zone<- lg_dat$avg_ev[lg_dat$zone_group == "Zone 1-9" & lg_dat$avg_ev != ""]
    EV_mid_quad<- lg_dat$avg_ev[lg_dat$zone_group == "Zone 11-14" & lg_dat$avg_ev != ""]
    LA_mid_zone<- lg_dat$avg_la[lg_dat$zone_group == "Zone 1-9" & lg_dat$avg_la != ""]
    LA_mid_quad<- lg_dat$avg_la[lg_dat$zone_group == "Zone 11-14" & lg_dat$avg_la != ""]
    B_mid_zone<- lg_dat$barrel_rate[lg_dat$zone_group == "Zone 1-9" & lg_dat$barrel_rate != ""]
    B_mid_quad<- lg_dat$barrel_rate[lg_dat$zone_group == "Zone 11-14" & lg_dat$barrel_rate != ""]
    
    
    # Create each statistic that is available in the 'type' argument of this function (Whiff %, Swing %, etc.)
    
    df <- pitch_df %>% mutate(whiff = ifelse(description == "swinging_strike",1,0),
                              swing = ifelse(description %in% c("foul","hit_into_play","swinging_strike", "foul_tip", "swinging_strike_blocked", "foul_bunt", "missed_bunt", "bunt_foul_"),1,0),
                              take = ifelse(description %in% c("ball","called_strike", "blocked_ball", "pitchout", "hit_by_pitch"),1,0),
                              hard_hit = ifelse(launch_speed >= 95,1,0),
                              sw_spot = ifelse(launch_angle >= 8 & launch_angle <= 32,1,0),
                              CsW = ifelse(description %in% c("swinging_strike", "called_strike"),1,0),
                              barrel = ifelse((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                                              & launch_angle > 4 & launch_angle < 50, 1, 0)) %>%
              filter(player_name == Player) %>%
              group_by(player_name,zone) %>%
              summarise(pitch_names = n(),
                       whiff = sum(whiff),
                       swing = sum(swing),
                       take = sum(take),
                       avg_ev = mean(launch_speed,na.rm=T),
                       avg_la = mean(launch_angle,na.rm=T),
                       run_value = sum(delta_run_exp, na.rm = T),
                       hard_hit = sum(hard_hit, na.rm=T),
                       sw_spot = sum(sw_spot, na.rm = T),
                       CsW = sum(CsW, na.rm = T),
                       barrel = sum(barrel, na.rm = T)) %>%
              ungroup() %>%
              mutate(whiff_pct = 100*round(whiff/swing,3),
                    take_pct = 100*round(take/pitch_names,3),
                    swing_pct = 100*round(swing/pitch_names,3),
                    avg_ev = round(avg_ev,1),
                    avg_la = round(avg_la,1),
                    run_value = round(run_value,1),
                    hard_hit = 100*round(hard_hit/swing,3),
                    sw_spot = 100*round(sw_spot/swing,3),
                    CsW = 100*round(CsW/pitch_names,3),
                    barrel_rate = 100*round(barrel/swing,3))
    
    
    # Use ifelse() function to create the plot for each 'type' argument option
    
    if (type == "Whiff %") {
      
      
      # For each of the 13 zones, store the selected 'type' (in this case, Whiff %) for that zone in a variable
      
      z1 <- df %>% dplyr::filter(zone == 1) %>%  dplyr::pull(whiff_pct)
      z2 <- df %>% filter(zone == 2) %>%  pull(whiff_pct)
      z3 <- df %>% filter(zone == 3) %>%  pull(whiff_pct)
      z4 <- df %>% filter(zone == 4) %>%  pull(whiff_pct)
      z5 <- df %>% filter(zone == 5) %>%  pull(whiff_pct)
      z6 <- df %>% filter(zone == 6) %>%  pull(whiff_pct)
      z7 <- df %>% filter(zone == 7) %>%  pull(whiff_pct)
      z8 <- df %>% filter(zone == 8) %>%  pull(whiff_pct)
      z9 <- df %>% filter(zone == 9) %>%  pull(whiff_pct)
      z11 <- df %>% filter(zone == 11) %>%  pull(whiff_pct)
      z12 <- df %>% filter(zone == 12) %>%  pull(whiff_pct)
      z13 <- df %>% filter(zone == 13) %>%  pull(whiff_pct)
      z14 <- df %>% filter(zone == 14) %>%  pull(whiff_pct)
      
      
      # To create the 13 zone visualization, it is a quadrant plot (2x2) that is layered underneath a 9-zone plot (3x3 strike zone)
      # First, piece together the quadrant plot
      
      # Create 'text_labels' data frame that will be used in geom_text() portion of the quadrant plot
      # This is important for locating the values of each quadrant in a place where they can be seen in the visualization
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      
      # Create the 'quadrant_data' data frame that will be used in geom_rect() portion of the quadrant plot
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      
      # Put the quadrant plot together
      
      quadrant_plot <- ggplot() + 
        
        # 'quadrant_data' is used in geom_rect() to create the quadrants representing pitches outside of the strike zone 
        geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        
        # Create a consistent plot area for the visualization by setting the limits and using coord_fixed()
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+
        coord_fixed()+
        
        # 'text_labels' is used in geom_text to place the Whiff % value for each quadrant in a place that will be seen
        geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        
        # Use scale_fill_gradient2() to create the above average and below average colors and values
        scale_fill_gradient2(low = "blue", high ="red", midpoint = whiff_mid_quad)+
        
        # 'theme_void()' and 'theme()' are used to make sure only the quadrant rectangle graphic is returned without axes 
        theme_void()+
        theme(legend.position = "None") +
        
        # Use 'ggtitle()' to create a title that updates with the inputs
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      
      # Create the 3x3 zone data frame
      # There's no need to create a text_labels data frame for the zone plot because the values can be displayed in the middle of each zone
      
      zone_data <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                              xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                              ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                              ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                              value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      # Create the 3x3 zone plot
      
      zone_plot <- ggplot() + 
        
        # Use geom_rect() to create the 9 zone strike zone
        geom_rect(data= zone_data, aes(xmin= xmin, xmax= xmax, ymin= ymin, ymax= ymax, fill= value), color= "black") +
        
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+
        coord_fixed()+
        
        # Use geom_text() to place the value labels in the center of each zone
        geom_text(data= zone_data, aes(x= (xmin+xmax)/2, y= (ymin+ymax)/2, label= paste(value,"%")), color="black") +
        scale_fill_gradient2(low = "blue",  high = "red", midpoint = whiff_mid_zone)+
        
        theme_void()+
        theme(legend.position = "None") +
        
        # This is code for a strike zone that actually does not appear in the app, but appears in Rstudio viewer when running 'get_zone_data()'
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      # Use cowplot::ggdraw() to display the zone plot on top of, and in the middle of, the quadrant plot
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
      
      #Repeat these steps for each 'type' argument
      
    } else if (type == "Average Exit Velocity") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(avg_ev)
      z2 <- df %>%  filter(zone == 2) %>%  pull(avg_ev)
      z3 <- df %>%  filter(zone == 3) %>%  pull(avg_ev)
      z4 <- df %>%  filter(zone == 4) %>%  pull(avg_ev)
      z5 <- df %>%  filter(zone == 5) %>%  pull(avg_ev)
      z6 <- df %>%  filter(zone == 6) %>%  pull(avg_ev)
      z7 <- df %>%  filter(zone == 7) %>%  pull(avg_ev)
      z8 <- df %>%  filter(zone == 8) %>%  pull(avg_ev)
      z9 <- df %>%  filter(zone == 9) %>%  pull(avg_ev)
      z11 <- df %>%  filter(zone == 11) %>%  pull(avg_ev)
      z12 <- df %>%  filter(zone == 12) %>%  pull(avg_ev)
      z13 <- df %>%  filter(zone == 13) %>%  pull(avg_ev)
      z14 <- df %>%  filter(zone == 14) %>%  pull(avg_ev)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"mph")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = EV_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"mph")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = EV_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    else if (type == "Take %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(take_pct)
      z2 <- df %>%  filter(zone == 2) %>%  pull(take_pct)
      z3 <- df %>%  filter(zone == 3) %>%  pull(take_pct)
      z4 <- df %>%  filter(zone == 4) %>%  pull(take_pct)
      z5 <- df %>%  filter(zone == 5) %>%  pull(take_pct)
      z6 <- df %>%  filter(zone == 6) %>%  pull(take_pct)
      z7 <- df %>%  filter(zone == 7) %>%  pull(take_pct)
      z8 <- df %>%  filter(zone == 8) %>%  pull(take_pct)
      z9 <- df %>%  filter(zone == 9) %>%  pull(take_pct)
      z11 <- df %>%  filter(zone == 11) %>%  pull(take_pct)
      z12 <- df %>%  filter(zone == 12) %>%  pull(take_pct)
      z13 <- df %>%  filter(zone == 13) %>%  pull(take_pct)
      z14 <- df %>%  filter(zone == 14) %>%  pull(take_pct)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = take_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = take_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
    }
    else if (type == "Swing %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(swing_pct)
      z2 <- df %>%  filter(zone == 2) %>%  pull(swing_pct)
      z3 <- df %>%  filter(zone == 3) %>%  pull(swing_pct)
      z4 <- df %>%  filter(zone == 4) %>%  pull(swing_pct)
      z5 <- df %>%  filter(zone == 5) %>%  pull(swing_pct)
      z6 <- df %>%  filter(zone == 6) %>%  pull(swing_pct)
      z7 <- df %>%  filter(zone == 7) %>%  pull(swing_pct)
      z8 <- df %>%  filter(zone == 8) %>%  pull(swing_pct)
      z9 <- df %>%  filter(zone == 9) %>%  pull(swing_pct)
      z11 <- df %>%  filter(zone == 11) %>%  pull(swing_pct)
      z12 <- df %>%  filter(zone == 12) %>%  pull(swing_pct)
      z13 <- df %>%  filter(zone == 13) %>%  pull(swing_pct)
      z14 <- df %>%  filter(zone == 14) %>%  pull(swing_pct)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = swing_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = swing_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
    }
    
    else if (type == "Average Launch Angle" | type == "Mean la") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(avg_la)
      z2 <- df %>%  filter(zone == 2) %>%  pull(avg_la)
      z3 <- df %>%  filter(zone == 3) %>%  pull(avg_la)
      z4 <- df %>%  filter(zone == 4) %>%  pull(avg_la)
      z5 <- df %>%  filter(zone == 5) %>%  pull(avg_la)
      z6 <- df %>%  filter(zone == 6) %>%  pull(avg_la)
      z7 <- df %>%  filter(zone == 7) %>%  pull(avg_la)
      z8 <- df %>%  filter(zone == 8) %>%  pull(avg_la)
      z9 <- df %>%  filter(zone == 9) %>%  pull(avg_la)
      z11 <- df %>%  filter(zone == 11) %>%  pull(avg_la)
      z12 <- df %>%  filter(zone == 12) %>%  pull(avg_la)
      z13 <- df %>%  filter(zone == 13) %>%  pull(avg_la)
      z14 <- df %>%  filter(zone == 14) %>%  pull(avg_la)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"°")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = LA_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"°")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = LA_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    else if (type == "Run Value") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(run_value)
      z2 <- df %>%  filter(zone == 2) %>%  pull(run_value)
      z3 <- df %>%  filter(zone == 3) %>%  pull(run_value)
      z4 <- df %>%  filter(zone == 4) %>%  pull(run_value)
      z5 <- df %>%  filter(zone == 5) %>%  pull(run_value)
      z6 <- df %>%  filter(zone == 6) %>%  pull(run_value)
      z7 <- df %>%  filter(zone == 7) %>%  pull(run_value)
      z8 <- df %>%  filter(zone == 8) %>%  pull(run_value)
      z9 <- df %>%  filter(zone == 9) %>%  pull(run_value)
      z11 <- df %>%  filter(zone == 11) %>%  pull(run_value)
      z12 <- df %>%  filter(zone == 12) %>%  pull(run_value)
      z13 <- df %>%  filter(zone == 13) %>%  pull(run_value)
      z14 <- df %>%  filter(zone == 14) %>%  pull(run_value)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"Runs")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = 0) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"Runs")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = 0) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    else if (type == "Hard Hit %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(hard_hit)
      z2 <- df %>%  filter(zone == 2) %>%  pull(hard_hit)
      z3 <- df %>%  filter(zone == 3) %>%  pull(hard_hit)
      z4 <- df %>%  filter(zone == 4) %>%  pull(hard_hit)
      z5 <- df %>%  filter(zone == 5) %>%  pull(hard_hit)
      z6 <- df %>%  filter(zone == 6) %>%  pull(hard_hit)
      z7 <- df %>%  filter(zone == 7) %>%  pull(hard_hit)
      z8 <- df %>%  filter(zone == 8) %>%  pull(hard_hit)
      z9 <- df %>%  filter(zone == 9) %>%  pull(hard_hit)
      z11 <- df %>%  filter(zone == 11) %>%  pull(hard_hit)
      z12 <- df %>%  filter(zone == 12) %>%  pull(hard_hit)
      z13 <- df %>%  filter(zone == 13) %>%  pull(hard_hit)
      z14 <- df %>%  filter(zone == 14) %>%  pull(hard_hit)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = HH_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = HH_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    else if (type == "SwSpot %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(sw_spot)
      z2 <- df %>%  filter(zone == 2) %>%  pull(sw_spot)
      z3 <- df %>%  filter(zone == 3) %>%  pull(sw_spot)
      z4 <- df %>%  filter(zone == 4) %>%  pull(sw_spot)
      z5 <- df %>%  filter(zone == 5) %>%  pull(sw_spot)
      z6 <- df %>%  filter(zone == 6) %>%  pull(sw_spot)
      z7 <- df %>%  filter(zone == 7) %>%  pull(sw_spot)
      z8 <- df %>%  filter(zone == 8) %>%  pull(sw_spot)
      z9 <- df %>%  filter(zone == 9) %>%  pull(sw_spot)
      z11 <- df %>%  filter(zone == 11) %>%  pull(sw_spot)
      z12 <- df %>%  filter(zone == 12) %>%  pull(sw_spot)
      z13 <- df %>%  filter(zone == 13) %>%  pull(sw_spot)
      z14 <- df %>%  filter(zone == 14) %>%  pull(sw_spot)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = SS_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = SS_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    else if (type == "CsW %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(CsW)
      z2 <- df %>%  filter(zone == 2) %>%  pull(CsW)
      z3 <- df %>%  filter(zone == 3) %>%  pull(CsW)
      z4 <- df %>%  filter(zone == 4) %>%  pull(CsW)
      z5 <- df %>%  filter(zone == 5) %>%  pull(CsW)
      z6 <- df %>%  filter(zone == 6) %>%  pull(CsW)
      z7 <- df %>%  filter(zone == 7) %>%  pull(CsW)
      z8 <- df %>%  filter(zone == 8) %>%  pull(CsW)
      z9 <- df %>%  filter(zone == 9) %>%  pull(CsW)
      z11 <- df %>%  filter(zone == 11) %>%  pull(CsW)
      z12 <- df %>%  filter(zone == 12) %>%  pull(CsW)
      z13 <- df %>%  filter(zone == 13) %>%  pull(CsW)
      z14 <- df %>%  filter(zone == 14) %>%  pull(CsW)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low ="blue", high = "red", midpoint = CsW_mid_quad) +
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low ="blue", high = "red", midpoint = CsW_mid_zone) +
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    if (type == "Barrel %") {
      
      z1 <- df %>%  filter(zone == 1) %>%  pull(barrel_rate)
      z2 <- df %>%  filter(zone == 2) %>%  pull(barrel_rate)
      z3 <- df %>%  filter(zone == 3) %>%  pull(barrel_rate)
      z4 <- df %>%  filter(zone == 4) %>%  pull(barrel_rate)
      z5 <- df %>%  filter(zone == 5) %>%  pull(barrel_rate)
      z6 <- df %>%  filter(zone == 6) %>%  pull(barrel_rate)
      z7 <- df %>%  filter(zone == 7) %>%  pull(barrel_rate)
      z8 <- df %>%  filter(zone == 8) %>%  pull(barrel_rate)
      z9 <- df %>%  filter(zone == 9) %>%  pull(barrel_rate)
      z11 <- df %>%  filter(zone == 11) %>%  pull(barrel_rate)
      z12 <- df %>%  filter(zone == 12) %>%  pull(barrel_rate)
      z13 <- df %>%  filter(zone == 13) %>%  pull(barrel_rate)
      z14 <- df %>%  filter(zone == 14) %>%  pull(barrel_rate)
      
      text_labels <- data.frame(
        quadrant = c(11, 12,13,14),
        x= c(0.5, 1.5, 0.5, 1.5),
        y=c(0.1,0.1, 1.9, 1.9),
        label=(c(z13,z14,z11,z12))
      )
      
      quadrant_data <- data.frame(
        xmin = c(0,0,1,1),
        xmax=c(1,1,2,2),
        ymin=c(0,1,0,1),
        ymax=c(1,2,1,2),
        value = c(z13,z11,z14,z12)
      )
      
      quadrant_plot <- ggplot() + geom_rect(data = quadrant_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=value),color = "black")+
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=text_labels, aes(x=x, y=y, label = paste(label,"%")), color = "black")+
        scale_fill_gradient2(low = "blue", high ="red", midpoint = B_mid_quad)+
        coord_fixed()+
        theme(legend.position = "None") +
        ggtitle(paste(toupper(type), "on", p_throws, pitch_type, ":", Player), subtitle = "Catcher's View")
      
      dat <- data.frame(xmin = c(0.25,0.75,1.25,0.25,0.75,1.25,0.25,0.75,1.25),
                        xmax = c(0.75, 1.25, 1.75, 0.75, 1.25, 1.75, 0.75, 1.25, 1.75),
                        ymin = c(0.25,0.25,0.25,0.75,0.75,0.75,1.25,1.25,1.25),
                        ymax = c(0.75, 0.75, 0.75, 1.25, 1.25, 1.25, 1.75, 1.75, 1.75),
                        value = c(z7,z8,z9,z4,z5,z6,z1,z2,z3))
      
      zone_plot <- ggplot() + geom_rect(data = dat, aes(xmin = xmin, xmax= xmax, ymin=ymin, ymax=ymax, fill=value), color= "black") +
        coord_cartesian(xlim=c(-0.5,2.1), ylim=c(-2,2.1))+theme_void()+geom_text(data=dat, aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=paste(value,"%")), color="black") +
        scale_fill_gradient2(low = "blue",  high = "red", midpoint = B_mid_zone)+
        coord_fixed()+
        theme(legend.position = "None") +
        geom_segment(aes(x = 0.25, y = -0.3, xend = 1.75, yend = -0.3), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.3, xend = 0.25, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 0.25, y = -0.5, xend = 1, yend = -0.7), size = 1, color = "black") + 
        geom_segment(aes(x = 1, y = -0.7, xend = 1.75, yend = -0.5), size = 1, color = "black") + 
        geom_segment(aes(x = 1.75, y = -0.3, xend = 1.75, yend = -0.5), size = 1, color = "black") 
      
      
      p <- ggdraw()+draw_plot(quadrant_plot)+draw_plot(zone_plot, x=0, y=-0.2, width=1, height=1)
      
    }
    
    
    # Return the plot
    
    return(p)
    
  }
  
  
# Use 'alldat' to isolate the spray chart data (Third tab)
  
  spray_dat <- alldat %>% select(pitch_name, pitch_type, player_name, des, hc_x, hc_y, p_throws, game_pk, at_bat_number,
                                 events, bb_type, launch_speed, launch_angle, game_year, zone, description, strikes, balls)
  
  
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
  
# Use 'alldat' to mutate, clean, and isolate the data needed for the Swing Decision data visualization (Fourth tab)
# This fourth tab is centered around dividing the batter's zone by 'heart' of the zone, 'shadow' or edge of the zone, 'chase' part of the zone, and 'waste' pitches

  swing_dat <- alldat %>% 
    filter(!is.na(plate_x)) %>% 
    mutate(count= paste(balls, strikes),
           pfx_x_in_pv = pfx_x*-12, 
           pfx_z_in = pfx_z*12) %>% 
    select(p_throws, game_year, pfx_x_in_pv, pfx_z_in, count, plate_x, plate_z, pitch_name, 
           pitch_type, sz_top, sz_bot, description, delta_run_exp, player_name, zone, balls, strikes)
  
  
  # I created a second get_years() function to execute this data visualization
  # 'df' is 'swing_dat'
  # Each argument in the function is an input in the app that will filter the data
  
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

  
# Trending Line Graphs (Fifth tab)
  # Instead of 'alldat' use only 1 season of data ('rs23') for this visualization
  
  trend_dat <- rs23 %>% select(game_date, pitch_name, player_name, events, at_bat_number, game_pk, woba_value,
                                 estimated_woba_using_speedangle, launch_angle, launch_speed, description, zone, bb_type)
  
  
  # Create 'get_slash_trend()' to make the plotly line graph (BA, OBP, SLG)
  # These plotly line graphs are built generally the same and take the same arguments
  # Trends vs. 'pitch_type' (Fastball, Breaking Ball, Offspeed), any date range this year ('start_date', 'end_date'), and any player this year ('name')
  
  get_slash_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    
    # Use distinct() to organize the data by the resultant pitch of each at bat
    
    tdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    
    
    # Filter 'tdf' by pitch type
    
    if(pitch_type == "Breaking Ball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                               "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- tdf
    }
    
    
    # Use 'bdf' to create the league average slash line table
    
    table <- bdf %>% 
      mutate(hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0),
             AB = ifelse(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out",
                                       "grounded_into_double_play", "force_out", "fielders_choice_out", "field_error"),1,0),
             PA = 1,
             OB = ifelse(events %in% c("single", "double", "triple", "home_run","walk", "hit_by_pitch"), 1, 0),
             SLG = case_when(events == "single" ~ 1,
                             events == "double" ~ 2,
                             events == "triple" ~ 3,
                             events == "home_run" ~ 4,
                             TRUE ~ 0)) %>% 
      mutate(avg = round(sum(hit)/sum(AB),3),
             obp = round(sum(OB)/sum(PA),3),
             slg = round(sum(SLG)/sum(AB),3)) %>% 
      select(avg, obp, slg)
    
    
    # Use 'bdf' to create the player-specific cumulative slash line data
    
    dat <- bdf %>% filter(player_name == name) %>% 
      mutate(hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0),
             AB = ifelse(events %in% c("single", "double", "triple", "home_run", "strikeout", "field_out",
                                       "grounded_into_double_play", "force_out", "fielders_choice_out", "field_error"),1,0),
             PA = 1,
             OB = ifelse(events %in% c("single", "double", "triple", "home_run","walk", "hit_by_pitch"), 1, 0),
             SLG = case_when(events == "single" ~ 1,
                             events == "double" ~ 2,
                             events == "triple" ~ 3,
                             events == "home_run" ~ 4,
                             TRUE ~ 0)) %>% 
      arrange(game_date, at_bat_number) %>% 
      mutate(cum_avg = round(cumsum(hit)/cumsum(AB),3),
             cum_obp = round(cumsum(OB)/cumsum(PA),3),
             cum_slg = round(cumsum(SLG)/cumsum(AB),3),
             game_date = as.Date(game_date)) %>% 
      select("AVG" = cum_avg, "OBP" = cum_obp, "SLG" = cum_slg, "Date" = game_date)
    
    
    # Make sure the data type is 'Date' so it can be read through the shiny::DateInputRange() in the ui
    
    dat <- dat %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    
    # Use ggplot() to create the plot
    
    p <- ggplot(dat, aes(x = `Date`)) +
      
      # Use geom_line to create the player-specific trending line
      
      geom_line(aes(y = `SLG`, color = "SLG"), linetype = "solid") +
      geom_line(aes(y = `OBP`, color = "OBP"), linetype = "solid") +
      geom_line(aes(y = `AVG`, color = "AVG"), linetype = "solid") +
      
      # Use geom_hline() to create the league average marker as a dashed horizontal line
      
      geom_hline(yintercept = table$obp, color = "#332288", linetype = "dashed") +
      geom_hline(yintercept = table$avg, color = "#DDCC77", linetype = "dashed") +
      geom_hline(yintercept = table$slg, color = "#CC6677", linetype = "dashed") +
      
      # Axis labels are not necessary since they are straight forward
      
      labs(x= NULL, y = NULL) +
      
      # Use ggtitle() to create an updated chart title based on the inputs
      
      ggtitle(paste("Trending Slash Line against", pitch_type, ":", name)) +
      
      # Use scale_color_manual() and theme() to create and customize the key for this chart
      
      scale_color_manual(
        values = c("OBP" = "#332288", "AVG" = "#DDCC77", "SLG" = "#CC6677"),
        labels = c("OBP", "AVG", "SLG"),
        name = ""
      ) +
      theme(legend.position = "right") +
      
      # Use scale_y_continuous() to label the y-axis
      # 'breaks =' argument makes the intervals of the graph the league average (geom_hline()) value
      # 'labels =' argument formats the y-axis interval labels to look like AVG, OBP, SLG (ex: '.300')
      
      scale_y_continuous(breaks = c(table$obp, table$avg, table$slg), labels = function(x) format(x, nsmall = 3, decimal.mark = ".", trim = TRUE)) 
    
    # Convert to a plotly
    # Use 'tooltip =' argument so the hover data in the plotly shows the Date and the stat value 
    
    q <- plotly::ggplotly(p, tooltip = c("y", "x")) %>%
      layout(hovermode = "closest")
    
    # Return the plotly
    
    return(q)
  }
  
  
  # Similar steps to create 'get_wOBA_trend()' to make the plotly line graph (wOBA, xwOBA)
  
  get_wOBA_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    tdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    
    if(pitch_type == "Breaking Ball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                               "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- tdf
    }
    
    table <- bdf %>% 
      mutate(PA = 1,
             lg_wOBA = ifelse(is.na(woba_value), 0, woba_value),  
             lg_xwOBA = ifelse(is.na(estimated_woba_using_speedangle), ifelse(is.na(woba_value), 0, woba_value), estimated_woba_using_speedangle)) %>%
      mutate(wOBA = round(sum(lg_wOBA)/sum(PA),3),
             xwOBA = round(mean(lg_xwOBA),3)) %>% 
      select(wOBA, xwOBA)
    
    dat <- bdf %>% filter(player_name == name) %>% 
      mutate(PA = 1,
             wOBA = ifelse(is.na(woba_value), 0, woba_value),  
             xwOBA = ifelse(is.na(estimated_woba_using_speedangle), ifelse(is.na(woba_value), 0, woba_value), estimated_woba_using_speedangle)) %>% 
      arrange(game_date, at_bat_number) %>% 
      mutate(cum_wOBA = round(cumsum(wOBA)/cumsum(PA),3),
             cum_xwOBA = round(cummean(xwOBA),3),
             game_date = as.Date(game_date)) %>% 
      select("xwOBA" = cum_xwOBA, "wOBA" = cum_wOBA, "Date" = game_date)
    
    dat <- dat %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    p <- ggplot(dat, aes(x = `Date`)) +
      geom_line(aes(y = `wOBA`, color = "wOBA"), linetype = "solid") +
      geom_line(aes(y = `xwOBA` , color = "xwOBA"), linetype = "solid") +
      geom_hline(yintercept = table$wOBA, color = "#CC6677", linetype = "dashed") +
      geom_hline(yintercept = table$xwOBA, color = "#332288", linetype = "dashed") +
      labs(x= NULL, y = NULL) +
      ggtitle(paste("Trending wOBA vs. xWOBA against", pitch_type, ":", name)) +
      scale_color_manual(
        values = c("wOBA" = "#CC6677", "xwOBA" = "#332288"),
        labels = c("wOBA", "xwOBA"),
        name = ""
      ) +
      scale_y_continuous(breaks = c(table$wOBA, table$xwOBA), labels = function(x) format(x, nsmall = 3, decimal.mark = ".", trim = TRUE)) +
      theme(legend.position = "right")
    
    q <- plotly::ggplotly(p, tooltip = c("y", "x")) %>%
      layout(hovermode = "closest")
    
    return(q)
  }
  
  
  
  # Create 'get_barrel_trend()' to make the plotly line graph (Hard %, SwSpot %, Barrel %)
  
  get_barrel_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    tdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    
    if(pitch_type == "Breaking Ball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                               "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- tdf
    }
    
    table <- bdf %>% 
      summarise(PAs = nrow(bdf),
                ABs = PAs - sum(events %in% c("walk", "field_error", "hit_by_pitch", "catcher_interf", "fielders_choice")),
                SwSpot = sum(launch_angle > 8 & launch_angle < 32, na.rm = T),
                barrels = sum((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 
                              & launch_angle > 4 & launch_angle < 50, na.rm = T),
                HH = sum(launch_speed >= 95, na.rm = T),
                in_play = sum(description == "hit_into_play")) %>%
      ungroup() %>%
      mutate(HH_pct = 100*round(HH/in_play,3),
             SwSpot_pct = 100*round(SwSpot/in_play,3),
             b_pct = 100*round(barrels/in_play,3)) %>%
      select(HH_pct, SwSpot_pct, b_pct)
    
    dat <- bdf %>% filter(player_name == name) %>% 
      mutate(AB = ifelse( events %in% c( "single", "double", "triple", "home_run", "strikeout", "field_out",
                                         "grounded_into_double_play", "force_out", "fielders_choice_out", "field_error"),1,0),
             PA = 1,
             barrel = ifelse( ((!is.na(launch_speed)) & (!is.na(launch_angle))) &
                                ((launch_speed * 1.5 - launch_angle) >= 117 & (launch_speed + launch_angle) >= 124 & launch_speed >= 97 & 
                                   launch_angle > 4 & launch_angle < 50),1,0),
             HH = ifelse((!is.na(launch_speed)) & (launch_speed >= 95), 1, 0),
             SwSp = ifelse((!is.na(launch_angle)) & (launch_angle > 8 & launch_angle < 32), 1, 0),
             ip = ifelse(description == "hit_into_play", 1, 0),
             game_date = as.Date(game_date)) %>%
      arrange(game_date, at_bat_number) %>%
      mutate(cum_bar = 100*round(cumsum(barrel) / cumsum(ip),3),
             cum_HH = 100*round(cumsum(HH) / cumsum(ip),3),
             cum_SwSp = 100*round(cumsum(SwSp) / cumsum(ip),3)) %>% 
      select("Barrel %" = cum_bar, "Hard %" = cum_HH, "SwSpot %" = cum_SwSp, "Date" = game_date)
    
    dat <- dat %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    p <- ggplot(dat, aes(x = `Date`)) +
      geom_line(aes(y = `Hard %` , color = "Hard %"), linetype = "solid") +
      geom_line(aes(y = `SwSpot %` , color = "SwSpot %"), linetype = "solid") +
      geom_line(aes(y = `Barrel %` , color = "Barrel %"), linetype = "solid") +
      geom_hline(yintercept = table$b_pct, color = "#DDCC77", linetype = "dashed") +
      geom_hline(yintercept = table$HH_pct, color = "#CC6677", linetype = "dashed") +
      geom_hline(yintercept = table$SwSpot_pct, color = "#332288", linetype = "dashed") +
      labs(x= NULL, y = NULL) +
      ggtitle(paste("Trending Batted Ball Stats against", pitch_type, ":", name)) +
      scale_color_manual(
        values = c("Barrel %" = "#DDCC77", "Hard %" = "#CC6677", "SwSpot %" = "#332288"),
        labels = c("Barrel %", "Hard %", "SwSpot %"),
        name = ""
      ) +
      scale_y_continuous(breaks = c(table$b_pct, table$HH_pct, table$SwSpot_pct), labels = scales::label_percent(scale = 1, suffix = "%")) +
      theme(legend.position = "right")
    
    q <- plotly::ggplotly(p, tooltip = c("y", "x")) %>%
      layout(hovermode = "closest")
    
    return(q)
  }
  
  
  
  # Create 'get_plate_disc_trend()' to make the plotly line graph (Hard %, SwSpot %, Barrel %)
  
  get_plate_disc_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    if(pitch_type == "Breaking Ball") {
      bdf <- df %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                              "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      bdf <- df %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      bdf <- df %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- df
    }
    
    table <- bdf %>%
      summarise(
        tp = nrow(df),
        swings = sum(description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked"), na.rm = T),
        z_swing = sum(zone %in% c(1:9) & description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")),
        o_swing = sum(zone %in% c(11:14) & description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")),
        o_con = sum(zone %in% c(11:14) & description %in% c("hit_into_play", "foul")),
        z_con = sum(zone %in% c(1:9) & description %in% c("hit_into_play", "foul"))) %>%
      ungroup() %>%
      mutate(
        o_swings = 100 * round(o_swing / swings, 3),
        z_swings = 100 * round(z_swing / swings, 3),
        o_cons = 100 * round(o_con / o_swing, 3),
        z_cons = 100 * round(z_con / z_swing, 3)
      ) %>%
      select(o_swings, z_swings, o_cons, z_cons)
    
    dat <- bdf %>%
      filter(player_name == name) %>%
      mutate(
        tp = 1,
        z_swing = ifelse((zone %in% c(1:9) & description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")), 1, 0),
        o_swing = ifelse((zone %in% c(11:14) & description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked")), 1, 0),
        o_con = ifelse((zone %in% c(11:14) & description %in% c("hit_into_play", "foul")), 1, 0),
        z_con = ifelse((zone %in% c(1:9) & description %in% c("hit_into_play", "foul")), 1, 0),
        swings = ifelse(description %in% c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked"), 1, 0),
      ) %>%
      arrange(game_date, at_bat_number) %>%
      mutate(
        cum_ZS = 100 * round(cumsum(z_swing) / cumsum(swings), 3),
        cum_ZC = 100 * round(cumsum(z_con) / cumsum(z_swing), 3),
        cum_OS = 100 * round(cumsum(o_swing) / cumsum(swings), 3),
        cum_OC = 100 * round(cumsum(o_con) / cumsum(o_swing), 3),
        game_date = as.Date(game_date)
      ) %>%
      select("Z-Swing %" = cum_ZS, "Z-Contact %" = cum_ZC, "O-Swing %" = cum_OS, "O-Contact %" = cum_OC, "Date" = game_date)
    
    dat <- dat %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    p <- ggplot(dat, aes(x = `Date`)) +
      geom_line(aes(y = `Z-Contact %`, color = "Z-Contact %"), linetype = "solid") +
      geom_line(aes(y = `Z-Swing %`, color = "Z-Swing %"), linetype = "solid") +
      geom_line(aes(y = `O-Contact %`, color = "O-Contact %"), linetype = "solid") +
      geom_line(aes(y = `O-Swing %`, color = "O-Swing %"), linetype = "solid") +
      geom_hline(yintercept = table$o_swings, color = "green", linetype = "dashed") +
      geom_hline(yintercept = table$z_swings, color = "#332288", linetype = "dashed") +
      geom_hline(yintercept = table$o_cons, color = "#DDCC77", linetype = "dashed") +
      geom_hline(yintercept = table$z_cons, color = "#CC6677", linetype = "dashed") +
      labs(x= NULL, y = NULL) +
      ggtitle(paste("Trending Plate Discipline Stats against", pitch_type, ":", name)) +
      scale_color_manual(
        values = c("Z-Contact %" = "#CC6677", "Z-Swing %" = "#332288", "O-Contact %" = "#DDCC77", "O-Swing %" = "green"),
        labels = c("Z-Contact %", "Z-Swing %", "O-Contact %", "O-Swing %"),
        name = ""
      ) +
      scale_y_continuous(breaks = c(table$o_swings, table$z_swings, table$o_cons, table$z_cons), labels = scales::label_percent(scale = 1, suffix = "%")) + 
      theme(legend.position = "right")
    
    q <- plotly::ggplotly(p, tooltip = c("y", "x")) %>%
      layout(hovermode = "closest")
    
    return(q)
  }
  
  
  
  # Create 'get_batted_ball_trend()' to make the plotly line graph (GB%, FB%, LD%, PU%)
  
  get_batted_ball_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    tdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    
    if(pitch_type == "Breaking Ball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                               "Slow Curve"))
    } else if (pitch_type == "Fastball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
    } else if (pitch_type == "Offspeed") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- tdf
    }
    
    table <- bdf %>% 
      summarise(
        GB=sum(bb_type == "ground_ball"),
        FB= sum(bb_type == "fly_ball"),
        LD = sum(bb_type == "line_drive"),
        PU = sum(bb_type == "popup"),
        in_play = sum(description == "hit_into_play")) %>%
      ungroup() %>%
      mutate(GB_pct= 100*round(GB/in_play,3),
             LD_pct = 100*round(LD/in_play,3),
             FB_pct = 100*round(FB/in_play,3),
             PU_pct = 100*round(PU/in_play,3)) %>%
      select(GB_pct, LD_pct, FB_pct, PU_pct)
    
    dat <- bdf %>% filter(player_name == name) %>% 
      mutate(GB = ifelse(bb_type == "ground_ball", 1,0),
             LD = ifelse(bb_type == "line_drive", 1,0),
             FB = ifelse(bb_type == "fly_ball",1,0),
             PU = ifelse(bb_type == "popup", 1, 0),
             ip = ifelse(description == "hit_into_play", 1, 0)) %>%
      arrange(game_date, at_bat_number) %>%
      mutate(cum_GB = 100*round((cumsum(GB) / cumsum(ip)),3),
             cum_LD = 100*round((cumsum(LD) / cumsum(ip)),3),
             cum_FB = 100*round((cumsum(FB) / cumsum(ip)),3),
             cum_PU = 100*round((cumsum(PU) / cumsum(ip)),3),
             game_date = as.Date(game_date)) %>% 
      select("GB %" = cum_GB , "LD %" = cum_LD, "FB %" = cum_FB, "PU %" = cum_PU, "Date" = game_date)
    
    dat <- dat %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    p <- ggplot(dat, aes(x = `Date`)) +
      geom_line(aes(y = `GB %` , color = "GB %"), linetype = "solid") +
      geom_line(aes(y = `FB %` , color = "FB %"), linetype = "solid") +
      geom_line(aes(y = `LD %` , color = "LD %"), linetype = "solid") +
      geom_line(aes(y = `PU %` , color = "PU %"), linetype = "solid") +
      geom_hline(yintercept = table$GB_pct, color = "#CC6677", linetype = "dashed") +
      geom_hline(yintercept = table$LD_pct, color = "#DDCC77", linetype = "dashed") +
      geom_hline(yintercept = table$FB_pct, color = "#332288", linetype = "dashed") +
      geom_hline(yintercept = table$PU_pct, color = "green", linetype = "dashed") +
      labs(x= NULL, y = NULL) +
      ggtitle(paste("Trending GB%/LD%/FB%/PU% against", pitch_type, ":", name)) +
      scale_color_manual(
        values = c("GB %" = "#CC6677", "LD %" = "#DDCC77", "FB %" = "#332288", "PU %" = "green"),
        labels = c("GB %", "LD %", "FB %", "PU %"),
        name = ""
      ) +
      scale_y_continuous(breaks = c(table$GB_pct, table$LD_pct, table$FB_pct, table$PU_pct), labels = scales::label_percent(scale = 1, suffix = "%")) + 
      theme(legend.position = "right")
    
    q <- plotly::ggplotly(p, tooltip = c("y", "x")) %>%
      layout(hovermode = "closest")
    
    return(q)
  }
  
  
  # Create 'get_KBB_trend()' to make plotly (K% BB%, Take%, CsW%)
  # This function requires 2 different data sets because K% & BB% are per AB and CsW% & Take% are per pitch
  
  get_KBB_trend <- function(df, pitch_type, start_date, end_date, name) {
    
    tdf <- distinct(df, game_pk, at_bat_number, .keep_all = TRUE)
    
    if(pitch_type == "Breaking Ball") {
     #Per AB data
      
      bdf <- tdf %>%  filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                               "Slow Curve"))
      
     #Per pitch data
      bdf2 <- df %>% filter(pitch_name %in% c("Slider", "Curveball", "Knuckle Curve", "Slurve", "Sweeper",
                                              "Slow Curve"))
      
    } else if (pitch_type == "Fastball") {
      bdf <- tdf %>%  filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
      bdf2 <- df %>% filter(pitch_name %in% c("4-Seam Fastball", "Sinker", "Cutter"))
      
    } else if (pitch_type == "Offspeed") {
      bdf <- tdf %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
      bdf2 <- df %>%  filter(pitch_name %in% c("Changeup", "Split-Finger", "Other", "Knuckleball", "Eephus"))
    } else if (pitch_type == "All") {
      bdf <- tdf
      bdf2 <- df
    }
    
    table1 <- bdf %>% 
      summarise(PAs = nrow(bdf),
                ABs = PAs - sum(events %in% c("walk", "field_error", "hit_by_pitch", "catcher_interf", "fielders_choice")),
                Ks=sum(events == "strikeout"),
                BBs = sum(events == "walk"),
                in_play = sum(description == "hit_into_play")) %>%
      ungroup() %>%
      mutate(K_rate = 100*round(Ks/PAs,3),
             BB_rate= 100*round(BBs/PAs,3)) %>%
      select(K_rate, BB_rate)
    
    table2 <- bdf2 %>% 
      summarise(tp = nrow(bdf2),
                CsW = sum(description %in% c("swinging_strike", "called_strike")),
                take = sum(description %in% c("ball", "called_strike", "blocked_ball", "pitchout", "hit_by_pitch"))) %>% 
      ungroup() %>%
      mutate(takes = 100*round(take/tp,3),
             CsWs= 100*round(CsW/tp,3)) %>%
      select(takes, CsWs)
    
    
    dat1 <- bdf %>% filter(player_name == name) %>% 
      mutate(PA = 1,
             ip = ifelse(description == "hit_into_play", 1, 0),
             game_date = as.Date(game_date)) %>%
      arrange(game_date, at_bat_number) %>%
      mutate(cum_K = 100*round(cumsum(events == "strikeout") / cumsum(PA),3),
             cum_BB = 100*round(cumsum(events == "walk") / cumsum(PA),3)) %>% 
      select("K %" = cum_K, "BB %" = cum_BB, "Date" = game_date)
    
    dat2 <- bdf2 %>% filter(player_name == name) %>% 
      mutate(tp = 1,
             take = ifelse(description %in% c("ball","called_strike", "blocked_ball", "pitchout", "hit_by_pitch"),1,0),
             CsW = ifelse(description %in% c("swinging_strike", "called_strike"),1,0),
             game_date = as.Date(game_date)) %>% 
      arrange(game_date, at_bat_number) %>% 
      mutate(cum_take = 100*round(cumsum(take) / cumsum(tp),3),
             cum_CsW = 100*round(cumsum(CsW)/ cumsum(tp),3)) %>%
      select("CsW %" = cum_CsW, "Take %" = cum_take, "Date" = game_date)
    
    dat1 <- dat1 %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    dat2 <- dat2 %>% filter(`Date` %in% seq(as.Date(start_date), as.Date(end_date), by = "day"))
    
    p <- ggplot() +
      geom_line(data = dat2, aes(x = `Date`, y = `Take %` , color = "Take %"), linetype = "solid") +
      geom_line(data = dat2, aes(x = `Date`, y = `CsW %` , color = "CsW %"), linetype = "solid") +
      geom_line(data = dat1, aes(x = `Date`, y = `K %` , color = "K %"), linetype = "solid") +
      geom_line(data = dat1, aes(x = `Date`, y = `BB %` , color = "BB %"), linetype = "solid") +
      geom_hline(yintercept = table1$K_rate, color = "#DDCC77", linetype = "dashed") +
      geom_hline(yintercept = table1$BB_rate, color = "green", linetype = "dashed") +
      geom_hline(yintercept = table2$takes, color = "#CC6677", linetype = "dashed") +
      geom_hline(yintercept = table2$CsWs, color = "#332288", linetype = "dashed") +
      labs(x= NULL, y = NULL) +
      ggtitle(paste("Trending Batted Ball Stats against", pitch_type, ":", name)) +
      scale_color_manual(
        values = c("Take %" = "#CC6677", "CsW %" = "#332288", "K %" = "#DDCC77", "BB %" = "green"),
        labels = c("K %", "BB %", "Take %", "CsW %"),
        name = ""
      ) +
      scale_y_continuous(breaks = c(table1$K_rate, table1$BB_rate, table2$takes, table2$CsWs), 
                         labels = scales::label_percent(scale = 1, suffix = "%")) +
      theme(legend.position = "right")
    
    q <- plotly::ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "closest")
    
    return(q)
  }



# Define UI

ui <- fluidPage(
  navbarPage(
    title = "MLB Batting App",
    
    
# Heat Maps (First tab)

# First Tab Panel text

    tabPanel(
      title = "Heat Maps",
      
      fluidPage(
  
        
# Heat Map page's header text
        
        titlePanel("Heat Maps"),
        
        
  # Heat Maps interactive user filters in the sidebar

        sidebarLayout(
          sidebarPanel(
            width = 3, 
            
    # Player filter
            selectizeInput(
              "player_name",
              "Search player",
              choices = unique(heatmap_data$player_name),
              multiple = FALSE,
              options = list(placeholder = "Type name...", maxOptions = 10)),
    # Year range filter
            selectInput("year_range", "Select season(s)", 
                        choices = unique(heatmap_data$game_year), 
                        multiple = TRUE, 
                        selected = "2023"),
    # Pitch type filter
            selectInput(
              "pitch_type",
              "Select pitch type",
              choices = c("Fastball", "Breaking Ball", "Offspeed")),
    # Velocity filter
            sliderInput(
              "release_speed",
              "Velocity (mph)",
              min = 50,
              max = 110,
              value = c(50, 110),
              step = 1),
    # Horizontal pitcher break filter
            sliderInput(
              "pfx_x_in_pv",
              "Horizontal Break (in.)",
              min = -30,
              max = 30,
              value = c(-30, 30),
              step = 0.1),
    # Vertical pitch break filter
            sliderInput(
              "pfx_z_in",
              "Vertical Break (in.)",
              min = -30,
              max = 30,
              value = c(-30, 30),
              step = 0.1),
    # Spin rate filter
            sliderInput(
              "release_spin_rate",
              "Spin Rate (rpm)",
              min = 0,
              max = 4000,
              value = c(0, 4000),
              step = 1)
          ),
    
  # Main panel with all of the plots 
          mainPanel(
            plotOutput("usage_plot"),
            plotOutput("take_plot"),
            plotOutput("swing_plot"),
            plotOutput("whiff_plot"),
            plotOutput("hit_plot"),
            plotOutput("barrel_plot"),
            plotOutput("GB_plot"),
            plotOutput("LD_plot"),
            plotOutput("FB_plot"),
            plotOutput("PU_plot")
          )
        )
      )
    ),


# 13 Zone Visualization (Second tab)

# Second Tab Panel text

    tabPanel(
      title = "Strike Zone",
      fluidPage(
        
      
  # 13 Zone page's header text
  
        titlePanel("13-Zone Data"),
        
        
  # 13 Zone Visualization interactive user filters in the sidebar
        
        sidebarLayout(
          sidebarPanel(

            
    # Player filter
            selectizeInput(
              "player_name2",
              "Search player",
              choices = unique(zonedat$player_name),
              multiple = FALSE,
              options = list(placeholder = "Judge, Aaron", maxOptions = 10)
            ),
    # Year filter
            checkboxGroupInput(
              inputId = "year_range2",
              label = "Select season(s)",
              choices = unique(zonedat$game_year),
              selected = "2023"
            ),
    # Statistical filter 
            selectInput(
              "type",
              "Select stat",
              choices = c("Whiff %", "Swing %", "Take %", "Average Exit Velocity", "Average Launch Angle", "Run Value", 
                          "Hard Hit %" , "SwSpot %", "CsW %", "Barrel %")
            ),
    # Pitch type filter
            selectInput(
              "pitch_type2",
              "Select pitch type",
              choices = c("Fastball", "Breaking Ball", "Offspeed", "All"),
              selected = "All"
            ),
    # Pitcher handedness filter
            selectInput(
              "p_throws2",
              "Select pitcher handedness",
              choices = c("B", "R", "L")
            )
          ),
    
  # Main panel displaying the plots
          mainPanel(
            plotOutput("plot"),
            plotlyOutput("plotly1"),
            plotlyOutput("plotly2")
          )
        )
      )
    ),


# Spray Chart (Third tab)

# Third Tab Panel text

  tabPanel(
    title = "Spray Chart",
    fluidPage(
      
      
  # Spray Chart page's header text
    
      titlePanel("Spray Chart + Hit Profile"),
      
      
      # Place the available Spray Chart filters in a sidebar
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          
    # Player filter
        selectizeInput(
          "player_name3",
          "Search player",
          choices = unique(spray_dat$player_name),
          multiple = FALSE,
          options = list(placeholder = "Judge, Aaron", maxOptions = 10)
        ),
    # Year filter
        selectInput("year_range3", "Select seasons(s)", 
                    choices = unique(spray_dat$game_year), 
                    multiple = TRUE, 
                    selected = "2023"
        ),
    # Pitch type filter
        selectInput(
          "pitch_type3",
          "Select pitch type",
          choices = c("Fastball", "Breaking Ball", "Offspeed", "All"),
          selected = "All"
        ),
    # Hit type filter
        selectInput(
          "hit_type",
          "Select hit type",
          choices = c("Single", "Double", "Triple", "Homerun", "All hits", "Groundball", "Linedrive", "Flyball", "Popup", "All batted balls"),
          selected = "All hits"
        )
      ),
    
    
    # Display the spray chart and 2 tables
    mainPanel(
      width = 9,
      fluidRow(
        column(6, plotOutput("sprayplot")),
        column(4, style = "height: 500px;", plotlyOutput("nplot", width = "100%", height = "600px"))
      ),
      fluidRow(
        column(12, style = "margin-top: 75px;", tableOutput("table4")),
        column(12, tableOutput("table5"))
      )
    )
        )
      )
    ),


# Swing Take Decisions (Fourth tab)

# Fourth Tab Panel text

    tabPanel(
      title = "Swing Quality",
      fluidPage(
         
        
  # Swing Take page's header text
        
        titlePanel("Swing/Take Run Value Data"),
        
        
    # Create the available Spray Chart filters
        
        fluidRow(
          column(
            width = 12,
            

      # Player filter
            selectizeInput(
              "player_name4",
              "Search player",
              choices = unique(swing_dat$player_name),
              multiple = FALSE,
              options = list(placeholder = "Judge, Aaron", maxOptions = 10)
            )
          )
        ),
      # Year filter
        fluidRow(
          column(
            width = 2,
            checkboxGroupInput(
              inputId = "year_range4",
              label = "Select seasons(s)",
              choices = unique(swing_dat$game_year),
              selected = "2023"
            )
          ),
      # Count filter
          column(
            width = 2,
            checkboxGroupInput(
              inputId = "count",
              label = "Select count(s)",
              choices = c(
                "0 0", "0 1", "0 2", "1 0", "1 1", "1 2",
                "2 0", "2 1", "2 2", "3 0", "3 1", "3 2"
              ),
              selected = c(
                "0 0", "0 1", "0 2", "1 0", "1 1", "1 2",
                "2 0", "2 1", "2 2", "3 0", "3 1", "3 2"))
          ),
      # Pitch type filter
          column(
            width = 2,
            selectInput(
              "pitch_type4",
              "Select pitch type",
              choices = c("All", "Fastball", "Breaking Ball", "Offspeed"),
              selected = "All"
            )
          ),
      # Pitcher handedness filter
          column(
            width = 4,
            selectInput(
              "p_throws4",
              "Select pitcher handedness",
              choices = c("B", "R", "L")
            )
          )
        ),
    
    
    # Main panel for the 3 plots
    
        mainPanel(
          width = 12,
          fluidRow(
            column(width = 4, plotOutput("rv_plot")),
            column(width = 4, plotOutput("plot3")),
            column(width = 3, plotOutput("plot2"))
          ),
          
          
        # Tables in a row underneath the plots
        
          fluidRow(
            tableOutput("table2"),
            tableOutput("table6"),
            tableOutput("table")
          )
        )
      )
    ),


# Trend Charts (Fifth tab)

 # Fifth Tab Panel text

    tabPanel(
      title = "Trend Charts",
      fluidPage(
        
        
  # Spray Chart page's header text
        
        titlePanel("Trend Charts"),
        
        
    # Place the available Spray Chart filters in a sidebar
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            
      # Player filter
            selectizeInput(
              "player_name5",
              "Search player",
              choices = unique(rs23$player_name),
              multiple = FALSE,
              options = list(placeholder = "Judge, Aaron", maxOptions = 10)
            ),
      # Date filter
            dateRangeInput(
              "date_range", 
              "Select date range",
              start = "2023-06-23",
              end = "2023-07-06"
            ),
      # Pitch type filter
            selectInput(
              "pitch_type5",
              "Select pitch type",
              choices = c("Fastball", "Breaking Ball", "Offspeed", "All"),
              selected = "All"
            )
          ),
      
  # Main panel for the 6 plotlys
  
          mainPanel(
            width = 6,
            fluidRow(
              align = "center",
              plotlyOutput("slash_trend"),
              plotlyOutput("KBB_trend"),
              plotlyOutput("plate_disc_trend"),
              plotlyOutput("wOBA_trend"),
              plotlyOutput("barrel_trend"))
         )
        )
       )
      ),
     )
    )


# Define Server

server <- function(input, output) {
  
# Heat Maps (First Tab)
  # Use the get_years() function to create a reactive data frame that filters the data by years selected in the user interface
  
    heatmap_dat <- reactive({
      get_years(heatmap_data, input$year_range)
        }
      )
  
  
  # Create a reactive element called 'B()' that will update each time the rest of the inputs are altered in the user interface 
  
    B <- reactive({
        data <- subset(heatmap_dat(), player_name == input$player_name & pitch_type == input$pitch_type & 
                      release_speed >= input$release_speed[1] & release_speed <= input$release_speed[2] &
                      pfx_x_in_pv >= input$pfx_x_in_pv[1] & pfx_x_in_pv <= input$pfx_x_in_pv[2] & 
                      pfx_z_in >= input$pfx_z_in[1] & pfx_z_in <= input$pfx_z_in[2] & 
                      release_spin_rate >= input$release_spin_rate[1] & release_spin_rate <= input$release_spin_rate[2])
        }
      )
  
  
  # Use 'B()' as the data to create each of the plots
    
    output$usage_plot <- renderPlot({
      ggplot(B(), aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location (in feet)")+
        ylab("Vertical Pitch Location (in feet)")+
        facet_wrap(~p_throws)+
        ggtitle(paste(input$pitch_type, "Location Heatmap :", input$player_name), 
                "Catcher's View")
   }
   )
  
    output$whiff_plot <- renderPlot({

  # Use the 'find_plots()' function to create the specific heat maps (Whiffs, Hits, Hard Hit, etc.)
  # In this example, whiffs are defined as swinging strikes and foul tips, hard hit is launch_speed >= 98 etc. 
      
      D <- find_plots(B(), "description", value = c("swinging_strike", "foul_tip"))
      
  # Use 'ggplot()' to create the heat map
      
      ggplot(D, aes(x=plate_x, y=plate_z)) +
        
      
  # ggplot::stat_density_2d() and scale_fill_distiller() are creating the heat map visualization
      
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        
   # coord_fixed() so the plots look like a strike zone 
        
        coord_fixed()+
        
   # xlim() and ylim() so the plot looks consistent each time
        
        xlim(c(-2,2))+
        ylim(c(0,5))+
  
    # Create the rectangle strike zone
        
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        
    # ggplot::theme_classic() includes the legend and an overall more detailed plot layout
        
        theme_classic()+
        
    # Label each axis
        
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        
    # Use facet_wrap() to make 2 separate heat maps: vs. RHP & vs. LHP
        
        facet_wrap(~p_throws)+
        
     # use ggtitle() to create a reactive title
        
        ggtitle(paste("Whiff Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
      
    })
    
    
  # Repeat similar steps for the rest of the graphs
    
    output$take_plot <- renderPlot({
      E <- find_plots(B(), "description", value = c("ball", "called_strike"))
      ggplot(E, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Take Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$hit_plot <- renderPlot({
      F <- find_plots(B(), "description", value = "hit_into_play")
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Hit Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$swing_plot <- renderPlot({
      F <- find_plots(B(), "description", value = c("foul", "hit_into_play", "swinging_strike", "foul_tip", "swinging_strike_blocked"))
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Swing Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$barrel_plot <- renderPlot({
      F <- find_plots(B(), "barrel", value = 1)
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Barrel Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$GB_plot <- renderPlot({
      F <- find_plots(B(), "bb_type", value = "ground_ball")
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Groundball Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$LD_plot <- renderPlot({
      F <- find_plots(B(), "bb_type", value = "line_drive")
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Linedrive Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    
    output$FB_plot <- renderPlot({
      F <- find_plots(B(), "bb_type", value = "fly_ball")
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Flyball Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
    output$PU_plot <- renderPlot({
      F <- find_plots(B(), "bb_type", value = "popup")
      ggplot(F, aes(x=plate_x, y=plate_z))+
        stat_density_2d(aes(fill=..density..), geom="raster", contour = F) +
        scale_fill_distiller(type="div")+
        coord_fixed()+
        xlim(c(-2,2))+
        ylim(c(0,5))+
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.6, ymax=3.5, fill="black", color="black", alpha=0.1)+
        theme_classic()+
        xlab("Horizontal Pitch Location")+
        ylab("Vertical Pitch Location")+
        facet_wrap(~p_throws)+
        ggtitle(paste("Popup Heatmap vs.", input$pitch_type, ":", input$player_name), 
                "Catcher's View")
    })
  

# 13-Zone Visualization (Second tab)
  # Use the 'get_years()' function to create a 2nd reactive element for the 13-Zone Visualization 
  
    zdat <- reactive({
     get_years(zonedat, input$year_range2)
      })
  

  # Use the 'get_zone_data()' function to create 13 zone visualization based on the user inputs
  
    output$plot <- renderPlot({
     zplot <- get_zone_data(zdat(), input$player_name2, input$type, input$pitch_type2, input$p_throws2)
       
      zplot})
  
  
  # Use 'zdat()' & plotly::renderPlotly() to create plotly histograms displaying the player's Launch Angles and Exit Velocities
    
    output$plotly1 <- renderPlotly({
       filtered_df <- filter(zdat(), player_name == input$player_name2)
      
    # Highlight the bins that represent the Sweet Spot
        filtered_df$fill_color <- ifelse(filtered_df$launch_angle >= 8 & filtered_df$launch_angle <= 30, "lightblue", "orange")
  
    # First, Create the data visualization using ggplot()    
        p <- ggplot(filtered_df, aes(x = launch_angle, fill = fill_color)) +
          geom_histogram(binwidth = 1, color = "black") +
          labs(title = paste("Launch Angles for", input$player_name2),
              x = "Launch Angle", y = "Batted Balls")+
          xlim(-75, 75)+theme(legend.position = "none")
      
    # Then, use ggplotly() to convert it into an interactive plotly visualization
       ggplotly(p, tooltip = c("x", "y")) %>%
          layout(hovermode = "closest")
      })
    
  
  # Use 'zdat()' & plotly::renderPlotly() to create plotly histograms displaying the player's Launch Angles and Exit Velocities
    
    output$plotly2 <- renderPlotly({
       filtered_df <- filter(zdat(), player_name == input$player_name2)
      
    # Highlight the bins that represent a Hard Hit ball
        filtered_df$fill_color <- ifelse(filtered_df$launch_speed >= 98, "lightgreen", "red")
  
    # First, Create the data visualization using ggplot()    
       p <- ggplot(filtered_df, aes(x = launch_speed, fill = fill_color)) +
        geom_histogram(binwidth = 1, color = "black") +
        labs(title = paste("Exit Velocity for", input$player_name2),
             x = "Exit Velocity", y = "Batted Balls")+
        xlim(15,130)+theme(legend.position = "none")
  
    # Then, use ggplotly() to convert it into an interactive plotly visualization
       ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "closest")
      })
    
    
  # Edit the hover data labels
    
    observeEvent(event_data("plotly_hover"), {
    hover_data <- event_data("plotly_hover")
      if (!is.null(hover_data)) {
        count <- hover_data$y
        value <- hover_data$x
        }
      })


# Spray Chart (Third tab)   
  # Use the 'get_years()' function to create a 3rd reactive element for the Spray Charts (Third tab)

   spdat <- reactive({
    get_years(spray_dat, input$year_range3)
    })
   
  # Use 'get_spraychart()' function to create the spray chart visualization based on the user inputs
  
    output$sprayplot <- renderPlot({
    get_spraychart(spdat(), input$player_name3, input$pitch_type3, input$hit_type)
      })
    
    output$nplot <- renderPlotly({
      get_launch_map(spdat(), input$player_name3, input$pitch_type3, input$hit_type)
    })
    
  
  # Use 'get_all_batter_data1()' function to create the first stats table for this player (BA, OBP, SLG, Barrel %, etc.)
  
   output$table4 <- renderTable({
     get_all_batter_data1(spdat(), input$player_name3)
     })
  
   
  # Use 'get_all_batter_data2()' function to create the second stats table for this player (Zone %, O-Swing %, Z-Contact %, etc.)
  
    output$table5 <- renderTable({
     get_all_batter_data2(spdat(), input$player_name3)
     })
  

# Swing Take Decisions (Fourth tab)
  # Use the 'get_years2()' function to create the reactive data frame
  
    ddat <- reactive({
     get_years2(swing_dat, input$year_range4, input$p_throws4, input$pitch_type4, input$count)
   })


  # Use the 'get_rv_plot()' function to create the 4-Zone Run Value graphic

    output$rv_plot <- renderPlot({
     get_rv_plot(ddat(), input$player_name4)
    })


  # Use 'get_loc_table()' to create a table that will display the amount of 'Swing Value' and 'Take Value' make up the overall 'Run Value' total

    output$table2 <- renderTable({
     df <- get_loc_table(ddat(), input$player_name4)
   
     df %>%
       summarise(across(c(run_value, swing_value, take_value), sum)) %>% 
       rename("Run Value" = run_value, "Swing Runs" = swing_value, "Take Runs" = take_value)
    })


  # Create the Swing% vs. Take% visualization (with League Average)

    output$plot3 <- renderPlot({
  
    # Use 'get_loc_table()' to get the requested player data
    
      data <- get_loc_table(ddat(), input$player_name4) 
    
    
    # Organize the 4 zones in the correct order 
    
      data$category <- factor(data$category, levels = c("waste", "chase", "shadow", "heart"))
    
    # 'Start' and 'End' will be used to position the bars in the visualization
    
      data$Start <- -data$swing_pct/2
      data$End <- data$take_pct/2
    
    
    # Use 'get_loc_table()' to get the relevant league average data
    
      lgdata <- get_lg_table(ddat())
      lgdata$category <- factor(data$category, levels = c("waste", "chase", "shadow", "heart"))
      lgdata$Start <- -lgdata$swing_pct/2
      lgdata$End <- lgdata$take_pct/2
    
    
    # Create the Swing% vs. Take% bar graph 
    
      pplot <- ggplot(data, aes(y = category)) +
       geom_segment(aes(x = Start, xend = End, y = category, yend = category, color = category),
                   size = 10, lineend = "butt") +
        geom_vline(xintercept = 0, color = "grey", size = .5, linetype = "solid") +
        geom_text(aes(x = -25, y = 4.5, label = "Swing"), hjust = 1, size = 6) +
        geom_text(aes(x = 25, y = 4.5, label = "Take"), hjust = 0, size = 6) +
        geom_text(aes(x = Start - 5, y = category, label = paste(swing_pct, "%")), hjust = .65, color = "black", size = 5) +
        geom_text(aes(x = End + 5, y = category, label = paste(take_pct, "%")), hjust = 0.38, color = "black", size = 5) +
        xlim(-55, 55) +
        scale_color_manual(values =  c("#E0DFDA", "#ffff7f", "#E9A4A7", "#E1AFDD")) +
        guides(color = FALSE) +
        theme_void() 
    
    
    # Create the league average bar graph
    
      lgplot <- ggplot(lgdata, aes(y = category)) +
        geom_segment(aes(x = Start, xend = End, y = category, yend = category),
                    size = 1.45, lineend = "square", linetype ="dotted", color = "grey") +
        geom_vline(xintercept = 0, color = "grey", size = .5, linetype = "solid") +
        geom_text(aes(x = Start - 5, y = category, label = paste(swing_pct, "%")), hjust = .65, color = "black", size = 4.5) +
        geom_text(aes(x = End + 5, y = category, label = paste(take_pct, "%")), hjust = 0.38, color = "black", size = 4.5) +
        xlim(-55, 55) +
        guides(color = FALSE) +
        theme_void() 
    
    
    # Use 'ggdraw()' to layer the 2 graphs
    
      p <- ggdraw()+draw_plot(pplot)+draw_plot(lgplot, x=0, y=-0.05, width=1, height=1)
      p
      
   })


  # Create the 'Run Value' visualization broken into 'Swing Runs' and 'Take Runs' for each of the 4 zones

    output$plot2 <- renderPlot({
  
      df <- get_loc_table(ddat(), input$player_name4)
  
      df <- df %>%
       mutate(category = factor(category, levels = c("waste", "chase", "shadow", "heart")))
  
  # Use tidyr::pivot_longer() to create a data frame that displays the 'swing_value' and 'take_value' for each of the 4 zones
  
    df_long <- pivot_longer(df, cols = c(take_value, swing_value), names_to = "variable", values_to = "value")
  
  
  # Create the double bar graph visualization
  # X-axis displays the Run Value, Y-axis displays each of the 4 categories, and the 'fill' color for each bar has to be factored so Swing Runs are on top
  
    cplot <- ggplot(df_long, aes(x = value, y = category, fill = factor(variable, levels = c("take_value", "swing_value")))) +
    
    # Orient the bar graph sideways and stylize the bar display
      geom_bar(stat = "identity", orientation = "y", position = "dodge", width = 0.6) +
    
    # Use geom_text() to display the run value and use ifelse() to position the text in the middle of the graph
      geom_text(aes(label = ifelse(variable == "swing_value" & category == "heart", paste(ifelse(value >= 0, paste("+", value), value), "Swing Runs"), ""), 
                  x = ifelse(value >= 0, -20, 20)), position = position_dodge(width = 0.75), vjust = 0.5, color = "black", size = 6) +
      geom_text(aes(label = ifelse(variable == "take_value" & category == "heart", paste(ifelse(value >= 0, paste("+", value), value), "Take Runs"), ""), 
                  x = ifelse(value >= 0, -20, 20)), position = position_dodge(width = 0.75), vjust = 0.5, color = "black", size = 6) +
      geom_text(aes(label = ifelse(category %in% c("shadow", "chase", "waste"), ifelse(value >= 0, paste("+", value), value), ""), 
                  x = ifelse(value >= 0, -8.5, 8.5)), position = position_dodge(width = 0.75), vjust = 0.5, color = "black", size = 6) +
    
    # Set the limits of the graph
      coord_cartesian(xlim = c(-40, 40)) +
    
    
    # This places a horizontal line in between the 2 bars for each category and color codes them 
      geom_hline(yintercept = c(1, 2, 3, 4), color = c("#E0DFDA", "#ffff7f", "#E9A4A7", "#E1AFDD"), size = 2) +
    
    
    # Create custom colors for 'Swing Runs' and 'Take Runs'
      scale_fill_manual(values = c("#e1ad01", "#6bd600"), guide = FALSE) +
      labs(title = "Run Value", x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(vjust = 175, hjust = 0.5, angle = 0),
          plot.title = element_text(vjust = 2.5, hjust = 0.5, size = 16))
  
    cplot
  
    })
  
  # Create a table of the information displayed in the graphics
    output$table <- renderTable({
  
      table <- get_loc_table(ddat(), input$player_name4)
  
      table %>% rename("Zone" = category, "Count" = count, "Usage %" = usage, "Swing %" = swing_pct, "Take %" = take_pct, 
                   "Run Value" = run_value, "Swing Runs" = swing_value, "Take Runs" = take_value)
    })
  
  
  # Create a table of the inputted batter's swing decisions (Chase %, Contact %, Z-Swing %, etc.)
  
    output$table6 <- renderTable({
    
    get_all_batter_data2(ddat(), input$player_name4)
    
    })
    
# Trending Charts (Fifth Tab)
  # Create a reactive data frame that changes the info in the rs23$game_date from character type to Date type
    
    trdat <- reactive ({
      trend_dat %>% mutate(game_date = as.Date(game_date))
    })
    
    
  # Use get_slash_trend() to create the first Plotly visualization (BA, OBP, SLG)
    
    output$slash_trend <- renderPlotly({
      get_slash_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
    
  # Use get_KBB_trend() to create the second Plotly visualization (K%, BB%, CsW%, Take%)
    
    output$KBB_trend <- renderPlotly({
      get_KBB_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
    
  # Use get_plate_disc_trend() to create the third Plotly visualization (O-Swing%, O-Con%, Z-Swing%, Z-Con%)
    
    output$plate_disc_trend <- renderPlotly({
      get_plate_disc_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
    
  # Use get_wOBA_trend() to create the fourth Plotly visualization (wOBA, xwOBA)
    
    output$wOBA_trend <- renderPlotly({
      get_wOBA_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
    
  # Use get_barrel_trend() to create the fifth Plotly visualization (HH%, SwSpot%, Barrel%)
    
    output$barrel_trend <- renderPlotly({
      get_barrel_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
    
  # Use get_batted_ball_trend() to create the sixth Plotly visualization (GB%, LD%, FB%, PU%)
    
    output$batted_ball <- renderPlotly({
      get_batted_ball_trend(trdat(), input$pitch_type5, input$date_range[1], input$date_range[2], input$player_name5)
    })
}


# Run the app

shinyApp(ui, server)
