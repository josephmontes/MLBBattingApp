# Use 'alldat' to isolate the data needed for the 13 zones data visualization (Second Tab)

  zonedat <- alldat %>% select(p_throws, pitch_name, pitch_type, description, launch_speed, launch_angle, delta_run_exp, zone, player_name, game_year)


# Create the get_zone_data() function that outputs the main 13 zone visualization (this function is very large)
  # 'df' argument will come from 'zonedat' once it is filtered through the get_years() function prompted by the user in the app
  # The rest of the arguments in this function will be inputs that are selected by the user in the Rshiny app
  
  get_zone_data <- function(df, Player, type = "whiff", pitch_type, p_throws) {
    
    
    # Filter the data by pitcher handedness ('pidf')
    
    if(p_throws == "R"){
      pidf <- df %>%  filter(p_throws=="R")
    } else if (p_throws == "L"){
      pidf <- df %>%  filter(p_throws== "L")
    } else if (p_throws == "B"){
      pidf <- df
    }
    
    
    # Filter the data by pitch type ('pitch_df')
    
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
     # Create the 'lg_dat' object using 'get_lg_table2()' function to get the league averages
    
      lg_dat <- get_lg_table2(pitch_df)

    
     # Use the object 'lg_dat' to isolate the appropriate values for each stat by zone
    
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

# The EV and LA Plotly histograms displayed in this Second Tab are built in the RShiny server
 # See 'UIServer.RShiny' lines 648 - 698
 # Or see 'FullMLBBattingApp.R' lines 2759 - 2809
