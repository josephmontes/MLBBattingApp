# Trending Line Graphs 
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
