#Load packages

library(shiny)
library(tidyverse)
library(baseballr)
library(cowplot)
library(plotly)

# Load the Statcast data from 2019-2023
 # After using baseballr::scrape_statcast_savant() to get each season's data, I loaded them individually into CSVs

s19 <- read.csv("season2019.csv")
s20 <- read.csv("season2020.csv")
s21 <- read.csv("season2021.csv")
s22 <- read.csv("season2022.csv")
rs23 <- read.csv("season231h.csv")
  
alldat <- rbind(s19, s20, s21, s22, rs23)


# Create the get_years() function that is used to filter seasons based on the user inputs (First, Second & Third tabs)

  get_years <- function(df, years){
    filtered_data <- df %>% filter(game_year %in% years)
    return(filtered_data)
    
  }
