



#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)
library(shiny)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(shinyWidgets)
library(cowplot)
library(thematic)
library(shinythemes)
library(shinyBS)

# Turn on automatic theming
thematic_on()
onStop(thematic_off)

# Output files
outputFiles <- list.files("shiny/data/")


# Load data
dailyDeaths <- read_csv(paste0("shiny/data/", "deaths_daily.csv")) %>%
    mutate(Date = as.Date(Date), date_model_run = as.Date(today())) %>%
    mutate(Metric = "Daily Deaths")

cumulativeDeaths <- read.csv(paste0("data/", outputFiles[which(grepl("deaths-cumulative", outputFiles))])) %>%
    mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
    mutate(Metric = "Cumulative Deaths")

dailyInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-daily", outputFiles))])) %>%
    mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
    mutate(Metric = "Daily Infections")

cumulativeInfected <- read.csv(paste0("data/", outputFiles[which(grepl("infected-cumulative", outputFiles))])) %>%
    mutate(Date = as.Date(Date), date_model_run = as.Date(date_model_run)) %>%
    mutate(Metric = "Cumulative Infections")

# Format data
# General
data <- bind_rows(dailyDeaths, dailyInfected, cumulativeDeaths, cumulativeInfected) %>%
    mutate(DataType = ifelse((Metric %in% c("Daily Deaths", "Cumulative Deaths")) & (Date < date_model_run), "Observed", "Modeled")) %>%
    mutate(DataType = ordered(DataType, level=c("Observed", "Modeled"))) %>%
    mutate(Metric = ordered(Metric, levels=c("Daily Infections", "Daily Deaths", "Cumulative Infections", "Cumulative Deaths")))

# Duplicate last observed date to make it also the first modeled date
firstModeled <- data %>%
    filter(DataType == "Observed") %>% # Keep only observations
    filter(Date == date_model_run - 1) %>% # Keep only data for the day before a model run
    mutate(DataType = "Modeled") %>% # Assign it as modeled data
    mutate(DataType = ordered(DataType, level=c("Observed", "Modeled"))) %>%
    mutate(Lower = Mean, Upper = Mean) # Assign lower and upper bounds = mean

# Add to master dataset
data %<>% bind_rows(., firstModeled) %>%
    arrange(Metric, date_model_run, Jurisdiction, Date)





