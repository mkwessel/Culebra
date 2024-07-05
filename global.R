##########################################################################
# Project: Culebra Water Quality Dashboard
# 
# Code Development by: Mike Wessel, Inferential Consulting,LLC
# Inferential.consulting@gmail.com

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in the upper right.
#

# Github Repo: https://github.com/mikewessel/NOAA-Culebra-LBSP/analysis
#
##########################################################################


library(shinyWidgets)
library(shiny) 
library(dplyr)
library(ggplot2)
library(conflicted)
library(lubridate)
library(leaflet)
library(plotly)
library(bslib)

conflicts_prefer(dplyr::filter, dplyr::lag)

#### Grab the data

params = read.csv("CulParams.csv") |> 
  mutate(ParameterUnits = paste0(Parameter, " (", Units, ")"))

dat <- readRDS("CulWQ_clean-2024-05-18.rds") %>%
  # μ character wasn't read correctly from CulParams.csv so removed it
  mutate(Param = ifelse(Param == "Surface.Conductivity..μS.cm.", "Surface.Conductivity",
                        ifelse(Param == "Bottom.Conductivity..μS.cm.", "Bottom.Conductivity", Param))) %>% 
  select(-Units) %>%
  arrange(Location, Site.ID, Date) %>%
  left_join(params) %>%
  rename(Site = Site.ID) %>%
  filter(Parameter != "Turbidity hach") # no non missing values

points <- readRDS("Cul_wqpoints.rds") %>% 
  rename(Site = Site.ID) %>%
  arrange(Location, Site)