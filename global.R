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
library(tidyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(plotly)
library(bslib)
library(googlesheets4)

gs4_auth(token = gargle::secret_read_rds(
  ".secrets/gs4-token.rds",
  key = "GARGLE_KEY"))

points <- readRDS("Cul_wqpoints.rds") %>% 
  rename(Site = Site.ID) %>%
  arrange(Location, Site)

process_qualifiers = function(data){
  data |>
    mutate(Qual = gsub('\\d|\\.|-', "", Value),                # assign quals as any non digit using regex
           Value = as.numeric(gsub('<|>|!|?|,', "", Value)))   # remove qualifying characters from Value column
}

calc_kdpar <- function(surf_lwr, surf_upr, bot_lwr, bot_upr){
  abs(log((surf_lwr/surf_upr)/(bot_lwr/bot_upr)))
}

wslab_raw = read_sheet("https://docs.google.com/spreadsheets/d/1qWWiaY-w2_Z-NJINq-mnOCpXk_fTmuQxA0sgXArYbgg",
                       col_types = "c", na = c("", ".", "NA", "N/A", "Error"))

wslab = wslab_raw |>
  select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Time")) |>
  rename(Date = `Date of Monitoring`, Site = `Sample ID`) |>
  pivot_longer(cols = !c(Date, Site), names_to = "Parameter", values_to = "Value")

wsfield_raw = read_sheet("https://docs.google.com/spreadsheets/d/1QA9c1yXKe87fepSy2IrKXdG5lwptLW3lu7fKk-ahDsc/",
                         col_types = "c", na = c("", ".", "NA", "N/A", "Error"))

wsfield = wsfield_raw |>
  select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Sample Time")) |>
  rename(Date = `Date of Monitoring`, Site = `Sample ID`, `DO (mg/l)` = `DO  mg/l`,
         `Chl-a red (ug/l)` = `Chla Red (ug/l)`, `Chl-a blue (ug/l)` = `Chl a  blue (ug/l)`,
         `Temperature (C)` = `Temperature (°C)`, `Conductivity (uS/cm)` = `Conductivity (μS/cm)`) |>
  pivot_longer(cols = !c(Date, Site), names_to = "Parameter", values_to = "Value")

ws = process_qualifiers(bind_rows(wslab, wsfield)) |>
  filter(!is.na(Value)) |>
  mutate(Date = mdy(Date))

ns_raw = read_sheet("https://docs.google.com/spreadsheets/d/1O3O3QfYCOVuQg-1aztPyQRtN_W9uO2i4yToqhNFGZ1Y/",
                    col_types = "c", na = c("", ".", "NA", "N/A", "Error"))

ns_tmp = ns_raw |>
  select(!c("Timestamp", "Time", "Data recorded by:", "Data entered by:", "Measurements completed and samples collected?",
            "Sea State", "Wind Direction", "Wind Speed", "Cloud Cover (%) 0 - 100", "Surface Sample Time",
            "Bottom Sample Time", "Notes (if applicable)")) |>
  rename(Date = `Date of Monitoring`, Site = `Site ID`, `Bottom Chl-a Fluorescence (µg/L)` = `Bottom Chl-a  Fluorescence (µg/L)`,
         `Enterococci (MPN/100ml)` = `Enterococci MPN/100ml (be sure to convert)`) |>
  pivot_longer(cols = !c(Date, Site), names_to = "ParameterRaw", values_to = "Value") |>
  mutate(Date = mdy(Date),
         SampleLevel = case_when(
           grepl("Surface", ParameterRaw) ~ "Surface",
           grepl("Bottom", ParameterRaw) ~ "Bottom",
           .default = "N/A"),
         Site = ifelse(Site == "Fulladosa  Ramp", "Fulladosa Ramp", Site),
         Parameter = gsub("Bottom |Bottom -|Bottom - |Surface |Surface -|Surface - ", "", ParameterRaw),
         Parameter = gsub("µ|μ", "u", Parameter)) |>
  process_qualifiers() |>
  select(-ParameterRaw)

kdpar = ns_tmp |>
  filter(grepl("Apogee", Parameter)) |>
  pivot_wider(id_cols = c("Date", "Site"), names_from = c("SampleLevel", "Parameter"), values_from = "Value") |>
  mutate(Value = calc_kdpar(`Surface_Lower Apogee sensor (Channel B)`,
                            `Surface_Upper Apogee sensor (Channel A)`,
                            `Bottom_Lower Apogee (Channel B)`,
                            `Bottom_Upper Apogee (Channel A)`),
         Parameter = "Normalized KdPAR",
         SampleLevel = "N/A") |>
  select(Date, Site, SampleLevel, Parameter, Value)

ns = bind_rows(kdpar, ns_tmp) |>
  filter(!is.na(Value))


