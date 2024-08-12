
# Packages ----------------------------------------------------

options(dplyr.summarise.inform = FALSE)
library(shinyWidgets)
library(shiny) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(leaflegend)
library(plotly)
library(bslib)
library(googlesheets4)
library(sf)

default_params = c("Water Quality" = "Temperature (C)",
                   "Nutrients" = "Nitrate (mg/L)",
                   "Seagrass" = "Thalassia Density")

# Functions ---------------------------------------------------------------

percentile <- function(x){
  rank_x = rank(x)
  rank_x/max(rank_x) * 100
}

process_chars = function(data){
  data |>
    mutate(Chars = gsub('\\d|\\.', "", Value),                   # assign chars as any non digit using regex
           Value = as.numeric(gsub('<|>|!|?|,', "", Value)))     # remove characters from Value column
}

calc_kdpar <- function(surf_lwr, surf_upr, bot_lwr, bot_upr){
  abs(log((surf_lwr/surf_upr)/(bot_lwr/bot_upr)))
}

# Auth --------------------------------------------------------------------

gs4_auth(token = gargle::secret_read_rds(
  ".secrets/gs4-token.rds",
  key = "GARGLE_KEY"))

# Drainages ---------------------------------------------------------------

drainages = st_read(file.path("data", "ContributingDrainageAreas.shp")) |> 
  st_transform(crs = 4326)

# Water Quality -----------------------------------------------------------

station_locations <- read.csv(file.path("data", "CulebraWQ-StationLocations.csv"))

## Watershed ---------------------------------------------------------------

wslab_raw = read_sheet("https://docs.google.com/spreadsheets/d/1qWWiaY-w2_Z-NJINq-mnOCpXk_fTmuQxA0sgXArYbgg", col_types = "c")

wslab = wslab_raw |>
  select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Time")) |>
  rename(Date = `Date of Monitoring`, Station = `Sample ID`, `Escherichia Coli (100ml)` = `Escherichia  Coli (100ml)`) |>
  pivot_longer(cols = !c(Date, Station), names_to = "Parameter", values_to = "Value")

wsfield_raw = read_sheet("https://docs.google.com/spreadsheets/d/1QA9c1yXKe87fepSy2IrKXdG5lwptLW3lu7fKk-ahDsc/", col_types = "c")

wsfield = wsfield_raw |>
  select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Sample Time")) |>
  rename(Date = `Date of Monitoring`, Station = `Sample ID`, `DO (mg/l)` = `DO  mg/l`,
         `Chl-a red (ug/l)` = `Chla Red (ug/l)`, `Chl-a blue (ug/l)` = `Chl a  blue (ug/l)`,
         `Temperature (C)` = `Temperature (°C)`, `Conductivity (uS/cm)` = `Conductivity (μS/cm)`) |>
  pivot_longer(cols = !c(Date, Station), names_to = "Parameter", values_to = "Value")

# only including 5 stations; ordered roughly west to east
ws_stations = c("Old Man Plaza", "AeropuertoY", "Coronel Pond", "Bridge", "Turtle Stream")

ws = process_chars(bind_rows(wslab, wsfield)) |>
  filter(!is.na(Value) & !(Station %in% c("P3_out", "P4_out", "P5_out", "Plant"))) |>
  mutate(Date = mdy(Date),
         Station = ifelse(Station == "Stream", "Turtle Stream", Station),
         Station = factor(Station, levels = ws_stations)) |> 
  select(-Chars)

ws_colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854") |> 
  setNames(ws_stations)

## Nearshore ---------------------------------------------------------------

ns_grp_colors = c("Restoration" = "#377eb8",
                  "Negative Reference" = "#e41a1c",
                  "Control" = "#ff7f00",             # alternatively, "#ffff33""
                  "Positive Reference" = "#4daf4a") 

ns_grps = read.csv(file.path("data", "NearshoreTrtGroups.csv")) |> 
  arrange(Order) |> 
  mutate(Group = factor(Group, levels = names(ns_grp_colors)),
         GroupStation = paste0(Group, "\n", Station))

ns_raw = read_sheet("https://docs.google.com/spreadsheets/d/1O3O3QfYCOVuQg-1aztPyQRtN_W9uO2i4yToqhNFGZ1Y/", col_types = "c")

ns_tmp = ns_raw |>
  select(!c("Timestamp", "Time", "Data recorded by:", "Data entered by:", "Measurements completed and samples collected?",
            "Sea State", "Wind Direction", "Wind Speed", "Cloud Cover (%) 0 - 100", "Surface Sample Time",
            "Bottom Sample Time", "Notes (if applicable)")) |>
  rename(Date = `Date of Monitoring`, Station = `Site ID`, `Bottom Chl-a Fluorescence (µg/L)` = `Bottom Chl-a  Fluorescence (µg/L)`,
         `Enterococci (MPN/100ml)` = `Enterococci MPN/100ml (be sure to convert)`) |>
  pivot_longer(cols = !c(Date, Station), names_to = "ParameterRaw", values_to = "Value") |>
  mutate(Date = mdy(Date),
         SampleLevel = case_when(
           grepl("Surface", ParameterRaw) ~ "Surface",
           grepl("Bottom", ParameterRaw) ~ "Bottom",
           .default = "N/A"),
         Station = ifelse(Station == "Fulladosa  Ramp", "Fulladosa Ramp", Station),
         Parameter = gsub("Bottom |Bottom -|Bottom - |Surface |Surface -|Surface - ", "", ParameterRaw),
         Parameter = gsub("µ|μ", "u", Parameter)) |>
  process_chars() |>
  select(-ParameterRaw)

kdpar = ns_tmp |>
  filter(grepl("Apogee", Parameter)) |>
  pivot_wider(id_cols = c("Date", "Station"), names_from = c("SampleLevel", "Parameter"), values_from = "Value") |>
  mutate(Value = calc_kdpar(`Surface_Lower Apogee sensor (Channel B)`,
                            `Surface_Upper Apogee sensor (Channel A)`,
                            `Bottom_Lower Apogee (Channel B)`,
                            `Bottom_Upper Apogee (Channel A)`),
         Parameter = "Normalized KdPAR",
         SampleLevel = "N/A") |>
  select(Date, Station, SampleLevel, Parameter, Value)

ns = bind_rows(kdpar, ns_tmp)|> 
  filter(!is.na(Value)) |>
  left_join(ns_grps) |> 
  filter(!is.na(Group)) |> 
  select(Date, Group, Station, GroupStation, SampleLevel, Parameter, Value) |> 
  arrange(Date, Group, Station) |> 
  mutate(Station = factor(Station, levels = ns_grps$Station),
         GroupStation = factor(GroupStation, levels = ns_grps$GroupStation))

ns_stations = lapply(names(ns_grp_colors), function(x) ns_grps$Station[ns_grps$Group == x]) |> 
  setNames(names(ns_grp_colors))

# Seagrass ----------------------------------------------------------------

sg_years = c(2014, 2022)
sg = read.csv(file.path("data", "SeagrassProcessed.csv")) |>
  left_join(ns_grps) |> 
  filter(!is.na(Group)) |> 
  select(Year, Group, Station, GroupStation, Parameter, Value) |> 
  arrange(Year, Group, Station) |> 
  mutate(Station = factor(Station, levels = ns_grps$Station),
         GroupStation = factor(GroupStation, levels = ns_grps$GroupStation))

# Nutrients ---------------------------------------------------------------

nut = read.csv(file.path("data", "NutrientsProcessed.csv")) |> 
  left_join(select(station_locations, Station, Environment)) |> 
  mutate(Date = ymd(Date))

nut_ws = nut |> 
  filter(Environment == "Watershed" & 
           !(Station %in% c("P3_out", "P4_out", "P5_out", "Plant"))) |>
  select(Date, Station, SampleLevel, Parameter, Value) |> 
  mutate(Station = factor(Station, levels = ws_stations))

nut_ns = nut |> 
  filter(Environment == "Nearshore") |> 
  left_join(ns_grps) |> 
  filter(!is.na(Group)) |> 
  select(Date, Group, Station, GroupStation, SampleLevel, Parameter, Value) |> 
  arrange(Date, Group, Station) |> 
  mutate(Station = factor(Station, levels = ns_grps$Station),
         GroupStation = factor(GroupStation, levels = ns_grps$GroupStation))
