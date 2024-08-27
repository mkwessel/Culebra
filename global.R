
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

default_params = c("Routine Water Quality" = "Temperature (C)",
                   "Special Nutrient Collection" = "Nitrate (mg/L)",
                   "Seagrass" = "Thalassia Density (shoots/100 cm2)")

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

# Spatial ---------------------------------------------------------------

# drainages = st_read(file.path("data", "ContributingDrainageAreas.shp")) |> 
#   st_transform(crs = 4326)

drainages = lapply(list("Nearshore" = st_read(file.path("data", "CDA_NearshoreMonitoringSites.shp")),
                        "Watershed" = st_read(file.path("data", "CDA_WatershedMonitoringSites.shp"))),
                   function(dfx) st_transform(dfx, crs = 4326))

station_locations <- read.csv(file.path("data", "CulebraWQ-StationLocations.csv"))

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

## Watershed ---------------------------------------------------------------

get_wslab_raw <- function(){
  read_sheet("https://docs.google.com/spreadsheets/d/1qWWiaY-w2_Z-NJINq-mnOCpXk_fTmuQxA0sgXArYbgg", col_types = "c")
}

get_wslab <- function(){
  get_wslab_raw() |>
    select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Time")) |>
    rename(Date = `Date of Monitoring`, Station = `Sample ID`, `Escherichia Coli (100ml)` = `Escherichia  Coli (100ml)`) |>
    pivot_longer(cols = !c(Date, Station), names_to = "Parameter", values_to = "Value")
}

get_wsfield_raw <- function(){
  read_sheet("https://docs.google.com/spreadsheets/d/1QA9c1yXKe87fepSy2IrKXdG5lwptLW3lu7fKk-ahDsc/", col_types = "c")
}

get_wsfield <- function(){
  get_wsfield_raw() |>
    select(!c("Timestamp", "Samples collected by:", "Data entered by:", "Notes:", "Sample Time")) |>
    rename(Date = `Date of Monitoring`, Station = `Sample ID`, `DO (mg/l)` = `DO  mg/l`,
           `Chl-a red (ug/l)` = `Chla Red (ug/l)`, `Chl-a blue (ug/l)` = `Chl a  blue (ug/l)`,
           `Temperature (C)` = `Temperature (°C)`, `Conductivity (uS/cm)` = `Conductivity (μS/cm)`) |>
    pivot_longer(cols = !c(Date, Station), names_to = "Parameter", values_to = "Value")
}

# only including 5 stations; ordered roughly west to east
ws_stations = c("Old Man Plaza", "AeropuertoY", "Coronel Pond", "Bridge", "Turtle Stream")

prep_ws <- function(ws_stations){
  process_chars(bind_rows(get_wslab(), get_wsfield())) |>
    filter(!is.na(Value) & !(Station %in% c("P3_out", "P4_out", "P5_out", "Plant"))) |>
    mutate(Date = mdy(Date),
           Station = ifelse(Station == "Stream", "Turtle Stream", Station),
           Station = factor(Station, levels = ws_stations)) |> 
    select(-Chars)
}

ws_colors = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854") |> 
  setNames(ws_stations)

# Seagrass ----------------------------------------------------------------

sg_years = c(2014, 2022)
sg_params = read.csv(file.path("data", "SeagrassParameters.csv"))
sg_url = "https://docs.google.com/spreadsheets/d/1zkGxY3KS0fJNv46btX0j6cbzdzQH6ON2dxa1CVQpm3k"

get_sg_2014 <- function(sg_url){
  read_sheet(sg_url, sheet = "2014TransectData", range = "A:S", col_types = "c", .name_repair = "universal") |>
    rename(SED.INDEX = SED_INDEX)
}

get_sg_2022 <- function(sg_url){
  read_sheet(sg_url, sheet = "2022TransectData", range = "A:V", col_types = "c", .name_repair = "universal")
}

get_sg <- function(sg_url, sg_params, ns_grps){
  sg_2014 = get_sg_2014(sg_url)
  sg_2022 = get_sg_2022(sg_url)
  
  bind_rows(mutate(setNames(sg_2014, toupper(names(sg_2014))), Year = 2014), 
            mutate(setNames(sg_2022, toupper(names(sg_2022))), Year = 2022)) |> 
    filter(!(SITE %in% c("Tamarindo", "Terruno")) & !grepl("EOB", TRANSECT)) |>
    mutate(SITE = case_when(SITE == "Fulladoza.Point" ~ "Fulladosa Point",
                            SITE == "Fulladoza.Ramp" ~ "Fulladosa Ramp",
                            SITE == "Fulladoza.Bay" ~ "Fulladosa Bay",
                            SITE == "Casa.Azul" ~ "Casa Azul",
                            SITE == "Little.Cabra" ~ "Little Cabra",
                            SITE == "Aereopuerto" ~ "Aeropuerto",
                            SITE == "Puerto Manglar_Reference" ~ "Puerto Manglar Reference",
                            SITE == "Puerto Manglar_1" ~ "Puerto Manglar Treatment",
                            SITE == "Puerto Manglar_Impaired" ~ "Puerto Manglar Control",
                            TRUE ~ SITE)) |> 
    pivot_longer(cols = all_of(sg_params$Code), names_to = "Code", values_to = "Value") |> 
    left_join(sg_params) |> 
    mutate(Value = as.numeric(Value),
           Type2 = ifelse(Parameter != "Epiphytes" & Type == "Index", "%Cover", Type),
           Parameter2 = ifelse(Parameter == "Epiphytes", Parameter, paste(Parameter, Type2)),
           Parameter2 = ifelse(grepl("Density", Parameter2), paste(Parameter2, "(shoots/100 cm2)"), Parameter2),
           Value2 = ifelse(Type2 != "%Cover", Value,
                           case_when(Value == 0 ~ 0,
                                     Value == 0.1 ~ 1,
                                     Value == 0.5 ~ 2.5,
                                     Value == 1 ~ 5,
                                     Value == 2 ~ 15,
                                     Value == 3 ~ 37.5,
                                     Value == 4 ~ 62.5,
                                     Value == 5 ~ 87.5,
                                     TRUE ~ NA_real_))) |> 
    select(Year, Station = SITE, Transect = TRANSECT, Parameter = Parameter2, Value = Value2) |>
    filter(!is.na(Value)) |> 
    arrange(Year, Station, Parameter) |>
    left_join(ns_grps) |> 
    filter(!is.na(Group)) |> 
    select(Year, Group, Station, GroupStation, Parameter, Value) |> 
    arrange(Year, Group, Station) |> 
    mutate(Station = factor(Station, levels = ns_grps$Station),
           GroupStation = factor(GroupStation, levels = ns_grps$GroupStation))
}

# Nutrients ---------------------------------------------------------------

nut_stn = read.csv(file.path("data", "NutrientStations.csv"))
nut_url = "https://docs.google.com/spreadsheets/d/1weam_x6m9dIZWfiNO0ylrKlA3Lghr36RJfu8zx7raLw/"

get_nut_meta <- function(nut_url){
  read_sheet(nut_url, sheet = "Inventory") |>
    mutate(Blank = ifelse(grepl("blank", Comments), "Yes", "No"),
           SampleLevel = case_when(
             grepl("surface", Comments) ~ "Surface",
             grepl("bottom", Comments) ~ "Bottom",
             .default = "N/A")) |>
    select(-Type, -Matrix, -Comments, -SDG)
}

get_nut <- function(nut_url){
  left_join(read_sheet(nut_url, sheet = "Dissolved"),
            read_sheet(nut_url, sheet = "Total")) |>
    left_join(get_nut_meta(nut_url)) |>
    filter(Blank == "No") |>
    mutate(`Nitrate + Nitrite (mg/L)` = `Nitrate (mg/L)` + `Nitrite (mg/L)`) |>
    select(Date = CollectionDate, LabID, SampleLevel, contains("mg/L")) |>
    pivot_longer(cols = !c(Date, LabID, SampleLevel), names_to = "Parameter", values_to = "Value") |>
    left_join(nut_stn) |>
    filter(!is.na(Value) & !is.na(Station)) |>
    group_by(Date, Station, SampleLevel, Parameter) |>
    # multiple observations for each date/station; for now, taking max value for each date/station
    summarise(Value = max(Value, na.rm = TRUE)) |>
    left_join(select(station_locations, Station, Environment))
}

get_nut_ws <- function(nut, ws_stations){
  nut |> 
    ungroup() |> 
    filter(Environment == "Watershed" & 
             !(Station %in% c("P3_out", "P4_out", "P5_out", "Plant"))) |>
    select(Date, Station, Parameter, Value) |> 
    mutate(Station = factor(Station, levels = ws_stations))
}

get_nut_ns <- function(nut, ns_grps){
  nut |> 
    filter(Environment == "Nearshore") |> 
    left_join(ns_grps) |> 
    filter(!is.na(Group)) |> 
    select(Date, Group, Station, GroupStation, SampleLevel, Parameter, Value) |> 
    arrange(Date, Group, Station) |> 
    mutate(Station = factor(Station, levels = ns_grps$Station),
           GroupStation = factor(GroupStation, levels = ns_grps$GroupStation))
}

