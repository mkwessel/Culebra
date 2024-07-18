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

points <- readRDS(file.path("data", "Cul_wqpoints.rds")) |>
  rename(Station = Site.ID) |>
  arrange(Location, Station)

ns_grps = read.csv(file.path("data", "NearshoreTrtGroups.csv")) |> 
  mutate(Station2 = ifelse(grepl("Puerto Manglar", Station), "Puerto Manglar", Station))

ns_grp_colors = c("LBSP Restoration" = "#377eb8", 
                  "Positive Reference" = "#4daf4a",
                  "LBSP Control" = "#ff7f00",         # alternatively, "#ffff33""
                  "Negative Reference" = "#e41a1c") 

process_chars = function(data){
  data |>
    mutate(Chars = gsub('\\d|\\.', "", Value),                   # assign chars as any non digit using regex
           Value = as.numeric(gsub('<|>|!|?|,', "", Value)))     # remove characters from Value column
}

calc_kdpar <- function(surf_lwr, surf_upr, bot_lwr, bot_upr){
  abs(log((surf_lwr/surf_upr)/(bot_lwr/bot_upr)))
}

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

ws = process_chars(bind_rows(wslab, wsfield)) |>
  filter(!is.na(Value)) |>
  mutate(Date = mdy(Date))

ws_stations = sort(unique(ws$Station))

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
  select(Date, Group, Station = Station2, SampleLevel, Parameter, Value) |> 
  mutate(Group = factor(Group, levels = names(ns_grp_colors)))

ns_stations = lapply(names(ns_grp_colors), function(x) sort(ns_grps$Station2[ns_grps$Group == x])) |> 
  setNames(names(ns_grp_colors))


# # For Bernardo Vargas-Angel
# 
# wslab_bva = wslab_raw |>
#   rename(`Escherichia Coli (100ml)` = `Escherichia  Coli (100ml)`) |>
#   pivot_longer(cols = !c("Timestamp", "Date of Monitoring", "Time", "Samples collected by:", "Data entered by:", "Notes:", "Sample ID"), 
#                names_to = "Parameter", values_to = "Value") |> 
#   process_chars() |> 
#   mutate(Timestamp = as.character(mdy_hms(Timestamp)),
#          `Date of Monitoring` = as.character(mdy(`Date of Monitoring`))) |> 
#   select(c("Timestamp", "Date of Monitoring", "Time", "Samples collected by:", "Data entered by:", 
#            "Sample ID", "Parameter", "Value", "Chars", "Notes:"))
# 
# wsfield_bva = wsfield_raw |>
#   rename(`DO (mg/l)` = `DO  mg/l`, `Chl-a red (ug/l)` = `Chla Red (ug/l)`, `Chl-a blue (ug/l)` = `Chl a  blue (ug/l)`,
#          `Temperature (C)` = `Temperature (°C)`, `Conductivity (uS/cm)` = `Conductivity (μS/cm)`) |>
#   pivot_longer(cols = !c("Timestamp", "Date of Monitoring", "Sample Time", "Samples collected by:", "Data entered by:", "Notes:", "Sample ID"), 
#                names_to = "Parameter", values_to = "Value") |> 
#   process_chars() |> 
#   mutate(Timestamp = as.character(mdy_hms(Timestamp)),
#          `Date of Monitoring` = as.character(mdy(`Date of Monitoring`))) |> 
#   select(c("Timestamp", "Date of Monitoring", "Sample Time", "Samples collected by:", "Data entered by:", 
#            "Sample ID", "Parameter", "Value", "Chars", "Notes:"))
# 
# ns_bva = ns_raw |>
#   select(!c("Measurements completed and samples collected?", "Sea State", "Wind Direction", "Wind Speed", "Cloud Cover (%) 0 - 100",)) |>
#   rename(`Bottom Chl-a Fluorescence (ug/L)` = `Bottom Chl-a  Fluorescence (µg/L)`,
#          `Enterococci (MPN/100ml)` = `Enterococci MPN/100ml (be sure to convert)`) |>
#   pivot_longer(cols = !c("Timestamp", "Date of Monitoring", "Time", "Surface Sample Time", "Bottom Sample Time", 
#                          "Data recorded by:", "Data entered by:", "Notes (if applicable)", "Site ID"), 
#                names_to = "ParameterRaw", values_to = "Value") |>
#   mutate(Timestamp = as.character(mdy_hms(Timestamp)),
#          `Date of Monitoring` = as.character(mdy(`Date of Monitoring`)),
#          `Sample Level` = case_when(
#            grepl("Surface", ParameterRaw) ~ "Surface",
#            grepl("Bottom", ParameterRaw) ~ "Bottom",
#            .default = "N/A"),
#          `Site ID` = ifelse(`Site ID` == "Fulladosa  Ramp", "Fulladosa Ramp", `Site ID`),
#          Parameter = gsub("Bottom |Bottom -|Bottom - |Surface |Surface -|Surface - ", "", ParameterRaw),
#          Parameter = gsub("µ|μ", "u", Parameter)) |>
#   process_chars() |>
#   select(c("Timestamp", "Date of Monitoring", "Time", "Surface Sample Time", "Bottom Sample Time", 
#            "Data recorded by:", "Data entered by:", "Site ID", "Sample Level", "Parameter", "Value", 
#            "Chars", "Notes (if applicable)"))
# 
# writexl::write_xlsx(list("Watershed (lab)" = wslab_bva,
#                          "Watershed (field)" = wsfield_bva,
#                          "Nearshore" = ns_bva),
#                     paste0("Culebra-WQData-", Sys.Date(), ".xlsx"))
