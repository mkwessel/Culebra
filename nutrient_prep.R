library(dplyr)
library(tidyr)
library(readxl)

nut_stn = read.csv(file.path("data", "NutrientStations.csv"))
nut_path = "Nutrients.xlsx"

nut_meta = read_xlsx(file.path("data", nut_path), sheet = "Inventory") |> 
  mutate(Blank = ifelse(grepl("blank", Comments), "Yes", "No"),
         SampleLevel = case_when(
           grepl("surface", Comments) ~ "Surface",
           grepl("bottom", Comments) ~ "Bottom",
           .default = "N/A")) |> 
  select(-Type, -Matrix, -Comments, -SDG)

nut = left_join(read_xlsx(file.path("data", nut_path), sheet = "Dissolved"), 
                read_xlsx(file.path("data", nut_path), sheet = "Total")) |> 
  left_join(nut_meta) |> 
  filter(Blank == "No") |> 
  mutate(`Nitrate + Nitrite (mg/L)` = `Nitrate (mg/L)` + `Nitrite (mg/L)`) |> 
  select(Date = CollectionDate, LabID, SampleLevel, contains("mg/L")) |> 
  pivot_longer(cols = !c(Date, LabID, SampleLevel), names_to = "Parameter", values_to = "Value") |> 
  left_join(nut_stn) |> 
  filter(!is.na(Value) & !is.na(Station)) |> 
  group_by(Date, Station, SampleLevel, Parameter) |> 
  # multiple observations for each date/station; for now, taking max value for each date/station
  summarise(Value = max(Value, na.rm = TRUE))
write.csv(nut, file.path("data", "NutrientsProcessed.csv"), row.names = FALSE)
