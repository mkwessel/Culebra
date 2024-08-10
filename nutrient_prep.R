library(dplyr)
library(tidyr)
library(readxl)

nut_stn = read.csv(file.path("data", "NutrientStations.csv"))
nut_path = "Nutrients.xlsx"
nut = left_join(read_xlsx(file.path("data", nut_path), sheet = "Dissolved"), 
                read_xlsx(file.path("data", nut_path), sheet = "Total")) |> 
  mutate(`Nitrate + Nitrite (mg/L)` = `Nitrate (mg/L)` + `Nitrite (mg/L)`) |> 
  select(Date = CollectionDate, LabID, contains("mg/L")) |> 
  pivot_longer(cols = !c(Date, LabID), names_to = "Parameter", values_to = "Value") |> 
  left_join(nut_stn) |> 
  filter(!is.na(Value) & !is.na(Station)) |> 
  select(-LabID)
write.csv(nut, file.path("data", "NutrientsProcessed.csv"), row.names = FALSE)
