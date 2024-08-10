library(dplyr)
library(tidyr)
library(readxl)

sg_path = "Seagrass monitoring data 2014 and 2022.xlsx"

sg_params = read.csv(file.path("data", "SeagrassParameters.csv"))

sg_2014 = read_xlsx(file.path("data", sg_path), sheet = "2014TransectData", 
                    col_types = "text", .name_repair = "universal") |> 
  rename(SED.INDEX = SED_INDEX)
sg_2022 = read_xlsx(file.path("data", sg_path), sheet = "2022TransectData", 
                    col_types = "text", .name_repair = "universal")

sg = bind_rows(mutate(setNames(sg_2014, toupper(names(sg_2014))), Year = 2014), 
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
         Value2 = ifelse(Type2 != "%Cover", Value,
                         case_when(Value == 0.1 ~ 1,
                                   Value == 0.5 ~ 2.5,
                                   Value == 1 ~ 5,
                                   Value == 2 ~ 15,
                                   Value == 3 ~ 37.5,
                                   Value == 4 ~ 62.5,
                                   Value == 5 ~ 87.5,
                                   TRUE ~ NA_real_)))

sg |> 
  select(Year, Station = SITE, Transect = TRANSECT, Parameter = Parameter2, Value = Value2) |>
  filter(!is.na(Value)) |> 
  arrange(Year, Station, Parameter) |> 
  write.csv(file.path("data", "SeagrassProcessed.csv"), row.names = FALSE)
