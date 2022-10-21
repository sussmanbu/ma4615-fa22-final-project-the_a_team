library(tidyverse)

ufo_data <- read_csv(here::here("dataset", "ufo-complete-geocoded-time-standardized.csv"), 
                      col_names = c("Date_and_Time", "City", "State", "Country", "Shape", "Time_in_Seconds", "Duration", "Summary", "Date_Posted", "Latitude", "Longitude"),
                      col_types = cols(
                        Date_and_Time = col_character(), # change to col_datetime() ? 
                        City = col_character(),
                        State = col_character(),
                        Shape = col_character(),
                        Time_in_Seconds = col_double(), 
                        Country = col_character(),
                        Duration = col_character(), 
                        Summary = col_character(),
                        Date_Posted = col_character(), #change to col_date() ? 
                        Latitude = col_character(),
                        Longitude = col_character(),
                      ))
ufo_data <- ufo_data %>% filter(!is.na(Date_and_Time)) %>% 
  filter(!is.na(City)) %>% 
  filter(!is.na(State)) %>% 
  filter(!is.na(Shape)) %>% 
  filter(!is.na(Time_in_Seconds)) %>% 
  filter(!is.na(Country)) %>% 
  filter(!is.na(Duration)) %>% 
  filter(!is.na(Date_Posted)) %>% 
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude))
## CLEAN the data
ufo_data_clean <- loan_data

write_csv(ufo_data_clean, file = here::here("dataset", "ufo_data_clean.csv"))

save(ufo_data_clean, file = here::here("dataset/ufo_data_clean.RData"))

