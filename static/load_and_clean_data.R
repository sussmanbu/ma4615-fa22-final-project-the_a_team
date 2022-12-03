library(tidyverse)
library(countrycode)
Food_waste <- read_csv(here::here("dataset", "FoodLossandWasteAll.csv"))

## CLEAN the Food_waste data:

#Food_waste$m49_code <- NULL 
#Food_waste$country <- NULL 
#Food_waste$region <- NULL
Food_waste$loss_percentage_original <- NULL
Food_waste$activity <- NULL
Food_waste$treatment <- NULL
Food_waste$cause_of_loss <- NULL
Food_waste$sample_size <- NULL
Food_waste$method_data_collection <- NULL
Food_waste$reference <- NULL
Food_waste$url <- NULL
Food_waste$notes <- NULL
Food_waste$loss_quantity <- NULL

Food_waste_clean <- Food_waste %>% group_by(year, country, commodity,m49_code) %>%
  summarise(mean_loss_percentage = mean(loss_percentage))

Food_waste_clean <- Food_waste_clean[c(1,5)]
Food_waste_clean <- Food_waste_clean %>% group_by(year) %>% summarise(count = n(), sum = sum(mean_loss_percentage,na.rm = TRUE)/count) #this is the best version of food clean

## CLEAN the Food_production data: ====================================================================Food Production Calculations

Food_production <- read_csv(here::here("dataset-ignore", "Production_All(Normalized).csv"))

#Food_production$`Domain Code` <- NULL 
#Food_production$Domain <- NULL 
#Food_production$`Area Code (M49)` <- NULL
#Food_production$Area <- NULL
#Food_production$Element <- NULL
Food_production$`Item Code (CPC)`<- NULL
Food_production$`Year Code` <- NULL
Food_production$Flag <- NULL
Food_production$Flag <- NULL
Food_production$`Flag Description`<- NULL

Food_production_clean <- filter(Food_production, Food_production$Element == "Production")
colnames(Food_production_clean)[2] <- "M49Code"


#MX49 <- gsub('[^[:alnum:] ]', ' ' , Food_production_clean)
as.numeric( sub("'","",Food_production_clean$M49Code, fixed = TRUE))
Food_production_clean <- Food_production_clean %>% mutate(M49 = as.numeric( sub("'","",Food_production_clean$M49Code, fixed = TRUE)))  

Food_production_clean1 <- Food_production_clean %>% filter(M49 == 1)
fasd <- Food_production_clean1 %>% filter(M49 = NA)

Food_production_clean1 <- Food_production_clean1[-c(1:7,9,11)]
Food_production_clean2 <- Food_production_clean1 %>% group_by(Year) %>% summarise(Production = sum(Value,na.rm = TRUE))  #best food prod version

#=============================================================================


#===========================combining production and food loss
Food_production_clean2 <- Food_production_clean2[-c(1:5,7),]
Food_waste_clean <- Food_waste_clean[-c(55),]

worldprod <- left_join(Food_production_clean2,Food_waste_clean, by = c("Year" = "year"))
worldprod <- worldprod[-c(3)]
worldprod <- worldprod %>% group_by(Year) %>% summarise(Loss = Production * (sum/100), Production, LossPercentage = sum)
#===========================
#this is how we get the countries into regions
Food_waste_clean <- Food_waste_clean %>%
  mutate(country_region = countrycode(m49_code, origin = "iso3n", destination = "region")) #this separates countries into regions, may not need it for final version
#=========================






## CLEAN the GDP data:

paste0(rep("row_index_", 54), c(218:271))

# Datasets cleaning: GDP, AgriPercentageGDP, LandPercentage, and Population
suppressWarnings({
  #GDP
  GDP_data <- read_csv(here::here("dataset", "GDP.csv"), show_col_types = F)
  GDP_data <- GDP_data %>% group_by(GDP_data[3]) %>% mutate_if(is.character, as.numeric) %>% ungroup()
  GDP_data <- GDP_data[-c(1, 2, 4)]
  GDP_data <- GDP_data[-c(218:271),]
  colnames(GDP_data)[1] <- "Country"
  GDP_data_clean <- GDP_data
  #AgriPercentageGDP
  AgriGDP_data <- read_csv(here::here("dataset", "AgriPercentageGDP.csv"), show_col_types = F)
  AgriGDP_data <- AgriGDP_data %>% group_by(AgriGDP_data[3]) %>% mutate_if(is.character, as.numeric) %>% ungroup()
  AgriGDP_data <- AgriGDP_data[-c(1, 2, 4)]
  AgriGDP_data <- AgriGDP_data[-c(218:271),]
  colnames(AgriGDP_data)[1] <- "Country"
  AgriGDP_data_clean <- AgriGDP_data
  #LandPercentage
  AgriLand_data <- read_csv(here::here("dataset", "LandPercentage.csv"), show_col_types = F)
  AgriLand_data <- AgriLand_data %>% group_by(AgriLand_data[3]) %>% mutate_if(is.character, as.numeric) %>% ungroup()
  AgriLand_data <- AgriLand_data[-c(1, 2, 4)]
  AgriLand_data <- AgriLand_data[-c(218:271),]
  colnames(AgriLand_data)[1] <- "Country"
  AgriLand_data_clean <- AgriLand_data
  #Population
  Population_data <- read_csv(here::here("dataset", "population.csv"), show_col_types = F)
  Population_data <- Population_data %>% group_by(Population_data[3]) %>% mutate_if(is.character, as.numeric) %>% ungroup()
  Population_data <- Population_data[-c(1, 2, 4)]
  Population_data <- Population_data[-c(218:537),]
  colnames(Population_data)[1] <- "Country"
  Population_data_clean <- Population_data
})


# Saving cleaned data

write_csv(Food_waste_clean, file = here::here("dataset", "FoodLossandWasteAllClean.csv"))
save(Food_waste_clean, file = here::here("dataset/FoodLossandWasteAllClean.RData"))

write_csv(Food_production_clean, file = here::here("dataset-ignore", "Food_production_clean.csv"))
save(Food_production_clean, file = here::here("dataset-ignore/Food_production_clean.RData"))

write_csv(GDP_data_clean, file = here::here("dataset", "GDP_data_clean.csv"))
save(GDP_data_clean, file = here::here("dataset/GDP_data_clean.RData"))

write_csv(AgriGDP_data_clean, file = here::here("dataset", "AgriGDP_data_clean.csv"))
save(AgriGDP_data_clean, file = here::here("dataset/AgriGDP_data_clean.RData"))

write_csv(AgriLand_data_clean, file = here::here("dataset", "AgriLand_data_clean.csv"))
save(AgriLand_data_clean, file = here::here("dataset/AgriLand_data_clean.RData"))

write_csv(Population_data_clean, file = here::here("dataset", "Population_data_clean.csv"))
save(Population_data_clean, file = here::here("dataset/Population_data_clean.RData"))

