library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

# temp_carbon %>%
#   .$year %>%
#   max()
# 
# temp_carbon %>%
#   filter(!is.na(carbon_emissions)) %>%
#   pull(year) %>%
#   max()
# 
# temp_carbon %>%
#   filter(!is.na(carbon_emissions)) %>%
#   max(year)
# 
# temp_carbon %>%
#   filter(!is.na(carbon_emissions)) %>%
#   .$year %>%
#   max()
# 
# temp_carbon %>%
#   filter(!is.na(carbon_emissions)) %>%
#   select(year) %>%
#   max()
# 
# temp_carbon %>%
#   filter(!is.na(carbon_emissions)) %>%
#   max(.$year)

# temp_year <- temp_carbon %>%
#   filter(!is.na(carbon_emissions))
# 
# index_1 <- which.max(temp_year$year)
# last_year <- temp_year$carbon_emissions[index_1]
# 
# index_2 <- which.min(temp_year$year)
# first_year <- temp_year$carbon_emissions[index_2]
# 
# last_year/first_year

# temp_carbon %>%
#   filter(!is.na(temp_anomaly)) %>%
#   select(year) %>%
#   min()

##Question 3
# temp_year <- temp_carbon %>%
#   filter(!is.na(temp_anomaly))
# 
# 
# index_1 <- which.max(temp_year$year)
# last_year <- temp_year$temp_anomaly[index_1]
# 
# index_2 <- which.min(temp_year$year)
# first_year <- temp_year$temp_anomaly[index_2]
# 
# difference <- last_year-first_year
# difference

# temp_carbon %>%
#   filter(!is.na(temp_anomaly), !is.na(land_anomaly), !is.na(ocean_anomaly)) %>%
#   ggplot() +
#   geom_line(aes(year, temp_anomaly), color = "blue") + 
#   geom_line(aes(year, land_anomaly), color = "red") + 
#   geom_line(aes(year, ocean_anomaly), color = "black") + 
#   geom_hline(aes(yintercept = 0), col = "blue") +
#   ylab("Temperature anomaly (degrees C)") +
#   ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
#   geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")


##Question 8-9
# greenhouse_gases %>%
#   ggplot(aes(year, concentration)) +
#   geom_line() +
#   facet_grid(gas ~ ., scales = "free") +
#   geom_vline(aes(xintercept = 1850)) +
#   ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
#   ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

# temp_carbon %>%
#     filter(!is.na(carbon_emissions)) %>%
#     ggplot() +
#     geom_line(aes(year, carbon_emissions), color = "blue")
    
co2_time <- historic_co2 %>%
  ggplot() +
  geom_line(aes(year, co2)) + 
  xlim(-3000, 2018)

co2_time
