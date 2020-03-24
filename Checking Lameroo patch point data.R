
  

library(readr)
Lameroo_data <- read_table2("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Lameroo.csv", 
                       col_names = FALSE, skip = 24)
head(Lameroo_data)
Lameroo_heading <- read_table2("W:/Pastures/Gridded_seasonal_break/Check_code_selected_sites/Lameroo.csv", 
                               col_names = FALSE, skip = 22)

Lameroo_heading <- head(Lameroo_heading, 1)

names(Lameroo_data) <- Lameroo_heading
str(Lameroo_data)
Lameroo_data_yrs <- filter(Lameroo_data,
                             year == 2000|
                             year == 2018|
                             year == 1984|
                             year == 1972)
Lameroo_data_days <- Lameroo_data_yrs %>% filter(between(day, 61, 212))
Lameroo_data_days <- select(Lameroo_data_days, year, day, rain, evap)
head(Lameroo_data_days)
Lameroo_data_days <- mutate(Lameroo_data_days,
                            rain_evap = rain - evap)

Lameroo_data_days_plot <- gather(Lameroo_data_days, variable, value, rain, evap, rain_evap)
str(Lameroo_data_days_plot)

year_input = 1972
site_name = "Lameroo"

filter(Lameroo_data_days_plot, year == year_input) %>% 
ggplot( aes(day, value, colour = variable))+
  geom_point(alpha = 0.2)+
  geom_line()+
  theme_bw()+
  labs(title= paste0("Check data for: ", site_name, ", year ", year_input),
       x ="day of year", 
       y = "",
       subtitle = "APSIM met file")+
  scale_colour_manual(values = c("red", "blue", "black"),
                      name="Climate data",
                      breaks=c("evap", "rain", "rain_evap"),
                      labels=c("Evaporation", "Rainfall", "Seasonal break"))
