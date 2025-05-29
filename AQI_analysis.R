library(tidyverse)
library(janitor)
library(lubridate)



"INDIA-AQI-DATA-2015-2020 .csv" %>% 
  read_csv() %>% 
  clean_names()-> aqidf


aqidf %>% 
  mutate(year = date %>% year(),
         month = date %>% month(),
         day = date %>% day(),
         week = date %>%  week(),
         weekend = date %>% wday(label = T)) -> aqidf1

colnames(aqidf1)

unique(aqidf1$city)

view(aqidf1)



# Pivot longer: reshape wide pollutant columns into long format
aqidf1 %>%
  pivot_longer(cols = 3:14,  # make sure these are your pollutant columns
               names_to = "pollutants",
               values_to = "values") -> aqidf2

# Optional: View reshaped data
View(aqidf2)  # Not necessary for code to run

# Year-wise average pollutant trends
aqidf2 %>%
  group_by(year, pollutants) %>%
  summarise(mean_value = mean(values, na.rm = TRUE), .groups = "drop") -> aqidf2

library(ggplot2)
## line graph
aqidf2 %>%
  ggplot(aes(x = year, y = mean_value))+
  geom_line()+
  facet_wrap(~pollutants, scales = "free_y")+
  labs(title = "Air Pollutants Trend",
       subtitle = "From 2015 to 2020",
       x=NULL,
       y="Pollutatnts values",
       caption = "source: AQI India Dataset")+
  theme_linedraw()-> plot1

ggsave("Air Pollutants Trends.pdf",
       plot = plot1,
       units = "in",
       width = 10,
       height = 6)







#2.Air quality trends for banglore 

aqidf_bangalore <- aqidf1 %>%
  filter(city == "Bengaluru")  

# Reshape data to long format for pollutants
aqidf_bangalore_long <- aqidf_bangalore %>%
  pivot_longer(
    cols = c(pm2_5, pm10, no2, so2, o3, co), 
    names_to = "pollutant",
    values_to = "value")

# Calculate year-wise average pollutant levels
aqi_bangalore_yearwise <- aqidf_bangalore_long %>%
  group_by(year, pollutant) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop")

# Plot year-wise average pollutant trends
ggplot(aqi_bangalore_yearwise, aes(x = year, y = mean_value, color = pollutant)) +
  geom_line(size = 1.2) +
  labs(
    title = "Year-wise Average Pollutant Trends in Bengaluru (2015–2020)",
    x = "Year",
    y = "Mean Pollutant Level",
    color = "Pollutant"
  ) +
  theme_minimal()


#3 co trends for all cities
#4 Air quality trends for Banglore , Chennai, Mumbai, Hyderabad
#5 pm2.5 trend for banglore for 2015-2020








ggplot()+
  labs(title = "plot")+
  labs(x="X-Axis")

#heat map
aqidf2 %>%
  filter(pollutants == "co") %>%
  group_by(week, weekend, month) %>%
  summarise(meanval = mean(values, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = week, y = weekend, fill = meanval)) +
  geom_tile(color = "white") +
  facet_wrap(~month, scales = "free_x") +
  scale_fill_gradientn(colors = c("darkgreen", "yellow", "red")) +
  theme_minimal() +
  labs(
    title = "CO Heatmap",
    subtitle = "For all cities (2015–2020)",
    x = "Week Number",
    y = "Day of Week",
    fill = "CO Level"
  )


