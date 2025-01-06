# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(reshape2)
library(viridis)
library(patchwork)
library(ggtext)


data <- read.csv("API_Download_DS2_en_csv_v2_60220.csv",skip=4)


data_long <- data %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "Year", 
               values_to = "Value")


data_long$Year <- as.integer(gsub("X", "", data_long$Year))

# Filter for the years 2013 to 2020
filtered_data <- data_long %>%
  filter(Year >= 2013 & Year <= 2020)



data_melt <- filtered_data %>%
  pivot_longer(cols = Value, names_to = "Indicator", values_to = "Value")



# 1. Life Expectancy Line Chart
p1 <- ggplot(data_melt %>% filter(Indicator.Name == 'Life expectancy at birth, total (years)'), 
             aes(x = Year, y = Value, color = Country.Name, group = Country.Name)) +
  geom_line(size = 1) +
  labs(title = 'Life Expectancy at birth(years)', color = 'Country', y = 'Number of Years') +
  scale_color_viridis_d() +
  theme_minimal()



# 2. Infant Mortality Rate Bar Chart
p2 <- ggplot(data_melt %>% filter(Indicator.Name == 'Mortality rate, infant (per 1,000 live births)'),
             aes(x = Year, y = Value, fill = Country.Name)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(title = 'Infant mortality rate (per 1,000 live births)', y = 'Rate (per 1,000 live births)', fill = 'Country') +
  scale_fill_viridis_d() +
  
  theme_minimal()


# 3. Box Plot for Prevalence of HIV
p3 <- ggplot(data_melt %>% filter(Indicator.Name == 'Prevalence of HIV, total (% of population ages 15-49)'),
             aes(x = Country.Name, y = Value, fill = Country.Name)) +
  geom_boxplot() +
  labs(title = 'Prevalence of HIV, total (% of population ages 15-49)', 
       y = 'Prevalence (%)', x='Country',
       fill = 'Country') +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_blank(),   
        axis.ticks.x = element_blank())  


# 4. Heatmap for Life Expectancy Maternal mortality ratio (modeled estimate, per 100,000 live births)
life_expectancy_data <- data_melt %>%
  filter(Indicator.Name == 'Maternal mortality ratio (modeled estimate, per 100,000 live births)') %>%
  select(Country.Name, Year, Value) %>%
  pivot_wider(names_from = Year, values_from = Value)

# Convert to long format for heatmap
heatmap_data <- melt(life_expectancy_data, id.vars = 'Country.Name')

p4 <- ggplot(heatmap_data, aes(x = variable, y = Country.Name, fill = value)) +
  geom_tile(color = "white") +  # Add borders to tiles
  labs(title = 'Maternal mortality ratio (per 100,000 live births)', x = 'Year', y = 'Country') +
  scale_fill_viridis(option = "D") +  # Use a consistent color scale
  theme_minimal() +
  theme(legend.position = 'right')


# Arrange plots using patchwork
final_plot <- (p1 + p2) / (p3 + p4)  #


final_plot <- final_plot + 
  plot_annotation(
    title = "Health Trends in Southern Africa: A 2013-2020 Overview",
    subtitle = "Exploring life expectancy, infant mortality,martenal mortality and HIV prevalence across Zimbabwe,Botswana,Mozambique and South Africa\n\n",
    #caption = 'Source: World Bank Data',
    theme = theme(
      plot.title = element_text(size = 18, face = "bold", color = "black"),  
      plot.subtitle = element_text(size = 13, color = "black"),  
      #plot.caption = element_text(size = 10, color = "gray") 
    )
  )

# Display the updated plot
final_plot
