# Lizeth Velderrain-Lopez
# A3 - Incarceration Data
# Analysis and Plots
# 2/25/22

# Load packages
library(tidyverse)
library(maps)
library(mapproj)

# Load dataset
dataset <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Total population of minority women in prison
dataset$total_minority_women_prison <- (dataset$black_female_prison_pop + dataset$latinx_female_prison_pop + dataset$aapi_female_prison_pop + 
                                         dataset$native_female_prison_pop + dataset$other_race_female_prison_pop)

# Percentage of Black women in prison
dataset$percentage_black_women_prison <- (dataset$black_female_prison_pop / dataset$female_prison_pop) * 100

# Percentage of AAPI women in prison 
dataset$percentage_aapi_women_prison <- (dataset$aapi_female_prison_pop / dataset$female_prison_pop) * 100

# Percentage of Latine prison populations
dataset$percentage_latine_prison_pop <- (dataset$latinx_prison_pop / dataset$total_prison_pop) * 100

# Percentage of Latine prison population from the total Latine population
dataset$percentage_latine_prison_from_total_pop <- (dataset$latinx_prison_pop / dataset$latinx_pop_15to64) * 100

# Percentage of minority prison admissions
dataset$total_minority_prison_adm <- ((dataset$black_prison_adm + dataset$latinx_prison_adm + dataset$aapi_prison_adm + 
  dataset$native_prison_adm + dataset$other_race_prison_adm) / dataset$total_prison_adm) * 100


# Filter data to get rid of NA values
filter_data <- na.omit(dataset)

# Find the state with highest percentage of minority prison admissions
state_highest_minority_percent <- filter_data %>%
  group_by(state) %>%
  summarize(total_minority_prison_adm = max(total_minority_prison_adm)) %>%
  filter(total_minority_prison_adm == max(total_minority_prison_adm)) %>%
  pull(state)

# Find the highest count of minority women in prison
county_highest_minority <- filter_data %>%
  group_by(county_name) %>%
  summarize(total_minority_women_prison = max(total_minority_women_prison)) %>%
  filter(total_minority_women_prison == max(total_minority_women_prison)) %>%
  pull(total_minority_women_prison)

# Find the state with the highest percentage of Black women in prison
state_max_black_women_prison <- filter_data %>%
  group_by(state) %>%
  summarize(percentage_black_women_prison = max(percentage_black_women_prison)) %>%
  filter(percentage_black_women_prison == max(percentage_black_women_prison)) %>%
  pull(state)

# Find the state with the highest percentage of AAPI women in prison
state_max_aapi_women_prison <- filter_data %>%
  group_by(state) %>%
  summarize(percentage_aapi_women_prison = max(percentage_aapi_women_prison)) %>%
  filter(percentage_aapi_women_prison == max(percentage_aapi_women_prison)) %>%
  pull(state)

# Find the county with the highest percentage of Latines in prison from the Latine population
county_highest_latine_prison <- filter_data %>%
  group_by(county_name) %>%
  summarize(percentage_latine_prison_from_total_pop = max(percentage_latine_prison_from_total_pop)) %>%
  filter(percentage_latine_prison_from_total_pop == max(percentage_latine_prison_from_total_pop)) %>%
  pull(county_name)

# Find data from California and Florida
ca_fl_data <- filter_data %>%
  filter(state == "CA" | state == "FL") 


# Create plot of percentage of Latine prison populations over time
latine_prison_pop_plot <- ca_fl_data %>%
  ggplot(mapping = aes(x=year, y=percentage_latine_prison_pop, color = state), alpha = .3) +
  geom_point() +
  labs(
    title = "Percentage of Latine Prison Populations Over Time in CA and FL",
    x = "Time (in Years)",
    y =  "Percentage",
    fill = "State"
  )

# Plot the graph
plot(latine_prison_pop_plot)


# Create scatterplot comparing the percentage of Black women and AAPI women prison population
black_vs_aapi_women_prison_pop <- ggplot(data = filter_data) +
  geom_point(mapping = aes(x = percentage_aapi_women_prison, y = percentage_black_women_prison), color = "#89b0ae", alpha = .5) +
  labs(
    title = "Percentage of Black vs AAPI Women Prison Population",
    x = "Percentage of AAPI Women",
    y = "Percentage of Black Women"
  )  

# Plot the graph
plot(black_vs_aapi_women_prison_pop)


# Create blank theme for minimalist map visual
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )

# Create map data of the states and their counties
state_shape <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Join the map data of the state and county shapes along with the filtered data
map_data <- state_shape %>%
  left_join(filter_data, by = "fips")

# Create choropleth map describing the variable of the total admission rate of minorities
map_plot <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_minority_prison_adm),
    color = "white",
    size = .1        
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(map_data$total_minority_prison_adm)), 
                        low = "#ffc2d4", high = "#8a2846") +
  blank_theme +
  labs(
    title = "Minority Prison Admissions in the US", 
    fill = "Percentage of Prison Admissions"
  )

# Plot the map
plot(map_plot)

