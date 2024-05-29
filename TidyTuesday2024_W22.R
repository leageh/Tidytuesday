# Twitter: @irgendeine_lea
# TidyTuesday 2024: W22
# To load the data I used the code of Steven Ponce (@sponce1) since I am very new to R :) Thanks!
# No perfect code since I am new to this but let me know if you have improvement ideas!


# Load necessary libraries
library(tidyverse)
library(janitor)
library(ggrepel)
library(RColorBrewer)
library(showtext)
library(ggplot2)
library(dplyr)
library(tidyr)

# Add Google font
font_add_google("Oswald", "oswald")
showtext_auto()

# Read in the data
tt <- tidytuesdayR::tt_load(x = as.double("2024"), week = as.double("22"))

# Clean and glimpse the datasets
datasets <- list(
  planting_2020 = tt$planting_2020,
  planting_2021 = tt$planting_2021,
  harvest_2020  = tt$harvest_2020,
  harvest_2021  = tt$harvest_2021,
  spending_2020 = tt$spending_2020,
  spending_2021 = tt$spending_2021
) %>% 
  map(~ .x %>% clean_names() %>% glimpse())

# Combine datasets using the function
combine_years <- function(df_2020, df_2021) {
  bind_rows(
    df_2020 %>% mutate(year = 2020),
    df_2021 %>% mutate(year = 2021)
  )
}

# Combine datasets
planting <- combine_years(tt$planting_2020, tt$planting_2021) %>% 
  mutate(vegetable = str_to_title(vegetable)) 

harvest <- combine_years(tt$harvest_2020, tt$harvest_2021) %>% 
  mutate(vegetable = str_to_title(vegetable)) 

spending <- combine_years(tt$spending_2020, tt$spending_2021) 

# Merge planting and harvest datasets
df3 <- full_join(planting, harvest, by = c('vegetable', 'variety', 'year')) %>% 
  select(-plot, -variety, -date.x, -number_seeds_exact, -notes, -date.y)

# Housekeeping
rm(list = c('harvest_2020', 'harvest_2021', 'planting_2020', 'planting_2021', 'spending_2020', 'spending_2021'))

# Aggregate and calculate ratios
df_aggregated <- df3 %>%
  filter(!is.na(number_seeds_planted) & !is.na(weight)) %>%
  group_by(vegetable, year) %>%
  summarize(
    total_seeds_planted = sum(number_seeds_planted),
    total_weight = sum(weight),
    kilo_harvest = sum(weight) / 1000
  ) %>%
  mutate(ratio = total_seeds_planted / kilo_harvest) %>%
  select(vegetable, year, total_seeds_planted, kilo_harvest, ratio) %>%
  mutate(vegetable = str_to_title(vegetable))

# Summarize and reorder the vegetable factor
vegetable_ranks <- df_aggregated %>%
  group_by(vegetable) %>%
  summarize(total_seeds_planted_all_years = sum(total_seeds_planted)) %>%
  arrange(desc(total_seeds_planted_all_years))

df_aggregated <- df_aggregated %>%
  mutate(vegetable = factor(vegetable, levels = vegetable_ranks$vegetable))

# Prepare data for arrows and labels
arrows_data <- df_aggregated %>%
  select(vegetable, year, total_seeds_planted, ratio) %>%
  pivot_wider(names_from = year, values_from = c(total_seeds_planted, ratio)) %>%
  filter(!is.na(total_seeds_planted_2020) & !is.na(total_seeds_planted_2021))


# Plot
p <- ggplot(df_aggregated, aes(x = total_seeds_planted, y = vegetable, colour = factor(year))) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Dark2") +  
  geom_line(aes(group = vegetable)) +
  geom_segment(data = arrows_data, 
               aes(x = total_seeds_planted_2020, y = vegetable, xend = total_seeds_planted_2021, yend = vegetable),
               arrow = arrow(length = unit(0.15, "cm")), 
               colour = "black") +
  geom_label_repel(data = df_aggregated, 
                   aes(x = total_seeds_planted, y = vegetable, label = round(total_seeds_planted)),
                   label.size = 0.2, 
                   nudge_y = 0.1, 
                   show.legend = FALSE, 
                   size = 3.6, 
                   label.padding = unit(0.05, "lines"),
                   min.segment.length = 0, 
                   colour = "#3E3E3E") +  
  theme_minimal() +
  theme(
    text = element_text(family = "oswald"),
    plot.background = element_rect(fill = "#8CC08D", color = NA),
    axis.text.x = element_text(color = "#3E3E3E", size = 10),
    axis.text.y = element_text(color = "#3E3E3E", size = 10),
    axis.title.x = element_text(size = 12, face = "bold"),  # Increase x axis title size
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 8),
    plot.title = element_text(size = 14, face = "bold"),  # Make title bold and bigger
    plot.subtitle = element_text(size = 12)
  ) +
  labs(colour = "Year",
       x = "Planted seed count",
       y = "Seed types",
       title = "Lisa's Vegetable Garden: A  Comparison of 2020 and 2021 Seed Plantings",
       subtitle = "While some vegetable types were popular and the seed amount got increased from 2020 to 2021\nsome were eliminated from the the 2021 season. Find details on the changes in the plot.",
       caption = "Source: {gardenR}\nAuthor: @irgendeine_lea\n#TidyTuesday 2024: W22") 
p

# Save as PNG 
ggsave("Lisa_Vegetable_Garden.png", plot = p)
