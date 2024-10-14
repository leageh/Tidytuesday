# Load Required Libraries
library(tidytuesdayR)
library(ggplot2)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggtext)  

# Set size for recording
camcorder::gg_record(
  dir = 'Your Direcotory',
  device = 'png',
  width = 25,   # Width in cm
  height = 30,  # Height in cm
  units = 'cm'
)

# Load Data
tuesdata <- tidytuesdayR::tt_load('2024-10-15')
df <- tuesdata$orcas

# Data Preparation
df$date <- as.Date(df$date)
df$month <- month(df$date, label = TRUE, abbr = TRUE)
monthly_observations <- df %>%
  group_by(year, month) %>%
  summarise(encounter_count = n(), .groups = 'drop') %>%
  filter(!is.na(year)) 

# Factor for month ordering
monthly_observations$month <- factor(monthly_observations$month, levels = month.abb)

# Identify max encounters
monthly_observations <- monthly_observations %>%
  group_by(year) %>%
  mutate(is_max = encounter_count == max(encounter_count))

# Define colors
colors <- c("FALSE" = "lightblue", "TRUE" = "red")

# Create Bar Chart
ggplot(monthly_observations, aes(x = month, y = encounter_count, fill = as.factor(is_max))) + 
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Orca Encounters in the Salish Sea",
    subtitle = "Highest count in <span style='color:red;'>red</span>.",
    x = "Month", 
    y = "Encounters",
    caption = "Source: Center for Whale Research\nAuthor: @irgendeine_lea\n#TidyTuesday 2024: W42"
  ) + 
  theme_minimal(base_size = 13) + 
  scale_fill_manual(values = colors) + 
  facet_wrap(~ year, ncol = 2, strip.position = "top") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, margin = margin(t = 10)),
    axis.title.x = element_text(margin = margin(t = 20), face = "bold"),
    axis.text.y = element_text(angle = 0, margin = margin(r = 10)),
    axis.title.y = element_text(margin = margin(r = 20), face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 1, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.background = element_rect(fill = "#F8F8F8", color = NA),
    plot.background = element_rect(fill = "#F8F8F8", color = NA)
  ) + 
  geom_text(aes(label = encounter_count), position = position_stack(vjust = 0.5), color = "black", size = 3) + 
  labs(subtitle = "Highest count in **<span style='color:red;'>red</span>**.") +
  theme(plot.subtitle = ggtext::element_markdown())
