# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(gglgbtq)

# Load data for the specific week from the tidytuesdayR package
tuesdata <- tidytuesdayR::tt_load(2024, week = 24)

# Extract relevant data frames
pride_index <- tuesdata$pride_index
pride_index_tags <- tuesdata$pride_index_tags

# Ensure campus_name columns match between the data frames
if (!all(pride_index$campus_name == pride_index_tags$campus_name)) {
  stop("The campus_name columns do not match between the two data frames.")
}

# Combine pride_index with a subset of pride_index_tags
df <- cbind(pride_index, pride_index_tags[, 3:ncol(pride_index_tags), with = FALSE])

# Create a new column 'PP' based on the values of 'public' and 'private'
df <- df %>%
  mutate(PP = case_when(
    public == TRUE & private == TRUE ~ "both",
    public == TRUE ~ "public",
    private == TRUE ~ "private"
  )) %>%
  filter(!is.na(PP))

# Summarize and sort data
grouped_table <- df %>%
  group_by(PP, community_type) %>%
  summarise(average_rating = mean(rating, na.rm = TRUE), .groups = 'drop') %>%
  arrange(PP, desc(average_rating))

# Create the plot
plot <- ggplot(grouped_table, aes(x = PP, y = average_rating, fill = community_type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.4, color = "#E1E6E1") +
  geom_text(aes(label = round(average_rating, 1)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, 
            size = 6, 
            color = "#E1E6E1") +
  labs(
    x = "School Type",
    y = "Average Rating",
    title = "Campus Pride Index: Public schools rank higher than private school",
    subtitle = "Comparison of Community Types",
    fill = "Community Type"
  ) +
  scale_y_continuous(limits = c(0, 5)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#3D3D3D", color = NA),
    plot.margin = margin(5, 5, 5, 5),
    axis.text = element_text(family = "sans", color = "#E1E6E1", size = 16, margin = margin(10)),
    axis.title = element_text(family = "oswald", color = "#E1E6E1", size = 18),
    panel.grid.minor = element_line(color = "#555555"),
    panel.grid.major = element_line(color = "#555555"),
    plot.title = element_text(family = "oswald", size = 28, face = "bold", color = "#E1E6E1"),
    plot.subtitle = element_text(family = "oswald", size = 22, color = "#E1E6E1"),
    legend.text = element_text(family = "sans", color = "#E1E6E1", size = 14),
    legend.title = element_text(family = "oswald", color = "#E1E6E1", size = 16),
    legend.position = "bottom",
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.4, "cm"),
    plot.caption = element_text(family = "sans", color = "#E1E6E1", size = 12)
  ) +
  scale_fill_manual(values = palette_lgbtq("rainbow")) +
  labs(
    caption = "Source: https://campusprideindex.org\nAuthor: @irgendeine_lea\n#TidyTuesday 2024: W24"
  )

plot

# Save the plot as a PNG file with dimensions in cm
ggsave(filename = "campus_pride_index_plot.png", plot = plot, width = 25, height = 15, units = "cm", dpi = 200)
