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
  geom_col(position = position_dodge(width = 0.7), width = 0.5, color = "#E1E6E1") +  # Adjust dodge width for bars
  geom_text(aes(label = round(average_rating, 1)), 
            position = position_dodge(width = 0.7),  # Match dodge width
            vjust = -0.5, 
            size = 8,
            family = "oswald",
            color = "#E1E6E1") +  # Label the bars
  labs(
    x = "School Type",  # Label for x-axis
    y = "Average Rating",  # Label for y-axis
    title = "Campus Pride Index: Public schools rank higher than private schools",  # Plot title
    fill = "Community Type",  # Legend title for fill (optional)
    caption = "Source: https://campusprideindex.org; Author: @irgendeine_lea; #TidyTuesday 2024: W24"
  ) +
  scale_y_continuous(limits = c(0, 5)) +  # Set y-axis limits
  theme_minimal() +  # Change theme to minimal for grey background
  theme(
    plot.background = element_rect(fill = "#3D3D3D"),  # Set plot background to grey
    axis.text = element_text(family = "oswald", color = "#E1E6E1", size = 25),  # Set axis text color and font
    axis.title = element_text(family = "oswald", color = "#E1E6E1", size = 35),  # Set axis titles color and font
    panel.grid.minor = element_line(color = "#555555"),  # Set minor grid lines color
    panel.grid.major = element_line(color = "#555555"),  # Set major grid lines color
    plot.title = element_text(family = "oswald", size = 40, face = "bold", color = "#E1E6E1"),  # Title font settings
    legend.text = element_text(family = "oswald", color = "#E1E6E1", size = 25),  # Set legend text color and font
    legend.title = element_text(family = "oswald", color = "#E1E6E1", size = 25),
    legend.position = "bottom",  # Set legend position
    legend.key.width = unit(0.6, "cm"),  # Set width of legend symbols
    legend.key.height = unit(0.1, "cm"),  # Set height of legend symbols
    plot.caption = element_text(family = "oswald", color = "#E1E6E1", size = 18)  # Adjust caption font, color, and size
  ) +
  scale_fill_manual(values = palette_lgbtq("rainbow")) 
    
plot

# Save the plot as a PNG file with dimensions in cm
ggsave(filename = "campus_pride_index_plot.png", plot = plot, width = 25, height = 15, units = "cm", dpi = 300)
