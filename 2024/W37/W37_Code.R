# libraries
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)
library(ggtext)

# Set size 
camcorder::gg_record(
  dir    = 'Your directory',
  device = 'png',
  width  = 20,   # Width in cm
  height = 15,   # Height in cm
  units  = 'cm'
)

# Load tdata
tuesdata <- tt_load(2024, week = 37)
college_admissions <- tuesdata$college_admissions

# Select columns
college_admissions <- college_admissions %>%
  select(super_opeid, name, par_income_bin, par_income_lab, attend)

# Define Ivy-Plus Colleges somehow were not in the category tier 
ivy_plus_colleges <- c("Harvard University", "Yale University", "Princeton University", 
                       "Columbia University In The City Of New York", "University Of Pennsylvania", 
                       "Dartmouth College", "Brown University", "Stanford University")

# Filter for Ivy-Plus colleges only
ivy_plus_data <- college_admissions %>%
  filter(name %in% ivy_plus_colleges)

# Convert attendance rates to percentage
ivy_plus_data <- ivy_plus_data %>%
  mutate(attend = attend * 100)

# Calculate average attendance rates by income bin
average_attendance <- ivy_plus_data %>%
  group_by(par_income_bin) %>%
  summarize(avg_attend = mean(attend, na.rm = TRUE), .groups = 'drop')

# Create violin plot with boxplot and average points
violin_boxplot <- ggplot(ivy_plus_data, aes(x = factor(par_income_bin), y = attend, fill = factor(par_income_bin))) +  
  geom_violin(trim = TRUE, alpha = 0.6, show.legend = FALSE) +  # Transparent violin
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA, show.legend = FALSE) +  # Boxplot without outliers
  geom_point(data = average_attendance, aes(x = factor(par_income_bin), y = avg_attend),  
             color = "red", size = 1, shape = 21, fill = "red", show.legend = FALSE) +  # Average points
  labs(title = "<span style='font-size:16pt; font-weight:bold;'>Ivy-Plus College Attendance by Parent Income</span>",  
       subtitle = "This plot displays the distribution of attendance rates for Ivy-Plus \ncolleges across different parent income bins, segregated by test scores.",
       x = "Parent Income Bin (Percentile)",  
       y = "Attendance Rate (%)",  
       caption = "* Attendance is the fraction of students attending a college among all test-takers within each income bin.\n\nSource: Opportunity Insights: College-Level Data for 139 Selective American Colleges\nAuthor: @irgendeine_lea\n#TidyTuesday 2024: W37") +  
  scale_fill_viridis_d() +  # Color palette
  theme_minimal(base_size = 12) +  
  theme(legend.position = "none",  # No legend
        axis.title.x = element_text(size = 10, face = "bold"),  # Smaller, bolder x-axis title
        axis.title.y = element_text(size = 10, face = "bold"),  # Smaller, bolder y-axis title
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
        plot.margin = margin(10, 5, 10, 5),  # Margin for space
        plot.caption = element_text(hjust = 1, vjust = -3, size = 7),  # Right-align first caption
        plot.subtitle = element_text(hjust = 0.5, size = 9, face = "italic"),  # Subtitle styling
        plot.title = element_markdown(hjust = 0.5, size = 14, face = "bold"),  # Title styling
        panel.background = element_rect(fill = "#FAFAFA", color = NA),  # Light background
        plot.background = element_rect(fill = "#FAFAFA", color = NA)) +  
  ylim(0, max(ivy_plus_data$attend, na.rm = TRUE) + 0.5) +  
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +  # Y-axis as percentage
  scale_x_discrete(labels = function(x) sprintf("%.1f", as.numeric(x)))  # X-axis labels with one decimal

# Print the updated plot
print(violin_boxplot)
