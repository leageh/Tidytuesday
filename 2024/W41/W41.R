# Load necessary libraries
library(ggraph)
library(ggplot2)
library(dplyr)
library(tidytuesdayR)
library(shiny)
library(waffle)
library(RColorBrewer)

# Load the data
# Uncomment the line below to load data
# tuesdata <- tt_load(2024, week = 41)  
data <- tuesdata$most_visited_nps_species_data

# Preprocess data
category_distribution <- data %>%
  group_by(ParkName, CategoryName) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(ParkName) %>%
  mutate(Proportion = Count / sum(Count),
         Parts = round(Proportion * 100)) %>%
  group_by(ParkName) %>%
  mutate(Difference = 100 - sum(Parts),
         Parts = ifelse(row_number() == which.max(Parts), Parts + Difference, Parts)) %>%
  ungroup() %>%
  select(-Difference)

# Unique park names for the dropdown
unique_parks <- unique(category_distribution$ParkName)

# UI for the Shiny application
ui <- fluidPage(
  tags$div(
    style = "max-width: 700px; margin: auto;",
    tags$div(
      style = "text-align: center; margin-bottom: 20px;",
      titlePanel("Species Distribution in the Top 15 Most Visited U.S. National Parks")
    ),
    tags$style(HTML("
      body {
        background-color: #f0f8ff;
      }
      .fixed-dropdown {
        width: 100%;
        max-width: 300px;
        margin: 0 auto;
        margin-bottom: 15px;
      }
    ")),
    tags$div(
      class = "fixed-dropdown",
      selectInput("park", "Select a Park:", choices = unique_parks)
    ),
    plotOutput("waffle_chart", width = "700px", height = "500px")
  )
)

server1 <- function(input, output) {
  # Define color palette for categories
  category_colors <- c(
    "Amphibian" = "#66C2A5",
    "Bacteria" = "#FC8D62",
    "Bird" = "#8DA0CB",
    "Chromista" = "#E78AC3",
    "Crab/Lobster/Shrimp" = "#A6D854",
    "Fish" = "#FFD92F",
    "Fungi" = "#E5C494",
    "Insect" = "#B3B3B3",
    "Mammal" = "red",
    "Non-vascular Plant" = "#1B9E77",
    "Other Non-vertebrates" = "#D95F02",
    "Protozoa" = "#7570B3",
    "Reptile" = "#E7298A",
    "Slug/Snail" = "#66A61E",
    "Spider/Scorpion" = "#E6AB02",
    "Vascular Plant" = "darkgreen"
  )
  
  output$waffle_chart <- renderPlot({
    req(input$park)  # Require a park to be selected
    
    # Filter data for selected park
    park_data <- category_distribution %>% filter(ParkName == input$park)
    
    ggplot(park_data, aes(fill = CategoryName, values = Parts)) +
      geom_waffle(n_rows = 10, size = 0.2, color = "white") +
      scale_fill_manual(values = category_colors) +
      theme_minimal() +
      labs(
        title = paste("Proportions of Species Categories in", input$park),
        fill = "Category Name",
        caption = "Source: NPSpecies - The National Park Service biodiversity database\nAuthor: @irgendeine_lea\n#TidyTuesday 2024: W41"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()
      )
  }, height = 500, width = 700)  # Set plot dimensions
}

# Run the application
shinyApp(ui = ui, server = server1)
