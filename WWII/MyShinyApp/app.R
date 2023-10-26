#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(shiny)
library(ggplot2)
library(dplyr)

# Read the CSV file
  casualty_data <- read_csv("ww2_dataset.csv")

ui <- fluidPage(
  titlePanel("WW2 Casualties by Significant Countries"),
  mainPanel(
    plotOutput("barplot")
  )
)

server <- function(input, output) {
  # Check if the "Totaldeaths" column exists in the dataset
  if ("Totaldeaths" %in% colnames(casualty_data)) {
    # Convert the "Totaldeaths" column to numeric
    casualty_data$Totaldeaths <- as.numeric(casualty_data$Totaldeaths)
    
    # Define significant countries
    significant_countries <- c("USA", "Germany", "Japan", "United Kingdom", "Soviet Union", "Italy", "France", "China", "Poland")
    
    # Filter the data to include only the significant countries
    filtered_data <- filter(casualty_data, Country %in% significant_countries)
    
    # Calculate the total casualties for the significant countries
    total_casualties <- sum(filtered_data$Totaldeaths)
    
    # Calculate the casualties for each country as a percentage of the total number of casualties from the significant countries
    percentages <- filtered_data %>%
      group_by(Country) %>%
      summarize(Percentage = Totaldeaths / total_casualties * 100)
    
    # Display the data as a bar chart
    output$barplot <- renderPlot({
      ggplot(data = percentages, aes(x = Country, y = Percentage)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(x = "Country", y = "Percentage of Casualties") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Expressed as a percentage of the casualties from the 8 significant countries")
    })
  } else {
    # If the "Totaldeaths" column doesn't exist, display an error message
    output$barplot <- renderText("The 'Totaldeaths' column does not exist in the dataset.")
  }
}

shinyApp(ui = ui, server = server)
