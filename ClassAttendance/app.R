#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Import Data
setwd("C:/Users/larn_/Documents/GitHub/Workshops-Certifications/DataScienceCertification")
gym_attendance_data <- read.csv("fitness_class_2212-clean.csv")

## Build App
ui <- fluidPage(
  titlePanel("Gym Attendance Tracker"),
  
  #Create a sidebar with different filters
  sidebarLayout(
    sidebarPanel(
      # Create a checkbox group to filter by class category
      checkboxGroupInput("categories", "Class Category:", 
                         unique(gym_attendance_data$category), 
                         selected = gym_attendance_data$category[1]),
      
      # Create a slider to filter by number of months as a member
      sliderInput("months_as_member", "Months as Member:", 1, 
                  max(gym_attendance_data$months_as_member), 
                  value = range(gym_attendance_data$months_as_member)),
      
      # Create a drop-down menu to filter by day of the week
      selectInput("day_of_week", "Day of the Week:", 
                  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
                  selected = gym_attendance_data$day_of_week[1]),
      
      # Create a drop-down menu to filter by time of class
      selectInput("time", "Time of Class:", c("Morning", "Afternoon", "Evening"), 
                  selected = gym_attendance_data$time[1])),
    
    mainPanel(
      
      # Create a table to show the filtered gym attendance data
      #tableOutput("gym_attendance_table"),
      
      # Create a plot to show the overall gym attendance rate
      plotOutput("gym_attendance_plot")
    )
  )
)

server <- function(input, output) {
  
  #Create a reactive expression to filter the gym attendance data
  filtered_gym_attendance_data <- reactive({
    gym_attendance_data %>%
      filter(category %in% input$categories,
             months_as_member >= input$months_as_member,
             day_of_week == input$day_of_week,
             time == input$time)
  })
  
  #Create a reactive expression to calculate the overall gym attendance rate
  overall_gym_attendance_rate <- reactive({
    mean(filtered_gym_attendance_data()$attended)
  })
  
  #Create a table to show the filtered gym attendance data
  output$gym_attendance_table <- renderTable({
    filtered_gym_attendance_data()
  })
  
  #Create a plot to show the overall gym attendance rate
  output$gym_attendance_plot <- renderPlot({
    plot(overall_gym_attendance_rate(), type = "p", main = "Overall Gym Attendance Rate")
  })
}

shinyApp(ui, server)
