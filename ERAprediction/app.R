#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(broom)
library(ggplot2)
library(tidymodels) 
library(vip)
library(stacks)
library(DALEX)
library(DALEXtra)
library(themis)
tidymodels_prefer()
library(bslib)
set.seed(494)
ERAswing <- readRDS("eradata.rds")
erasplit <- initial_split(ERAswing, strata = "p_era")
era_training <- training(erasplit)
era_testing <- testing(erasplit)

era_training <- era_training %>% 
  select(-first_name, -last_name, -player_id, -year)
model <- readRDS("eralasso.rds")

era_lasso_explain <- 
  explain_tidymodels(
    model = model,
    data = era_testing %>% select(-p_era), 
    y = as.numeric(era_training$p_era),
    label = "lasso"
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(),
  titlePanel("ERA Prediction Based on Swing and Pitch Location Data"),

  sidebarLayout(
    sidebarPanel(
    # Application title
    

   
    sliderInput(inputId = "z_swing_percent", 
                label = "Zone Swing Percentage",
                min = 50, 
                max = 100, 
                value = 75,
                step = 0.1),
    sliderInput(inputId = "z_swing_miss_percent", 
                label = "Zone Swing and Miss Percentage",
                min = 0, 
                max = 50, 
                value = 25,
                step = 0.1),
    sliderInput(inputId = "oz_swing_percent", 
                label = "Out of Zone Swing Percentage",
                min = 0, 
                max = 50, 
                value = 25,
                step = 0.1),
    sliderInput(inputId = "oz_swing_miss_percent", 
                label = "Out of Zone Swing and Miss Percentage",
                min = 0, 
                max = 75, 
                value = 50,
                step = 0.1),
    sliderInput(inputId = "meatball_swing_percent", 
                label = "Meatball Swing Percentage",
                min = 50, 
                max = 100, 
                value = 75,
                step = 0.1),
    sliderInput(inputId = "meatball_percent", 
                label = "Meatball Percentage",
                min = 0, 
                max = 25, 
                value = 10,
                step = 0.1),
    sliderInput(inputId = "iz_contact_percent", 
                label = "In Zone Contact Percentage",
                min = 50, 
                max = 100, 
                value = 75,
                step = 0.1),
    sliderInput(inputId = "in_zone_percent", 
                label = "In ZonePercentage",
                min = 25, 
                max = 75, 
                value = 50,
                step = 0.1),
    sliderInput(inputId = "edge_percent", 
                label = "Edge Percentage",
                min = 25, 
                max = 75, 
                value = 50,
                step = 0.1),
    sliderInput(inputId = "swing_percent", 
                label = "Swing Percentage",
                min = 25, 
                max = 75, 
                value = 50,
                step = 0.1),
    submitButton(text = "Create my plot!")),
  mainPanel(
    plotOutput(outputId = "predictor")
)))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$predictor <- renderPlot({
      predict_test <- data.frame(input$z_swing_percent, input$z_swing_miss_percent, input$oz_swing_percent, input$oz_swing_miss_percent, input$meatball_swing_percent, input$meatball_percent, input$iz_contact_percent, input$in_zone_percent, input$edge_percent, input$swing_percent)
      names(predict_test) <- c("z_swing_percent", "z_swing_miss_percent", "oz_swing_percent", "oz_swing_miss_percent", "meatball_swing_percent", "meatball_percent", "iz_contact_percent", "in_zone_percent", "edge_percent", "swing_percent")
      
      
      
      test <- predict_parts(explainer = era_lasso_explain,
                            new_observation = predict_test,
                            type = "break_down") #default
      test_pp <- plot(test, title = "Predicted ERA") + theme(plot.title = element_text(hjust = .5, size = 15, color = "black", face = "bold"))
      
      test_pp
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
