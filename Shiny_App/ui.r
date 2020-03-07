
## Load and install the packages
library("tidyverse", "shiny")
theme_set(theme_minimal())


# Define UI for the application 
fluidPage(# Application title
  titlePanel("PAR data methods"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "Latitude",
        label = "Latitude:",
        min = -90,
        max = 90,
        step = 1,
        value = 0
      ),
      sliderInput(
        "Longitude",
        label = "Longitude:",
        min = -180,
        max = 180,
        step = 1,
        value = 0
      ),
      sliderInput(
        "year",
        label = "Year:",
        min = 2000,
        max = 2050,
        step = 1,
        value = 2020
      ),
      sliderInput(
        "day",
        label = "Day:",
        min = 1,
        max = 365,
        step = 1,
        value = 1
      ),
      sliderInput(
        "time",
        label = "time:",
        min = 0,
        max = 2,
        step = 1 / 8,
        value = 1
      ),
      checkboxInput(
        "Wang",
        "See Wang PAR/ratio",
        TRUE
      ),
      checkboxInput(
        "ratio",
        "See ratio PAR/ratio",
        TRUE
      ),
      h3("Data and Code"), uiOutput("tab")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(img(src = "Sun-Diagram-Solo.PNG", width = 600, height = 400), img(src = "Sun-Diagram-Zoomed.PNG", width = 600, height = 400), plotOutput("elevation"), plotOutput("PAR"), plotOutput("ratio")
              ))
  )
