
## Load and install the packages
library("tidyverse", "shiny")
theme_set(theme_minimal())


# Define UI for the application 
navbarPage(title = "Photosynthetically Active Radiation (PAR)",
           tabPanel("Methods", 
                    withMathJax(),
                    includeMarkdown("PAR-Daily-Average.md")),
           tabPanel("Plots",
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:inherit;",
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
      )
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("ratio"), br(), br(), br(), plotOutput("elevation"), br(), br(), br(), plotOutput("PAR")
              ))),
  tabPanel("Data",
           h2("Daily Average Data"),
           uiOutput("results"),
           h3("Images:"),
           tabsetPanel(
             tabPanel("January 1",
                      img(src ="Jan1.PNG")),
             tabPanel("April 1",
                      img(src ="April1.PNG")),
             tabPanel("July 1",
                      img(src ="July1.PNG")),
             tabPanel("October 1",
                      img(src = "Oct1.PNG"))
           ),
           br(), br(),br(),
           h2("Input Data"), uiOutput("data")),
  tabPanel("Code",
           h3("Matlab Code"), uiOutput("matlab"),
           br(),br(),br(),
           h5("I am working on translating the matlab code to R code. When it is done it will be posted on here as well as on the GitHub page."))
  )
