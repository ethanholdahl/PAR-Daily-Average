#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Latitude",
                        "Latitude:",
                        min = -90,
                        max = 90,
                        value = 0),
            sliderInput("Longitude",
                        "Longitude:",
                        min = -180,
                        max = 180,
                        value = 0),
            dateInput("Date",
                      "Date:",
                      format = "mm-dd",
                      value = "2009-01-01"
                      )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("sunPath")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
