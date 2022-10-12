# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)

# Load data --------------------------------------------------------------------

spldata <- read.csv(file = "https://raw.githubusercontent.com/sheriethecat/S3729C_Green_Team/main/spldata.csv", header = TRUE, sep = ",")
All_Team <- sort(unique(spldata$Team_Name))
# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("lumen"),
                
                sidebarLayout(
                  sidebarPanel(
                    selectInput(
                      inputId = "y",
                      label = "Y-axis:",
                      choices = c(
                        "Number of goals in 2020" = "Year2020_Goals",
                        "Number of goals in 2021" = "Year2021_Goals",
                        "Number of goals in 2022" = "Year2022_Goals"
                      ),
                      selected = "Year2022_Goals"
                    ),
                    
                    selectInput(
                      inputId = "x",
                      label = "X-axis:",
                      choices = c(
                        "Number of goals in 2020" = "Year2020_Goals",
                        "Number of goals in 2021" = "Year2021_Goals",
                        "Number of goals in 2022" = "Year2022_Goals"
                      ),
                      selected = "Year2020_Goals"
                    ),
                    
                    selectInput(
                      inputId = "z",
                      label = "Color by:",
                      choices = c(
                        "Club Names" = "Team_Name",
                        "Number of goals in 2021" = "Year2021_Goals",
                        "Number of goals in 2022" = "Year2022_Goals"
                      ),
                      selected = "Team_Name"
                    ),
                    
                    sliderInput(
                      inputId = "alpha",
                      label = "Alpha:",
                      min = 0, max = 1,
                      value = 0.5
                    ),
                    
                    sliderInput(
                      inputId = "size",
                      label = "Size:",
                      min = 0, max = 5,
                      value = 2
                    ),
                    
                    textInput(
                      inputId = "plot_title",
                      label = "Plot title",
                      placeholder = "Enter text to be used as plot title"
                    ),
                    
                    actionButton(
                      inputId = "update_plot_title",
                      label = "Update plot title"
                    ),
                    
                    br(), br(),
                    
                    selectInput(
                      inputId = "Team_Name",
                      label = "Select the Team:",
                      choices = All_Team,
                      selected = "Tanjong Pagar United FC",
                      multiple = TRUE
                    ),
                    
                    downloadButton('download',"Download data")
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "scatterplot", hover = "plot_hover"),
                    dataTableOutput(outputId = "spldatatable"),
                    br(),
                    dataTableOutput(outputId = "spldatatablehover")
                  )
                )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session){
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    ggplot(data = spldata, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$spldatatable <- renderDataTable({
    nearPoints(spldata, input$plot_hover) %>%
      select(Team_Name, Year2020_Goals, Year2021_Goals, Year2022_Goals)
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)