# Load packages ----------------------------------------------------------------

library(shiny)
library(shinythemes)
library(ggplot2)
library(tools)

# Load data --------------------------------------------------------------------

FIH_Player_Stats_for_Sherie <- read.csv(file = "https://raw.githubusercontent.com/sheriethecat/S3729C_Green_Team/main/FIH%20Player%20Stats%20for%20Sherie.csv", header = TRUE, sep = ",")
All_countries <- sort(unique(FIH_Player_Stats_for_Sherie$Countries))

# Define UI --------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("FIH Women's Hockey World Cup 2022",
                           
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      selectInput(
                                        inputId = "y",
                                        label = "Y-axis:",
                                        choices = c(
                                          "Played in tournament " = "Played",
                                          "Number of goals scored" = "Goals",
                                          "Field Goal" = "FG",
                                          "Penalty Corner" = "PC",
                                          "Penalty Stroke" = "PS"
                                        ),
                                        selected = "Goals"
                                      ),
                                      
                                      selectInput(
                                        inputId = "x",
                                        label = "X-axis:",
                                        choices = c(
                                          "Played in tournament " = "Played",
                                          "Number of goals scored" = "Goals",
                                          "Field Goal" = "FG",
                                          "Penalty Corner" = "PC",
                                          "Penalty Stroke" = "PS"
                                        ),
                                        selected = "PS"
                                      ),
                                      
                                      selectInput(
                                        inputId = "z",
                                        label = "Color by:",
                                        choices = c(
                                          "Country of players" = "Countries",
                                          "Yellow Card" = "YellowC",
                                          "Red Card" = "RedC"
                                        ),
                                        selected = "Countries"
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
                                        inputId = "Countries",
                                        label = "Select the Countries:",
                                        choices = All_countries,
                                        selected = "Argentina",
                                        multiple = TRUE
                                      ),
                                      
                                      downloadButton('download',"Download data")
                                      
                                    ),
                                    
                                    mainPanel(
                                      plotOutput(outputId = "scatterplot", hover = "plot_hover"),
                                      dataTableOutput(outputId = "FIH_Player_Stats_for_Sherietable"),
                                      br(),
                                      dataTableOutput(outputId = "FIH_Player_Stats_for_Sherietablehover")
                                      
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           tabPanel("About", 
                                    titlePanel("About"),
                                    HTML('<center><img src="https://upload.wikimedia.org/wikipedia/en/6/67/2022_Hockey_Women%27s_World_Cup_logo.jpg"></center>'),
                                    br(),
                                    p("The 2022 Women's FIH Hockey World Cup was the 15th edition of the Women's FIH Hockey World Cup, the quadrennial world championship for women's national field hockey teams organized by the International Hockey Federation. It was held from 1 to 17 July 2022 at the Estadi Olímpic de Terrassa in Terrassa, Spain and at the Wagener Stadium in Amstelveen, the Netherlands."),
                                    br(),
                                    strong("Terminology:"),
                                    p("where",
                                      span("FG", style = "color:blue"), 
                                      "represents field goal,", 
                                      span("PC", style = "color:orange"), 
                                      "represents penalty corner,",
                                      span("PS", style = "color:green"),
                                      "represents penalty stroke."),
                                    br(),
                                    strong("How to use:"),                                    
                                    p("There are different areas you can take a look at such as number of goals scored, the number of match a player played during the tournament, field goals, penalty corners and stroke. By hovering the mouse on the plot it will show the player names as well."),
                                    br(),
                                    strong("References"),                                    
                                    p("1. Wikimedia Foundation. (2022, August 7). 2022 women's FIH Hockey World Cup. Wikipedia. Retrieved October 1, 2022, from https://en.wikipedia.org/wiki/2022_Women's_FIH_Hockey_World_Cup "),
                           ) #tabPanel(), About
                           
                ) # navbarPage()
) # fluidPage()


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  output$scatterplot <- renderPlot({
    ggplot(data = FIH_Player_Stats_for_Sherie, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$FIH_Player_Stats_for_Sherietable <- renderDataTable({
    nearPoints(FIH_Player_Stats_for_Sherie, input$plot_hover) %>%
      select(Players, Played, Goals, FG, PC, PS, GreenC, YellowC, RedC)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
