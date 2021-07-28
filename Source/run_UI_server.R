library(shiny)

# Define UI ----
  ui <- fluidPage(
    titlePanel("Instrument Coordinator"),
    
    sidebarLayout(
      sidebarPanel(strong('Sample Table:'),
                   br(),
                   actionButton('load', 'Load'),
                   br(),
                   actionButton('refresh', 'Reload'),
                   br(),
                   actionButton('import', 'Import'),
                   br(),
                   br(),
                   strong('Run Control:'),
                   br(),
                   actionButton('run', 'Start'),
                   br(),
                   actionButton('stop', 'Stop'),
                   br(),
                   actionButton('goto', 'Go to Sample'),
                   br(),
                   actionButton('kill', 'Kill'),
                   ),
      
      mainPanel("main panel")
    )
  )

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
