#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling"),
    titlePanel("Downloading Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
      # Button
      downloadButton("downloadData", "Download")
            output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
            # Horizontal line ----
            tags$hr(),
            # action button
            actionButton("run", "Graph Linear Model"),
           # Horizontal line ----
            tags$hr(),
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$distPlot <- renderPlot({
        ggplot() +
  geom_point(aes(x = dataInput()$x, y = dataInput()$y),
             colour = 'red') +
  ggtitle('X vs Y') +
  xlab('X') +
  ylab('Y')
    })
 
 #Linear Model   
    output$lmPlot <- renderPlot({
        coefs <- coef(model(), 2)
        intercept <- round(coefs[1], 2)
        slope <- round(coefs[2],2)    
        
          ggplot() +
              geom_point(aes(x = dataInput()$x, y = dataInput()$y),
                         colour = 'red') +
              geom_line(aes(x = dataInput()$x, y = predict(model(), newdata = dataInput())),
                        colour = 'blue') +
              ggtitle('X vs Y') +
              xlab('X') +
              ylab('Y') +
              geom_text(aes(x=10,y=11,label = paste("intercept =",intercept))) +
              geom_text(aes(x=10,y=12,label = paste("slope =",slope))) +
              geom_text(aes(x=10,y=13,label = paste("coefficient =",coefs)))
    })
    
   model <- eventReactive(input$run, {
       lm(formula = y ~ x,
        data = dataInput())
  })  
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
