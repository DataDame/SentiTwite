
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sentiment Analysis of Twitter Messages"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      textInput("text", label = h4("Twitter text to analyze:"), 
                value = "Donald Trump"),
      
      sliderInput("slider", label = h4("Number of messages to analyze:"),
                  min = 0, max = 1000, value = 100, step = 10),
      
      dateInput("StartDate", "Start Date:", value = "2015-01-01"),
      dateInput("EndDate", "End Date:", value = Sys.Date()),
      
      selectInput("Type", "Select Message Type:", choices = c('Mixed'='mixed','Recent'='recent','Popular'='popular')),
      
      submitButton("Submit")
    
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Histogram", plotOutput("distPlot")),
                    tabPanel("Polarity", plotOutput("polarityPlot")),
                    tabPanel("Word Cloud", plotOutput("wordCloud")),
                    tabPanel("Messages", tableOutput("tableMessages")),
                    tabPanel("Help", verbatimTextOutput("helpText"))
        )
    )
  )
))