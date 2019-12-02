library(shiny)
library(shinythemes)


ui = tagList(
  shinythemes::themeSelector(),
  navbarPage(
    #theme = "cerulean",  # <--- To use a theme, uncomment this
    "CA ONE",
    tabPanel("Navbar 1",
             sidebarPanel(
               selectInput("fileType", "Select",
                           choices = c("Select" = "Select","Built-IN" = "builtin",
                                       "Upload" = "upload",
                                       "URL" = "url",
                                       "YAHOO!" = "yahoo")
               ),
               conditionalPanel(
                 condition = "input.fileType == 'builtin'",
                 selectInput(inputId = "builtIn", label = "Built In data set", choices = ls("package:datasets")),
               ),
               # Input: Select a file
               conditionalPanel(
                 condition = "input.fileType == 'upload'",
                 fileInput("datafile", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"))
               ),
               #Read from url
               conditionalPanel(
                 condition = "input.fileType == 'url'",
                 textInput(inputId = "urlpath", label = "Enter URL to csv", value="https://people.sc.fsu.edu/~jburkardt/data/csv/cities.csv")
               ),
               #YAHOO!
               conditionalPanel(
                 condition = "input.fileType == 'yahoo'",
                 textInput(inputId = "ticker", label = "Enter TICKER", value = "GOOGL"),
                 dateInput(inputId = "fromDate", label = "From", value = Sys.Date(), min = NULL, max = Sys.Date(),
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en"),
                 dateInput(inputId = "toDate", label = "To", value = Sys.Date(), min = NULL, max = Sys.Date(),
                           format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                           language = "en"),
                 radioButtons(inputId = "interval", label = "Interval", inline = TRUE,
                              choices = c("Day" = "1d","Week" = "1wk","Month" = "1mo"), selected = "1mo")
               ),
               conditionalPanel(
                 condition = "input.fileType == 'upload'",
                 textInput(inputId='separator', label='File Separator',value=",")
               ),
               selectInput(inputId = "columns", label = "Select a Column", choices = ""),
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis.2'",
                 selectInput(inputId = "columns.2", label = "Select 2nd Column", choices = "")
               ),
               
               sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10),
               selectInput("conmodel", "Select Model",
                           choices = c("Normal" = "normal",
                                       "Exponential" = "exponential",
                                       "Uniform" = "uniform",
                                       "Hypothesis" = "hypothesis",
                                       "Hypothesis 2" = "hypothesis.2",
                                       "Linear Regression" = "linear"),
                           selected = "exponential"
               ),
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis'",
                 numericInput("mu","Mean of Population",value=5),
                 radioButtons("alpha","Significance Level", 
                              choices = c("0.025" = "0.025", "0.05" = "0.05","0.1" = "0.01"), 
                              inline=TRUE),
                 radioButtons("alternative","Alternative", choices = c("Lower Tailed" = "less", "Upper Tailed" = "greater","Two-tailed" = "two.sided"), inline=TRUE)
               ),
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis.2'",
                 radioButtons("alpha.2","Significance Level", choices = c("0.025" = "0.025", "0.05" = "0.05","0.01" = "0.01"), inline=TRUE),
                 radioButtons("alternative.2","Alternative", choices = c("Lower Tailed" = "less", "Upper Tailed" = "greater","Two-tailed" = "two.sided"), inline=TRUE)
               ),
               conditionalPanel(
                 condition = "input.conmodel == 'linear'",
                 selectInput(inputId = "predictor", label = "Predictor column", choices = ""),
                 selectInput(inputId = "response", label = "Response Column", choices = ""),
                 selectInput(inputId = "decide", label = "Decision", choices = "")
               )
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("View", (DT::dataTableOutput('extdata'))),
                 tabPanel("Prediction",
                          h1(tableOutput('prob'))
                 ),
                 tabPanel("GLM", plotOutput('linear')),
                 tabPanel("Histogram",plotOutput('histogram'))
               )
             )
    ),
    tabPanel("Navbar 2", "This panel is intentionally left blank"),
    tabPanel("Navbar 3", "This panel is intentionally left blank")
  )
)
