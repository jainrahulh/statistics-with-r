### Application name : CA1 B9DA101 - Statistics for Data Analytics
### Course : MSc (Data Analytics) - Sep 2019 - Group C 
### Developed by : Rahul Jain(10533047) / Viraj Kamdar(10527201) / Vaibhav Shah(10532808) / Jayesh Aswar(10531366)
### College : Dublin Business School 
### URL : https://jainrahul.shinyapps.io/ShinyApp/


library(shiny)
library(shinythemes)
library(shinyjs)

ui = fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme('paper'),
  useShinyjs(),
  navbarPage(
    "Statistics",
    tabPanel(icon = icon("fas fa-bars"),"Dashboard",
             div(id="SidebarpanelD",sidebarPanel(
               ###<<< Begin   <<< Vaibhav Shah(10532808) <<<
               selectInput("fileType", "Data Source",
                           choices = c("Select" = "Select","Built-IN" = "builtin",
                                       "Upload" = "upload",
                                       "URL" = "url")
               ###<<< END   <<< Vaibhav Shah(10532808) <<<
               ),
               ###<<< Begin   <<< Rahul Jain(10533047) <<<
               conditionalPanel(
                 condition = "input.fileType == 'builtin'",
                 selectInput(inputId = "builtIn", label = "Built In data set", choices = ls("package:datasets"),selected = "cars"),
               ),
               ###<<< End   <<< Rahul Jain(10533047) <<<
               
               ###<<< Begin   <<< Jayesh Aswar(10531366) <<<
               # Input: Select a file
               conditionalPanel(
                 condition = "input.fileType == 'upload'",
                 fileInput("datafile", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"))
               ),
               ###<<< End   <<< Jayesh Aswar(10531366) <<<
               
               ###<<< Begin   <<< Rahul Jain(10533047) <<<
               #Read from url
               conditionalPanel(
                 condition = "input.fileType == 'url'",
                 textInput(inputId = "urlpath", label = "Enter URL to csv", value="https://people.sc.fsu.edu/~jburkardt/data/csv/cities.csv")
               ),
               ###<<< End   <<< Rahul Jain(10533047) <<<
               
               ###<<< Begin   <<< Viraj Kamdar(10527201) <<<
               conditionalPanel(
                 condition = "input.fileType == 'upload'",
                 textInput(inputId='separator', label='File Separator',value=",")
               ),
               selectInput(inputId = "columns", label = "Select a Column", choices = ""),
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis.2'",
                 selectInput(inputId = "columns.2", label = "Select 2nd Column", choices = "")
               ),
               
               sliderInput("s", "Number of simulated data" ,min=1, max=1000, value = 10),
               selectInput("conmodel", "Select Model",
                           choices = c("Normal" = "normal",
                                       "Exponential" = "exponential",
                                       "Uniform" = "uniform",
                                       "Hypothesis" = "hypothesis",
                                       "Hypothesis 2" = "hypothesis.2"),
                           selected = "exponential"
               ),
               ###<<< End   <<< Viraj Kamdar(10527201) <<<
               
               ###<<< Begin   <<< Rahul Jain(10533047) <<<
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis'",
                 numericInput("mu","Mean of Population",value=5),
                 radioButtons("alpha","Significance Level", 
                              choices = c("0.025" = "0.025", "0.05" = "0.05","0.1" = "0.01"), 
                              inline=TRUE),
                 radioButtons("alternative","Alternative", choices = c("Lower Tailed" = "less", "Upper Tailed" = "greater","Two-tailed" = "two.sided"), inline=TRUE)
               ),
               ###<<< End   <<< Rahul Jain(10533047) <<<
               
               
               ###<<< Begin   <<< Jayesh Aswar(10531366) <<<
               conditionalPanel(
                 condition = "input.conmodel == 'hypothesis.2'",
                 radioButtons("alpha.2","Significance Level", choices = c("0.025" = "0.025", "0.05" = "0.05","0.01" = "0.01"), inline=TRUE),
                 radioButtons("alternative.2","Alternative", choices = c("Lower Tailed" = "less", "Upper Tailed" = "greater","Two-tailed" = "two.sided"), inline=TRUE)
               )
             )),
             ###<<< End   <<< Jayesh Aswar(10531366) <<<
             
              ###<<< Begin   <<< Vaibhav Shah(10532808) <<<
             mainPanel(
               actionButton("toggleSidebarD", "Toggle"),
               h1(),
               navbarPage("Dashboard",
                 tabPanel("View", wellPanel(DT::dataTableOutput('extdata')),style="color:#000000"),
                 tabPanel("Summary",
                          h1("Descriptive Analytics",style="color:teal"),
                          verbatimTextOutput("sum"),
                          h1("Box Plot",style="color:teal"),
                          wellPanel(plotOutput("box"))
                          ),
                 tabPanel(title = uiOutput("title_panel"),
                          h1("Prediction",style="color:teal"),
                          h1(verbatimTextOutput('prob')),
                          plotOutput("plotModel")
                 ),
                 tabPanel("Histogram",plotOutput('histogram'))
               )
             )
    ),
            ###<<< End   <<< Vaibhav Shah(10532808) <<<
    
    ###<<< Begin   <<< Viraj Kamdar(10527201) <<<
    tabPanel(icon = icon("fas fa-chart-line"),"GLM", 
             sidebarPanel(
               selectInput("ds", "Data Source :",
                           c("File" = "file",
                             "Built-in" = "ib"
                           )),
               # Input: Select a files ----
               conditionalPanel(
                 condition = "input.ds == 'file'",
                 fileInput("datafileg", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values/text/plain",
                                      ".csv"))
               ),
               conditionalPanel(
                 condition = "input.ds == 'ib'",
                 selectInput(inputId = "ib", label = "Select a Dataset", choices = ls("package:datasets"))
               ),
               
               selectInput(inputId = "tarvar", label = "Response Variable", choices = ""),
               
               selectInput(inputId = "indvar", label = "Independent Variables", multiple = TRUE, choices = ""),
               
               sliderInput("ratio", "Partition", min = 1, max = 100, value = 80),
               
               uiOutput("Input_Ind"),
               
             ),
             
             mainPanel(
               navbarPage("GLM",
                           tabPanel("View", DT::dataTableOutput("glmextdata")),
                           tabPanel("Plot", h1("Scatterplot",style="color:teal"), plotOutput("simplePlot")),
                           tabPanel("Graphs", plotOutput("glmperf")),
                           tabPanel("RMSE", DT::dataTableOutput("RMSE")),
                           tabPanel("Predcited", DT::dataTableOutput("Prediction"))
               )
             )
    ),
              ###<<< End   <<< Viraj Kamdar(10527201) <<<
    
               ###<<< Begin   <<< Rahul Jain(10533047) <<<
    tabPanel("YAHOO!", icon = icon("fas fa-yahoo"),
             div(id="Sidebar",sidebarPanel(
               textInput(inputId = "ticker", label = "Enter TICKER", value = "GOOGL"),
               dateInput(inputId = "fromDate", label = "From", value = Sys.Date() - 90, min = NULL, max = Sys.Date(),
                         format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                         language = "en"),
               dateInput(inputId = "toDate", label = "To", value = Sys.Date(), min = NULL, max = Sys.Date(),
                         format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                         language = "en"),
               radioButtons(inputId = "interval", label = "Interval", inline = TRUE,
                            choices = c("Day" = "1d","Week" = "1wk","Month" = "1mo"), selected = "1d")
             )),
             mainPanel(actionButton("toggleSidebar", "Toggle sidebar"),
               h1("Live Finance Data",style="color:teal"),
               tabPanel("View", wellPanel((DT::dataTableOutput('yahooData'))),
                        wellPanel(plotOutput("yahooLine")))
             )
    ),
              ###<<< End   <<< Rahul Jain(10533047) <<<
       
             ###<<< Begin   <<< Vaibhav Shah (10532808) <<<
    
    tabPanel("About Us",icon = icon("fas fa-users"),
             mainPanel(
               tabPanel("ABOUT US", 
                        fluidPage(
                          fluidRow(
                            column(12,
                                   tableOutput('aboutUs')
                            )
                          )
                        ),
                       h1("https://github.com/jainrahulh/statistics-with-r",style="color:#000") 
               )
             )
    )
            ###<<< End   <<< Vaibhav Shah (10532808) <<<  
    
            ###<<< Begin   <<< Jayesh Aswar(10531366) <<<
  ),tags$style(type = 'text/css', '.navbar {
                           font-family: Lucida Console;
                           font-size: 15px;
                           font-weight: bold}'
               
  ),tags$style(type = 'text/css', 'h1 {
                           font-family: Lucida Console;
                           font-size: 20px;
                           color: #FF0000;
                           font-weight: bold}'
               
  ),tags$style(type = 'text/css', '.navbar .navbar-default {
                           background-color: #4979;}'
           
  ),tags$style(type = 'text/css', '#toggleSidebarD {
                           background-color: #4979;}' 
               
  ),tags$style(type = 'text/css', '#toggleSidebar{
                           background-color: #4979;}'
  ),tags$style(type = 'text/css', '.navbar{
               background-color: #4979;}'
  ),tags$style(type = 'text/css', 'body .navbar-default .navbar-nav li a{
               color: #fff;font-size: 17px;}'
  ),tags$style(type = 'text/css', 'body .navbar-default .navbar-brand{
               color: #fff;}'
  )
)
            ###<<< End   <<< Jayesh Aswar(10531366) <<<

