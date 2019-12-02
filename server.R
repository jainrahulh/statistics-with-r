library(shiny) 
library(pdfetch)

shinyServer( 
  function(input, output,session) {
    myData <- reactive({
      validate(
        need(input$fileType != "Select", "Please select a data set")
      )
      
      if (input$fileType == 'builtin') {
        file1 <- get(input$builtIn)
        if (is.null(file1)) {
          return() 
        }
        data = file1
      } else if (input$fileType == 'upload') {
        file1 <- input$datafile
        if (is.null(file1)) {
          return() 
        }
        separator = ','
        if(input$separator !='')
          separator = input$separator
        
        data = read.csv(file=file1$datapath,sep = separator)
      } else if (input$fileType == 'url') {
        
        url = input$urlpath
        data <- read.csv(url)
        if (is.null(data)) { 
          return() 
        }
      } else if (input$fileType == 'yahoo') {
        data <- data.frame(pdfetch_YAHOO(input$ticker,
                          fields = c("open","high","low","close"),
                          from = as.Date(input$fromDate), to = as.Date(input$toDate),
                          interval = input$interval))
        if (is.null(data)) {
          return() 
        }
      }
      data
    })
    
    observe({
      updateSelectInput(session, "columns",
                        choices = colnames(myData()))
      updateSelectInput(session, "columns.2",
                        choices = colnames(myData()))
      updateSelectInput(session, "predictor",
                        choices = colnames(myData()))
      updateSelectInput(session, "response",
                        choices = colnames(myData()))
      updateSelectInput(session, "decide",
                        choices = colnames(myData()))
    })
    
    output$extdata = DT::renderDataTable({
      extdata <- myData()
      tryCatch({
        DT::datatable(extdata, options = list(lengthChange = TRUE))
      },error=function(e){
        
      }, warning=function(e){
        
      })
      
    })
    
    output$prob <- renderPrint({
      df <- na.omit(data.frame(myData()))
      x <- df[,input$columns]
      
      # normal
      if (input$conmodel == 'normal') { 
        print(paste('Predicted Value :',mean(rnorm(input$s,mean(x), sd(x)))))
      }
      
      # exponential
      if (input$conmodel == 'exponential') {
        print(paste('Predicted Value :',mean(rexp(input$s,1/mean(x)))))
      }
      
      if (input$conmodel == 'uniform') {
        print(paste('Predicted Value :',mean(runif(input$s,1/mean(x)))))
      }
      
      if (input$conmodel == 'hypothesis') {
        
        confLevel = 1 - as.numeric(input$alpha)
        t = t.test(x,mu=as.numeric(input$mu),alternative = input$alternative, conf.level = confLevel)
        decision = 'Accept H_0'
        if(t$p.value<input$alpha){
          decision = 'Reject H_0'
        }
        print(paste("Decision is -> ",decision))
        #print(paste("Test  -> ",t))
        print(t)
      }
      
      if (input$conmodel == 'hypothesis.2') {
        df <- na.omit(data.frame(myData()))
        y <- df[,input$columns.2]
        confLevel = 1 - as.numeric(input$alpha.2)
        t = t.test(x,y,alternative = input$alternative.2, conf.level = confLevel)
        decision = 'Accept H_0'
        if(t$p.value<input$alpha){
          decision = 'Reject H_0'
        }
        print(paste("Decision is -> ",decision))
        #print(paste("Test  -> ",t))
        print(t)
      }
    })
    
    #Histogram
    output$histogram <- renderPlot({
      df <- na.omit(data.frame(myData()))
      x <- df[,input$columns]
      print(typeof(x))
      hist(x, xlab = input$columns, main = paste("Histogram of" , input$columns), col = c("orange", "red", "gray", "green"))
    })
    
    #Linear Regression
    output$linear <- renderPlot({
      dataset <- na.omit(data.frame(myData()))
      col1<- mapply(anyNA,dataset)  #apply function anyNA() on all columns of airquality dataset
      col1
      
      #Impute monthly mean in Ozone is Target
      
      for (i in 1:nrow(dataset)){
        if(is.na(dataset[i,input$predictor])){
          dataset[i,input$predictor]<- mean(dataset[which(dataset[,input$decide]==dataset[i,input$decide]),input$predictor],na.rm = TRUE)
        }
        #Impute monthly mean in Solar.R is Predictor
        
        if(is.na(dataset[i,input$response])){
          dataset[i,input$response]<- mean(dataset[which(dataset[,input$decide]==dataset[i,input$decide]),input$response],na.rm = TRUE)
        }
        
      }
      #Normalize the dataset so that no particular attribute has more impact on clustering algorithm than others.
      normalize<- function(x){
        return((x-min(x))/(max(x)-min(x)))
      }
      dataset<- normalize(dataset) # replace contents of dataset with normalized values
      str(dataset)
      
      
      # Apply Linear regression algorithm LSM on 'ozone' and 'Solar.R'
      Y<- dataset[,input$response] # select Target attribute
      X<- dataset[,input$predictor] # select Predictor attribute
      
      model1<- lm(Y~X)
      model1      # provides regression line coefficients i.e. slope and y-intercept
      
      plot(Y~X)      # scatter plot between X and Y
      abline(model1, col="blue", lwd=3)       # add regression line to scatter plot to see relationship between X and Y
      
      # Apply Linear regression algorithm LSM on 'Ozone' and 'wind'
      
      Y<- dataset[,input$response]     # select Target attribute
      X<- dataset[,input$predictor]      # select Predictor attribute
      
      model2<- lm(Y~X)
      model2      # provides regression line coefficients i.e. slope and y-intercept
      
      
      plot(Y~X) # scatter plot between X and Y
      abline(model2, col="blue", lwd=3)     # add regression line to scatter plot to see relationship between X and Y
      
      
      #Prediction of Values
      #use the line coefficients for two equations that we got in model1 and model2 to predict value of Target for any given value of Predictor.
      
      # Prediction of 'Ozone' when 'Solar.R'= 20
      p1<- predict(model1,data.frame("X"=20))
      p1
      
      
      # Prediction of 'Ozone' when 'Wind'= 15
      p2<- predict(model2,data.frame("X"=15))
      p2
    })
  } 
)
