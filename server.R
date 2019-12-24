### Application name : CA1 B9DA101 - Statistics for Data Analytics
### Course : MSc (Data Analytics) - Sep 2019 - Group C 
### Developed by : Rahul Jain(10533047) / Viraj Kamdar(10527201) / Vaibhav Shah(10532808) / Jayesh Aswar(10531366)
### College : Dublin Business School 
### URL : https://jainrahul.shinyapps.io/ShinyApp/

#install.packages("pastecs")
library(shiny)
library(pdfetch)
library(shinyjs)

shinyServer( 
  function(input, output,session) {
    
    ###<<< Begin   <<< Rahul Jain(10533047) <<<
    ################################Start of Yahoo Finance#################################
    #Fetching Yahoo Finance Data
    yData <- reactive({
      
      tryCatch({
        data <- data.frame(pdfetch_YAHOO(input$ticker,
                                         fields = c("open","high","low","close"),
                                         from = as.Date(input$fromDate), to = as.Date(input$toDate),
                                         interval = input$interval))
      }, error=function(e) {
        #cat(paste("in err handler\n"),e)
      }, warning=function(w) {
        #cat(paste("in warn handler\n"),w)
      })
      if (is.null(data)) {
        return(NULL)
      }
      data
    })
    
    ##View Yahoo Data in DataTable
    output$yahooData = DT::renderDataTable({
      yahooData <- yData()
      validate(
        need(!is.null(yahooData), "Enter a Valid Ticker")
      )
      tryCatch({
        DT::datatable(yahooData, options = list(lengthChange = TRUE))
      },error=function(e){
        print("error")
      }, warning=function(e){
        print("warning")
      })
      
    })
    ##Yahoo Draw Line Chart for historic data
    output$yahooLine = renderPlot({
      yahooData <- yData()
      validate(
        need(!is.null(yahooData), "Enter a Valid Ticker")
      )
      tryCatch({
        DT::datatable(yahooData, options = list(lengthChange = TRUE))
      },error=function(e){
        print("error")
      }, warning=function(e){
        print("warning")
      })
      
      plot(yahooData[,4],data=yahooData,ylab = "CLOSE",type = "l", col = "green", main = "History of Stock")
      
    })
    #################################End of Yahoo Finance#################################
    ###<<< End   <<< Rahul Jain(10533047) <<<
    
    ###<<< Begin   <<< Rahul Jain(10533047) <<<
    #Selecting data source and retrieving the data and columns dynamically 
    myData <- reactive({
      validate(
        need(input$fileType != "Select", "Please select a data set")
      )
      
      if (input$fileType == 'builtin') {
        file1 <- get(input$builtIn)
        if (is.null(file1)) {
          return()
        }
        data = data.frame(file1)
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
      }
      data
    })
    ###<<< End   <<< Rahul Jain(10533047) <<<
    
    ###<<< Begin   <<< Viraj Kamdar(10527201) <<<
    
    #Observe function to populate select box with column names if data source changes
    observe({
      updateSelectInput(session, "columns",
                        choices = colnames(myData()))
      updateSelectInput(session, "columns.2",
                        choices = colnames(myData()))
    })
    
    #View selected data in a datatable
    output$extdata = DT::renderDataTable({
      extdata <- myData()
      tryCatch({
        DT::datatable(extdata, options = list(lengthChange = TRUE))
      },error=function(e){
        
      }, warning=function(e){
        
      })
      
    })
    
    
    #Prediction of value
    output$prob <- renderPrint({
      df <- na.omit(data.frame(myData()))
      x <- df[,input$columns]
      
      # normal distribution
      if (input$conmodel == 'normal') { 
        print(paste('Predicted Value :',mean(rnorm(input$s,mean(x), sd(x)))))
      }
      
      # exponential distribution
      if (input$conmodel == 'exponential') {
        print(paste('Predicted Value :',mean(rexp(input$s,1/mean(x)))))
      }
      
      #uniform distribution
      if (input$conmodel == 'uniform') {
        print(paste('Predicted Value :',mean(runif(input$s,1/mean(x)))))
      }
    
      ###<<< End   <<< Viraj Kamdar(10527201) <<<
      
        
      ###<<< Begin   <<< Rahul Jain(10533047) <<<
      #Hypothesis Test for One population
      if (input$conmodel == 'hypothesis') {
        
        confLevel = 1 - as.numeric(input$alpha)
        t = t.test(x,mu=as.numeric(input$mu),alternative = input$alternative, conf.level = confLevel)
        c_value = abs(qnorm(as.numeric(input$alpha), length(x)))
        decision = 'Accept H_0'
        if(as.numeric(t$p.value) < as.numeric(input$alpha)){
          decision = 'Reject H_0'
        }
        print(paste("Decision is -> ",decision))
        print(t)
      }
      ###<<< End   <<< Rahul Jain(10533047) <<<
      
      ###<<< Begin   <<< Vaibhav Shah(10532808) <<<
      
      #Hypothesis Test for Two population
      if (input$conmodel == 'hypothesis.2') {
        df <- na.omit(data.frame(myData()))
        y <- df[,input$columns.2]
        confLevel = 1 - as.numeric(input$alpha.2)
        t = t.test(x,y,alternative = input$alternative.2, conf.level = confLevel)
        c_value = abs(qt(as.numeric(input$alpha)/2, length(x))) # 75% confidence, 1 sided (same as qt(0.75, 40))
        decision = 'Accept H_0'
        if(as.numeric(t$p.value) < as.numeric(input$alpha)/2){
          decision = 'Reject H_0'
        }
        print(paste("Decision is -> ",decision))
        print(t)
      }
    })
    
    #Histogram Plot
    output$histogram <- renderPlot({
      df <- na.omit(data.frame(myData()))
      x <- df[,input$columns]
      print(typeof(x))
      hist(x, xlab = input$columns, main = paste("Histogram of" , input$columns), col = c("orange", "red", "gray", "green"))
    })
    
    ###<<< End   <<< Vaibhav Shah(10532808) <<<
    
    
    ###<<< Begin   <<< Rahul Jain(10533047) <<<
    #Scatterplot
    ##Student ID : 10533047
    output$plotModel <- renderPlot({
      df <- na.omit(data.frame(myData()))
      
      #To add noise in data
      # normal
      if (input$conmodel == 'normal') { 
        df <- df + rnorm(nrow(df))
      }
      
      # exponential
      if (input$conmodel == 'exponential') {
        df <- df + rexp(nrow(df))
      }
      
      if (input$conmodel == 'uniform') {
        df <- df + runif(nrow(df))
      }
      
      plot(df,col="darkseagreen",pch=16)
    })
    ###<<< End   <<< Rahul Jain(10533047) <<<
    
    ###<<< Begin   <<< Rahul Jain(10533047) <<<
    #Descriptive Analytics using "pastecs" library
    ##Student ID : 10533047
    output$sum <- renderPrint({
      library(pastecs)
      par(mfrow = c(1,2))
      x <- myData()
      x <- na.omit(x)
      x <- x[complete.cases(x),]
      #summary(x)
      stat.desc(x)
    })
    ###<<< End   <<< Rahul Jain(10533047) <<<
    
   ###<<< Begin   <<< Viraj Kamdar(10527201) <<<
    
    #Boxplot to understand outliers
    output$box <- renderPlot({
      par(mfrow = c(1,1))
      x<-myData()
      boxplot(x,col="darkseagreen",border="black")
    })
    
    
    output$dbscan <- renderPlot({
      par(mfrow = c(1,1))
      x<-myData()
      boxplot(x,col="darkseagreen",border="black")
    })
    
    #Dynamic Tab name change based on select box
    output$title_panel = renderText({
      switch(input$conmodel, "normal" = "Prediction",
             "exponential" = "Prediction",
             "uniform" = "Prediction",
             "hypothesis" = "Hypothesis",
             "hypothesis.2" = "Hypothesis") 
    })
    
    
    #Toggle sidebar
    observeEvent(input$toggleSidebar, {
      shinyjs::toggle(id = "Sidebar",anim = TRUE)
    })
    
    observeEvent(input$toggleSidebarD, {
      shinyjs::toggle(id = "SidebarpanelD", anim = TRUE)
    })
    
    
    ###<<< End   <<< Viraj Kamdar(10527201) <<<
    
    #################################Start of GLM CODE#################################
    
    ###<<< Begin   <<< Jayesh Aswar (10531366) <<<
    
    RMSE <- 0
    values <- reactiveValues()
    glmData <- reactive({
      
      switch(input$ds,
             file = {
               file1 <- input$datafileg
               if (is.null(file1)) {
                 return()
               }
               data = read.csv(file=file1$datapath)
             },
             
             ib = { 
               data = data.frame(get(input$ib))
             }
             
      )
      return(data)
    })
    
    observe({
      updateSelectInput(session, "indvar",
                        choices = colnames(glmData()))
      
      updateSelectInput(session, "tarvar",
                        choices = colnames(glmData()))
      
      
    })
    
    output$glmextdata = DT::renderDataTable({
      
      DT::datatable(glmData(), options = list(lengthChange = TRUE))
      
    })
    
    output$summary <- renderPrint({
      df <- na.omit(glmData())
      TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
      colnames(TarIndData) = c(input$tarvar,input$indvar)
      summary(TarIndData)
    })
    
    ###<<< End   <<< Jayesh Aswar (10531366) <<<  
    
    ###<<< Begin   <<< Viraj Kamdar (10527201) <<<
    
    myColors <- reactive({
      df <- na.omit(glmData())
      TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
      colnames(TarIndData) = c(input$tarvar,input$indvar)
      switch(input$indvar)
    })
    
    # Show a simple x,y plot
    output$simplePlot <- renderPlot({
      df <- na.omit(glmData())
      TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
      colnames(TarIndData) = c(input$tarvar,input$indvar)
      
      
      plot(TarIndData[,c(input$indvar,input$tarvar)], xlab = input$indvar, ylab = input$tarvar, col="darkseagreen", pch=16)
      
    })
    
    
    
    
    output$glmperf <- renderPlot({
      
        df <- na.omit(glmData())
        TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
        colnames(TarIndData) = c(input$tarvar,input$indvar)
        
        
        colnames(TarIndData)[1] <- "Y"
        
        set.seed(199)
        n=nrow(TarIndData)
        indexes = sample(n,n*(input$ratio/100))
        trainset = data.frame(TarIndData[indexes,])
        testset = data.frame(TarIndData[-indexes,])
        actual <- testset$Y
        pred_test <- data.frame(testset)
        
        
        full.model <- glm(Y ~., data = trainset, family='gaussian')
        #summary(full.model)
        
        values$full <- full.model
        
        pred_full <- predict(full.model, testset[,input$indvar])
        
        rmse_full = sqrt(sum((pred_full -actual)^2)/(nrow(testset)))
        # apply stepAIC to reduce model and find rmse for reduced model
        
        reduced.model=stepAIC(full.model)
        
        values$full <- full.model
        values$reduced <- reduced.model
        pred_red = predict(reduced.model, testset[,input$indvar])
        rmse_red = sqrt(sum((pred_red -actual)^2)/(nrow(testset)))
        
        values$rmse <- data.frame('Full'=rmse_full, 'Reduced'=rmse_red)
        
        par(mfrow=c(1,2))
        
        plot(actual,type= "o", col = "red", xlab = "observations", ylab = input$tarvar,
             main = "FULL")
        
        lines(pred_full, type = "o", col = "blue")
        
        legend(
          "topleft",
          lty=c(1,1),
          col=c("red", "blue"),
          legend = c("Real", "Predicted")
        )
        
        plot(actual,type = "o", col = "red", xlab = "observations", ylab = input$tarvar,
             main = "Reduced")
        
        lines(pred_red, type = "o", col = "blue")
        
        legend(
          "topleft",
          lty=c(1,1),
          col=c("red", "blue"),
          legend = c("Real", "Predicted")
        )
    })
    
     ###<<< End   <<< Viraj Kamdar (10527201) <<<
    
    
    ###<<< Begin   <<< Vaibhav Shah (10532808) <<<
    
    output$selData <- DT::renderDataTable({
      
      df <- glmData()
      TarIndData <- cbind(df[,input$tarvar],df[,input$indvar])
      colnames(TarIndData) = c(input$tarvar, input$indvar)
      
      DT::datatable(TarIndData, options = list(lengthChange = TRUE))
      
    })
    
    output$RMSE <- DT::renderDataTable({
      
      DT::datatable(values$rmse, options = list(lengthChange = TRUE))
    })
    
    output$Input_Ind <- renderUI({
      Var_count <- 0
      Var_count <- length(input$indvar)
      
      if (Var_count != 0) {
        lapply(1:Var_count, function(i) {
          numericInput(inputId = paste0(input$indvar[i]), label = input$indvar[i], value = 0)
          
        })
      }
    })
    
    forecast_out <- reactive({
      Var_Count <- length(input$indvar)
      
      new_data <- as.numeric(paste(lapply(1:Var_Count, function(i) {
        inputName <- paste0(input$indvar[i])
        input[[inputName]]
        
      })))
      
      input_data <- data.frame(t(new_data))
      
      for (i in 1:Var_Count)
      {
        colnames(input_data)[i] <- input$indvar[i]
        
      }
      
      new_predict_full <- predict(values$full,input_data)
      new_predict_red <- predict(values$reduced,input_data)
      
      pred_data_new <- data.frame(new_predict_full, new_predict_red)
      
      colnames(pred_data_new)[1] <- paste('Full Mode - ', input$tarvar)
      colnames(pred_data_new)[2] <- paste('Reduced Mode - ', input$tarvar)
      
      #pred_red = predict( values$reduced, new_data)
      return(pred_data_new)
      
    })
    
    output$Prediction <- DT::renderDataTable({
      
      DT::datatable(forecast_out(), options = list(lengthChange = TRUE))
    })
    
    ###<<< End   <<< Vaibhav Shah(10532808) <<<
    
    #################################End of GLM CODE#################################
    
    ###<<< Begin   <<< Jayesh Aswar (10531366) <<<
    
    output$aboutUs <- renderTable({
      smoke <- matrix(c(1,"Rahul Jain",10533047,3,"Viraj Kamdar",10527201,3,"Vaibhav Shah",10532808,4,"Jayesh Aswar",10531366),ncol=3, byrow=TRUE)
      colnames(smoke) <- c("Sr No","Student Name", "Student ID")
      smoke
    })
  } 
)
    ###<<< End   <<< Jayesh Aswar (10531366) <<<
