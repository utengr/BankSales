library(shiny)
library(tree)

rawdata <- read.csv("bank_clean.csv", header=T)
set.seed(1)
train <- sample(1:nrow(rawdata), 40000)
summary(rawdata[train,])
summary(rawdata[-train,])
actual <- rawdata$y
actual <- actual[-train]
tree.bank <- tree(y~., data = rawdata, subset = train)
tree.pred <- predict(tree.bank, rawdata[-train,], type = "class")
tree.percent <- predict(tree.bank, rawdata[-train,], type = "vector")
tree.percent <- as.data.frame(tree.percent)
tree.percent <- cbind(tree.percent, actual)

shinyServer(function(input, output) {

   #Create summary of data showing Head and Tail
     output$filetable <- renderDataTable({
         rawdata
         #tmp <- rawdata[,seq_along(1:ncol(rawdata))]
         #<=10] # show max 10 columns and bind head tail calls
         #rbind(head(tmp,10),tail(tmp,10))
     })

   #Create Summary Statistics of Data
     output$stattable <- renderTable({
     summary(rawdata)
     })

  # Pick Threshold with Slider
      output$choose_threshold <- renderUI(function() {
        # Pick the threshold for predicting Success
        sliderInput("threshold", "Choose Probabilty Threshold for Success",
            min = 0.05, max = 0.50, value = 0.50, step = 0.05)
      })

  # Input Valuse for cost of Sales Activity
      output$choose_cost <- renderUI(function() {
        # Input data for cost of Sales Activity
        numericInput("cost", "Input Cost of Activity ($):",
            value = 20.0)
      })

  # Pick alpha value with Slider
      output$choose_revenue <- renderUI(function() {
        # Input data for expected revenue
        numericInput("revenue", "Input Expected Revenue if Successful ($):",
            value = 500)
      })

  # Generate Cross Table
      output$crosstable <- renderTable(function() {
        tree.percent$pred <- ifelse(tree.percent$yes >= input$threshold, "yes", "no")
        pred.table <- table(tree.percent$pred, tree.percent$actual,dnn = c("Predicted", "Actual"))
		pred.table
	    })
        
  # Generate Rates Table
      output$ratetable <- renderTable(function() {
        tree.percent$pred <- ifelse(tree.percent$yes >= input$threshold, "yes", "no")
        pred.table <- table(tree.percent$pred, tree.percent$actual,dnn = c("Predicted", "Actual"))
        Prediction_Rate <- pred.table[2,2]/(pred.table[2,2] + pred.table[2,1])*100
        Actual_Rate <- (pred.table[1,2] + pred.table[2,2])/(pred.table[1,1]+pred.table[2,1] +pred.table[1,2]+pred.table[2,2])*100
        cbind(Actual_Rate, Prediction_Rate)
	    })
		
  # Generate Expected Value Results
      output$EVRtable <- renderTable(function() {
      tree.percent$pred <- ifelse(tree.percent$yes >= input$threshold, "yes", "no")
      pred.table <- table(tree.percent$pred, tree.percent$actual,dnn = c("Predicted", "Actual"))
      Pred_Cost <- (input$cost*(pred.table[2,1] + pred.table[2,2]))
      Pred_Gross <- (input$revenue*pred.table[2,2])
      Pred_Net <- (Pred_Gross - Pred_Cost)
	  Ratio <- Pred_Net / Pred_Cost
      cbind(Pred_Cost, Pred_Gross, Pred_Net, Ratio)
	    })
		
  # Generate Normal Value Results
      output$Stdtable <- renderTable(function() {
      tree.percent$pred <- ifelse(tree.percent$yes >= input$threshold, "yes", "no")
      pred.table <- table(tree.percent$pred, tree.percent$actual,dnn = c("Predicted", "Actual"))
      Cost <- (input$cost*(pred.table[1,1] + pred.table[1,2] + pred.table[2,1] + pred.table[2,2]))
      Gross <- (input$revenue*(pred.table[2,2] + pred.table[1,2]))
      Net <- (Gross - Cost)
	  Ratio <- Net / Cost
      cbind(Cost, Gross, Net, Ratio)
	    })
})