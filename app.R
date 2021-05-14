


library(RCurl) 
library(randomForest)
library(caret)
library(readr)
library(e1071)




rawdata <- read_csv("hiringraw.csv")


TrainingIndex <- createDataPartition(rawdata$positive, p=0.80, list = FALSE)
TrainingSet <- rawdata[TrainingIndex,]
TestingSet <- rawdata[-TrainingIndex,] 

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]



model <- randomForest(as.factor(positive) ~ ., data = TrainSet, ntree = 200, mtry = 4, importance = TRUE)

fit_rf <- train(as.factor(positive)~.,
                TrainSet,
                method = "rf",
                metric = "Accuracy",
                importance = TRUE,
                nodesize = 14,
                ntree = 200,
                maxnodes = 24)



library(shiny)
library(data.table)
library(randomForest)
library(shinythemes)


require(reshape2)


theme = shinythemes::shinytheme("flatly")




ui <- fluidPage( theme = shinytheme("superhero"),
                 
                 titlePanel(
                     
                     
                     h2(p("Attrition Predictor", style = "color:#FFD0B9")
                        ,style = "font-size:300%")),
                 
                 h5("This app will help Recruitment Team to predict the chances of New hire to churn in less than 3 months"),
                 
                 
                 
                 sidebarLayout(
                     
                     sidebarPanel (
                         
                         
                         tags$label(h3('Input data')),
                         
                         tags$head(tags$style("body {background-color:#2E5894; }")
                         ),
                         numericInput("onl", 
                                      label = "Online Exam(%)", 
                                      value = 85),
                         numericInput("int1", 
                                      label = "Interview1 Score(%)", 
                                      value = 90),
                         numericInput("int2", 
                                      label = "Interview2 Score(%)", 
                                      value = 87),
                         numericInput("typ", 
                                      label = "Typing Test Score(%)", 
                                      value = 89),
                         numericInput("gen", 
                                      label = "Gender (M=1,F=2)", 
                                      value = 1),
                         numericInput("age", 
                                      label = "Age (years.months, sample = 26.3)", 
                                      value = 26.5),
                         numericInput("exp", 
                                      label = "BPO Experience (in months, sample = 0,1,2,3...)", 
                                      value = 3),
                         
                         actionButton("submitbutton", "Submit", 
                                      class = "btn btn-primary"),
                         
                         
                         helpText("note : We used fake data!"),
                         
                     ),
                     
                     
                     
                     mainPanel(
                         
                         
                         verbatimTextOutput('contents'),
                         tableOutput('tabledata'),style = "color:#FDFF00", 
                         tags$style(type="text/css", "#view tr:last-child {font-weight:bold;}"),
                         tags$label('The above prediction is the likelihood for our NH to churn in less than 3 months'),
                         verbatimTextOutput("summary1"),
                         
                         plotOutput("plot"),
                         
                         
                         helpText("This tool calculate prediction using random forest algorithm."),
                         helpText("Random forest create multiple decision trees and combined together to get a more accurate and stable prediction"),
                         helpText("It is a popular machine learning algorithm."),
                         
                         
                     )
                 ))


server<- function(input, output, session) {
    
    
    datasetInput <- reactive({  
        
        df <- data.frame(
            Name = c("onl",
                     "int1",
                     "int2",
                     "typ",
                     "gen",
                     "age",
                     "exp"),
            Value = as.character(c(input$onl,
                                   input$int1,
                                   input$int2,
                                   input$typ,
                                   input$gen,
                                   input$age,
                                   input$exp)),
            stringsAsFactors = FALSE)
        
        positive <- 0
        df <- rbind(df, positive)
        input <- transpose(df)
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
        
        Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 4))
        print(Output)
        
    })
    
    
    output$contents <- renderPrint({
        if (input$submitbutton>0) { 
            isolate("Calculation complete") 
        } else {
            return("This tool is ready for calculation.")
        }
    })
    
    
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    output$plot <-renderPlot({
        accuracy_impact <- model
        varImpPlot(accuracy_impact)
        
    })
    output$summary1 <- renderPrint({
        
        print(model)
    })
    output$summary2 <- renderPrint({
        
        varImp(fit_rf)
    })
}


shinyApp(ui = ui, server = server)
