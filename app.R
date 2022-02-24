library(shiny)
library(readr)
library(rsconnect)
library(DescTools)
library(car)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Cabin Forestry: A regression tool for testing scaling relationships & generating model diagnostics (dataset:iris, Fisher 1936)"),
    (img(src='cabin_green.png', height="15%", width="15%", align = "right")),
    fileInput("uploaded_file", "Upload CSV File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
    ),
    
    
    
    sidebarLayout(
        sidebarPanel(
            p("Select regression function between 'Univariate', 'Multivariate' and 'Interaction Effect' to compare model accuracy, summary results, and to screen influential outliers through model diagnostics"),
            selectInput("model_type", label = h5(strong("Model to evaluate")),
                        choices = list("Univariate: Petal.Width ~ Petal.Length" = "model1",
                                       "Multivariate: Petal.Width ~ Petal.Length + Species" = "model2",
                                       "Interaction effect: Petal.Width ~ Petal.Length * Species" = "model3" 
                        ), selected = 1)
            
            
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Scatterplot", plotOutput("scatterplot")),
                        tabPanel("Summary Results", verbatimTextOutput("summary")),
                        tabPanel("Model Diagnostics",
                                 fluidRow(column(6, plotOutput("diagnosticplot1")),
                                          column(6, plotOutput("diagnosticplot2"))
                                 ),
                                 fluidRow(column(6, plotOutput("diagnosticplot3")),
                                          column(6, plotOutput("diagnosticplot4"))
                                 ),
                                 fluidRow(verbatimTextOutput("outlier_table")
                                 )
                        )
            )
        )
    ))







# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # load data
    data = iris
    #data = read.csv("efi-cleaned-lq.csv")
    #data$beclabel = as.factor(data$beclabel)
    #data <- reactive({read.csv(input$filedata$datapath)})
    
    # Regression output
    output$summary <- renderPrint({
        if (input$model_type == "model1") {
            regressor = lm(Petal.Width ~ Petal.Length, data = iris)
            summary(regressor)
        }
        else if (input$model_type == "model2") {
            regressor = lm(Petal.Width ~ Petal.Length + Species, data = iris)
            summary(regressor) 
        }
        else if (input$model_type == "model3") {
            regressor = lm(Petal.Width ~ Petal.Length*Species, data = iris)
            summary(regressor) 
        }
    })
    
    
    #generate residuals to plot regression slopes and outlier tables
    model1.formula = lm(Petal.Width ~ Petal.Length, data = data)
    model1.predict = predict(model1.formula)
    model1.resid = residuals(model1.formula)
    summary(model1.formula)
    RMSE(model1.formula)
    
    model2.formula = lm(Petal.Width ~ Petal.Length + Species, data = data)
    model2.predict = predict(model2.formula)
    model2.resid = residuals(model2.formula)
    summary(model2.formula)
    RMSE(model2.formula)
    
    model3.formula = lm(Petal.Width ~ Petal.Length*Species, data = data)
    model3.predict = predict(model3.formula)
    model3.resid = residuals(model3.formula)
    summary(model3.formula)
    RMSE(model3.formula)
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        if (input$model_type == "model1") {
            plot(Petal.Width ~ Petal.Length, data = data, 
                 main="Univariate model: R^2 = ?, RMSE = ?",
                 ylab="Petal.Width", xlab="Petal.Length", pch=16, cex=.25)
            abline(model1.formula,col="blue")
        }
        else if (input$model_type == "model2") {
            plot(Petal.Width ~ Petal.Length, data = data, 
                 main="Multivariate model: R^2 = ?, RMSE = ?",
                 ylab="Petal.Width", xlab="Petal.Length", pch=16, cex=.25)
            abline(-3.482, 0.8747,col="red")
            lines(model1.formula, col="blue")
        }
        else if (input$model_type == "model3") {
            plot(Petal.Width ~ Petal.Length, data = data, 
                 main="Interactive model: R^2 = ?, RMSE = ?)",
                 ylab="Petal.Width", xlab="Petal.Length", pch=16, cex=.25)
            abline(-3.298, 0.8761, col="green")
            abline(-3.482, 0.8747, col="red", lwd=2)
            lines(model1.formula, col="blue")
            
        }
    })
    
    
    # Diagnostic plots
    output$diagnosticplot1 <- renderPlot({
        if (input$model_type == "model1") {
            plot(model1.formula, 1, pch=18,cex=.15)
        }
        else if (input$model_type == "model2") {
            plot(model2.formula, 1, pch=18,cex=.15)
        }
        else if (input$model_type == "model3") {
            plot(model3.formula, 1, pch=18,cex=.15)
        }
    })
    
    output$diagnosticplot2 <- renderPlot({
        if (input$model_type == "model1") {
            plot(model1.formula, 3, pch=18,cex=.15)
        }
        else if (input$model_type == "model2") {
            plot(model2.formula, 3, pch=18,cex=.15)
        }
        else if (input$model_type == "model3") {
            plot(model3.formula, 3, pch=18,cex=.15)
        }
    })
    
    output$diagnosticplot3 <- renderPlot({
        if (input$model_type == "model1") {
            plot(model1.formula, 4, pch=18,cex=.15)
        }
        else if (input$model_type == "model2") {
            plot(model2.formula, 4, pch=18,cex=.15)
        }
        else if (input$model_type == "model3") {
            plot(model3.formula, 4, pch=18,cex=.15)
        }
    })
    
    output$diagnosticplot4 <- renderPlot({
        if (input$model_type == "model1") {
            plot(model1.formula, 5, pch=18,cex=.15)
        }
        else if (input$model_type == "model2") {
            plot(model2.formula, 5, pch=18,cex=.15)
        }
        else if (input$model_type == "model3") {
            plot(model3.formula, 5, pch=18,cex=.15)
        }
    })
    
    # Outlier Table
    output$outlier_table <- renderPrint({
        if (input$model_type == "model1") {
            outlierTest(model1.formula, data=data)
        } 
        else if (input$model_type == "model2") {
            outlierTest(model2.formula, data=data)
        }
        else if (input$model_type == "model3") {
            outlierTest(model3.formula, data=data)
        } 
    })
}   

#plot(model1.formula, 3, pch=18,cex=.15)            # check homogeneity of variance
#plot(model1.formula, 2, pch=18,cex=.5)             # check normality of residuals
# row 2
#plot(model1.formula, 4, pch=18,cex=.15)            # check cook's distance for outlier trends 
#plot(model1.formula, 5, pch=18,cex=.15)            # check influence of outliers
#spreadLevelPlot(model1.formula, main=NULL, pch=18,cex=.15) # check influence sample- of outliers
# row 3: table for targeting outliers by #ID 
#outlierTest(model1.predict, data=data)

#plot(model2.formula, 3, pch=18,cex=.15)            # check homogeneity of variance
#plot(model2.formula, 2, pch=18,cex=.15)            # check normality of residuals
# row 2
#plot(model2.formula, 4, pch=18,cex=.15)            # check cook's distance for outlier trends 
#plot(model2.formula, 5, pch=18,cex=.15)            # check influence of outliers
#spreadLevelPlot(model2.formula,main=NULL, pch=18,cex=.15) # check influence sample- of outliers
#row 3: table generated for targeting individual outliers by #ID 
#outlierTest(model2.formula, data=data)

#plot(model3.formula, 3, pch=18,cex=.15)            # check homogeneity of variance
#plot(model3.formula, 2, pch=18,cex=.15)            # check normality of residuals
# row 2
#plot(model3.formula, 4, pch=18,cex=.15)            # check cook's distance for outlier trends 
#plot(model3.formula, 5, pch=18,cex=.15)            # check influence of outliers
#spreadLevelPlot(model3.formula,main=NULL, pch=18,cex=.15) # check influence sample- of outliers
#row 3: table generated for targeting individual outliers by #ID 
# outlierTest(model3.formula, data=data) 

shinyApp (ui = ui, server = server)