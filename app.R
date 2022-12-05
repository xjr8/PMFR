#######################################
# DS501 - Introduction to Data Science#
# Case Study 3 - Shiny Web App for PMFR Predictions #
# Author: Prajwal Bharadwaj#
#######################################

# Import Libraries
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(caret)
library(DT)
library(stargazer)
library(ggplot2)
library(corrplot)
library(bslib)
library(ggplot2)
library(GGally)


####################################
# User interface                   #
####################################
ui = fluidPage(
  theme =  bslib::bs_theme(bootswatch = "lux"),
  titlePanel("Digital Twin for Polymer Processing"),
  navbarPage(
    title = "",
    
    #Introduction Tab
    tabPanel("Introduction", 
             h3("Background"), 
             p("Common polymers such as polyethylene,
                polypropelyne are created from monomer building 
               blocks in slurry reactors. A catalyst is 
               injected with the monomers under controlled
               temperature and pressure conditions 
               to cause a chain reaction that grows the 
               polymer chains. The length of the polymer chain
               is controlled by using Hydrogen gas as a controlling
               agent. If the chains are long, the resulting
               polymer is viscous and if the chains are
               short, then resulting polymer is highly compliant. 
               The length of the polymer chain is determined by the 
               desired application for it."),
             p("To maintain the length of the polymer at an appropriate
               level, samples are periodically collected from the reactor
               and studied to maintain the polymer slurry at the right
               viscosity for each particular grade. The grade of the
               polymer being manufactured is determined by a customer and
               can include specifications for:"), 
             list(
               p("1. Melt Index"),
               p("2. Melt Flow Rate"),
               p("3. Density")
             ),
             h3("Motivation"), 
             p("The goal of this application is to create 
               a machine learning based digital twin that uses measurements 
               from process instrumentation (such as sensors) 
               to predict relationships between different processing
               conditions within the reactor. There is a need
               for such a continuously measuring soft-sensor 
               to supplement the infrequent, difficult to extract, experimental
               measurements to enable more efficient polymer processing.")
             ),
             
    
    # Dataset Tab
    tabPanel("DataSet",
             height = "1000px",
             width = 12,
             tabsetPanel(
               tabPanel("About", h3("Description"), 
                        p("The data file considered contains
                                         readings recorded from process 
                                         instruments such as sensors. 
                                         It contains 2564 
                                         observations of the following
                                         9 features:"),              
                                        list(
                                           p("1. Time: Time of Measurements"),
                                           p("2. C3=: Propylene (C3=) Feed Rate (kg/hr)"),
                                           p("3. H2R: Hydrogen to C3= Ratio"),
                                           p("4. Pressure: Reactor Pressure (bar)"),
                                           p("5. Level: Reactor Bed Level (m)"),
                                           p("6. C2=: Ethylene (C2=) Flow (kg/hr)"),
                                           p("7. Cat: Catalyst Feed Rate (kg/hr)"),
                                           p("8. Temp: Reactor Temperature"),
                                           p("9. MFR: Melt Flow Rate")
                                         ),
                        h3("Pre-Processing"),
                        p("The data is pre-processed by excluding the time 
                          stamps that correlate to the instances at which
                          measurements were made. NaN values are also removed 
                          from the given dataset. The remaining data within 
                          the set is numeric in nature.")
                        ),
               tabPanel("Data",
                        box(withSpinner(DTOutput(
                          "Data"
                        )), width = 10)),
               tabPanel("Exploratory Analysis",
                        tabsetPanel(
                          tabPanel("Distribution",
                                   h3("Description"),
                                   p("The distribution of different variables
                                     can be visualized through a density plot.
                                         Density plots are single variable plots 
                                     that let you get a sense of the 
                                     distribution of a numeric variable. 
                                     They are similar to histograms except that 
                                     they create a continuous approximation of 
                                     the distribution of a variable instead of 
                                     showing the distribution as a series of 
                                     discrete bars with heights determined 
                                     by the number of records that fall 
                                     within each bin."),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(
                                         "variable","Select Variable:",
                                         choices = names((select(pmfr, -Time)))
                                       )
                                     ),
                                     mainPanel(
                                       plotOutput("densityPlot1D")
                                       ),
                                   )

                            
                          ),
                          tabPanel(
                            "Direct Correlation", h3("Description"),
                            p("Direct variable correlation can be used quantify
                            the relationships between different variables
                            in a given dataset through a correlation plot. 
                       Correlation is a measure of how much of a variable 
                       is explained by another. The closer the correlation 
                       is to 1 the more of the change in one 
                       variable is explained by the other. 
                       A correlation closer to 0 points to 
                       no correlation between the two variables. A positive
                       correlations indicate that the dependence of one variable
                       increases with the second variable whereas a negative 
                       correlation indicates that the two variables have an
                       inverse or decreasing relationship."),
                            p(" To obtain a correlation plot, 
                            a correlation matrix is computed
                       from the given dataset. This matrix contains row entries
                       that correspond to the strength of the correlation 
                       between variables. This matrix is then plotted 
                       as a heatmap:"),
                            box(withSpinner(plotOutput("corrPlot")), width = 25),
                            p("It is be observed that the 
                              polymer melt flow rate (MFR) is strongly 
                              correlated to the H2R and pressure variables. 
                              Temperature is weakly correlated to the MFR and
                              Cat variables. C2 is strongly related to the
                              H2R and C3 variables while C3 is strongly related
                              to C2 and Cat variables. Several negative 
                              correlations can be observed, as seen in the
                              case of Cat variable which is negatively correlated
                              to the Pressure and MFR variables. This 
                              preliminary analysis of the data provides insights
                              into the potential relationships that can exist
                              between these variables."),
                            
                            
                          ),
                          tabPanel("Pair Plots",
                                   h3("Description"),
                                   p("Similar to
                                   direct variable correlation, 
                                   pair plots can also be used to visualize the 
                                   relationships that exist between different
                                   variables. Pair plots are a matrix of scatter
                                     plots where the diagonal shows the densities
                                     of the 
                                     variables within the dataset and the upper
                                     elements output the correlation
                                     between different variables. The lower
                                     elements present a scatter plot of the
                                     variables that correspond
                                     to that particular entry in the
                                     matrix of plots."),
                                   box(withSpinner(plotOutput("pairPlot")),
                                       width = 25),
                                   p("The pair plot is a powerful tool to 
                                     develop a preliminary understanding
                                     of the relationships between different 
                                     variables that are present in a given data
                                     set.")
                            
                          ),
                          tabPanel("Density Plots",
                                   h3("Description"),
                                   p("2D density
                                     plots can be used
                                     to study the
                                     relationships 
                                     between numeric
                                     variables when
                                     considering
                                     large volumes
                                     of data. 2D density
                                     plots prevent overlaps
                                     in data points by dividing
                                     the plot area into smaller
                                     sub-areas that represent
                                     the number of points in a
                                     given fragment."),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("xVar","Select X-Dimension:",
                                                   choices = names((select(pmfr, -Time)))),
                                       selectInput("yVar","Select Y-Dimension:",
                                                   choices = names((select(pmfr, -Time))))
                                     ),
                                     mainPanel(
                                       plotOutput("densityPlot2D")
                                     )
                                   )
                                   ),
                          
                          tabPanel(
                            "Summary", 
                            h3("Description"),
                            p("A statistical summary of our data is
                              constructed here using the stargazer package to
                              report the distribution of the numeric data in the
                              given set."), 
                            verbatimTextOutput("Summary")
                            )
                          )
                        )
                 
                 
             )),
    
    
    # ML Tab
    tabPanel("Machine Learning",
                 tabsetPanel(
                   
                   # Details Tab
                   tabPanel("Model Details",
                            h3("Conceptual Background"),
                            p("Linear regression can be used to describe the
                              relationship between a dependent variable and
                              an independent variable. If there are multiple
                              independent variables involved, the regression
                              is said to be multivariate. Linear regression
                              can be used to estimate a continuous value
                              as a linear function of other variables. Input
                              variables into this model can be both continuous
                              or discrete. The output of the linear regression
                              is a set of coefficients that indicate the impact
                              of each driving variable."),
                            p("The process of linear regression involves
                              finding an equation of the line that best
                              fits the data. It involves finding the coefficients
                              of the independent variables such that the fit
                              values for the dependent variable are as
                              close the actual (observed) values. 
                              The difference between the observed value
                              and the fit value is called the residual.
                              These unknown coefficients are calculated using
                              the least squares method which involves the
                              minimization of the sum of the squared residuals."),
                            p("There exist other metrics that can be used 
                              to evaluate the linear regression model. The 
                              coefficient of determination is one such metric.
                              This metric represents the proportion of the
                              total sample variability that is explained by the
                              regression model. It is indicative of how well
                              the model fits the data. Some other evaluation
                              metrics include the root mean squared error, mean
                              squared error and the mean absolute error."),
                            h3("Objectives"), 
                            p("The goal of this study is to develop a digital
                              twin that is capable of predicting the relation
                              ships between various polymer processing 
                              conditions in order to enable more effective
                              polymer processing."),
                            p("Single and multivariate linear regression models
                              are considered in order to generate insight
                              on the relationships between two variables and
                              also to understand the influence of different
                              model features on the prediction of a 
                              target variable.")
                          
                            ),
                   # Linear Regression Tab
                   tabPanel("Linear Regression", 
                            h3("Description"), 
                            p("Here a simple linear regressor is used 
                              interactively
                              to identify the relationships between different, 
                              individual reactor conditions. 
                              The R-sqaured error metric reported quantifies 
                              the total variation of all the predicted values 
                              found on the regression line or plane from the 
                              mean value of all the values of 
                              the response variables. R-squared value is used to
                              measure the goodness of fit or best-fit line. 
                               The greater the value of R-Squared, the better is 
                              the regression model as most of the variation 
                              of actual values from the 
                              mean value are explained by the regression model"),
                            sidebarLayout(
                              sidebarPanel(
                                sidebarPanel(
                                  selectInput("xLabel","Select X-Dimension:",
                                              choices = names((select(pmfr, -Time)))),
                                    selectInput("yLabel","Select Y-Dimension:",
                                                choices = names((select(pmfr, -Time))))
                                )
                              ),
                              mainPanel(plotOutput("linPlots"), 
                                        uiOutput("regressionData")))),
                            

                   # Multivariate Regression Tab
                   tabPanel("Multivariate Regression", h3("Description"),
                            p("Here a linear multivariate regressor is used 
                              interactively
                              to identify the influence of different
                              reactor conditions on a target label."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("ySelect","Select Label:",
                                choices = names((select(pmfr, -Time)))),
                                sliderInput("Slider",
                                            "Adjust Test-Train Split(%):",
                                            0, 100, 80))
                                  ,
                              mainPanel(
                                           tabsetPanel(
                                             tabPanel(
                                               "Details",
                                               box(
                                                 withSpinner(verbatimTextOutput("Model")),
                                                 width = 6,
                                                 title = "Model Details"
                                               ),
                                               p("The table reports
                                                 the performance of the linear 
                                                 multivariate regression
                                                 that is performed on the 
                                                 selected
                                                 prediction label. The estimated
                                                 coefficients report
                                                 the influence of each feature
                                                 on the regression model. By
                                                 studying this table, the
                                                 features with the most 
                                                 influence on the target label
                                                 can be isolated by considering
                                                 their absolute magnitudes.
                                                 R-squred error metrics are 
                                                 also reported to understand
                                                 the goodness of fit of
                                                 the model considered."),
                                               ),
                                             tabPanel("Evaluation Plots",
                                                      plotOutput("evalPlots"),
                                                      plotOutput("residualPlots")
                                             ),
                                             
                                             tabPanel("Importance Measures", 
                                                      box(
                                                        withSpinner(verbatimTextOutput("varImportance")),
                                                        width = 6,
                                                        title = "Variable Importance"
                                                      ),
                                                      p("From this table,
                                                        influence of each
                                                        feature variable on the
                                                        target variable can
                                                        be determined. This is
                                                        a useful step to 
                                                        determine variables that
                                                        influence the model's
                                                        predictions the most.
                                                        Comparision of variable
                                                        importance's
                                                        can help in discovering
                                                        interrelations
                                                        between variables. The 
                                                        identification of the
                                                        most important variables
                                                        can also be helpful
                                                        in assessing the validity
                                                        of the model
                                                        based on domain knowledge.
                                                        
                                                        ")
                                                      )
                                           )
                            
                   ))),
                   

                 )
               )
  )           
)
  
####################################
# Server                           #
####################################
server <- function(input, output, session){
  thematic::thematic_shiny()
  
  # Data
  read.csv("pmfr.csv")
  rawData <- reactive({pmfr})
  cleanData <- reactive({na.omit(select(pmfr, -Time))})
  
  # Distribution
  densityPlot1D <- reactive({
    d1 <- cleanData()
    ylabel <- "Density"
    d1 %>% ggplot(aes_string(x = input$variable)) + 
      geom_density() + 
      ylab(ylabel) +
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))
  })
  
  # Variable Correlation 
  corMatrix <- reactive({cor(cleanData())})
  correlationPlot <- reactive({corrplot(
    corMatrix(),
    type = "full",
    order="hclust",
    method = "number"
  )})
  
  # Pair Plots
  pairPlot <- reactive({ggpairs(cleanData())})
  
  # 2D Density Plots
  densityPlot2D <- reactive({
    dataVar <- cleanData()
    dataVar %>% ggplot(aes_string(x = input$xVar, y = input$yVar)) +
      geom_density2d_filled() + 
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))
  })
  # Output Data Summary
  output$Data <- renderDT(rawData())
  output$Summary <- renderPrint({summary(cleanData())})
  
  # Linear Regression
  linearPlots <- reactive({
    df <- cleanData()
    ggplot(df, aes_string(x = input$xLabel, y = input$yLabel)) + 
      geom_point()+
      geom_smooth(method = lm, formula = y~x) +
      theme(
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))})
  
  regressionData <- reactive({
    fit <- lm(get(input$xLabel) ~ get(input$yLabel), data = cleanData())
    withMathJax(
      paste0(
        "\\(R^2 = \\)", round(summary(fit)$adj.r.squared, 2),
        ", \\(\\beta_0 = \\)", round(fit$coef[[1]],2),
        ", \\(\\beta_1 = \\)", round(fit$coef[[2]],2),
        ", P-Value","\\(= \\)", signif(summary(fit)$coef[2,4],2)
        
      )
    )
  })
  
  # Multivariate Regression
  sliderSplit <- reactive({input$Slider/100})
  
  set.seed(1000)

  splitIndex <- reactive({
    sample(1:nrow(cleanData()),
           sliderSplit() * nrow(cleanData()))})
  
  trainingData <- reactive({trainData <- cleanData()
    trainData[splitIndex(), ]})
  
  testingData<- reactive({testData <- cleanData()
    testData[-splitIndex(),]})
  
  linearFormula <- reactive({as.formula(paste(input$ySelect, "~."))})
  
  linearModel <- reactive({lm(linearFormula(), data = trainingData())})
  
  trainActual <- reactive({
    a1 <- testingData() 
    a1[,input$ySelect]
  })
  
  testActual <- reactive({
    a2 <- testingData() 
    a2[,input$ySelect]
  })
  
  trainPreds <- reactive({
    b1 <- predict(linearModel(), trainingData())
    b1 <- unname(b1, force = FALSE)
    })
  
  testPreds <- reactive({
    b2 <- predict(linearModel(), testingData())
    b2 <- unname(b2, force = FALSE)
    })
  
  dfTrain <- reactive({data.frame(cbind(actualValues = as.numeric(unlist(trainActual())), 
                                        predictedValues = trainPreds()))})
                                        
  dfTest <- reactive({data.frame(cbind(actualValues = as.numeric(unlist(testActual())), 
                                        predictedValues = testPreds()))})
  
  evalPlot <- reactive({
    c1 <- dfTest()
    xlabel <- paste("Actual Value of ", input$ySelect)
    ylabel <- paste("Predicted Value of ", input$ySelect)
    ggplot(c1, aes(x = actualValues, y = predictedValues)) + geom_point() +
      xlab(xlabel) + ylab(ylabel) +  theme(
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"))
  })

  
  impMeasures <- reactive({
    imp <- as.data.frame(varImp(linearModel()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]})

  
  # Outputs
  output$densityPlot1D <- renderPlot(densityPlot1D())
  output$densityPlot2D <- renderPlot(densityPlot2D())
  output$corrPlot <-renderPlot(correlationPlot())
  output$pairPlot <- renderPlot(pairPlot())
  output$linPlots <- renderPlot(linearPlots())
  output$regressionData <- renderUI({regressionData()})
  output$Model <- renderPrint(summary(linearModel()))
  output$varImportance <- renderPrint(impMeasures())
  output$evalPlots <- renderPlot(evalPlot())
  output$residualPlots <- renderPlot({
                par(mfrow = c(2, 2))
                plot(linearModel())
                par(mfrow = c(1, 1))})
  }
####################################
# Connections                      #
####################################
shinyApp(ui, server)