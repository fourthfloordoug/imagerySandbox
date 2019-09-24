#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tibble)
library(magrittr)
library(ggplot2)
library(dplyr)
library(imager)
library(tidyr)
library(purrr)
library(GauPro)
library(ash)

source("helpFunctions.R")
noiseValues = c(0.1,1.0,1.0)
numTypes = 4
truthData <- readTruthData()
listOfFunctionLists = initializeFunctionLists(truthData,noiseValues)

#These define our dldt images.  Only need to define once at startup
imageLimits <- list(matrix(c(0,-2,100,2),2,2),matrix(c(0,0,100,150),2,2),matrix(c(0,0,100,30),2,2))
xPixels = 16
yPixels = 16
bins <- c(xPixels,yPixels)
numPixels = xPixels * yPixels

kerasModel <- load_model_hdf5("rgbModel.h5")

typeNames <- c("Type1","Type2","Type3","Type4")



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Light Curve and Other Factors Simulator"),
   
   # Sidebar with a slider input for number of bins 
   
      
      
    
        
      fluidRow(
        column(2,actionButton("newSample", "New Sample"),
               br(),
               br(),
               sliderInput("time", "Current Time",min = 0, max = 100,value = 0),
               br(),
               br(),
               textOutput("truthType")),
        column(3,plotOutput('plot1')),
        column(3,plotOutput('plot2')),
        column(3,plotOutput('plot3'))
      ),
      
      fluidRow(
        column(6,plotOutput('plot4')),
        column(6,plotOutput('plot5'))
      )
        
      
   
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   values <- reactiveValues()  
   values$probTab  <- tibble(time=c(0,0,0,0),value=c(.25,.25,.25,.25),type = c("Type1","Type2","Type3","Type4"))  
  
   truthType <- eventReactive(input$newSample,sample(1:numTypes,1), ignoreNULL = FALSE)
   
   
   #truthData <- reactive()
   
   sampledDataSet <- reactive(generateSampleTable(listOfFunctionLists,noiseValues,truthType()))  
  
   timeLimitedDataSet <- reactive({sampledDataSet() %>% filter(time <= input$time)})
  
   getFrameOfImages <- reactive(getDLDTImages(timeLimitedDataSet(),imageLimits,bins))
   
   minTimeOfSamples <- reactive(min(sampledDataSet()$time))
   
   getRGBImage <- reactive( 
     getFrameOfImages() %>% 
       spread(processIndex,value) %>% 
       mutate(r = r/128,g = g/128,b = b/128) %>% 
       mutate(rgb.val=rgb(r,g,b)))
   
   getProbabilityOfImage <- reactive({
     
     wideInputImageList <- list(getFrameOfImages() %>% filter(processIndex == 'r') %>% 
                                  select(-processIndex) %>% select(-c(x,y)) %>% mutate(pixelIndex=1:numPixels) %>% spread(pixelIndex,value), 
                                getFrameOfImages() %>% filter(processIndex == 'g') %>% 
                                  select(-processIndex) %>% select(-c(x,y)) %>% mutate(pixelIndex=1:numPixels) %>% spread(pixelIndex,value),
                                getFrameOfImages() %>% filter(processIndex == 'b') %>% 
                                  select(-processIndex) %>% select(-c(x,y)) %>% mutate(pixelIndex=1:numPixels) %>% spread(pixelIndex,value))
     
     testImage <- bind_cols(wideInputImageList[[1]],wideInputImageList[[2]],wideInputImageList[[3]]) %>% as.matrix()
     predictions <- kerasModel %>% predict_proba(testImage) %>% as.tibble()
     
     tidyPredictions <- tibble(time=rep(input$time,4),value=c(predictions[[1,1]],predictions[[1,2]],predictions[[1,3]],predictions[[1,4]]),type= c("Type1","Type2","Type3","Type4"))
   })
   
   addData <- observe({
     
     updateProbs <- getProbabilityOfImage()
     isolate(values$probTab <- bind_rows(values$probTab,updateProbs))
     
   })
   
   cleanData <- observe({
     
     input$newSample
     updateSliderInput(session, "time", value = 0)
     isolate(values$probTab <- tibble(time=c(0,0,0,0),value=c(.25,.25,.25,.25),type = c("Type1","Type2","Type3","Type4")))
   })
   
   
   output$plot1 <- renderPlot({
     
      subTruth = truthData %>% filter(processIndex == 1,funcIndex == truthType()) 
     
      data <- timeLimitedDataSet() %>% filter(processIndex == 1)     
      
      ggplot(mapping=aes(x=time,y=value)) + geom_point(data=data) + 
        geom_line(data=subTruth,alpha=0.25,color='gray') + xlim(c(0,100)) + ylim(c(-0.5,2)) + 
        ggtitle("Measurements for Process 1") + ylab("Value") + xlab("Time")
   })
   
   output$plot2 <- renderPlot({
     
     subTruth = truthData %>% filter(processIndex == 2,funcIndex == truthType())
     
     data <- timeLimitedDataSet() %>% filter(processIndex == 2)     
     ggplot(mapping=aes(x=time,y=value)) + geom_point(data=data) + 
       geom_line(data=subTruth,alpha=0.2,color='gray') + xlim(c(0,100)) + ylim(c(-1,150)) + 
       ggtitle("Measurements for Process 2") + ylab("Value") + xlab("Time")
     
   })
   
   output$plot3 <- renderPlot({
     
     subTruth = truthData %>% filter(processIndex == 3,funcIndex == truthType())
     
     data <- timeLimitedDataSet() %>% filter(processIndex == 3)     
     
     ggplot(mapping=aes(x=time,y=value)) + geom_point(data=data) + 
       geom_line(data=subTruth,alpha=0.2,color='gray') + xlim(c(0,100)) + ylim(c(-1,30)) + 
       ggtitle("Measurements for Process 3") + ylab("Value") + xlab("Time")
     
   })
   
   
   output$plot4 <- renderPlot({
     
     getRGBImage() %>% ggplot(aes(x,y)) + geom_raster(aes(fill=rgb.val)) + 
       scale_fill_identity() + ggtitle("Constructed RGB Image")       
     
   })
   
   output$plot5 <- renderPlot({
     
     values$probTab %>% filter(time >= minTimeOfSamples(), time <= input$time) %>% 
       ggplot(aes(time,value,color=type)) + geom_point() + geom_line() + 
       ggtitle("Probability of Type vs. Time") + xlab("P(T)") + ylab("Time") + xlim(c(0,100)) + ylim(c(0,1))   
     # 
     # numbers = tibble(x=c(1,2,3,4,5),y=c(1,2,3,4,5))
     # numbers %>% ggplot(aes(x,y)) + geom_line()   
     
   })
   
   output$truthType <- renderText({ 
     paste("Truth type =",truthType())
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)





  
