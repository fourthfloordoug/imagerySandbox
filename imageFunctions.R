calculateDValDTime <- function(valueTable) {
 
  require(tibble)
  require(dplyr)
  require(purrr)
  require(magrittr)
  
  numberOfRows <- nrow(valueTable)
  indexSeq = seq(1,(numberOfRows))
  
  valueTable %<>% arrange(time)
  
  diffTimeLists <- map(.x=indexSeq,.f=function(index) {
    transmute(valueTable,diff=valueTable$time-valueTable$time[index])
  })
  
  diffTimeMatrix <- as.matrix(bind_cols(diffTimeLists))
  diffTimeVector <- diffTimeMatrix[lower.tri(diffTimeMatrix)]
  
  diffValueLists <- map(.x=indexSeq,.f=function(index) {
    transmute(valueTable,diff=valueTable$value-valueTable$value[index])
  })
  
  diffValueMatrix <- as.matrix(bind_cols(diffValueLists))
  diffValueVector <- diffValueMatrix[lower.tri(diffValueMatrix)]
  
  diffTable <- tibble(time=diffTimeVector,value=diffValueVector) %>% arrange(time)
}


binDValDTime <- function(diffTable,limits,numBins) {
  
  require(ash)
  require(imager)
  
  bins <- bin2(as.matrix(diffTable),limits,numBins)
  #bins <- t(bins$nc)
  bins <- bins$nc
  numRows <- dim(bins)[1]
  numCols <- dim(bins)[2]
  
  imageArray <- array(NA, dim=c(numRows,numCols,1,1))
  imageArray[,,,] <- bins
  imageArray <- as.cimg(imageArray)
}


learnGP <- function(trainingDataTable) {
  
  require(GauPro)
  
  gp <- GauPro(X=as.vector(trainingDataTable$time),Z=as.vector(trainingDataTable$value),parallel=FALSE)
}

generateGPSamples <- function(gp,sampleInfoTable) {
  
  require(GauPro)
  require(tibble)
  require(purrr)
  require(dplyr)
  require(magrittr)
  
  numTrials = nrow(sampleInfoTable)
  
  listOfSamples <- map(.x=1:numTrials,.f=function(index) {
    
    xPoints = sampleInfoTable[[index,2]]
    sampleValues <- gp$sample(XX=xPoints)
    tibble(time=xPoints,value=sampleValues,trial=(rep(index,length(xPoints))))
  })
  sampleTable <- bind_rows(listOfSamples)
}

widenImageDataFrame <- function(imageFrame,numPixels) {
  
  #We're expecting a structure where an image is a dataframe made from imager.
  #It should have columns like (x,y,value,type,trial).  
  #We also expect that each type has the same number of trials.
  numTrialsAllTypes = length(unique(imageFrame$type)) * max(imageFrame$trial)
  
  imageFrame %<>% select(-c(x,y)) %>% mutate(pixelIndex=rep(1:numPixels,numTrialsAllTypes))
  imageFrame %>% spread(pixelIndex,value)
}


getTSNEEmbedding <- function(wideImageFrame,numPixels) {
  
  require(Rtsne)
  
  #We're expecting a wide dataframe here where the first two columns include information
  #on the type and trial#.  
  
  #We have to eliminate any duplicate points
  duplicatedRows <- duplicated(wideImageFrame[,3:(2+numPixels)])
  filteredData <- wideImageFrame %>% mutate(duplicated=duplicatedRows) %>% filter(duplicated==FALSE)
  
  distinctInputData <- filteredData %>% select(-c(type,trial,duplicated))
  
  #distinctInputData <- wideImageFrame[,3:(2+numPixels)] %>% distinct()
  
  inputDataMatrix <- as.matrix(distinctInputData)
  # Set a seed if you want reproducible results
  set.seed(42)
  tsneOutput <- Rtsne(inputDataMatrix,pca=FALSE,perplexity=30,theta=0.0)
  
  as.tibble(tsneOutput$Y) %>% mutate(type=filteredData$type,trial=filteredData$trial)
}


trainKerasModel <- function(wideImageFrame) {
  
  require(keras)
  require(tensorflow)
  
  numPixels = ncol(wideImageFrame) - 2
  maxPixelValue = 256
  numTypes = length(unique(wideImageFrame$type))
  
  #Do some manipulations with the labels to get in the right format for keras.
  inputDataMatrix <- as.matrix(wideImageFrame[,3:(2+numPixels)])
  #scale everything from 0 to 1.  We'll pretend it is an 16 bit image
  inputDataMatrix <- inputDataMatrix / (maxPixelValue -1)
  
  categoryTable <- wideImageFrame %>% select(type) 
  
  #We need to convert the types to integer values for use in keras
  #categoryTable %<>% transmute(type = as.integer(substr(type,6,6)))
  #This has been taken care of outside.
  
  #unfortunately, the class names have to be 0 indexed, so we subtract
  categoryTable %<>% transmute(type = type-1)
  
  typeValues <- to_categorical(as.vector(categoryTable$type))
  
  #define the model
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = maxPixelValue, activation = 'relu', input_shape = c(numPixels)) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = (maxPixelValue/2), activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = numTypes, activation = 'softmax')
  
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  #The history object here has fit parameters, but we will only use them for diagnostics.
  #We'll want to return the model object which apparently is stateful.
  history <- model %>% fit(
    inputDataMatrix, typeValues, 
    epochs = 30, batch_size = 128, 
    validation_split = 0.2
  )
  
  model
}


testKerasModel <- function(wideImageFrame,kerasModel) {
  
  require(keras)
  require(tensorflow)
  
  numPixels = ncol(wideImageFrame) - 2
  maxPixelValue = 256
  numTypes = length(unique(wideImageFrame$type))
  
  #Separate the categories from the data as with the training set.
  
  inputDataMatrix <- as.matrix(wideImageFrame[,3:(2+numPixels)])
  inputDataMatrix <- inputDataMatrix / (maxPixelValue -1)
  
  categoryTable <- wideImageFrame %>% select(type) #%>% transmute(type = as.integer(substr(type,6,6)))
  typeValues <- as.vector(categoryTable$type)
  
  
  #This returns a vector of classes
  predictions <- kerasModel %>% predict_classes(inputDataMatrix)
  
  #because keras is zero indexed on type, we have to add 1 to all of the predictions
  predictions = predictions + 1
  
  #Work everything together in a tibble
  tibble(truth=typeValues,trial=wideImageFrame$trial,predict=predictions) %>% 
    mutate(correct=ifelse(truth==predict,TRUE,FALSE))
}


testKerasModelForProbType <- function(wideImageFrame,kerasModel) {
  
  require(keras)
  require(tensorflow)
  
  numPixels = ncol(wideImageFrame) - 2
  maxPixelValue = 256
  types = unique(wideImageFrame$type)
  numTypes = length(types)
  
  #Separate the categories from the data as with the training set.
  
  inputDataMatrix <- as.matrix(wideImageFrame[,3:(2+numPixels)])
  inputDataMatrix <- inputDataMatrix / (maxPixelValue -1)
  
  categoryTable <- wideImageFrame %>% select(type) #%>% transmute(type = as.integer(substr(type,6,6)))
  typeValues <- as.vector(categoryTable$type)
  
  
  #This returns a matrix with rows=numpoints and cols=numtypes
  predictions <- kerasModel %>% predict_proba(inputDataMatrix) %>% as.tibble()
  
  colnames(predictions) <- types
  
  predictions %>% mutate(truth=typeValues,trial=wideImageFrame$trial) %>% 
    gather(key,value,types) %>% rename(predictedType=key,probability=value)

  
  
  # #Work everything together in a tibble
  # tibble(truth=typeValues,trial=wideImageFrame$trial,predict=predictions) %>% 
  #   mutate(correct=ifelse(truth==predict,TRUE,FALSE))
}
