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
  
  inputDataMatrix <- as.matrix(wideImageFrame[,3:(2+numPixels)])
  # Set a seed if you want reproducible results
  set.seed(42)
  tsneOutput <- Rtsne(inputDataMatrix,pca=FALSE,perplexity=30,theta=0.0)
  
  as.tibble(tsneOutput$Y) %>% mutate(type=wideImageFrame$type,trial=wideImageFrame$trial)
}

