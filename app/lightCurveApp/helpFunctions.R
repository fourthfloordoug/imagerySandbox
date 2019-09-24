readTruthData <- function() {
  
  directory = './truthFiles/'
  fileNames = c('Pro1Type1.csv','Pro1Type2.csv','Pro1Type3.csv','Pro1Type4.csv',
                'Pro2Type1.csv','Pro2Type2.csv','Pro2Type3.csv','Pro2Type4.csv',
                'Pro3Type1.csv','Pro3Type2.csv','Pro3Type3.csv','Pro3Type4.csv')
  
  numProcesses = 3
  numTypes = 4
  
  truthData <- fileNames %>% map_df(.f=function(fileName) {
    
    processNumber = substr(fileName,4,4) %>% as.integer()
    typeNumber = substr(fileName,9,9) %>% as.integer()
    
    path = paste(directory,fileName,sep="")
    data = read.csv(path) %>% as.tibble() %>% mutate(processIndex = processNumber,funcIndex = typeNumber)
    
  })
  
  truthData %<>% mutate(processIndex = as.factor(processIndex),funcIndex=as.factor(funcIndex))
}

initializeFunctionLists <- function(truthData,noiseValues) {
  
  timeVals = unique(truthData$time) %>% as.array()
  typeNames = c('1','2','3','4')

  truthPointsToKeep = 100
  

  sampledTruthData <- map2_df(.x=1:numProcesses,.y=noiseValues,.f=function(procIndex,noise) {

    map_df(.x=1:numTypes,.f=function(typeIndex) {

      keptTimes = sort(sample(timeVals,truthPointsToKeep))

      sampledData <- truthData %>% filter(processIndex==procIndex,funcIndex==typeIndex,time %in% keptTimes)
      #sampledData %<>% mutate(noiseValue = value + rnorm(n(),0,noise))
      sampledData %<>% mutate(noiseValue = value)
    })

  })
  
  # sampledTruthData

  1:numProcesses %>% map(.f=function(procIndex) {
    
    functionList <- 1:numTypes %>% map(.f=function(typeIndex) {
      
      functionInput = sampledTruthData %>% filter(processIndex == procIndex,funcIndex==typeIndex) %>% transmute(time=time,value=noiseValue) 
      approxfun(as.vector(functionInput$time),as.vector(functionInput$value))
    })
  })
  
  # gpListofLists <- 1:numProcesses %>% map(.f=function(procIndex){
  # 
  #   gpList <- 1:numTypes %>% map(.f=function(typeIndex) {
  # 
  #     sampledTruthData %>% filter(processIndex==procIndex,funcIndex==typeIndex) %>% transmute(time=time,value=noiseValue) %>% learnGP()
  # 
  #   })
  # 
  # })
}


learnGP <- function(trainingDataTable) {
  
  require(GauPro)
  
  gp <- GauPro(X=as.vector(trainingDataTable$time),Z=as.vector(trainingDataTable$value),parallel=FALSE)
}


generateSampleTable <- function(listOfFunctionLists,noiseValues,truthType) {
  
  maxTime = 100
  numPointsInSample = 20:50
  
  numProcesses = length(listOfFunctionLists)
  numTypes = length(listOfFunctionLists[[1]])
  #randomType = sample(1:numTypes,1)
  
  
  listOfFunctions = 1:numProcesses %>% map(.f=function(procIndex) {
    
    listOfFunctionLists[[procIndex]][[truthType]]
  })
  
  numObservations = sample(numPointsInSample,1)
  observationTimes = sort(sample(1:maxTime,numObservations))
  
  1:numProcesses %>% map_df(.f=function(procIndex) {
    
    observations <- listOfFunctions[[procIndex]](observationTimes)
    noise = rnorm(length(observationTimes),0,noiseValues[[procIndex]])
    observations = observations + noise
    
    tibble(time=observationTimes,value=observations) %>% mutate(processIndex = procIndex)
    #observations <- generateGPSamples(listOfGPs[[procIndex]],observationTimes) %>% mutate(processIndex = procIndex)
      
  })
  
}

generateGPSamples <- function(gp,observationTimes) {
  
    sampleValues <- gp$sample(XX=observationTimes)
    tibble(time=observationTimes,value=sampleValues)
}


getDLDTImages <- function(sampleTable,limits,bins) {
  
  numProc = length(unique(sampleTable$processIndex))
  
  if (nrow(sampleTable) > 1) {
  
    dldtData <- 1:numProc %>% map_df(.f=function(procIndex){
      
      sampleTable %>% filter(processIndex == procIndex) %>% calculateDValDTime() %>% mutate(processIndex = procIndex)
    })  
    
    
    colorNames = c('r','g','b')
    map_df(.x=1:numProc,.f=function(procIndex) {
      
      dldtData %>% filter(processIndex == procIndex) %>% 
        binDValDTime(limits[[procIndex]],bins) %>% as.data.frame() %>% mutate(processIndex = colorNames[procIndex])
      
    })
  } else {
    
    list(array(0, dim=c(bins[1],bins[1],1,1)) %>% as.cimg() %>% as.data.frame() %>% mutate(processIndex = rep("r",256)),
         array(0, dim=c(bins[1],bins[1],1,1)) %>% as.cimg() %>% as.data.frame() %>% mutate(processIndex = rep("g",256)),
         array(0, dim=c(bins[1],bins[1],1,1)) %>% as.cimg() %>% as.data.frame() %>% mutate(processIndex = rep("b",256))) %>% bind_rows()
    
  }
  
}


calculateDValDTime <- function(valueTable) {
  
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
  
  bins <- bin2(as.matrix(diffTable),limits,numBins)
  #bins <- t(bins$nc)
  bins <- bins$nc
  numRows <- dim(bins)[1]
  numCols <- dim(bins)[2]
  
  imageArray <- array(NA, dim=c(numRows,numCols,1,1))
  imageArray[,,,] <- bins
  imageArray <- as.cimg(imageArray)
}



