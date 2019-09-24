createProcess1Funcs <- function(numTypes) {
  
  fullFunctionList = list(function(x){sin(x/35)},
                      function(x){1.5*sin(x/35)},
                      function(x){sin(x/35)+0.01*x},
                      function(x){sqrt(x)/10 + 0.01*x})
  
  fullFunctionList[1:numTypes]
}

createProcess2Funcs <- function(numTypes) {
  
  fullFunctionList = list(function(x){x*x/150},
                          function(x){0.75*x},
                          function(x){0.5*x + x*x/100},
                          function(x){exp(x/25)})
  
  fullFunctionList[1:numTypes]
}

createProcess3Funcs <- function(numTypes) {
  
  fullFunctionList = list(function(x){3*sqrt(x)},
                          function(x){sqrt(3*x)},
                          function(x){2*log10(x+1)},
                          function(x){2*x^(1/3)})
  
  fullFunctionList[1:numTypes]
}

createTruthData <- function(timeRange,typeNames,functionList) {
  
  require(tibble)
  require(purrr)
  require(dplyr)
  require(magrittr)
  
  truthTable <- map2(.x=typeNames,.y=functionList,.f=function(name,func) {
    
    initTable <- tibble(time=timeVals)
    initTable %>% mutate(!!name := func(time)) %>% gather(key='func',value='value',-time)
  }) %>% bind_rows()
  
  truthTable
}


downSampleTruthPoints <- function(truthTable,numToKeep,typeNames,timeVals) {
  
  require(tibble)
  require(purrr)
  require(dplyr)
  require(magrittr)
  
  keptPoints <- map(.x=typeNames,.f=function(name) {
    tibble(func = rep(name,numToKeep),point = sort(sample(timeVals,numToKeep)))
  }) %>% bind_rows()
  
  
  typeNames %>% map(.f=function(name) {
    
    indices = keptPoints %>% filter(func==name) %>% select(point)
    originalSamples = truthTable %>% filter(func==name)
    originalSamples %>% filter(time %in% as.array(indices$point))
  }) %>% bind_rows()
}


fitGaussianProcesses <- function(typeNames,sampledTable) {
  
  require(purrr)
  require(GauPro)
  require(magrittr)
  
  typeNames %>% map(.f=function(name) {
    
    sampledTable %>% filter(func==name) %>% transmute(time=time,value=noiseValue) %>% learnGP()
  })
}


createGaussianProcessPredictions <- function(listOfGPs,typeNames,timeVals) {
  
  require(purrr)
  require(GauPro)
  require(magrittr)
  require(dplyr)
  
  predictions <- map2(.x=listOfGPs,.y=typeNames,.f=function(gp,name) {
    
    prediction <- gp$predict(timeVals,se=TRUE)
    prediction %<>% mutate(time=timeVals,upper=mean+2*se,lower=mean-2*se,type=rep(name,length(timeVals)))
  }) %>% bind_rows()
  
  predictions
}


generateSamples <- function(gpList,
                            typeNames,
                            maxTimesForBins,
                            numProfilesPerTimeBin,
                            numPointsPerFrofileRanges) {
  
  numBins = length(maxTimesForBins)
  
  
  numObsPerProfileList = 
    map2(.x=numPointsPerProfileRanges,
         .y=numProfilesPerTimeBin,
         .f=function(range,numProfiles) {
    
    sample(range,numProfiles,replace=TRUE)
  })
  

  observationTimeTable = 1:numBins %>% map(.f=function(binIndex) {

    profileTimesInBin = 1:numProfilesPerTimeBin[binIndex] %>% map(.f=function(profileIndex) {

      sort(sample(1:maxTimesForBins[binIndex],numObsPerProfileList[[binIndex]][profileIndex]))
    })
    
    tibble(profileIndex = 1:numProfilesPerTimeBin[binIndex],timePoints=profileTimesInBin)
  }) %>% bind_rows()
  
  
  observations <- map2(.x=gpList,.y=typeNames,.f=function(gp,name) {
    
    samplesForType <- generateGPSamples(gp,observationTimeTable)
    samplesForType %>% mutate(type=rep(name,n()))
  }) %>% bind_rows()  
  
}



convertSamplesToDValDt <- function(sampleTable,typeNames,numProfilesPerType) {
  
  dldtData <- 1:length(typeNames) %>% map(.f= function(typeIndex) {
    
    singleType <- sampleTable %>% filter(type == typeNames[typeIndex])
    #and then separate by trial
    dldts <- 1:numProfilesPerType %>% map(.f=function(trialIndex){
      
      inputTable <- singleType %>% filter(trial==trialIndex) %>% select(time,value)
      calculateDValDTime(inputTable) %>% mutate(trial = rep(trialIndex,n()))    
    }) %>% bind_rows()
    
    dldts %>% mutate(type = rep(typeNames[typeIndex],n()))
  }) %>% bind_rows()
  
  dldtData
}


createImages <- function(dldtData,typeNames,numProfilesPerType,limits,bins) {
  
  #We do the same loops as above
  imageListOverType <- 1:length(typeNames) %>% map(.f= function(typeIndex) {
    
    singleType <- dldtData %>% filter(type == typeNames[typeIndex])
    #and then separate by trial
    imageListOverTrial <- 1:numProfilesPerType %>% map(.f=function(trialIndex){
      
      inputTable <- singleType %>% filter(trial==trialIndex) %>% select(time,value)
      imageFrame <- binDValDTime(inputTable,limits,bins) %>% as.data.frame() 
      imageFrame %>% mutate(trial=rep(trialIndex,n()))     
    }) %>% bind_rows() 
    
    imageListOverTrial %>% mutate(type = rep(typeNames[typeIndex],n()))
  }) %>% bind_rows()
  
  imageListOverType
}

