# a function which assigns NA to every infinite value


#' removeInfinites
#'
#' a function which assigns NA to every infinite value
#'


removeInfinites <- function(data){

  for(n in 1:dim(data)[2])
  {
    if(any(!is.finite(data[,n])))
    {
      data[!is.finite(data[,n]) , n ] <- NA
    }

  }

}


#' evaluateFunList
#'
#' Returns a data.frame that consist of the applied data and the additional features calculated with the functions from funlist.
#'
#'
#'
#' @param funlist: a list that contains functions from whom the first argument is data.
#' @param data: a data.frame on which the functions from funList should be evaluated on.
#'
#'


evaluateFunList <- function(funList , data){

  #For every entrie in funList call the function on data
  for(i in 1:length(funList)){

    # mainly nessesary in case the user uploads a self defined function
    data <- try(funList[[i]](data) , silent = T)

  }

  return(data)

}



#' removeNAs
#'
#' removes every row that contain at least one NA
#'
#'
#'



removeNAs <- function(data){

  idxNA <- apply(data , 1 , function(x) any( is.na(x) ) )

  data <- data[!idxNA,]
}


#' plotTargetDensity
#'
#' plots the target density
#'
#'
#'


plotTargetDensity <- function(predictionLFQ , thresholdLFQ , lowConfidenceTargets = NULL){

  data <- predictionLFQ$data

  dataline <- data.frame(Intensity_type = "LFQ" , threshold = thresholdLFQ )

  data$truth <- ifelse(data$truth == TRUE , "TRUE" , "FALSE")

  if(!is.null(lowConfidenceTargets)){
    data$truth[rownames(data) %in% lowConfidenceTargets$x] <-  "low confidence"
  }
  plot <- ggplot(data , aes(x = truth  , y = prob.TRUE, group = truth , color = truth , fill = truth)) + geom_violin(alpha = 0.5 , scale = "width") + ylab("target probability") + labs(color = "Target class" , fill = "Target class") + xlab(" ") + geom_hline( data = dataline , aes(yintercept =threshold) )

  plot

}

#' beanPlotFeatures
#'
#' plots a beanplot ot two classes
#'
#'
#'


beanPlotFeatures <- function(data , target , subsetvariable = NULL , col = c("orange" , "blue") ,quantileCut = F  , interquantile = 10 ,  ...){

  idx <- vector()

  for(n in names(data))
  {
    idx[n]<- class(data[,n]) == "numeric" || class(data[,n]) == "integer"
  }

  features <- names(data)[idx]



  for(nfeature in features)
  {

    if(quantileCut){

      ylimLower <- -interquantile*(fivenum(data[,nfeature] , na.rm = T)[4]-fivenum(data[,nfeature] , na.rm = T)[2])
      ylimUpper <- interquantile*(fivenum(data[,nfeature] , na.rm = T)[4]-fivenum(data[,nfeature] , na.rm = T)[2])
    }else{
      ylimLower <- NULL
      ylimUpper <- NULL

    }


    tmp <- split(data[,nfeature] , data[,c(target , subsetvariable) ])

    if(length(tmp) == 2){
      par(mar=c(5.1,4.1,4.1,2.1))
      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , show.names = F , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = names(tmp) , pch = 19  , bty = "n")



    }else if(length(unique(data$target)) == 2){
      par(mar=c(11.1,4.1,4.1,2.1))

      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , side = "both" ,  col = list(col[1], c(col[2], "white")) , ... , ylim = c(ylimLower , ylimUpper) )
      legend("topright" , col = col , legend = unique(data$target) , pch = 19  , bty = "n")
      par(mar=c(5.1,4.1,4.1,2.1))

    }else{

      beanplot::beanplot(tmp , what = c(1,1,1,0) , ylab = nfeature  , bw = "nrd0" , las = 2 , ...  , ylim = c(ylimLower , ylimUpper))



    }



  }


}



#it was added all with prob.TRUE on 27.2

nearestNeighbors <- function(searchspace , newData , nNeighbor = 1000  , targetColumn = "Target" )
{
  # searchspace contains only columns of intrest

  # newdata needs to contain all columns which are also present in the searchspace

  newData <- newData[ , grep(paste0(paste0("^" , colnames(searchspace) , "$") , collapse = "|") , names(newData))]

  if(dim(newData)[2] != dim(searchspace)[2]){

    stop("Search space and new data do not contain the same column.")
  }

  newData <- newData[,colnames(searchspace)]

  lengthSearchspace <- dim(searchspace)[1]

  searchspace <- rbind(searchspace , newData)

  searchdf <- BBmisc::normalize(x = searchspace[,grep(targetColumn , names(searchspace) , invert = T)] , method = "standardize" )

  nearestNeighbor <- RANN::nn2(data = searchdf[-(lengthSearchspace+1) , ] , query = searchdf[(lengthSearchspace+1) , ] , k = nNeighbor)

  tmpData <- searchspace[nearestNeighbor[["nn.idx"]] , ]

  indexObserved <- tmpData[,targetColumn] == TRUE


  # minus one nessesary due to the fact that the first nearest neighbor is the observation itself and should not occur
  # later on the one is added to the index again
  obsTrue <- min(which(indexObserved == TRUE), na.rm = T)

  obsFalse <- min(which(indexObserved == FALSE) , na.rm = T)

  title <- c("nearest neighbor: TRUE" , "nearest neighbor: FALSE")

  neighborsdata <- cbind(tmpData[c(obsTrue , obsFalse),], title = title)
  neighborsdata <- rbind(neighborsdata , cbind(newData , title = "Selected observation"))


  return(neighborsdata)


}



predictionNames <- function(prediction , class = c("TP" , "TN" , "FP" , "FN")){


  if(class == "TP"){

    name <- rownames(prediction$data)[prediction$data$truth == TRUE & prediction$data$response == TRUE]

  }else if(class == "TN"){

    name <- rownames(prediction$data)[prediction$data$truth == FALSE & prediction$data$response == FALSE]

  }else if(class == "FP"){

    name <- rownames(prediction$data)[prediction$data$truth == FALSE & prediction$data$response == TRUE]

  }else if(class == "FN"){

    name <- rownames(prediction$data)[prediction$data$truth == TRUE & prediction$data$response == FALSE]

  }

  return(name)

}



summarizeModel <- function(combinedModel){

  dataSummary <- t(as.data.frame(unlist(getHyperPars(combinedModel$modelpars$learner))))

  dataSummary <- cbind(dataSummary , 'number of train.data' = combinedModel$model$task.desc$size)


  return(dataSummary)

}


#Converts the class of a integer to numerical and form logical/characters to factors

convertClass <- function(data){
  for(n in 1:dim(data)[2])
  {
    if(class(data[,n]) == "integer")
    {
      data[,n] <- as.numeric(data[,n])
    }else if(class(data[,n]) == "logical" ||class(data[,n]) == "character"){

      data[,n] <- as.factor(data[,n])

    }

  }

  return(data)

}


#' getThresholdCV
#'
#' This function returns a single numeric value, that represents the optimal threshold.
#'
#' @param resampleResult: The result of the resample function from the mlr package.
#' @param train.data: The data.frame used to train the model.
#' @param tprThreshold: A single numeric value specifying towards which true positive rate the threshold should be tuned.
#'
#'
#'


getThresholdCV <- function(resampleResult , train.data , tprThreshold = 0.99){

  thresholdVec <- vector(mode = "numeric" , length = length(resampleResult$models))


  for(n in 1:length(resampleResult$models)){

    model <- resampleResult$models[[n]]

    idxTrain <- setdiff(1:nrow(train.data) , resampleResult$pred$instance$train.inds[[n]])

    prediction <- predict(model , newdata = train.data[idxTrain,])

    tmp <- generateThreshVsPerfData(obj = prediction , measures = list(tpr)  , gridsize = 200 )

    threshold <- tmp$data[tmp$data$tpr > tprThreshold,]

    threshold <- threshold[order(threshold$threshold , decreasing = T),]

    thresholdVec[n] <- threshold$threshold[1]



  }

  return(mean(thresholdVec , na.rm = T))


}

