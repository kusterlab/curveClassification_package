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

removeNAs <- function(data){

  idxNA <- apply(data , 1 , function(x) any( is.na(x) ) )

  data <- data[!idxNA,]
}


#Converts the class of a integer to numerical and form logical/characters to factors


#' convertClass
#'
#' converts all columns to class factor or numeric
#'


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

