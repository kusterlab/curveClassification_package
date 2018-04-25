#' predict.WrappedCombiModel
#'
#' This function returns a prediction object from the package mlr.
#'
#' @param combinedModel: a model of class WrappedCombiModel.
#' @param newdata: a data.frame containing new data that should be predicted.
#' @param NAtoZero: if TRUE, all observations that contain a NA in one of the features used by the model will be predicted with a probability of 0. If FALSE, for those observations NA is returned.
#'
#'
#'
#' @import mlr
#' @import caret
#'
#' @export










predict.WrappedCombiModel <- function(combinedModel , newdata , NAtoZero = F , ...){

  #checking if all features are calculated or not, if all features are aviable the prediction is done based on the data otherwise the featrues are calculated
  if(! all( combinedModel$model$features %in% colnames(newdata) ) && !is.null(combinedModel[["funList"]])){

    newdata <- evaluateFunList(funList = combinedModel$funList , data = newdata)


  }

  # checking if the new data contains now all of the required features
  if(! all( combinedModel$model$features %in% colnames(newdata) )){
    stop("Erorr: The features which could be calculated from the functions stored in the WrappedCombiModel do not match the features used to train the model")
  }

  #nessesary to adjust the factor levels to those used for the training, grep is nessesary to discard the training dataset

  for(n in grep(combinedModel$model$task.desc$target , names(combinedModel$model$factor.levels) , invert = T , value = T)){

    newdata[,n] <- factor(x = newdata[,n] , levels = combinedModel$model$factor.levels[[n]])

  }


  newdata <- convertClass(newdata)


  #prediction done with the underlaying predict function

  prediction <- predict(combinedModel$model , newdata = newdata)

  #if threshold is unequal to NULL the threshold is set to the prediction

  if(! is.null(combinedModel$threshold)){

    prediction <- mlr::setThreshold(pred = prediction , threshold = combinedModel$threshold)

  }

  #assignin all predictions that contains NA to prob.True = 0
  if(NAtoZero){

    idx <- is.na(prediction$data$prob.FALSE)

    prediction$data[idx , "prob.FALSE"] <- 1
    prediction$data[idx , "prob.TRUE"] <- 0
    #TODO: think about this what happens if others than true or false are returned
    prediction$data[idx , "response"] <- FALSE


  }



  return(prediction)




}
