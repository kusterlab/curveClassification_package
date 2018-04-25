#' combineModel
#'
#' This function combines the output from generateModel with other parameters to a model of class WrappedCombiModel.
#'
#' @param trainOutput: a list containing a model returned by generateModel.
#' @param featureFunctionList: a list of functions returned by generateFunctionList.
#' @param test.data: a data.frame containing the data used for testing the model.
#' @param threshold: an optional single numeric value that represents the threshold that should be applied during the prediction.
#' @param positveClass: a character vector of length one representing a string which specifies a positive example
#'
#' @export










# function which generates a new list of functions
combineModel <- function(trainOutput , featureFunctionList , test.data = NULL ,  threshold = NULL , positveClass = NULL){

  combinedModel <- list()

  combinedModel[["model"]] <- trainOutput$model

  combinedModel[["funList"]] <- featureFunctionList

  if(is.null(threshold)){

    combinedModel[["threshold"]] <- trainOutput$optimalthreshold

  }else{
    combinedModel[["threshold"]] <- threshold
  }


  combinedModel[["modelpars"]] <- trainOutput["learner"]

  combinedModel[["data"]] <- cbind(test.data , group = rep("test" , times = nrow(test.data)))

  combinedModel[["data"]] <- rbind(combinedModel[["data"]] , cbind(trainOutput[["train.data"]] , group = rep("train" , times = nrow(trainOutput[["train.data"]]))))


  combinedModel[["positiveClass"]] <- positveClass

  # Setting a new class for the combined Model in order to predict it with a new function
  class(combinedModel) <- "WrappedCombiModel"

  return(combinedModel)

}
