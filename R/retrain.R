#' retrain
#'
#' This function retrains a existing model of class WrappedCombiModel and returns it.
#'
#' @param combinedModel: a model of class WrappedCombiModel.
#' @param newdata: a data.frame containing new data that should be used for retraining.
#' @param estimatingThreshold: a boolean vector of length one specifying whether the threshold should be estimated or not.
#' @param tprThreshold: a single numeric value between 0 and 1 that specifies the true positive  rate onto which the threshold should be tuned.
#' @param keepData: if TRUE the data stored in the combinedModel were kept. In this case the layout of the data has to be identical. If FALSE only newdata is stored in the retrained model and the retraining is based only on this data.
#'
#' @export












retrain <- function(combinedModel , newdata , estimatingThreshold = F , tprThreshold = 0.995 , keepData = F){

  if(! all( combinedModel$model$features %in% colnames(newdata) ) && !is.null(combinedModel[["funList"]])){

    newdata <- evaluateFunList(funList = combinedModel$funList , data = newdata)


  }

  # checking if the new data contains now all of the required features
  if(! all( combinedModel$model$features %in% colnames(newdata) )){
    stop("Erorr: The features which could be calculated from the functions stored in the WrappedCombiModel do not match the features used to train the model")
  }



  ntrain <- sample(1:nrow(newdata) , 0.8*nrow(newdata))

  #for test data the features are not subseted
  ntest <- setdiff(1:nrow(newdata) , ntrain)

  newdata[ntrain ,"group"] <- "train"
  newdata[ntest , "group"] <- "test"

  if(is.null(combinedModel$data)){

    combinedModel$data <- newdata
  }else if(keepData){

    combinedModel$data <- rbind(combinedModel$data , newdata[ , names(combinedModel$data) ])


  }else{

    combinedModel$data <-  newdata[ , names(combinedModel$data) ]


  }

  newdata <- combinedModel$data[combinedModel$data$group == "train" ,c(combinedModel$model$features , combinedModel$model$task.desc$target)]

  newdata <- removeNAs( data = newdata )

  newdata <- convertClass(newdata)

  task <- makeClassifTask(id = combinedModel$model$task.desc$id , data =  newdata , target = combinedModel$model$task.desc$target , positive = combinedModel$model$task.desc$positive)


  combinedModel$model <- mlr::train(learner = combinedModel$modelpars$learner , task = task)

  if(estimatingThreshold){

    resamp <- mlr::makeResampleDesc("CV" , iters = 10 ,stratify = F , predict = "both")

    # performing the resampling
    resampResult <- mlr::resample(combinedModel$modelpars$learner , task , resamp , measure = list(tpr , fpr , acc , ppv , auc) , models = T)

    # calculating the Threshold based on the prediction of the 10 fold CV
    combinedModel[["threshold"]] <- getThresholdCV(resampleResult = resampResult , train.data = newdata , tprThreshold = tprThreshold)


  }


  return(combinedModel)

}
