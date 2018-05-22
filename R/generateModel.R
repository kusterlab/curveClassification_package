
#' generateModel
#'
#' This function returns a model a list of with the trained model and their parameters.
#'
#' @param classifier: a character vector of length one specifying the classification algorithm to use. The name has to be applied as specified in the mlr package e.g. "classif.randomForest"
#' @param us.rate: a single numeric value between 0 and 1 that specifies the undersampling rate that is applied
#' @param features: a character vector representing column names that are used for model training
#' @param data: a data.frame, which should be used for training. If additional features were calculated data have to contain them already.
#' @param targetVariable: a character vector of length one representing the column that should be used as target.
#' @param positiveClass: a character vector of length one representing a string which specifies a positive example
#' @param hyperpars: a list of hyper parameters passed to the mlr model
#' @param estimatingThreshold: a boolean vector of length one specifying whether the threshold should be estimated or not.
#' @param tprThreshold: a single numeric value between 0 and 1 that specifies the true positive rate onto which the threshold should be tuned.
#'
#' @export



generateModel <- function(classifier , us.rate , features , data , targetVariable , positiveClass , hyperpars = list() , estimatingThreshold = F , tprThreshold = 0.995){

  #InitialTrainData to have it consistent with testdata

  initialData <- data

  #Generating the initial Learner
  learner <- makeLearner(classifier , predict.type = "prob" , par.vals = hyperpars)

  #normalizing the data
  learner <- makePreprocWrapperCaret(learner ,  ppc.scale = TRUE , ppc.center = TRUE)

  #create an undersample wrapper
  learner <- makeUndersampleWrapper(learner , usw.rate = us.rate)

  # subsetting the data to those variables which are needed
  data <- data[, c(features , targetVariable) ]

  # removing all rows which contains any NA
  data <- removeNAs( data = data )

  #converting character and logicals to factors and integers to numerics
  data <- convertClass( data )

  if(dim(data)[1] == 0){
    stop("After removal of all rows which contain a NA no observation is left")

  }

  # generating the task for the model
  task <- makeClassifTask( data = data , target = targetVariable , positive = positiveClass )


  # if the Threshold should be estimated a CV is run and the threshold is returned
  if(estimatingThreshold){

    # resample Description
    resamp <- mlr::makeResampleDesc("CV" , iters = 10 ,stratify = F , predict = "both")

    # performing the resampling
    resampResult <- mlr::resample(learner , task , resamp , measure = list(tpr , fpr , acc , ppv , auc) , models = T)

    # calculating the Threshold based on the prediction of the 10 fold CV
    optimalthreshold <- getThresholdCV(resampleResult = resampResult , train.data = data , tprThreshold = tprThreshold)


  }else{

    optimalthreshold <- NULL

  }

  #training the model
  model <- mlr::train(learner = learner , task = task)


  output <- list(model = model , optimalthreshold = optimalthreshold , train.data = initialData , task = task , learner = learner)

  return(output)


}
