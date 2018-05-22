#' generateFunctionList
#'
#'Returns a list, that contains all functions specified in functions with the corresponding pattern.
#'
#' @param functions: a character vector that contains names of functions that exist in the global environment
#' @param pattern: a character vector that represent patterns of the dataset to whom the functions should be applied. If pattern is of length one the same pattern is applied to all functions. Otherwise pattern has to be of the same length as functions.
#'
#'
#' @export



generateFunctionList <- function(functions = vector() , pattern = vector())
{
  # initialize a list for functions
  functionList <- list()

  if(length(functions) > 1 && length(pattern) == 1){
    pattern <- rep(pattern , times = length(functions))

  }else if(length(functions)>1 && length(pattern) != length(functions)){

    stop("If you specifiy more than one pattern the number of patterns must match the number of functions")

  }
  # for every function in functions and for every pattern a list entry is generated
  for(i in 1:length(functions)){

    npat <- pattern[i]
    nfunc <- functions[i]

    # assigning the function to the list element
    functionList[[paste(nfunc , npat ,sep = "_")]] <- try(eval(parse(text = nfunc)) , silent = T)

    # setting the pattern in the function
    formals(functionList[[paste(nfunc , npat ,sep = "_")]])[["pattern"]] <- npat

  }

  return(functionList)

}


