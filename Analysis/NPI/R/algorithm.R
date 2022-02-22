algorithm <- function( algorithm_NPO, compArgs, iter_conts, primeCount ) {
  #' @export
  library(evaluate)
  library(future)

  storedData <- NULL
  compArgs <- compArgs
  doneCount <- 0

  evaluate( text_algo, debug=TRUE )
  
  # This is needed to allow "bucket_L" to notify "algorithm" when it completes.
  initialize <- function( A ) {
    bucket_L$setAlgo( A )
  }

  # 1.
  run <- function(events) {
    bucket_F$run( events )
  }
  
  incDoneCount <- function( ) {
    doneCount <<- doneCount + 1
#    print( paste0( "algorithm doneCount: ", doneCount ) )
  }
  
  getDoneCount <- function() {
    return( doneCount )
  }
  
  flush <- function() {
    bucket_L$flush()
  }
  
  # 2.
  obj <- list(initialize=initialize,run=run,add=add,getValue=getValue,setData=setData,incDoneCount=incDoneCount,getDoneCount=getDoneCount,flush=flush)
  class(obj) <- c('algorithm' )
  return( obj )
}


