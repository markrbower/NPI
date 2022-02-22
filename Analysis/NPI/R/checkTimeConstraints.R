checkTimeConstraints <- function( info, case ) {
  timeConstraints <- vector()
  #print( paste0( "case: ", case ) )
  #print( case$analysisStart )
  #print( case$analysisStop )
  if ( is.null(case$analysisStart) ) {
    print( "Start at the beginning.")
    timeConstraints['start']     <- info$header$recording_start_time + 1.01E6
  } else {
    timeConstraints['start']     <- case$analysisStart
  }
  if ( is.null(case$analysisStop) ) {
    print( "Stop at the end.")
    timeConstraints['stop']      <- info$header$recording_end_time - 1.01E6
  } else {
    timeConstraints['stop']      <- case$analysisStop
  }
  timeConstraints['usWindow']  <- timeConstraints['stop'] - timeConstraints['start']
  timeConstraints['isValid']   <- 1
  
  return( timeConstraints )
}

