merge_peaks <- function( prev_peaks, peaks ) {
  #' @export
  
  prev_T <- attr( prev_peaks, 'T' )
  T <- attr( peaks, 'T' )
  new_T <- c( prev_T, T )
  
  prev_peaks <- cbind( prev_peaks, peaks )
  attr( prev_peaks, 'T' ) <- new_T

#  print( paste0( "MERGE: length of peaks: ", ncol(prev_peaks) ) )
#  print( paste0( "MERGE: attribute: ", length(attr(prev_peaks,"T")) ) )
#  
  return( prev_peaks )  
}
