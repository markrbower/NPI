firstRunOfLength <- function( startIdx, L, names, value, durations ) {
  #' @export
  
#  valid <- NULL
#  y <- rle( names )
#  bigger_idx <- which( y$lengths >= L & y$values == value )
#  if ( length(bigger_idx) > 0 ) {
#    MBI <- min(bigger_idx)
#    if ( MBI == 1 ) { # special case
#      valid <- c( start=1, stop=sum(y$lengths[1:(MBI)]) ) + startIdx - 1
#    } else {
#      valid <- c( start=1+sum(y$lengths[1:(MBI-1)]), stop=sum(y$lengths[1:(MBI)]) ) + startIdx - 1
#    }
#  }
#  return( valid )
  
  valid <- NULL
  idx <- which( names == value & durations >= L )
  if ( length(idx) > 0 ) {
    MBI <- min( idx )
    valid <- MBI + startIdx - 1
  }
  return(valid)
  
  
  
  
}
