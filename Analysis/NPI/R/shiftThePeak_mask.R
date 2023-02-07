shiftThePeak_mask <- function( x, mask, raw_data ) {
  # Return the shifted mask indices
  max_idx <- 0
  max_shift <- 5
  check_idx = x+seq(-max_shift,max_shift)
  center_idx <- max_shift + 1
  # As long as this has been checked by "isLocalPeak_mask.R", it is not needed here
#  bad <- which( idx<1 | idx>length(data) )
#  if ( length(bad) > 0 ) {
#    idx <- idx[-bad]
#  }
  if ( raw_data[x] >= 0 ) {
    peak_idx <- which( raw_data[check_idx] == max(raw_data[check_idx]) )
    shift <- peak_idx - center_idx 
  } else {
    valley_idx <- which( raw_data[check_idx] == min(raw_data[check_idx]) )
    shift <- valley_idx - center_idx
  }
  new_idx <- shift + x + mask
  return( raw_data[new_idx] )
}
