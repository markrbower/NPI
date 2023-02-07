isLocalPeak_mask <- function( c_, mask, filt_data ) {
  # Must ensure that a complete waveform can be retrived by "mask"
  # even if the peak requires the maximum shift.
  flag <- FALSE
  max_shift <- 5
  idx = c_+mask
  bad <- which( idx<(1+max_shift) | idx>(length(filt_data)-max_shift) )
  if ( length(bad) == 0 ) {
    check <- filt_data[idx]
    # A local peak is "sovereign" for it's polarity;
    # i.e., it is still a "peak" even if the subsequent "valley" is deeper.
    if ( filt_data[c_]>=0 ) {
      if ( filt_data[c_]>=max(check) ) {
        flag <- TRUE
      }     
    } else {
      if ( filt_data[c_]<=min(check) ) {
        flag <- TRUE
      }     
    }
  }
  return( flag )
}
