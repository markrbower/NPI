computeCC <- function( compArgs, peak_matrix, t0 ) {
  #' @export
  
  # Handle parameters: What parameter info is needed for this task?
  CW <- compArgs$get( 'correlationWindow' )
  cc_threshold <- compArgs$get( 'CCthreshold' )
  ed_threshold <- compArgs$get( 'EDthreshold' )
  
  # Default and check
  CCandWaveforms <- data.frame()
  if ( !is.null(peak_matrix) ) {
    if ( ncol( peak_matrix ) > 0 ) {
      #    database access
      T <- attr( peak_matrix, 'T' )
      # Which peaks should be considered "targets"?
      idx <- which( T >= t0 )
      if ( length(idx) > 0 ) {
        startN <- idx[1]
        CCandWaveforms <- computeCCbwd( CW, cc_threshold, ed_threshold, peak_matrix, compArgs, startN )
      } else {
        print( "No new peaks")
      }
    }
  }
  # This should be a dataframe, unlike the input.
  # It must include clusterid for "prev_peak" entries.
  return( CCandWaveforms )
}
