computePeaks <- function( variables, data, compArgs ) {
  #' @export
  #' 
  peak_matrix <- matrix()
  if ( length( data ) > 1 ) {
    # filter
    filteredData <- NPI:::filterForSignalType( variables, variables$buffer, data )
    # find
    fdata <- filteredData$filt_data_detect
    dw = diff(fdata);
    sdw = sign(dw);
    dsdw = diff(sdw);
    C = which( dsdw>0 | dsdw<0 ) + 1;
    peak_matrix <- NULL # default
    C <- C[ which( C > abs(min(variables$blackout_mask)) | C < (length(C)-max(variables$blackout_mask)) ) ]
    if ( length(C) > 0 ) {
      idx <- which( sapply( C, function(x) {NPI:::isLocalPeak_mask(x,variables$blackout_mask,fdata)}) )
      idx <- C[ unlist( idx )]
      # Return the waveforms as a matrix.
      peak_matrix <- as.matrix( sapply( idx, function(x) { NPI:::shiftThePeak_mask(x,variables$waveform_mask,fdata)} ) )
      T <- attr(data,'t0') + (idx-1)*attr(data,'dt')
      # Minimum energy requirement
      cm <- compArgs$get('computation_mask')
      if ( ncol(peak_matrix)>0 & nrow(peak_matrix)>0 ) {
        energy <- sapply( 1:ncol(peak_matrix), function(x) sum( peak_matrix[cm,x] * peak_matrix[cm,x] ) )
        bad_idx <- which( energy < 1E4 )
        if ( length(bad_idx) > 0 ) {
          peak_matrix <- peak_matrix[,-bad_idx]
          T <- T[-bad_idx]
        }
        # attributes
        attr( peak_matrix, 'T' ) <- T
      }
    }
  }
#  print( paste0( "COMPUTE: length of peaks: ", ncol(peak_matrix) ) )
#  print( paste0( "COMPUTE: attribute: ", length(attr(peak_matrix,"T")) ) )
  
  return(peak_matrix)  # This variable is passed as the input to the next "bucket".
}

