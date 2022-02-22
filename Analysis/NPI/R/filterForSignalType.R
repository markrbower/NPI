filterForSignalType <- function( parameters, buffer, data ) {
  library( signal )
  
  bufferSize <- length(buffer)
  skipSize <- 1

  # Load the data into the buffer
  # What to do when data size is (perhaps much!) smaller than buffer size?
  padSize <- min( round( ( bufferSize - length(data) ) / 2 ), floor(length(data)/2) )
  buffer[1:padSize] <- rev( data[1:padSize] )
  dataIdx <- seq(1,length(data))
  buffer[dataIdx + padSize] <- data
  padIdx <- seq( (padSize + length(data) + 1), min( (length(data) + 2*padSize), length(buffer) ) )
  dataIdx <- seq( (length(data)-length(padIdx)+1), length(data) )
  buffer[padIdx] <- rev(data[dataIdx])

  filt_buffer <- filtfilt( parameters$parms_filter_detect, buffer )
  dataIdx2 <- dataIdx
  dataIdx <- seq( from=(padSize+1), to=(padSize + length(data)), by=skipSize )
  filt_data_detect <- filt_buffer[dataIdx]

  # Load the data into the buffer
  padSize <- min( round( ( bufferSize - length(data) ) / 2 ), floor(length(data)/2) )
  buffer[1:padSize] <- rev( data[1:padSize] )
  dataIdx <- seq(1,length(data))
  buffer[dataIdx + padSize] <- data
  padIdx <- seq( (padSize + length(data) + 1), min( (length(data) + 2*padSize), length(buffer) ) )
  dataIdx <- seq( (length(data)-length(padIdx)+1), length(data) )
  buffer[padIdx] <- rev(data[dataIdx])

  # - Use higher-frequencies for actual data.
  filt_buffer <- filtfilt( parameters$parms_filter_keep, buffer )
  dataIdx <- seq( from=(padSize+1), to=(padSize + length(data)), by=skipSize )
  filt_data_keep <- filt_buffer[dataIdx]
  
  # Timestamp of the first value
  s0 <- attr( data, 's0' )
  t0 <- attr( data, 't0' )
  dt <- attr( data, 'dt' )
  info <- attr( data, 'info' )

  filteredData <- list( filt_data_detect=filt_data_detect, filt_data_keep=filt_data_keep,s0=s0,t0=t0,dt=dt,info=info )
  #print( "Returning")
  return( filteredData )
}
