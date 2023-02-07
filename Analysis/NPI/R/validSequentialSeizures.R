validSequentialSeizures <- function( data, zTimes, validIdx, minDuration ) {
  PROXIMITY <- 3 # hours
  
  N <- length( zTimes )

  idx <- which( data$label[validIdx]=='Awake')
  validWake <- validIdx[idx]
  idx <- which( data$label[validIdx]=='N')
  validSleep <- validIdx[idx]
  idx <- which( data$label[validIdx]=='REM')
  validREM <- validIdx[idx]
  
  validFirstSeizureIdx <- list()
  
  for ( z_cnt in seq( 1, N-1 ) ) {
    z1 <- zTimes[ z_cnt ]
    # PRE #1
    idx <- which(data$omega[validWake] <= (z1-minDuration*1e6) )
    beforeWake <- validWake[idx]
    PRE_W1 <- last(beforeWake)
    
    idx <- which(data$omega[validSleep] <= data$alpha[PRE_W1] )
    beforeSleep <- validSleep[idx]
    PRE_N1 <- last(beforeSleep)
    idx <- which(data$omega[validREM] <= data$alpha[PRE_W1] )
    beforeSleep <- validREM[idx]
    PRE_R1 <- last(beforeSleep)
    PRE_S1 <- min( PRE_N1, PRE_R1 )
    
    idx <- which(data$omega[validWake] <= data$alpha[PRE_S1] )
    beforeWake <- validWake[idx]
    PRE_X1 <- last(beforeWake)

    # POST #1
    idx <- which(data$alpha[validWake] >= z1 )
    afterWake <- validWake[idx]
    POST_W1 <- first(afterWake)
    
    idx <- which(data$alpha[validSleep] >= data$omega[POST_W1] )
    afterSleep <- validSleep[idx]
    POST_N1 <- first(afterSleep)
    idx <- which(data$alpha[validREM] >= data$omega[POST_W1] )
    afterSleep <- validREM[idx]
    POST_R1 <- first(afterSleep)
    POST_S1 <- max( POST_R1, POST_N1 )

    idx <- which(data$alpha[validWake] >= data$omega[POST_S1] )
    afterWake <- validWake[idx]
    POST_X1 <- first(afterWake)

    z2 <- zTimes[ z_cnt+1 ]
    # PRE #2
    idx <- which(data$omega[validWake] <= (z2-minDuration*1e6) )
    beforeWake <- validWake[idx]
    PRE_W2 <- last(beforeWake)
    
    idx <- which(data$omega[validSleep] <= data$alpha[PRE_W2] )
    beforeSleep <- validSleep[idx]
    PRE_N2 <- last(beforeSleep)
    idx <- which(data$omega[validREM] <= data$alpha[PRE_W2] )
    beforeSleep <- validREM[idx]
    PRE_R2 <- last(beforeSleep)
    PRE_S2 <- min( PRE_N2, PRE_R2 )
    
    idx <- which(data$omega[validWake] <= data$alpha[PRE_S2] )
    beforeWake <- validWake[idx]
    PRE_X2 <- last(beforeWake)

    # POST #2
    idx <- which(data$alpha[validWake] >= z2 )
    afterWake <- validWake[idx]
    POST_W2 <- first(afterWake)
    
    idx <- which(data$alpha[validSleep] >= data$omega[POST_W2] )
    afterSleep <- validSleep[idx]
    POST_N2 <- first(afterSleep)
    idx <- which(data$alpha[validREM] >= data$omega[POST_W2] )
    afterSleep <- validREM[idx]
    POST_R2 <- first(afterSleep)
    POST_S2 <- max( POST_N2, POST_R2 )
    
    idx <- which(data$alpha[validWake] >= data$omega[POST_S2] )
    afterWake <- validWake[idx]
    POST_X2 <- first(afterWake)

    if ( length(PRE_X1)>0 & length(PRE_S1)>0 & length(PRE_W1)>0 &
         length(POST_X1)>0 & length(POST_S1)>0 & length(POST_W1)>0 &
#         ( (length(PRE_REM1)>0 & length(POST_REM1)>0) | (length(PRE_REM2)>0 & length(POST_REM1)>0) ) &
         length(PRE_X2)>0 & length(PRE_S2)>0 & length(PRE_W2)>0 &
         length(POST_X2)>0 & length(POST_S2)>0 & length(POST_W2)>0 ) {
      latency_PRE1 <- (z1 - data$alpha[PRE_X1]) / (1e6 * 3600)
      latency_POST1 <- (data$omega[POST_X1] - z1) / (1e6 * 3600)
  
      latency_PRE2 <- (z2 - data$alpha[PRE_X2]) / (1e6 * 3600)
      latency_POST2 <- (data$omega[POST_X2] - z2) / (1e6 * 3600)
      
      if ( latency_PRE1 <= 24  & latency_POST1 <= 24  &  latency_PRE2 <= 24  & latency_POST2 <= 24 ) {
        latency_PRE1 <- (data$omega[PRE_W1] - data$alpha[PRE_X1]) / (1e6 * 3600)
        latency_PRE2 <- (data$omega[PRE_W2] - data$alpha[PRE_X2]) / (1e6 * 3600)
        latency_POST1 <- (data$omega[POST_X1] - data$alpha[POST_W1]) / (1e6 * 3600)
        latency_POST2 <- (data$omega[POST_X2] - data$alpha[POST_W2]) / (1e6 * 3600)
        if ( latency_PRE1 <= PROXIMITY  & latency_POST1 <= PROXIMITY  &  latency_PRE2 <= PROXIMITY  & latency_POST2 <= PROXIMITY ) {
          validFirstSeizureIdx <- append( validFirstSeizureIdx, z_cnt )
        }
      }
    }
  }
  return( validFirstSeizureIdx )
}
