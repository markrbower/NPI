script_NPI_toyProblem <- function() {
  source('~/Dropbox/Documents/Concepts/2019_11_19_NetworkParameterOutlier/NPO/Analysis/NPO/parallelWindowNPO_test.R')
  set.seed(1)
  # Create data
  v <- matrix( nrow=4, ncol=7 )
  v[1,] <- c( 0, 5, 20, -30, -50, 0, 0 )
  v[2,] <- -1 * c( 0, 5, 20, -30, -50, -20, 0 )
  v[3,] <- c( 0, 10, 50, -120, -100, -80, -40 )
  v[4,] <- c( 0, 0, 0, 0, 0, 0, 0 )
  # Model parameters
  SD = 5
  DRIFT_MAG = 2.0
  DRIFT_CNT = 200
  # Algorithm parameters (6 of them)
  bandpassFilterFrequency_High <- 6.0E3 # These aren't used here,
  bandpassFilterFrequency_Low  <- 0.6E3 # but are included for completeness.
  cc_threshold <- 0.85
  ed_threshold <- 0.5
  CW <- 2000  # Correlation Window duration
  firingRateThreshold <- 0.01 # In Hz

  MW <- 20000 # Message Window: Each window should get 100 samples, so 10 windows total
  
  NC <- 4 # number of categories
  N <- 10000 # total number of peaks
  T <- cumsum(rpois(N,lambda=5)+4) # blackout of 4
  L <- sample( rep( c(1,2,2,3,3,3,4,4,4,4), times=N/10 ), N, replace=FALSE )
  D <- matrix( unlist( sapply( 1:N, function(i) list( v[L[i],] + rnorm(n=7,mean=0,sd=SD)) ) ), ncol=7, byrow=TRUE )

  # Add drift to cluster2
  idx2 <- which( L == 2 )
  length2 <- length(idx2)
  begin_drift <- idx2[ round(length2/2) ]
  changed_idx <- which( idx2 >= begin_drift)
  drift_idx <- changed_idx[1:DRIFT_CNT]
  drifted_idx <- setdiff( changed_idx, drift_idx )
  idx_start <- drift_idx[1]
  idx_end <- drift_idx[DRIFT_CNT]
  D[idx2[drift_idx],] <- ( ( (DRIFT_MAG-1.0) / DRIFT_CNT ) * seq(1,DRIFT_CNT) + 1 ) * D[idx2[drift_idx],]
  D[idx2[drifted_idx],] <- DRIFT_MAG * D[idx2[drifted_idx],]
  
  # Make a dataframe of the time and data
  Dl <- list()
  for ( j in seq(1,N) ) { Dl <- append( Dl, list(D[j,]))}
  data <- as.data.frame( cbind( time=T, voltage=Dl ) )
  # Compute message/map breaks
  messages <- list()
  t <- T[1]
  while ( t <= T[N] ) {
    idx <- which( T >= t & T < (t+MW) )
    messages <- append( messages, list(T[idx]) )
    t <- t + MW
  }
  # Loop on messages
  cntMessage <- 0
  if ( length(messages) > 0 ) {
    iter_messages <- itertools::ihasNext( iterators::iter( messages ) )
    message_pre <- list()
    if ( itertools::hasNext(iter_messages) ) {
      message <- iterators::nextElem( iter_messages )
      message <- as.numeric( unlist( message ) )
      CC_next <- NULL
      output <- NULL
      while ( itertools::hasNext(iter_messages) ) {
        cntMessage <- cntMessage + 1
        print( paste0( cntMessage, " of ", length(messages) ) )
        message_post <- iterators::nextElem( iter_messages )
        message_post <- as.numeric( unlist( message_post ) )
        # Computes CC on "message" into "message_post"
        dataIdxMsg <- unlist( sapply( message, function(x) which(data$time==x)) )
        voltage <- data$voltage[dataIdxMsg]
        dataIdxBoth <- unlist( sapply( union(message,message_post), function(x) which(data$time==x)) )
        # This could be sent to "worker" ...
        results <- NPO:::computeCCfwd( CW, cc_threshold, ed_threshold, cntMessage-1, message, voltage, data$time[dataIdxBoth], data$voltage[dataIdxBoth] )
        # ... with this step waiting for a Future
        CC <-  rbind( CC_next, results[["CC"]] )
        output <- NPO:::findLocalCommunitiesCW( output, CW, CC, unlist(message), data$time[dataIdxBoth], data$voltage[dataIdxBoth] )
        output <- NPO:::findGlobalCommunities( output, message ) # from subsequent nodes
        # Shift the messages
        CC_next <- results[["CC_next"]]
        message_pre <- message
        message <- message_post
      } # if there is a next message
    } # while messages hasNext
  } # length(messages)>0
  parallelWindowNPO_test( L, output )
  save(file="parallelWindowNPO_final.RData", output, L )
  signalClusterNames <- separateSignalFromNoise( output, firingRateThreshold )
}
