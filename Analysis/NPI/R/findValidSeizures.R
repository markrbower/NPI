findValidSeizures <- function( result, windowHours=c(-24,24), includeRecovery=FALSE, includeREM=FALSE ) {
  # Apply constraints to results from findValidEpochs()
  #
  # INPUT:
  # - result:       result list from findValidEpochs.R
  # - windowHours:  vector describing latency bounds in hours
  # - includeREM:   boolean declaring whether valid REM is a constraint
  #
  # OUTPUT:
  # - index of valid entries
  #
  # b <- findValidSeizures( result_5min )
  # r <- findValidSeizures( result_5min, includeRecovery = TRUE )
  # R <- findValidSeizures( result_5min, includeREM = TRUE )
  # recREM <- findValidSeizures( result_5min, includeRecovery = TRUE, includeREM = TRUE )
  #
  # USAGE: Score outputs as follows
  # pre_lat <- (beh$Ei_start[b] - beh$seizure_start[b])/(60*60*1e6)
  # post_lat <- (beh$Oearly_stop[b] - beh$seizure_start[b])/(60*60*1e6)
  #
  # pre_lat <- (beh$Ei_start[r] - beh$seizure_start[r])/(60*60*1e6)
  # post_lat <- (rec$Oi_stop[r] - beh$seizure_start[r])/(60*60*1e6)

  windowMicro <- windowHours * 60 * 60 * 1e6

  beh <- result$behavior
  rec <- result$recovery
  rem <- result$rem
    
  pre_latency <- beh$Ei_start - beh$seizure_start
  post_latency <- beh$Oearly_stop - beh$seizure_start
  latency_idx <- which( (pre_latency > windowMicro[1]) & (post_latency < windowMicro[2]) )
  valid_idx <- intersect( which(complete.cases(beh) & beh$interveningPRE==FALSE & beh$interveningPOST==FALSE), latency_idx )

  if ( includeRecovery == TRUE ) {
    post_latency <- rec$Ri_stop - beh$seizure_start
    latency_idx <- which( (pre_latency > windowMicro[1]) & (post_latency < 2*windowMicro[2]) )
    rec_idx <- intersect( which(complete.cases(rec)), latency_idx )  
    valid_idx <- intersect( valid_idx, rec_idx )  
  }  
  
  if ( includeREM == TRUE ) {
    valid_idx <- intersect( which(complete.cases(rem)), valid_idx )  
  }  
  return( valid_idx )
}
