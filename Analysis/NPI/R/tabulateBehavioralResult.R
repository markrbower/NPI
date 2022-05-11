tabulateBehavioralResult <- function( result ) {
  # INPUT:
  # - result:   Data.Frame; e.g., result <- findValidEpochs( 'NV', '24_005', 2 )

  # Find all pairs of valid sequential seizures
  behavior_idx <- which( complete.cases( validEpochs$behavior ) )
  recovery_idx <- which( complete.cases( validEpochs$recovery ) )
  rem_idx <- which( complete.cases( validEpochs$rem ) )
  z_idx <- which( complete.cases( validEpochs$seizureTimes ) )
  valid_seizures <- intersect( intersect( z_idx, behavior_idx ), intersect( recovery_idx, rem_idx ) )
  
  
  valid_pairs_firstIdx <- which( diff(valid_seizures) == 1 )
  
  
  # ERROR!
  # You cannot identify valid_behavior-singl_seizure based on pair indices.
  
  # Find non-overlapping sequential seizure pairs with latency less than one day.
  behavior <- validEpochs$behavior[valid_seizures,]
  recovery <- validEpochs$recovery[valid_seizures,]
  rem <- validEpochs$rem[valid_seizures,]
  seizure <- validEpochs$seizureTimes[valid_seizures,]
  N <- nrow( behavior)
  on <- behavior$Ei_start[2:N]
  off <- recovery$Ri_stop[1:(N-1)]
  overlap_latency <- on-off
  nonoverlap_pairs <- which(overlap_latency>0 & overlap_latency<2E12) 

  validSeizureInfo <- list(analysisStart=behavior$Ei_start,analysisStop=recovery$Ri_stop,behavior=behavior,recovery=recovery,rem=rem,seizure=seizure,nonoverlap_pairs=nonoverlap_pairs)
   
  return( validSeizureInfo )
}



