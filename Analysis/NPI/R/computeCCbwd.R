computeCCbwd <- function( CW, cc_threshold, ed_threshold, peaks, compArgs, startN=1 ) {

  cm <- compArgs$get( 'computation_mask' )
  
  compute_idx <- startN:ncol(peaks)
  if ( length(compute_idx) > 0 ) {
    time <- attr( peaks, 'T' )
    # Remember that peaks is Nsample rows by the number of peaks ...
    IDX_source_list <- lapply( time[compute_idx], function(t) which( time > (t-CW) & time < (t) ) ) # The "bwd" part
    LL <- lengths(IDX_source_list)
    IDX_target_list <- lapply( seq(1,length(LL)), function(x) rep(compute_idx[x],times=LL[x]) )
    # Unroll
    IDX_source <- unlist( IDX_source_list ) # This is the earlier time of the two.
    IDX_target <- unlist( IDX_target_list )
    # Compute
    cc_all <- mapply( function(idx_source,idx_target) cor( peaks[cm,idx_source],  peaks[cm,idx_target]), IDX_source, IDX_target )
    er_all <- mapply( function(idx_source,idx_target) sum( peaks[cm,idx_source] * peaks[cm,idx_source] ) /
                                                      sum( peaks[cm,idx_target] * peaks[cm,idx_target] ), IDX_source, IDX_target )
    # Round the values to save space.
    V <- lapply( seq(1,length(time)), function(x) paste0( round(peaks[,x],1),collapse=',') )
    # Unroll
    Ttarget=time[IDX_target]
    WVtarget=unname(unlist(V[IDX_target]))
    Tsource=time[IDX_source]
    WVsource=unname(unlist(V[IDX_source]))
    weight=unlist(cc_all)
    # Keep the valid values
    keepIdx <- which( cc_all > cc_threshold & er_all < ed_threshold & er_all > (1/ed_threshold) )
#    keepIdx <- which( cc_all > cc_threshold )
    Ttarget <- round( Ttarget[keepIdx] ) # This is a weird problem of text-to-numeric at large values.
    WVtarget <- WVtarget[keepIdx]
    Tsource <- round( Tsource[keepIdx] ) # This is a weird problem of text-to-numeric at large values.
    WVsource <- WVsource[keepIdx]
    weight <- weight[keepIdx]
    # If the earlier node had a clusterid, then propagate that.
    CC <- data.frame( Tsource=Tsource, Ttarget=Ttarget, weight=weight )
    CC$WVtarget <- WVtarget
    CC$WVsource <- WVsource
  
  #  return( result )
    attr( CC, 'timeStart' ) <- min( time )
    attr( CC, 'timeStop'  ) <- max( time )
    return( CC )
  } else {
    return( NULL )
  }
}


