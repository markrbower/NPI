NPI <- function( compArgs ) {
  # Network Pattern Identifier
  #
  #' @export
  library(future)
  NPI:::setOptions()
  nbrWorkers <- parallel::detectCores()
  cat( nbrWorkers )
  plan(multisession,workers=nbrWorkers) # "multisession" is portable, "multicore" is not
  nbrFutures = round(1.5 * nbrWorkers)
#  p_limit <- 0
  p_limit <-  nbrWorkers + round( ( nbrFutures - nbrWorkers ) / 2 )
  
  # Create the databaseInsertBuffer
  fields <- c('subject','channel','time','waveform','clusterid','seizureUsed','peak','energy','incident','weights','UUID');
  updateFields <- c('clusterid')
  dib <<- topconnect::databaseInsertBuffer('NV','NeuroVista_24_005_IIS_0_60000000_50_P', fields, 500, updates=updateFields, dbuser='root', host='localhost', password='' );
  
  db_future <- future::future( 5 )
  
  algoFxn <- function( peaks ) {
    # Each of these functions has a "function" that takes "input data".

#    CC computation
    CC <- NPI:::computeCC( compArgs, peaks )
    
#    network generation
    nodes <- NPI:::findGraphicalCommunities( CW, CC, compArgs, dib )
    
    rm(CC)
    gc()
    return( nodes )    
  }
  
  futs <- list()
  
  compArgs_base <- NPI:::checkRestartAndPassword( compArgs )
  CW <- compArgs$get( 'correlationWindow' )
  bufferSizePower <- 24
  compArgs_base$findClass('analysisInformer')$add( list(bufferSize=2^bufferSizePower) )
  correlationWindow <- compArgs_base$get('correlationWindow')
  fileProvider <- compArgs_base$findClass( 'fileProvider' )
  while ( fileProvider$hasNext() ) {
    compArgs_file <- compArgs_base
    filename <- fileProvider$nextElem()
    print( filename )
    compArgs_file <- topconnect::appendFileMetadata( compArgs_file, filename ) # 'info' should be added to 'compArgs' here
    cases <- topconnect::caseIter( compArgs_file, 6 )
    #    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)
    while ( cases$hasNext() ) {
      case <- cases$nextElem()
#      cat( "Beginning work on ", case$centerTime, "\n" )
      compArgs <- compArgs_file
      compArgs$findClass('metadataInformer')$set( "case", case )
      # Now, you can populate the 'static_fields' in the databaseUpdateBuffer
      static_fields <- list('subject','channel','seizureUsed','UUID')
      if ( topconnect::currentProcessedLevel( compArgs, case, 0 ) ) {
        static_values <- list(compArgs$get('subject'),compArgs$get('channel'),as.numeric(compArgs$get('case')$centerTime),compArgs$get('case')$UUID)
        dub <<- topconnect::databaseUpdateBuffer('NV','NeuroVista_24_005_IIS_0_60000000_50_P', 5000, static_fields, static_values, 'time', 'clusterid' );
        idx = 0
        timeConstraints <- NPI:::checkTimeConstraints( compArgs$get('info'), case )
        iterCont <- meftools::MEFcont( filename, 'erlichda', compArgs$get('bufferSize'), window=timeConstraints, info=compArgs$get('info') )
        prev_peaks <<- c(0)
        prev_nodes <<- data.frame()
        
        # Prepare to compute peaks in the data
        # set the filter parameters
        peakComputationVariables <- list()
        info <- compArgs$get('info')
        Flow  <- compArgs$get('filter_detect_lowF')  / ( info$header$sampling_frequency / 2 )
        Fhigh <- compArgs$get('filter_detect_highF') / ( info$header$sampling_frequency / 2 )
        peakComputationVariables$parms_filter_detect <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
        Flow  <- compArgs$get('filter_keep_lowF')  / ( info$header$sampling_frequency / 2 )
        Fhigh <- compArgs$get('filter_keep_highF') / ( info$header$sampling_frequency / 2 )
        peakComputationVariables$parms_filter_keep <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
        peakComputationVariables$waveform_mask <- compArgs$get( 'waveform_mask' )
        peakComputationVariables$blackout <- compArgs$get('blackout')
        peakComputationVariables$microsecondsPerSample <- 1E6 / info$header$sampling_frequency
        peakComputationVariables$samplesInBlackout <- round( peakComputationVariables$blackout / peakComputationVariables$microsecondsPerSample )
        peakComputationVariables$blackout_mask <- seq( -peakComputationVariables$samplesInBlackout, peakComputationVariables$samplesInBlackout )
        peakComputationVariables$bufferSize <- compArgs$get('bufferSize')
        peakComputationVariables$buffer <- vector( mode='double', length=peakComputationVariables$bufferSize )

        while ( iterCont$hasNext() ) {
          iterData <- iterCont$nextElem()
          while ( iterData$hasNext() ) {
            idx = idx + 1
            if ( (idx%%25) == 0 ) {
              print( idx )
            }
            # Get the next chunk of data
            data = iterData$nextElem()
            T <- attr(data,'t0') + ( seq_along(data)-1 )*attr(data,'dt')
            attr(data,'T') <- T
            attr( data, 'counter' ) <- idx
            
            idx_ = ( (idx-1) %% nbrFutures ) + 1
            
            # peak detection and management
            peaks <- NPI:::computePeaks( peakComputationVariables, data )
            rm( data )
            gc()
            if ( length(prev_peaks) > 1 ) {
              # Remove the old
              T <- attr( peaks, 'T' )
              minT <- min(T)
              prev_T <- attr( prev_peaks, 'T' )
              bad_idx <- which( prev_T < (minT-CW) )
              if ( length(bad_idx) > 0 ) {
                prev_peaks <- prev_peaks[-bad_idx]
              }
              # Add the new
              if ( length(prev_peaks)>0 & length(peaks)>0 ) {
                prev_peaks <- NPI:::merge_peaks( prev_peaks, peaks )
              }
            } else {
              prev_peaks <- peaks
            }
          nodes <- algoFxn( prev_peaks )
#          futs[[ idx_ ]] = future::future(  algoFxn( peaks ) )
#          cat( resolved(futs), "\n" )
          # The Gate
          old_node_cnt <- future::value( db_future ) # Wait for the previous write to finish
          # Process
          j = idx - p_limit
          j_ = ( (j-1) %% nbrFutures ) + 1
          nodes <- future::value( futs[[j_]] )
          # remove prev_nodes that are no longer needed
          if ( nrow(prev_nodes) > 0 ) {
            removeIdx <- which( prev_nodes$time < (nodes$time[1]-CW) )
            if ( length(removeIdx) > 0 ) {
              prev_nodes <<- prev_nodes[-removeIdx,]
            }
          }
          # merge nodes
          prev_nodes <<- rbind( prev_nodes, nodes )

          # global cluster computation
          resultStrx <- NPI:::assignClusterids( prev_nodes, compArgs )
          prev_nodes <<- resultStrx$prev_nodes
          persistIdxs <<- resultStrx$persistIdxs
          db_future <-  future::future( dub$run( prev_nodes[persistIdxs,] ) )
#          dub$run( prev_nodes[persistIdxs,] )
#          prev_nodes <- NPI:::keepNodesAndResultsWithinCW( nodes, CW )
          }
        }
        print( "Flushing" )
        flushIdx <- idx - p_limit + 1
        while ( flushIdx < idx ) {
          j = flushIdx - p_limit
          j_ = ( (j-1) %% nbrFutures ) + 1
          nodes <- future::value( futs[[j_]] )
          # global cluster computation
          nodes <- NPI:::assignClusterids( nodes, compArgs )
          # persist
          old_nodes <- future::value( db_future ) # Wait for the previous write to finish
          rm(old_nodes)
          # How to do just the "new" ones?
#          db_future <- future::future( dub$run( nodes ) )
          dub$run( nodes )
          flushIdx <- flushIdx + 1
        }
        gc()
        future::value( db_future )
        topconnect::markAsProcessed( compArgs, case, 1 )
      } # currentProcessedLevel
      rm(dub);gc()
    } # cases$hasNext
  } # fileProvider$hasNext
  v <- future::value( db_future ) # Wait for the previous write to finish
  plan( sequential )
}

