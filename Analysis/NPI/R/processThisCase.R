processThisCase <- function( case, compArgs_file, filename ) {
  # Run the Network Parameter Identifier algorithm
  library(DBI)
  
  #      cat( "Beginning work on ", case$centerTime, "\n" )
  CW <- compArgs_file$get( 'correlationWindow' )
  print( CW )
  compArgs_caseSpecific <- compArgs_file
  #case <- topconnect:::expandStringsToFields( case, "parameters", sep1=":::", sep2="::" )
  compArgs_caseSpecific$findClass('metadataInformer')$set( "case", case )
  # Now, you can populate the 'static_fields' in the databaseUpdateBuffer
  if ( topconnect::currentProcessedLevel( compArgs_caseSpecific, case, 0 ) ) {
    # Set up the database handlers
    # Create the databaseInsertBuffer
    fields <- c('subject','channel','time','waveform','clusterid','seizureUsed','peak','energy','incident','weights','UUID');
    dib <- topconnect::databaseInsertBuffer('NV',compArgs_caseSpecific$get("P"), fields, 500, updates=c('clusterid'), dbuser='root', host='localhost', password='' );
    static_fields <- list('subject','channel','seizureUsed','UUID')
    static_values <- list(compArgs_caseSpecific$get('subject'),compArgs_caseSpecific$get('channel'),as.numeric(compArgs_caseSpecific$get('case')$centerTime),compArgs_caseSpecific$get('case')$UUID)
    dub <- topconnect::databaseUpdateBuffer('NV',compArgs_caseSpecific$get('P'), 5000, static_fields, static_values, 'time', 'clusterid' );

    counterIdx = 0
    timeConstraints <- NPI:::checkTimeConstraints( compArgs_caseSpecific$get('info'), case )
    iterCont <- meftools::MEFcont( filename, 'erlichda', compArgs_caseSpecific$get('bufferSize'), window=timeConstraints, info=compArgs_caseSpecific$get('info') )
    prev_peaks <- c(0)
    prev_nodes <- data.frame()
    
    # Prepare to compute peaks in the data
    # set the filter parameters
    peakComputationVariables <- list()
    info <- compArgs_caseSpecific$get('info')
    Flow  <- compArgs_caseSpecific$get('filter_detect_lowF')  / ( info$header$sampling_frequency / 2 )
    Fhigh <- compArgs_caseSpecific$get('filter_detect_highF') / ( info$header$sampling_frequency / 2 )
    peakComputationVariables$parms_filter_detect <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
    Flow  <- compArgs_caseSpecific$get('filter_keep_lowF')  / ( info$header$sampling_frequency / 2 )
    Fhigh <- compArgs_caseSpecific$get('filter_keep_highF') / ( info$header$sampling_frequency / 2 )
    peakComputationVariables$parms_filter_keep <- signal::butter( 3, c(Flow,Fhigh), 'pass' )
    peakComputationVariables$waveform_mask <- compArgs_caseSpecific$get( 'waveform_mask' )
    peakComputationVariables$blackout <- compArgs_caseSpecific$get('blackout')
    peakComputationVariables$microsecondsPerSample <- 1E6 / info$header$sampling_frequency
    peakComputationVariables$samplesInBlackout <- round( peakComputationVariables$blackout / peakComputationVariables$microsecondsPerSample )
    peakComputationVariables$blackout_mask <- seq( -peakComputationVariables$samplesInBlackout, peakComputationVariables$samplesInBlackout )
    peakComputationVariables$bufferSize <- unlist(compArgs_caseSpecific$get('bufferSize'))
    peakComputationVariables$buffer <- vector( mode='double', length=peakComputationVariables$bufferSize )
    
    prevMaxT <- 0
    while ( iterCont$hasNext() ) {
      iterData <- iterCont$nextElem()
      while ( iterData$hasNext() ) {
        counterIdx = counterIdx + 1
        if ( (counterIdx%%25) == 0 ) {
          print( counterIdx )
        }
#        print( counterIdx )
        # Get the next chunk of data
        data = iterData$nextElem()
        
        t0 <- attr( data, 't0' )
        attr( data, 'counter' ) <- counterIdx
        T <- t0 + ( seq_along(data)-1 )*attr(data,'dt')
        attr(data,'T') <- T
        if ( prevMaxT > 0 ) {
          skipTime <- t0 - prevMaxT
        }
        prevMaxT <- max(T)
#        print( paste0( "Data: ", length(data) ) )

#        # Database: look for clusterid 5 to 456 members
#        dbp <- compArgs_caseSpecific$findClass('databaseProvider')
#        conn <- dbp$connect()
#        query <- "select count(*) as count from NeuroVista_24_005_IIS_0_NVC1001_24_005_01_900000000_50_P where clusterid=5;"
#        rs <- DBI::dbGetQuery( conn, query )
#        if ( rs$count == 456 ) {
#          print( "ready")
#        }
#        DBI::dbDisconnect(conn)

        # peak detection and management
        peaks <- NPI:::computePeaks( peakComputationVariables, data, compArgs_caseSpecific )
        rm( data )
        gc()
        if ( length(peaks) > 0 ) {
          if ( length(prev_peaks) > 1 ) {
            # Remove the old
            T <- attr( peaks, 'T' )
            minT <- min(T)
            prev_T <- attr( prev_peaks, 'T' )
            bad_idx <- which( prev_T < (minT-CW) )
            if ( length(bad_idx) > 0 ) {
              prev_peaks <- prev_peaks[,-bad_idx]
              attr( prev_peaks, 'T' ) <- prev_T[-bad_idx]
            }
            # Add the new
            if ( length(prev_peaks)>0 & length(peaks)>0 ) {
              prev_peaks <- NPI:::merge_peaks( prev_peaks, peaks )
            }
          } else {
            prev_peaks <- NPI:::merge_peaks( NULL, peaks )
          }
          
          # Only compute CC to peaks >= t0.
          CC <- NPI:::computeCC( compArgs_caseSpecific, prev_peaks, t0 )
        
          # Only find communities for the targets.
          nodes <- NPI:::findGraphicalCommunities( CW, CC, compArgs_caseSpecific, dib, counterIdx )
#          print( paste0( "length of new nodes: ", nrow(nodes) ) )
          # Remove prev_nodes that are no longer needed
          if ( length(prev_nodes) > 0 ) {
            if ( nrow(prev_nodes) > 0 ) {
              removeIdx <- which( prev_nodes$time < (nodes$time[1]-CW) )
              if ( length(removeIdx) > 0 ) {
                prev_nodes <- prev_nodes[-removeIdx,]
              }
            }
          }
          # merge nodes
#          Lpn <- nrow(prev_nodes)
#          Ln <- nrow(nodes)
          prev_nodes <- rbind( prev_nodes, nodes )
#          print( paste0( "length of merged nodes: ", nrow(prev_nodes) ) )

#          Lt <- nrow( prev_nodes )
#          print( paste0( Lpn, " : ", Ln, " : ", Lt ) )
        
          # global cluster computation
          # Limit to the most recent 1,000 clustered peaks
#          Ncl <- length( which( prev_nodes$clusterid > 0 ) )
#          if ( Ncl > 10000 ) {
#            bad_idx <- seq(1,(Ncl-10000))
#            prev_nodes <- prev_nodes[-bad_idx,]
#          }
          resultStrx <- NPI:::assignClusterids( prev_nodes, compArgs_caseSpecific, counterIdx )
          prev_nodes <- resultStrx$prev_nodes
          persistIdxs <- resultStrx$persistIdxs
          dub$run( prev_nodes[persistIdxs,c('time','clusterid')] )
          #          dub$run( prev_nodes[persistIdxs,] )
          #          prev_nodes <- NPI:::keepNodesAndResultsWithinCW( nodes, CW )
        } # length(peaks) > 0
      } # iterData$hasNext
    } # iterCont$hasNext
    topconnect::markAsProcessed( compArgs_caseSpecific, case, 1 )
  } # currentProcessedLevel
  return(1)
}

