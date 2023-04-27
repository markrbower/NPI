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
  
  futs <- vector( "list", nbrWorkers )
  for ( idx in seq(1,nbrWorkers) ) { futs[[idx]] <- future::future(1) }
  
  compArgs_base <- NPI:::checkRestartProgressAndPassword( compArgs )
  CW <- compArgs$get( 'correlationWindow' )
  bufferSizePower <- 24
  compArgs_base$findClass('analysisInformer')$add( list(bufferSize=2^bufferSizePower) )
  correlationWindow <- compArgs_base$get('correlationWindow')
  fileProvider <- compArgs_base$findClass( 'fileProvider' )
  idx <- 0

  # Look for 'firstSeizureOfValidPair' for this subject in the tasks table.  
  # OLD WAY: load( file='/Users/localadmin/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/R/useMatrix.RData' )
  # NEW WAY
  db_provider <- compArgs$findClass( 'databaseProvider' )
  conn <- db_provider$connect()
  query <- paste0( "select analysisStart,analysisStop from tasks where subject=\'", compArgs$get('subject'),"\' and taskname='firstSeizureOfValidPair';")
  rs_seizureIDs <- DBI::dbGetQuery( conn, query )
  
  
  

  vectorOfCases <- use_matrix

  subject <- compArgs$get('subject')
  while ( fileProvider$hasNext() ) {
    filename <- fileProvider$nextElem()
    compArgs_file <- createPtable( compArgs_base, filename )
    print( filename )
    compArgs_file <- topconnect::appendFileMetadata( compArgs_file, filename ) # 'info' should be added to 'compArgs' here
    cases <- topconnect::caseIter( compArgs_file, nbrCases=12, vectorCases=vectorOfCases[subject,] )
    #    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)
    while ( cases$hasNext() ) {
      case <- cases$nextElem()
      # I am checking here, but this is also checked in NPI.
      # Completion is also marked in NPI, but not here.
      if ( topconnect::currentProcessedLevel( compArgs_file, case, 0 ) ) {
        compArgs_file$findClass('metadataInformer')$set( "case", case )
        cat( " : ", case$centerTime )
        idx <- idx + 1
        if ( idx > nbrWorkers ) {
          idx <- 1
        }
        # Gate
        val <- future::value( futs[[idx]] )
        futs[[idx]] <- future::future( processThisCase( case, compArgs_file, filename ) )
  #      processThisCase( case, compArgs_file, filename )
      } # cases$hasNext
    }
    rm( compArgs_file)
    gc()
  } # fileProvider$hasNext
#  v <- future::value( db_future ) # Wait for the previous write to finish
#  plan( sequential )
}

