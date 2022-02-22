test_NPI <- function() {
  library(future)
  NPI:::setOptions()
  nbrWorkers <- parallel::detectCores()
  cat( nbrWorkers )
  plan(multisession,workers=nbrWorkers) # "multisession" is portable, "multicore" is not
  nbrFutures = round(1.5 * nbrWorkers)
  p_limit = nbrWorkers + round( ( nbrFutures - nbrWorkers ) / 2 )
  
  algoFxn <- function( prev_data, data ) {
    N <- length(data)
    result <- 0
    mask <-  c(1,2,3,4,5,6,7)
    for ( j in seq(1,20)) {
      for ( i in seq_along(prev_data) ) {
        if (max(i+mask) <= N ) {
          result <- result + cor( prev_data[i+mask], mask )
        }       
      }
      for ( i in seq_along(data) ) {
        if (max(i+mask) <= N ) {
          result <- result + cor( data[i+mask], mask )
        }       
      }
    }
    return( result )
  }
  
  compArgs <- RFactories::argumentComposite()
#  dbp <- RFactories::databaseProvider(user="root",vault_user='localadmin',vault_key='NV_password',host='localhost',dbname='NV')
  dbp <- RFactories::databaseProvider(user="root",vault_user='markrbower',vault_key='NV_password',host='localhost',dbname='NV')
  compArgs$add( dbp )
#  compArgs$add( RFactories::fileProvider(path='/Volumes/eplab/Raw_Data/NV_Human/NVC1001_24_005_2',iterationType='directory',pattern="*.mef") )
  compArgs$add( RFactories::fileProvider(path='/Volumes/Data/NV/NVC1001_24_005_2',iterationType='directory',pattern="*.mef") )
  aInf <- RFactories::analysisInformer(experiment='NeuroVista',subject='24_005',centerTime=0,pattern="*.mef",lab="RNCP")
  compArgs$add( aInf )
  pInf <- RFactories::parameterInformer(signalType='IIS')
  pInf$loadParameters( dbp, aInf )  #  The parameterInformer requires a databaseProvidere to load parameters from the database.
  compArgs$add( pInf )
  
  futs <- list()
  
  compArgs_base <- NPI:::checkRestartAndPassword( compArgs )
  bufferSizePower <- 22
  compArgs_base$findClass('analysisInformer')$add( list(bufferSize=2^bufferSizePower) )
  correlationWindow <- compArgs_base$get('correlationWindow')
  fileProvider <- compArgs_base$findClass( 'fileProvider' )
  while ( fileProvider$hasNext() ) {
    compArgs_file <- compArgs_base
    filename <- fileProvider$nextElem()
    print( filename )
    compArgs_file <- topconnect::appendFileMetadata( compArgs_file, filename ) # 'info' should be added to 'compArgs' here
    cases <- topconnect::caseIter( compArgs_file )
    #    foreach::foreach(case = cases) %dopar% { # have the ability to do files in parallel as well as run futures (below)
    while ( cases$hasNext() ) {
      case <- cases$nextElem()
      compArgs <- compArgs_file
      
      compArgs$findClass('metadataInformer')$set( "case", case )
      if ( topconnect::currentProcessedLevel( compArgs, case, 0 ) ) {
        idx = 0
        prev_data <- NULL
        timeConstraints <- NPI:::checkTimeConstraints( compArgs$get('info'), case )
        iterCont <- meftools::MEFcont( filename, 'erlichda', compArgs$get('bufferSize'), window=timeConstraints, info=compArgs$get('info') )
        while ( iterCont$hasNext() ) {
          iterData <- iterCont$nextElem()
          while ( iterData$hasNext() ) {
            idx = idx + 1
            data = iterData$nextElem()
            idx_ = ( (idx-1) %% nbrFutures ) + 1
            futs[[ idx_ ]] = future( algoFxn( prev_data, data ) )
            cat( resolved(futs), "\n" )
            if ( idx > p_limit ) {
              j = idx - p_limit
              j_ = ( (j-1) %% nbrFutures ) + 1
              v <- value( futs[j_] )
              cat( "Database operation: ", j, "\tvalue: ", length(v), "\n" )
            }
            prev_data <- NULL
            gc()
            prev_data = data
          }
        }
      }  
    }
  }  
}

