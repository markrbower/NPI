checkRestartProgressAndPassword <- function( compArgs ) {
  # Built from multiple files
  library(stringr)
  
  print( "In checkRestart:")
  print( paste0( "experiment: ", compArgs$get('experiment')))
  print( paste0( "subject: ", compArgs$get('subject')))
  print( paste0( "centerTime: ", compArgs$get('centerTime')))
  
  db_provider <- compArgs$findClass( 'databaseProvider' )
  conn <- db_provider$connect()
  
  ## From NPI:::tableNamingLogic.R
  prefix <- paste0( compArgs$get('experiment'), '_', compArgs$get('subject'), '_', compArgs$get('signalType'), '_',
                    compArgs$get('centerTime'), '_', compArgs$get('correlationWindow'), '_', round(100*as.numeric(compArgs$get('CCthreshold'))) )
  prefix <- str_replace_all( prefix, '::', '_' )
  prefix <- str_replace_all( prefix, '=', '_' )
  prefix <- str_replace_all( prefix, '-', 'MINUS' )
  print( paste0( "prefix: ", prefix ) )

  ## From NPI:::createTablesForNPI.R
  progress <- paste0( prefix, '_progress' )

  # Make sure that these tables are not already created.
  # progress
  if ( !(NPI:::sqlTableExists( conn, progress ) ) ) {
    query <- paste0( 'create table ', progress, ' like progress;' )
    print( query )
    DBI::dbSendQuery( conn, query )
  }
  
  table_names <- c( progress=progress )
  # Store table_names into compArgs. Where? How? Values are stored in "components"
  analysisInformer <- compArgs$findClass( 'analysisInformer' )
  analysisInformer$add( table_names )

  ## From NPI:::checkRestart
  if ( compArgs$isValid( '--restart') ) {
    # Restart for ALL channels for this subject and centerTime
    
    # Progress
    progress_table <- table_names['progress']
    query <- paste0( "update ", progress_table, " set done=0 where subject=\'", compArgs$get('subject'), "\';" )
    DBI::dbSendQuery( conn, query )
    
    # delete graph file
    filenames <- list.files( path=".", pattern=paste0( compArgs$get('subject'), "[[:alnum:]_.]+", "_graph.xml" ) )
    for ( filename in filenames ) 
      #Delete file if it exists
      file.remove( filename )
  } 
  
  ## From NPI:::checkMEFpassword
  library( secret )
  # Check that the current project has a valid MEF password
  if ( is.null(compArgs$get('service')) ) {
    if ( is.null(compArgs$get('dbName')) ) {
      if ( is.null(compArgs$get('dbname')) ) {
      } else {
        password_key <- paste0( compArgs$get('dbname'), '_password' )
      }
    } else {
      password_key <- paste0( compArgs$get('dbName'), '_password' )
    }
  } else {
    password_key <- paste0( compArgs$get('service'), '_password' )
  }
  vault <- topsecret::get_secret_vault()
  if ( !( password_key %in% secret::list_secrets(vault=vault)$secret)) {
    add_secret(name=password_key,value=readline("Enter MEF file password: "),users=Sys.info()['user'],vault=vault)
  }
  
  DBI:::dbDisconnect( conn )
  
  return( compArgs ) # This now has updated table_names.
}

