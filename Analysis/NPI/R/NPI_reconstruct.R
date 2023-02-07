NPI_reconstruct <- function( compVars, P_table, M_table, channel ) {
  #' @export
  library( igraph )
  library( stringr )
  library( foreach )
  library( parallel )
  library( doParallel )
  nbrWorkers <- parallel::detectCores()
  #  nbrWorkers <- 6
  doParallel::registerDoParallel(nbrWorkers)

  compVars <- RFactories::script_createCompositeVariablesObject()
  subject <- compVars$get('subject')
  
  dbp <- compVars$findClass('databaseProvider')
  conn <- dbp$connect();
  # Find all channels for this subject
  query <- paste0( 'select seizureUsed,clusterid from ', P_table, ' where subject=', inquotes(subject), " AND channel=", inquotes(channel), " group by seizureUsed,clusterid;" )
  rs <- DBI::dbGetQuery( conn, query )
  df <- as.data.frame(rs)
  DBI::dbDisconnect(conn)
#  for ( clusterid in clusterids ) {
  foreach ( i = rownames(df) ) %dopar% {
    seizureUsed <- df[i,'seizureUsed']
    clusterid <- df[i,'clusterid']
      # Load the cluster data from the P table
    conn <- dbp$connect();
    query <- paste0('select time,incident,weights,energy,waveform from ',P_table,' where subject=',inquotes(subject),' and channel=', inquotes(channel),' and seizureUsed=',seizureUsed,' and clusterid=',clusterid,';')
    rs <- DBI::dbGetQuery( conn, query )
    
    # 
    time <- as.numeric( rs$time )
    incident <- lapply( seq(1,length(time)), function(x) unlist(strsplit( rs$incident[x], ',' )) )
    LL <- lengths( incident )
    source <- unlist( incident )
    target <- unlist( lapply( seq(1,length(time)), function(x) rep(time[x],times=LL[x]) ) )
    weights <- unlist( lapply( seq(1,length(time)), function(x) unlist(strsplit( rs$weights[x], ',' )) ) )
    # Values for the entire cluster
    energy <- mean( as.double( rs$energy ) )
    sum_wv <- vector( mode='double', length(as.double(unlist(strsplit(rs$waveform[1],',')))) )
    for ( idx in seq(1:length(incident)) ) {
      sum_wv = sum_wv + as.double(unlist(strsplit(rs$waveform[idx],',')))
    }
    waveform <- sum_wv / length(incident)
    DBI::dbDisconnect(conn)
    
    # Construct the graph
    tryCatch({
      gdf <- data.frame( source=source, target=target, weight=weights )
#      gdf <- gdf[1:1000000,]
      grph <- igraph::graph_from_data_frame( gdf, directed=FALSE )
    },error=function(cond) {
      print( cond )
      return()
    })

    # Compute the parameters
    column <- list()
    value <- list()
    column[1] <- 'subject'; value[1] <- compVars$get('subject')
    column[2] <- 'channel'; value[2] <- channel
    column[3] <- 'seizureUsed'; value[3] <- seizureUsed
    column[4] <- 'session'; value[4] <- compVars$get('session')
    column[5] <- 'label'; value[5] <- ''
    column[6] <- 'count'; value[6] <- length(time)
    column[7] <- 'clusterid'; value[7] <- clusterid
    column[8] <- 'waveform'; value[8] <- paste0(waveform,collapse=',')
    column[9] <- 'minT'; value[9] <- min(time)
    column[10] <- 'maxT'; value[10] <- max(time)
    column[11] <- 'duration'; value[11] <- max(time) - min(time)
    column[12] <- 'rate'; value[12] <- 1E6 * length(time) / ( max(time) - min(time) )
    column[13] <- 'energy'; value[13] <- energy
    column[14] <- 'diameter'; value[14] <- igraph::diameter( grph )
    column[15] <- 'edge_density';value[15] <- igraph::edge_density( grph )
    column[16] <- 'degree';value[16] <- igraph::degree( grph )
    column[17] <- 'hub_score';value[17] <- mean( unlist( igraph::hub_score( grph ) ) )
    column[18] <- 'mean_distance';value[18] <- igraph::mean_distance( grph )
    column[19] <- 'transitivity';value[19] <- igraph::transitivity( grph )
    spectrum <- igraph::spectrum( grph )
    column[20] <- 'eigenvalue';value[20] <- paste0(spectrum$values, collapse=',')
    column[21] <- 'betweenness';value[21] <- igraph::betweenness( graph=grph )
    column[22] <- 'closeness';value[22] <- igraph::closeness( graph=grph )

    # Persist to the M table
    conn <- dbp$connect();
    query <- paste0("insert into ", M_table, " (", paste0( column, collapse="," ), ") VALUES (\'", paste0(value,collapse="\',\'" ), "\');" )
    query <- str_replace_all(query, '\'NULL\'', 'NULL')
    query <- str_replace_all(query, '\'NA\'', 'NULL')
    DBI::dbGetQuery( conn, query )
    DBI::dbDisconnect(conn)
  }

}
