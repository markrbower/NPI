plotClusterRecordset <- function( table, seizureUsed, clusterid ) {
  # rs should be a recordset from MySQL database: time, waveform (as CSV)
  conn <- topconnect::db( dbname='NV', host='localhost', db_user='root', password='' )
  query <- "select time,waveform from NeuroVista_24_005_IIS_0_NVC1001_24_005_01_600000000_90_P where seizureUsed=1321695821810680 and clusterid=4;"
  rs <- DBI::dbGetQuery(conn,query)
  wv <- sapply( 1:ncol(rs), function(x) as.numeric( unlist( stringr::str_split(rs$waveform[[x]],',') ) ) )
  DBI::dbDisconnect(conn)
  
  df <- data.frame(x=1:41)
  df$wv <- wv

}

