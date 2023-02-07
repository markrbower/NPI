saveClusterPagesToPDF <- function( host, user, password, dbname, table, ylim=c(-250,250) ) {
  library( ggplot2 )
  library( reshape2 )
  library( plyr )
  library( tidyverse )
  
  source("~/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Analysis/NPI/plotClusterWaveforms.R")
  
  conn <- topconnect::db( dbname=dbname, host=host, db_user=user, password=password )
  query <- paste0( 'select distinct(seizureUsed) as seizuresUsed from ', table, ';')
  rs <- DBI::dbGetQuery(conn,query)
  seizuresUsed <- rs$seizuresUsed
  DBI::dbDisconnect(conn)
  
  for ( seizureUsed in seizuresUsed ) {
    print( seizureUsed )
    conn <- topconnect::db( dbname=dbname, host=host, db_user=user, password=password )
    query <- paste0( 'select distinct(clusterid) as clusterids from ', table, ' where seizureUsed=',seizureUsed,';')
    rs <- DBI::dbGetQuery(conn,query)
    clusterids <- rs$clusterids
    DBI::dbDisconnect(conn)
    
    for ( clusterid in clusterids ) {
      print( clusterid )
      filename <- paste0( "/Users/markrbower/Dropbox/Documents/Concepts/2021_11_19_NetworkPatternIdentifier/NPI/Presentation/Figures/clusters/Cluster_24_005_600_90_1pt5_",seizureUsed,"_CL",clusterid,".pdf" )
      p <- plotClusterWaveforms( host, user, password, dbname, table, seizureUsed, clusterid, ylim=ylim )
      pdf( file=filename )
      print(p)
      dev.off()
    }
  }
}

