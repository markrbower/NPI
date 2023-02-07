assignClusterids <- function( nodes, compArgs, counter ) {
  #' @export
  
  # Connect communities by votes.
  #
  # 'nodes': comes from findGraphicalCommunities and has 3 fields: time, incident, clusterid
  #
  # Returns a dataframe that has two fields: 'identityValue' and 'updateValue'
  # for use by the databaseUpdateBuffer.
  #
  N <- nrow(nodes)
  if ( N==0 ) {
    return(nodes)
  }
  
  # Cluster the new nodes.
  tryCatch({
    idxs <- which( nodes$counter==counter )
      for ( idx in idxs ) {
      incident_times <- as.numeric(unlist(strsplit(nodes$incident[idx],",")))
      if ( length(incident_times) > 0 ) {
        incident_idx <- unlist( sapply( incident_times, function(x) which(nodes$time==x)) )
        if ( length(incident_idx) > 0 ) {
          clusterids <- nodes$clusterid[incident_idx]
          clusterids <- clusterids[ which(clusterids>0) ]
          if ( length(clusterids) > 0 ) {
            votes <- sort( table( clusterids, useNA='no' ), decreasing = TRUE )
            nodes$clusterid[idx] = as.numeric(names(votes)[1])
          }
        } else {
          nodes$clusterid[idx] <- NPI:::maxIDfromVectorOrDatabase( nodes$clusterid, compArgs ) + 1
        }
      } else { # This node is isolated from previous nodes. It has no input. It was only a source
        nodes$clusterid[idx] <- NPI:::maxIDfromVectorOrDatabase( nodes$clusterid, compArgs ) + 1
      }
    } # message_idx
  }, error=function(x) {print(x)}
  )
#  save(file="parallelWindowNPO_2.RData",nodes)
  
  resultStrx <- list( prev_nodes=nodes, persistIdxs=idxs )
  return( resultStrx )
}


