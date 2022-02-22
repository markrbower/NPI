findGraphicalCommunities <- function( CW, CC, compArgs, dib ) {
  # The structure of CC:
  library(Matrix)
  
  if ( is.null(CC) ) {
    cat( "findGraphicalCommunities: null CC\n" )
    return(NULL)
  }
  
  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?
  cm <- compArgs$get( 'computation_mask' )
  
  N <- 0
  output <- data.frame( time=integer(length=N), incident=character(length=N), clusterid=integer(length=N) )
  
  # Create the full graph
  if ( nrow(CC) > 0 ) {
    tryCatch({
      grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)
    },error=function(cond) {
      print( nrow(CC) )
      return()
    })
    uniqueSource <- unique( CC$Tsource )
    uniqueTarget <- unique( CC$Ttarget )
    uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )
    
    # Process the sub-graphs
    # The variable "output" will be the data structure sent to the "databaseInsertBuffer",
    # which means the column names need to match the database fields.
    N <- length(uniqueTarget)
    # get the case
    case <- compArgs$findClass('metadataInformer')$get('case')

    N <- length(uniqueTarget)
    output <- data.frame( time=integer(length=N), incident=character(length=N), clusterid=integer(length=N) )
    subject <- compArgs$get('subject')
    channel <- compArgs$get('channel')
    clusterid <- 0
    seizureUsed <- as.numeric(compArgs$get('case')$centerTime)
    UUID <- compArgs$get('case')$UUID
    
    for ( idx in seq(1,N) ) {
      tryCatch({
        tCN <- as.numeric(uniqueTarget[idx])
#        if ( tCN == 1307429373897842 ) {
#          print( "hi")
#        }
        # Loop through the individual timestamps and find the subgraph
        sub_idx <- (as.numeric(igraph::V(grph)$name)>=(tCN-CW) & as.numeric(igraph::V(grph)$name)<=(tCN+CW))
        if ( length(which(sub_idx)) > 0 ) {
          sub_grph <- igraph::induced_subgraph(grph, sub_idx, impl="auto")
          #    	compute communities
          sub_cliques <- igraph::cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
          # cluster: _infomap, _walktrap, _fast_greedy
          sub_cliques_membership <- igraph::membership( sub_cliques )
          clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
          member_idx <- which( sub_cliques_membership == clusterid )
          grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
          rm( sub_grph )
          gc()
          
          # needed: incident, weights
          edz <- igraph::incident( grph_clique, topigraph::IDv(grph_clique, tCN) )
          enz <- igraph::ends( grph_clique, edz )
          weights <- edz$weight
          mat <- cbind( enz, weights )
          keep_idx <- which( as.numeric(mat[,2]) == tCN )
          str_incident <- paste0( mat[keep_idx,1], collapse=',' )
          str_weights <- paste0( round(as.numeric(mat[keep_idx,3]),4), collapse=',' )
          
          # database
          cc_idx <- which( CC$Ttarget == tCN )
          waveform_str <- unlist( CC$WVtarget[cc_idx[1]] )
          waveform <- as.numeric( unlist( stringr::str_split(waveform_str,',') ) )
          abs_waveform <- abs( waveform[cm] )
          peak_idx <- which( abs_waveform == max( abs_waveform ) )
          peakVal <- abs_waveform[ peak_idx[1] ] * sign(waveform[cm[peak_idx[1]]])
          energyVal <- sqrt( sum( abs_waveform * abs_waveform ) )
          result <- data.frame(subject=subject,channel=channel,time=tCN,waveform=waveform_str,clusterid=clusterid,seizureUsed=seizureUsed,peak=peakVal,energy=energyVal,incident=str_incident,weights=str_weights,UUID=UUID)
          dib$insert( result )

          # data to pass forward
          output$time[idx] <- tCN
          output$incident[idx] <- str_incident
          output$clusterid[idx] <- 0 # Creating the space to be used in the next function
          
        } else {
          print( "empty subgraph" )
          return()
        }
      },error=function(cond){
        print(cond)
        return()
      })
      
      
      # ALL OF THIS GOES AWAY
      # Build entry for the output dataframe
      #    output$clusterid[idx] <- clusterid # REMEMBER: This function doesn't assign clusterid's!!!!
    }
    
    rm( grph )
    gc()
  }
  return(output)
}

