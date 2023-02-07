findGraphicalCommunities <- function( CW, CC, compArgs, dib, counter ) {
  # Runs the Leiden clustering algorithm
  #' @export
  
  # The structure of CC:
  library(Matrix)
  library(foreach)
  library(igraph)
  
  if ( is.null(CC) ) {
    cat( "findGraphicalCommunities: null CC\n" )
    return(NULL)
  }
  
  # One difference with the existing solution is that CC are already computed.
  # Therefore, do I need to store the waveform in the vertex attrributes?
  cm <- compArgs$get( 'computation_mask' )
  
  N <- 0
  output <- data.frame(counter=integer(length=N),time=integer(length=N), incident=character(length=N), clusterid=integer(length=N) )
  
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
    # get the case
    case <- compArgs$findClass('metadataInformer')$get('case')

    N <- length(uniqueTarget)
    output <- data.frame( time=integer(length=N), incident=character(length=N), clusterid=integer(length=N), waveform=character(length=N) )
    subject <- compArgs$get('subject')
    channel <- compArgs$get('channel')
    clusterid <- 0
    seizureUsed <- as.numeric(compArgs$get('case')$centerTime)
    UUID <- compArgs$get('case')$UUID
    
#    foreach( idx = seq(1,N) ) %dopar% {
    for ( idx in seq(1,N) ) {
      tryCatch({
#        cat( idx, " of ", N, "\n" )
        tCN <- as.numeric(uniqueTarget[idx])
        sub_idx <- as.numeric(igraph::V(grph)$name)>=(tCN-CW) & as.numeric(igraph::V(grph)$name)<=(tCN)
        if ( length(which(sub_idx)) > 1 ) { # The community is larger than just this node.
          sub_grph <- igraph::induced_subgraph(grph, sub_idx, impl="auto")
          #    	compute communities
#          sub_cliques <- igraph::cluster_louvain( sub_grph, weight=igraph::get.edge.attribute(sub_grph, 'weight' ) )
          r <- quantile( strength(sub_grph))[2] / (gorder(sub_grph) - 1)
          #r <- 1 / ( 2 * sub_grph.ecount() )
          sub_cliques <- igraph::cluster_leiden( sub_grph, resolution_parameter = r )
          sub_cliques_membership <- igraph::membership( sub_cliques )
          clusterid <- sub_cliques_membership[as.numeric(names(sub_cliques_membership))==tCN]
          member_idx <- which( sub_cliques_membership == clusterid )
          grph_clique <- igraph::induced_subgraph(sub_grph, member_idx )
            
          # needed: incident, weights
          edz <- igraph::incident( grph_clique, topigraph::IDv(grph_clique, tCN) )
          enz <- igraph::ends( grph_clique, edz )
          mat <- matrix( as.numeric(enz), ncol=2 )
#          if ( nrow(mat) < 5 ) {
#            print( mat )
#            print( "small" )
#          }
          weights <- as.numeric(edz$weight)
          for(i in seq_len(nrow(mat))) mat[i,] <- sort(mat[i,], method='quick')
          str_incident <- paste0( paste0( mat[,1], collapse=',' ) )
          str_weights <- paste0( paste0( round(weights,4), collapse=',' ) )
          
          # database
          cc_idx <- which( CC$Ttarget == tCN )
          waveform_str <- unlist( CC$WVtarget[cc_idx[1]] )
          waveform <- as.numeric( unlist( stringr::str_split(waveform_str,',') ) )
          abs_waveform <- abs( waveform[cm] )
          peak_idx <- which( abs_waveform == max( abs_waveform ) )
          peakVal <- abs_waveform[ peak_idx[1] ] * sign(waveform[cm[peak_idx[1]]])
          energyVal <- sqrt( sum( abs_waveform * abs_waveform ) )
          result <- data.frame(counter=counter,subject=subject,channel=channel,time=tCN,waveform=waveform_str,clusterid=0,seizureUsed=seizureUsed,peak=peakVal,energy=energyVal,incident=str_incident,weights=str_weights,UUID=UUID)
          dib$insert( result )
            
          # data to pass forward
          output$counter[idx] <- counter
          output$time[idx] <- tCN
          output$incident[idx] <- str_incident
          output$clusterid[idx] <- 0 # Creating the space to be used in the next function
          output$waveform[idx] <- waveform_str          
        }
      },error=function(cond){
        print( counter )
        print( subject )
        print( channel )
        print( tCN )
        print( waveform_str )
        print( clusterid )
        print( seizureUsed )
        print( peakVal )
        print( energyVal )
        print( str_incident )
        print( str_weights )
        print( UUID )
        print(cond)
        return()
      })
    }
    rm( grph )
    gc()
  }

  dib$flush()
  dib$initialize()
  return(output)
}

