fgc_mini <- function( CC, CW, compArgs ) {
#  load(file='findGraphicalCommunities.RData')
  tryCatch({
    grph <- igraph::graph.data.frame(CC[c('Tsource','Ttarget','weight')], directed=FALSE)
  },error=function(cond) {
    print( nrow(CC) )
    return()
  })
  uniqueSource <- unique( CC$Tsource )
  uniqueTarget <- unique( CC$Ttarget )
  uniqueTimes <- sort( union( uniqueSource, uniqueTarget ) )
  
  N <- length(uniqueTarget)
  
  for ( idx in seq(1,N) ) {
    tCN <- as.numeric(uniqueTarget[idx])
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
    }
  }
}
  
