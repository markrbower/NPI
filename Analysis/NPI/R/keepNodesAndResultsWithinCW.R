keepNodesAndResultsWithinCW <- function( nodes, CW ) {
  keepThreshold <- max( nodes$time ) - CW
  keepIdx <- which( nodes$time >= keepThreshold )
  prev_nodes <- nodes[keepIdx,]
  return( prev_nodes )  
}
