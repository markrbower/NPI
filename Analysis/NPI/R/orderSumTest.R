orderSumTest <- function(N) {
  nbrs <- runif( N, 0, 1 )
  s1 <- 0
  for ( x in seq(from=1,to=N,by=1) ) {
    s1 <- s1 + nbrs[x]
  }
  s2 <- 0
  for ( x in seq(from=N,to=1,by=-1) ) {
    s2 <- s2 + nbrs[x]
  }
  
  return( s1 - s2 )
}
