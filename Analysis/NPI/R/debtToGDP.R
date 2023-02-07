debtToGDP <- function() {
  D <- 30.4
  G <- 24.4
  
  for ( year in 2023:2072 ) {
    D <- D * 1.07
    G <- G * 1.05
    print( paste0( year, ": ", D/G ) )
  }
  
}
