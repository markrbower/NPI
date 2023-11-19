H <- function( pct ) {
  h  <- -pct * log2( pct );
  if ( is.nan(h) ) {
    h = 0.0;
  }
  return(h);
}

