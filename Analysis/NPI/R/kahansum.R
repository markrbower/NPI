# https://gist.github.com/jmcastagnetto/7b0b5f47d97a7027f14c

kahansum <- function(x) {
  ks <- 0
  c <- 0
  for ( i in seq(1,x) ) {
    y <- 1.1 - c
    kt <- ks + y
    c = (kt - ks) - y
    ks = kt
  }
  ks
}
