init_here <- function() {
  `%!in%` = Negate(`%in%`)
  files <- dir( all.files = T )
  while ( ".here" %!in% files ) {
    setwd("..")
    getwd()
    files <- dir( all.files = T )
  }
  library(here)
}
