setOptions <- function() {
  options( scipen = 999 )
  options(warn=-1)
  options(stringsAsFactors = FALSE);
  options(future.globals.maxSize= 1073741824)
  topconnect::clearAllDBcons()
}

