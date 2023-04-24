getNVsubjectFromFilename <- function( filename ) {
  str_extract( filename, '[0-9A-Za-z]+_[0-9A-Za-z]+_[0-9A-Za-z]+' )
}
