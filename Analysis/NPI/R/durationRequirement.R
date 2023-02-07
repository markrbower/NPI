durationRequirement <- function( conn, epochTableName, subjectID, minuteDuration ) {
  library( dplyr )
  library( data.table )
  
  # 'minuteDuration' is in seconds
  minuteDuration <- minuteDuration * 1e6
  
  query <- paste0( 'select name from subjectNamesIDs where id=', subjectID, ';' )
  rs <- dbGetQuery( conn, query )
  subjectName <- rs$name
  
  # Find all time durations
  query <- paste0( 'select * from epochs where subject=\'', subjectName, '\';' )
  data <- dbGetQuery( conn, query )
  data[data=='N1'] <- 'N'
  data[data=='N2'] <- 'N'
  data[data=='N3'] <- 'N'
  data <- data %>% group_by(grp = rleid(label), label) %>% summarise(alpha = min(alpha), omega = max(omega))
    
  # Find those that exceed the minimum
  durations <- data$omega - data$alpha
  validIdx <- which( durations >= minuteDuration )
  return( list(data=data,validIdx=validIdx) )  
}

