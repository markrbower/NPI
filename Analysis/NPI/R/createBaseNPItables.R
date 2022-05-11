createBaseNPItables <- function( ... ) {
  #' @export
  
  library( DBI )
  library( RMySQL )
  
  args <- list(...)
  print( args )
  hostname <- NPI:::parseArg( args, 'hostname' )
  if ( length(hostname) == 0 ) {
    hostname <- 'localhost'
  }
  password <- NPI:::parseArg( args, 'password' )
  if ( length(password) == 0 ) {
    password <- ''
  }
  dbName <- NPI:::parseArg( args, 'dbName' )
  if ( length(dbName) == 0 ) {
    dbName <- 'mysql'
  }
  
  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=password, host=hostname, dbName=dbName)   # for software test
  query <- paste0( "use ", dbName, ";")
  DBI::dbGetQuery( conn, query )
  
  # tasks
  query <- paste0( "create table if not exists tasks (username varchar(128), institution varchar(64), lab varchar(32)," )
  query <- paste0( query, "nodename varchar(128),experiment varchar(64)," )
  query <- paste0( query, "subject varchar(32), path varchar(256), service varchar(128),taskname varchar(128)," )
  query <- paste0( query, "signaltype varchar(32), iterationtype varchar(32), centerTime bigint," )
  query <- paste0( query, "analysisStart bigint, analysisStop bigint, parameters text," )
  query <- paste0( query, "UUID varchar(36), done boolean, created timestamp default current_timestamp," )
  query <- paste0( query, "modified timestamp default current_timestamp on update current_timestamp," )
  query <- paste0( query, "primary key(subject,experiment,taskname,centerTime,UUID));")
  DBI::dbGetQuery( conn, query )

  # epochs
  query <- paste0( "create table if not exists epochs ( subject varchar(256), session varchar(256),start bigint(20) unsigned, " )
  query <- paste0( query, "stop bigint(20) unsigned, label varchar(256), primary key(subject,session,start));" )
  DBI::dbGetQuery( conn, query )
  
  # progress
  query <- paste0( "create table if not exists progress (subject varchar(32), channel varchar(256), session varchar(128)," )
  query <- paste0( query, "done boolean, timestamp bigint,primary key(subject,channel,timestamp));" )
  DBI::dbGetQuery( conn, query )
  
  # P
  query <- paste0( "create table if not exists P (counter int,subject varchar(32),channel varchar(32),seizureUsed bigint," )
  query <- paste0( query, "time bigint,waveform mediumtext,peak double,energy double,incident mediumtext,weights mediumtext," )
  query <- paste0( query, "clusterid int,UUID varchar(36),created_on DATETIME DEFAULT CURRENT_TIMESTAMP, ")
  query <- paste0( query, "primary key(subject,channel,seizureUsed,time,UUID));")
  DBI::dbGetQuery( conn, query )

  # M
  query <- paste0( "create table if not exists M (subject varchar(32),channel varchar(256),seizureUsed bigint,session varchar(128)," )
  query <- paste0( query, "label varchar(32),count int,clusterid int,waveform mediumtext,minT double,maxT double," )
  query <- paste0( query, "duration double,rate double,energy double,diameter double,edge_density double," )
  query <- paste0( query, "degree double,hub_score double,mean_distance double,transitivity double, " )
  query <- paste0( query, "eigenvalue double,betweenness double,closeness double, " )
  query <- paste0( query, "primary key(subject,channel,seizureUsed,clusterid));")
  DBI::dbGetQuery( conn, query )
  
  # T
  query <- paste0( "create table if not exists T (subject varchar(32),channel varchar(256),seizureUsed bigint, " )
  query <- paste0( query, "session varchar(128), waveform mediumtext, time bigint, clusterid int, behavior varchar(32), " )
  query <- paste0( query, "primary key(subject,channel,seizureUsed,session,time));" )
  DBI::dbGetQuery( conn, query )
  
  DBI::dbDisconnect( conn )
}
