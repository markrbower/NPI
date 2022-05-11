script_kmeans_3clusters <- function(password,hostname,dbName) {
  hostname <- 'localhost'
  query <- paste0( "create table if not exists M (subject varchar(32),channel varchar(256),seizureUsed bigint,session varchar(128)," )
  query <- paste0( query, "label varchar(32),count int,clusterid int,waveform mediumtext,minT double,maxT double," )
  query <- paste0( query, "duration double,rate double,energy double,diameter double,edge_density double," )
  query <- paste0( query, "degree double,hub_score double,mean_distance double,transitivity double, " )
  query <- paste0( query, "eigenvalue double,betweenness double,closeness double, " )
  query <- paste0( query, "primary key(subject,channel,seizureUsed,clusterid));")
  conn <- DBI::dbConnect( RMySQL::MySQL(), user="root", password=password, host=hostname, dbName=dbName)   # for software test
  query <- 'select * from M;'
  rs <- DBI::dbGetQuery( conn, query )
  keepers <- c(7,13:16,18:22)
  rs_analysis <- rs[,keepers]
  rs_scale <- scale(rs_analysis[,2:10])
  rs_view <- cbind( clusterid=rs_analysis$clusterid, rs_scale )
  rs_view <- as.data.frame( rs_view )
  k3 <- kmeans( rs_view[2:10], centers=3, nstart=25)
  fviz_cluster( k3, data=rs_view, geom=c("text"), ellipse.type="euclid")
  border <- c(8,20,35)
  rs_view$clusterid[border]
  zero_cluster <- which( rs_view$clusterid == 0)
  zero_cluster
  rs_view[c(10,17,34),] # artifact
  rs_view[c(6,24,32),]  # signal,
  rs_view[c(2,29,42),]  # noise
  
  rs_view.pca <- prcomp(rs_view[,c(2:10)], center = TRUE,scale. = TRUE)
  summary(rs_view.pca)
  install.packages('ggbiplot')
  library(devtools)
  install_github("vqv/ggbiplot")
  library(ggbiplot)
  ggbiplot(rs_view.pca)
}

