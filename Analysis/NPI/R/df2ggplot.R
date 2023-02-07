df2ggplot <- function( df, xfield=NULL ) {
  library(RColorBrewer)
  
  labels <- names(df)
  N <- length(labels)
  
  if ( is.null(xfield) ) {
    df <- data.frame(x=seq_along(df[,1]),df)
    df_m <- melt(df,id.vars='x')
  } else {
    df_m <- melt(df,id.vars=xfield)
    N <- N-1
  }
  
  myColors <- brewer.pal( max(N,3),"Set1")
  if ( N==2 ) {
    myColors <- myColors[c(1,3)]
  } else if ( N==1 ) {
    myColors <- myColors[c(2)]
  }

    names(myColors) <- levels(df_m$variable)
  colScale <- scale_colour_manual(name = "time",values = myColors)
  p <- ggplot(df_m,aes(power,value,colour = variable)) + geom_line()
  p1 <- p + colScale
}
