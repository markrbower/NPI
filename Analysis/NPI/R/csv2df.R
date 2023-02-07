csv2df <- function() {
  
  # Get the user input
  txt <- readLines()
  
  # Parse the labels
  N <- length(txt)
  labels = unlist(strsplit(gsub("[^[:alnum:] ]","",txt[1]), " "))
  df = data.frame(matrix(nrow = 0, ncol = length(labels)))
  
  # Parse the values
  for ( i in 2:N ) {
    values = as.numeric(unlist(strsplit(gsub("[^[:alnum:],.]","",txt[i]), ",")))
    df <- rbind( df, values )    
  }
  
  # Add the labels
  colnames(df) <- labels
  return(df)
  
  # To plot
  # 
  # myColors <- brewer.pal(4,"Set1")
  # names(myColors) <- levels(df$cw)
  # colScale <- scale_colour_manual(name = "cw",values = myColors)
  # p <- ggplot(df,aes(blackout,sys,colour = cw)) + geom_point()
  # p1 <- p + colScale
  # p1
}


