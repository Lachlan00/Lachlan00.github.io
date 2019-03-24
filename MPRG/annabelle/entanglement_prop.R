yXX <- data.frame(Entanglement.Score=sample.int(3,size=200,replace=TRUE)) # make random data

plot.EntangProp <- function(df){
  barplot(height = table(factor(df$Entanglement.Score, levels=min(df$Entanglement.Score):max(df$Entanglement.Score)))/length(df$Entanglement.Score),
          ylab = "proportion",
          xlab = "values",
          main = "histogram of x (proportions)") 
}

plot.EntangProp(yXX)


fun.add5 <- function(x){
  output <- x + 5
  return(output)
}