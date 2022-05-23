meanbycol.tip <- function(colname){
  value <- unique(tips[, colname])
  result <- list()
  for(i in 1:length(value)) {
    idx <- which(tips[ ,colname] == value[i])
    result[i] <- mean(tips[idx, 'tip'])
  }
  names(result) <- value
  return(result)
}
