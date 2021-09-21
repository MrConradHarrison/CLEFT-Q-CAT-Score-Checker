positionLabelx <- function(a) {
  if (a > 85) {
    x <- -5
  } else {
    x <- 5
  }
  return(x)
}



getPercent <- function(score_distribution, score) {
  getPercentile <- ecdf(score_distribution$Score)
  percentile <- getPercentile(score) * 100
  return(percentile)
}
