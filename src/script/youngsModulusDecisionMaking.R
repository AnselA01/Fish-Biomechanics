# determines correct value of young's modulus
# arg ym.result: the return value from ym.calculate
determine <- function(ym.result) {
  similar <- similarity(slopes = getSlopes(ym.result), scores = getScores(ym.result))
  return(similar)

  # here is the decision logic
  # we start with the number of methods that have similar scores AND respectively similar slopes.
  switch(similarity$number,
    "0" = {
      
    },
    
    # 1 is similar 
    "1" = {
      
    },
    
    # all are similar
    "3" = {
      
    }
  )
}

# extract slopes from ym.result
getSlopes <- function(ym.result) {
  return(
    list(
      max = ym.result$max.slope,
      inflection = ym.result$inflection.slope,
      fds = ym.result$fds.slope
    )
  )
}

# extract scores from ym.result
getScores <- function(ym.result) {
  return(
    list(
      max = ym.result$max.score,
      inflection = ym.result$inflection.score,
      fds = ym.result$fds.score
    )
  )
}

# extract strains from ym.result
getStrains <- function(ym.result) {
  return(
    list(
      max = ym.result$max.strain,
      inflection = ym.result$inflection.strain,
      fds = ym.result$fds.strain
    )
  )
}



# determines which methods have respectively similar slopes and scores.
# returns a boolean list of who is similar in both slope and score
similarity <- function(slopes, scores) {
  percentDifference <- function(x, y) {
    return(abs(x - y) / pmax(x, y) * 100)
  }
  
  threshold.percent <- 10
  method.names <- c("max", "inflection", "fds")
  
  slopes <- matrix(c(slopes$max, slopes$inflection, slopes$fds), ncol = 3, byrow = TRUE)
  colnames(slopes) <- method.names
  scores <- matrix(c(scores$max, scores$inflection, scores$fds), ncol = 3, byrow = TRUE)
  colnames(scores) <- method.names
  
  # differences between all columns
  score.diffs <- outer(scores[1,], scores[1,], FUN = Vectorize(percentDifference))
  slope.diffs <- outer(slopes[1,], slopes[1,], FUN = Vectorize(percentDifference))
  # similar.mat is a 3x3 matrix whose intersections indicate whether the two methods have similar slopes and scores.
  similar.mat <- score.diffs <= threshold.percent & slope.diffs <= threshold.percent
  
  # there are three possible values of number: 0, 1, and 3. 0 means none, 1 means one pair, 3 means all
  # 2 is impossible because if two unique pairs are similar then their intersection must also be similar.
  diag(similar.mat) <- FALSE # no self comparisons
  number <- sum(similar.mat) / 2 # / 2 because each is in the matrix twice
  

  return(list(
    max_inflection = similar.mat[1, 2], 
    max_fds = similar.mat[1, 3], 
    inflection_fds = similar.mat[2, 3],
    number = number
  ))
}

# determines if you are "too close" to 0 strain. Too close is within ____? the distance should be numeric because the strain scales are the same
# returns a boolean list of who is close to 0 strain and the number of close methods
strainDistance <- function(strains) {
  threshold <- 0
  
  
  
  close.max <- distamce.max <= threshold
  close.inflection <- distamce.inflection <= threshold
  close.fds <- distamce.fds <= threshold
  number <- sum(close.max, close.inflection, close.fds)
  
  return (max = close.max, 
          inflection = close.inflection, 
          fds = close.fds,
          number = number
  )
}

