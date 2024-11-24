# determines correct value of young's modulus from a ym.result
# arg ym.result: the return value from ym.calculate
ym.determine <- function(ym.result) {
  slopes <- getSlopes(ym.result)
  strains <- getStrains(ym.result)
  scores <- getScores(ym.result)
  
  matching.pairs <- similarity(slopes = slopes, scores = scores)
  
  # 0 match
  if (is.null(matching.pairs[[1]])) {
    return(medianStrainSlope(slopes, strains, scores))
  }
  # 1 or all match
  else {
    return (minScoreSlope(slopes, strains, scores, unique(unlist(matching.pairs))))
  }
}

# finds the min slope of the method with the min score
# arg slopes: all slopes
# arg scores: all scores
# arg methods: valid methods to check
# returns slope of method with lowest score

# Using this means no pairs are matching.
medianStrainSlope <- function(slopes, strains, scores) {
  methods = c("max", "inflection", "fds")
  strains.unlist <- unlist(strains)
  strain.median <- median(strains.unlist)
  strain.median.index <- which(strains.unlist == strain.median)
  method.median <- methods[strain.median.index]
  
  return(
    list(
      method = methods[strain.median.index],
      slope = slopes[strain.median.index][[1]],
      strain = strain.median,
      score = scores[strain.median.index][[1]]
    )
  )
}

# Using this means there is 1 matching pair or 3 matching pairs 
minScoreSlope <- function(slopes, strains, scores, methods) {
  min.score.index <- which.min(unlist(scores[methods]))
  methods.slopes <- unlist(slopes[methods])
  return(
    list(
      method = methods[min.score.index],
      slope = methods.slopes[min.score.index][[1]],
      strain = strains[min.score.index][[1]],
      score = scores[min.score.index][[1]]
    )
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
# returns a list of lists of matching pairs
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

  # differences between all column pairs
  score.diffs <- outer(scores[1,], scores[1,], FUN = Vectorize(percentDifference))
  slope.diffs <- outer(slopes[1,], slopes[1,], FUN = Vectorize(percentDifference))
  # similar.mat is a 3x3 matrix whose intersections indicate whether the two methods have similar slopes and scores.
  similar.mat <- score.diffs <= threshold.percent & slope.diffs <= threshold.percent
  diag(similar.mat) <- FALSE # no self comparisons

  # return a list of lists where each inner list is the names of the matching pair
  upper.triangle <- which(similar.mat, arr.ind = TRUE)
  matching.indices <- upper.triangle[upper.triangle[, "row"] < upper.triangle[, "col"], ]
  pairs <- lapply(seq_len(nrow(upper.triangle)), function(row) {
    index <- upper.triangle[row, ]
    return(list(rownames(similar.mat)[index["row"]], colnames(similar.mat)[index["col"]]))
  })
  
  return(pairs[1:(sum(similar.mat) / 2)]) # divide by 2 because matches are x,y and y,x and we want only one of them
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

