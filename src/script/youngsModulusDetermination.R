source("./src/script/youngsModulus.R")
source("./src/script/helpers/general.R")

global.methods.all <- c("max", "inflection", "fds")
global.similar.threshold.percent <- 30
global.similar.strain.threshold <- 0.015

global.name <- ""

# wrapper around ym.calculate and ym.determine to perform calculation and determination in one step.
# arg: bone
# returns: list of calculation result and choice
ym.calculateAndDetermine <- function(bone) {
  results <- ym.calculate(bone)
  if (is.null(results)) {
    message(getName(bone), ": could not calculate young's modulus")
    return(NULL)
  }
  return(list(results = results, choice = ym.determine(results)))
}

# determines correct value of young's modulus from a ym.result
# arg ym.result: the return value from ym.calculate
# returns: method name, slope, strain, and score of determined method
ym.determine <- function(ym.result) {
  global.name <<- ym.result$name
  
  ym.result.values <- extractYmResultValues(ym.result)
  result.slopes <- ym.result.values$slopes
  result.strains <- ym.result.values$strains
  result.scores <- ym.result.values$scores
  
  matching.pairs <- similarity(slopes = result.slopes, strains = result.strains, scores = result.scores)

  # 0 pairs match
  if (is.null(matching.pairs)) {
    return(medianStrainSlope(result.slopes, result.strains, result.scores))
  }
  # 1 or 3 pairs match
  else {
    return(maxSlope(result.slopes, result.strains, result.scores, matching.pairs))
  }
}

# method with the median strain
# Using this means there are no matching pairs
# arg slopes: all slopes
# arg scores: all scores
# arg methods: valid methods to check

# Using this means no pairs are matching.
medianStrainSlope <- function(slopes, strains, scores) {
  strains.unlist <- unlist(strains)
  strain.median <- median(strains.unlist)
  strain.median.index <- which(strains.unlist == strain.median)
  method.median <- global.methods.all[strain.median.index]
  
  return(
    list(
      name = global.name,
      methods = global.methods.all,
      calculation = "median strain",
      method = global.methods.all[strain.median.index][[1]],
      slope = slopes[strain.median.index][[1]],
      strain = strain.median,
      score = scores[strain.median.index][[1]]
    )
  )
}

# method with the minimum score
# Using this means there is 1 matching pair or 3 matching pairs 
# arg slopes: all slopes
# arg scores: all scores
# arg chosenMethods: chosen methods
# returns slope of method with lowest score
maxSlope <- function(slopes, strains, scores, chosenMethods) {
  max.slope.index <- which.max(unlist(slopes[chosenMethods]))
  methods.slopes <- unlist(slopes[chosenMethods])
  return(
    list(
      name = global.name,
      methods = chosenMethods,
      calculation = "maximum slope",
      method = chosenMethods[max.slope.index][[1]],
      slope = methods.slopes[max.slope.index][[1]],
      strain = strains[max.slope.index][[1]],
      score = scores[max.slope.index][[1]]
    )
  )
}

# method with the max slopes
# Using this means there is 1 matching pair or 3 matching pairs 
# arg slopes: all slopes
# arg scores: all scores
# arg chosenMethods: chosen methods
# returns slope of method with lowest score
minScoreSlope <- function(slopes, strains, scores, chosenMethods) {
  min.score.index <- which.min(scores[chosenMethods])
  methods.slopes <- unlist(slopes[chosenMethods])
  return(
    list(
      name = global.name,
      methods = chosenMethods,
      calculation = "minimum score",
      method = chosenMethods[min.score.index][[1]],
      slope = methods.slopes[min.score.index][[1]],
      strain = strains[min.score.index][[1]],
      score = scores[min.score.index][[1]]
    )
  )
}

# floor to an integer. helpful for turning negligible differences to 0.
percentDifference <- function(x, y) {
  return(floor(abs(x - y) / pmax(x, y) * 100))
}

# returns matrix of pairwise differences using fun 
pairwiseDifferences <- function(values, fun, absolute = FALSE) {
  if (absolute) {
    return(outer(values[1,], values[1,], FUN = function(a, b) abs(a - b)))
  }
  return(outer(values[1,], values[1,], FUN = fun))
}

findEqualStrain <- function(strain.diffs) {
  equal.mat <- !strain.diffs
  diag(equal.mat) <- FALSE
  # there is at least one equal pair. it doesn't matter which one we return.
  if (sum(equal.mat) > 0) {
    index <- which(equal.mat == 1 & upper.tri(equal.mat), arr.ind = TRUE) # upper.tri for unique matches (not x,y and y,x)
    return(unlist(list(rownames(equal.mat)[index[, 1]], colnames(equal.mat)[index[, 2]])))
  }
  return(NULL)
}

# determines which methods have similar slopes and scores. Prioritizes exact matches even if all pairs match.
# Similarity is defined by a <= 10% percent difference between two values from the larger value.
# returns a list of lists of matching pairs
similarity <- function(slopes, strains, scores) {
  strains <- matrix(c(strains$max, strains$inflection, strains$fds), ncol = 3, byrow = TRUE)
  colnames(strains) <- global.methods.all
  slopes <- matrix(c(slopes$max, slopes$inflection, slopes$fds), ncol = 3, byrow = TRUE)
  colnames(slopes) <- global.methods.all
  scores <- matrix(c(scores$max, scores$inflection, scores$fds), ncol = 3, byrow = TRUE)
  colnames(scores) <- global.methods.all

  strain.diffs <- pairwiseDifferences(strains, fun = "-", absolute = TRUE)
  
  # first check for exact strain matches
  equalStrain <- findEqualStrain(strain.diffs)
  if (!is.null(equalStrain)) {
    return(equalStrain)
  }
  
  score.diffs <- pairwiseDifferences(scores, fun = percentDifference)
  slope.diffs <- pairwiseDifferences(slopes, fun = percentDifference)
  
  # similar.mat is a 3x3 matrix whose intersections indicate whether the two methods have similar (within 0.015) strains
  similar.mat <- (strain.diffs <= global.similar.strain.threshold)
  
  diag(similar.mat) <- FALSE # no self comparisons

  # return a list of lists where each inner list is the names of the matching pair
  upper.triangle <- which(similar.mat, arr.ind = TRUE)
  matching.indices <- upper.triangle[upper.triangle[, "row"] < upper.triangle[, "col"], ]
  pairs <- lapply(seq_len(nrow(upper.triangle)), function(row) {
    index <- upper.triangle[row, ]
    return(list(rownames(similar.mat)[index["row"]], colnames(similar.mat)[index["col"]]))
  })
  
  return(unique(unlist(pairs[1:(sum(similar.mat) / 2)]))) # divide by 2 because matches are x,y and y,x and we want only one of them
}

# extracts values from youngs modulus calculation result
extractYmResultValues <- function(ym.result) {
  return(
    list(
      slopes = list(
        max = ym.result$max.slope,
        inflection = ym.result$inflection.slope,
        fds = ym.result$fds.slope
      ),
      strains = list(
        max = ym.result$max.strain,
        inflection = ym.result$inflection.strain,
        fds = ym.result$fds.strain
      ),
      scores = list(
        max = ym.result$max.score,
        inflection = ym.result$inflection.score,
        fds = ym.result$fds.score
      )
    )
  )
}
