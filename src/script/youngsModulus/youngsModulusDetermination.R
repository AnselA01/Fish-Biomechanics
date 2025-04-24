source("./src/script/youngsModulus/youngsModulus.R")
source("./src/script/helpers/general.R")

global.methods.all <- c("fds", "inflection", "max")
global.similar.threshold.percent <- 5
global.strain.similar.threshold.percent <- 5

# inconclusive thresholds
global.inconclusive.slope.threshold <- 20
global.inconclusive.score.threshold <- 0.65

global.name <- ""

# wrapper around ym.calculate and ym.determine to perform calculation and determination in one step.
# arg: bone
# returns: list of calculation result and choice
ym.calculateAndDetermine <- function(bone) {
  global.name <<- getName(bone)
  message(paste0("\033[37m", global.name, " calculating Young's modulus\033[0m"))
  results <- ym.calculate(bone)
  if (is.null(results)) {
    message(global.name, " could not calculate young's modulus")
    return(NULL)
  }
  return(list(results = results, choice = ym.determine(results, bone)))
}

# determines correct value of young's modulus from a ym.result
# arg ym.result: the return value from ym.calculate
# returns: method name, slope, strain, and score of determined method
ym.determine <- function(ym.result, bone) {
  ym.result.values <- extractYmResultValues(ym.result)
  result.slopes <- ym.result.values$slopes
  result.strains <- ym.result.values$strains
  result.scores <- ym.result.values$scores
  
  matching.pairs <- similarity(slopes = result.slopes, strains = result.strains, scores = result.scores)
  nearby.pairs <- nearby(strains = result.strains, maxStrain = max(bone$Strain))

  # if there are no matching pairs take the method with the median strain. If there is a matching pair(s) take the method with the minimum score.
  decision <- if (is.null(matching.pairs)) medianStrainSlope(result.slopes, result.strains, result.scores)
            else minScoreSlope(result.slopes, result.strains, result.scores, matching.pairs)
  decision$inconclusive <- inconclusiveDecision(decision)
  
  if (length(nearby.pairs == 2)) {
    if (decision$method == "max") { # ok
      return(decision)
    }
    # check if the match is inflection and fds AND both their scores are higher than max
    if (all(c("fds", "inflection") %in% nearby.pairs) && !("max" %in% nearby.pairs)) {
      max_score.threshold <- result.scores$max * 0.75 # the max score must not be too much higher
      if (result.scores$fds > max_score.threshold && result.scores$inflection > max_score.threshold) {
        return(
          list(
            name = global.name,
            methods = c("max"),
            calculation = "max",
            method = "max",
            slope = result.slopes$max,
            strain = result.strains$max,
            score = result.scores$max,
            inconclusive = result.scores$max > global.inconclusive.score.threshold
          )
        )
      }
    }    
  }
  
  return(decision)
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
  method.median <- names(strains.unlist[strain.median.index])[[1]]
  

  return(
    list(
      name = global.name,
      methods = global.methods.all,
      calculation = "median strain",
      method = method.median,
      slope = slopes[method.median],
      strain = strains[method.median],
      score = scores[method.median]
    )
  )
}

# method with the minimum score
# Using this means there is 1 matching pair or 3 matching pairs 
# arg slopes: all slopes
# arg scores: all scores
# arg chosenMethods: chosen methods
# returns slope of method with lowest score
minScoreSlope <- function(slopes, strains, scores, chosenMethods) {
  # check for one under 1 and two over.
  # scores.under.one <- numUnderOneScore(scores = result.scores)
  # if (length(scores.under.one) == 1) {
  #   return(minScore(result.slopes, result.strain, result.scores))
  # }
  
  min.score.index <- which.min(unlist(scores[chosenMethods]))
  method.min = names(scores[chosenMethods][min.score.index])[[1]]

  methods.slopes <- unlist(slopes[chosenMethods])
  return(
    list(
      name = global.name,
      methods = chosenMethods,
      calculation = "minimum score",
      method = method.min,
      slope = slopes[method.min],
      strain = strains[method.min],
      score = scores[method.min]
    )
  )
}

percentDifference <- function(x, y) {
  return(round(abs(x - y) / pmax(x, y) * 100, 1))
}

# determines nearby methods
# returns a list of lists of matching pairs
nearby <- function(strains, maxStrain) {
  strains <- matrix(c(strains$fds, strains$inflection, strains$max), ncol = 3, byrow = TRUE)

  # differences between all column pairs
  strain.percents <- mapply(function(strain) (strain / maxStrain) * 100, strains)
  strain.percent.diffs <- abs(outer(strain.percents, strain.percents, FUN = `-`))

  # similar.mat is a 3x3 matrix whose intersections indicate whether the two methods have similar slopes and scores.
  similar.mat <- strain.percent.diffs <= global.strain.similar.threshold.percent
  colnames(similar.mat) <- global.methods.all
  rownames(similar.mat) <- global.methods.all
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

# determines which methods have similar slopes and scores. Prioritizes exact matches even if all pairs match.
# Similarity is defined by a <= 5% percent difference between two values from the larger value.
# returns a list of lists of matching pairs
similarity <- function(slopes, strains, scores) {
  strains <- matrix(c(strains$max, strains$inflection, strains$fds), ncol = 3, byrow = TRUE)
  colnames(strains) <- global.methods.all
  slopes <- matrix(c(slopes$max, slopes$inflection, slopes$fds), ncol = 3, byrow = TRUE)
  colnames(slopes) <- global.methods.all
  scores <- matrix(c(scores$max, scores$inflection, scores$fds), ncol = 3, byrow = TRUE)
  colnames(scores) <- global.methods.all

  # differences between all column pairs
  strain.diffs <- outer(strains[1,], strains[1,], FUN = Vectorize(percentDifference))
  score.diffs <- outer(scores[1,], scores[1,], FUN = Vectorize(percentDifference))
  slope.diffs <- outer(slopes[1,], slopes[1,], FUN = Vectorize(percentDifference))
  
  # similar.mat is a 3x3 matrix whose intersections indicate whether the two methods have similar slopes and scores.
  similar.mat <- (score.diffs <= global.similar.threshold.percent) & (slope.diffs <= global.similar.threshold.percent)
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
        fds = ym.result$fds.slope,
        max = ym.result$max.slope,
        inflection = ym.result$inflection.slope
      ),
      strains = list(
        fds = ym.result$fds.strain,
        max = ym.result$max.strain,
        inflection = ym.result$inflection.strain
      ),
      scores = list(
        fds = ym.result$fds.score,
        max = ym.result$max.score,
        inflection = ym.result$inflection.score
      )
    )
  )
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

# determines whether the results from three methods indicates a need for user intervention in the decision making process
# conditions for inconclusiveness are:
# 1. The slope for the chosen method is less than 20.
# 2. Or the score for the chosen method is > 2. This cutoff is generous. Consider 1 or 1.5. Also consider a better score using the residuals of a linear model
# since we will be using a line to find yield strength anyway.
# * arg result: a ym calculation results
# * returns boolean indicating if our method decision is inconclusive. 
inconclusiveDecision <- function(decision) {
  return(decision$slope < global.inconclusive.slope.threshold || decision$score > global.inconclusive.score.threshold)
}



