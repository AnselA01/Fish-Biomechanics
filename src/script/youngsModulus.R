library(splines2)

# per-bone variables
name <- ""
r.squared <- NA

# constants
degrees.freedom <- 10
filter <- 0.2

# Calculates Young's Modulus for a bone using the three methods:
# 1. Global max slope
# 2. Slope at the inflection point
# 3. First local max of the first derivative spline fit
ym.calculate <- function(bone) {
  name <<- paste0(bone$Individual[[1]], tolower(bone$Segment[[1]]), bone$Trial[[1]]) # <<- escapes block scoping
  bone <- filter(bone, Strain < filter)
  
  spline.fit.result <- fitStressSpline(bone, fitFirstDeriv = FALSE)
  if (is.null(spline.fit.result)) return(NULL)
  
  firstSecondDerivResult <- calculateFirstSecondDerivatives(spline.fit.result$strainGrid$Strain, spline.fit.result$coefs)
  # ugly but we need a different spline and first and second derivatives for the fds method
  fds.spline.fit.result <- fitStressSpline(numericDifferentiation(bone), fitFirstDeriv = TRUE)
  fds.firstSecondDerivResult <- calculateFirstSecondDerivatives(fds.spline.fit.result$strainGrid$Strain, fds.spline.fit.result$coefs)
  
  return(
    c(
      max = globalMax(
        firstSecondDerivResult$first.deriv,
        firstSecondDerivResult$second.deriv,
        spline.fit.result$strainGrid$Strain
      ),
      inflection = inflectionPoint(
        firstSecondDerivResult$first.deriv,
        firstSecondDerivResult$second.deriv,
        spline.fit.result$strainGrid$Strain
      ),
      fds = localMax(
        fds.spline.fit.result$first.deriv.spline.fit,
        fds.firstSecondDerivResult$first.deriv,
        fds.spline.fit.result$strainGrid$Strain
      ),
      r.squared = r.squared
    )
  )
}

# creates a grid with Strain points spaced argument increment apart.
# arg increment: grid spacing
# returns tibble with gridded Strain column.
createGrid <- function(bone, increment = 0.0001) {
  start <- 0
  end <- max(bone$Strain)
  return(data.frame(Strain = seq(start, end, by = increment)))
}

# performs numeric differentiation on Stress and Strain values
# returns: original bone with two new columns: first.deriv and second.deriv
numericDifferentiation <- function(bone) {
  return(bone |>
    mutate(
      lag2Strain = lag(Strain, 2),
      first.deriv = (Stress - lag(Stress, 2)) / (Strain - lag2Strain),
      second.deriv = (first.deriv - lag(first.deriv, 2)) / (Strain - lag2Strain)
    ) |>
    filter(!is.na(first.deriv), is.finite(first.deriv)) |>
    dplyr::select(-lag2Strain))
}

# fit a model with an abstract formula.
# TODO CHANGE THIS TO MARS PACKAGE METHOD MAYBE
fitSpline <- function(formula, bone, strain) {
  result <- tryCatch(
    {
      model <- lm(formula, data = bone)
      r.squared <<- summary(model)$r.squared
      predictions <- predict(model, newdata = strain)
      return(list(model = model, predictions = predictions))
    },
    error = function(e) {
      return(NULL)
    }
  )
  return(result)
}

# fits stress and/or stress first derivative splines
# returns coefficients, strainGrid, and R-square
fitStressSpline <- function(bone, fitFirstDeriv = FALSE) {
  strainGrid <- createGrid(bone)
  
  # fit and predict stress spline
  stress.spline.fit <- fitSpline(formula = Stress ~ bSpline(Strain, df = degrees.freedom), bone, strainGrid)
  if (is.null(stress.spline.fit)) {
    message(name, ": Failed to fit spline to stress")
    return(NULL)
  }
  strainGrid$stress.spline.fit <- stress.spline.fit$predictions
  
  result <- list(coefs = coef(stress.spline.fit$model), strainGrid = strainGrid)
  
  # optionally fit and predict first derivative spline. this is used only by the FDS method.
  if (fitFirstDeriv) {
    deriv.spline.fit <- fitSpline(formula = first.deriv ~ bSpline(Strain, df = degrees.freedom), bone, strainGrid)
    if (is.null(deriv.spline.fit)) {
      message(name,  ": Failed to fit spline to first derivatives")
      return(NULL)
    }
    result$first.deriv.spline.fit <- deriv.spline.fit$predictions
    result$first.deriv.coefs <- coef(deriv.spline.fit$model)
    if (anyNA(result$first.deriv.coefs)) {
      message(name, ": Failed to fit all knots in first derivative spline fit")
      return(NULL)
    }
  }
  
  return(result)
}

# calculates the first and second derivatives of `strain` values using the coefficients from a spline fit.
# arg strain: strain values to calculate derivatives for
# arg coefficients: the coefficients of a spline fit to original Stress/Strain data
calculateFirstSecondDerivatives <- function(strain, coefficients) {
  first.deriv.basis.mat <- dbs(strain, df = degrees.freedom, derivs = 1)
  second.deriv.basis.mat <- dbs(strain, df = degrees.freedom, derivs = 2)
  
  # TODO pf15cp2 has two NA coefficients probably due to overfitting. Parse the warning message and refit with n-1 df?

  return(list(first.deriv = first.deriv.basis.mat %*% coefficients[-1], # coefficients[1] is the intercept
         second.deriv = second.deriv.basis.mat %*% coefficients[-1]))
}

inflectionIndex <- function(second.deriv) {
  if (anyNA(second.deriv)) {
    return(NULL)
  }
  for (i in 2:(length(second.deriv) - 1)) {
    if (sign(second.deriv[i]) != sign(second.deriv[i + 1])) { # TODO change this to sign + to - not from any difference.
      return(i)
    }
  }
  return (NULL)
}

# Slope Variability Index evaluates the variance of slopes +- 100 ticks (0.0001) away from the selected index.
# we theorize that a "good" young's modulus position has a low SVI
# arg index: starting index
# arg vals: values to calculate cv from from a grid spaced 0.0001.
# arg n: window size is 2 * n + 1
# returns: coefficient of variation for the window of 2*n+1 values 
SVI <- function(index, vals, n = 100) {
  start.index <- max(1, index - n) # cap start and end at index 1 and strain length
  end.index <- min(length(vals), index + n)
  window <- vals[start.index:end.index]
  
  # an alternative method is a linear model of the window. This would give us r-squared as our 
  # "score" as well as a slope to filter out false-positive negative and zero slopes.

  return(sd(window) / mean(window)) # coefficient of variation formula is sd/mean
}

# finds global max of arg 1 (first deriv)
# returns: first derivative value at the  global max and corresponding strain value and slope score
globalMax <- function(first.deriv, second.deriv, strain) {
  d1.max.index <- which.max(first.deriv)
  return(
    list(
      slope = first.deriv[d1.max.index],
      strain = strain[d1.max.index],
      name = name,
      score = SVI(d1.max.index, first.deriv),
      r.squared = r.squared
    )
  )
}

# finds the inflection point of arg 2 (second deriv)
# returns: first derivative value at the inflection point and corresponding strain value and slope score
inflectionPoint <- function(first.deriv, second.deriv, strain) {
  d2.inflection.index <- inflectionIndex(second.deriv)
  if (is.null(d2.inflection.index)) {
    message(name, ": Failed to find second derivative inflection point")
    return(NULL)
  }
  return(
    list(
      slope = first.deriv[[d2.inflection.index]],
      strain = strain[[d2.inflection.index]],
      name = name,
      score = SVI(d2.inflection.index, first.deriv),
      r.squared = r.squared
    )
  )
}


# finds first maximum of first derivative spline fit
# returns: first derivative at first derivative local max and corresponding strain value and slope score
# TODO Filter first ~10 values to skip the first small peak
localMax <- function(first.deriv.spline.fit, first.deriv, strain) {
  d1.localMax.index <- min(which.min(diff(first.deriv.spline.fit) > 0))
  return(
    list(
      slope = first.deriv[d1.localMax.index],
      strain = strain[d1.localMax.index],
      name = name,
      score = SVI(d1.localMax.index, first.deriv),
      r.squared = r.squared
    )
  )
}
