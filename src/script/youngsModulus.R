library(splines2)

source("./src/script/helpers/general.R")

# per-bone variables
name <- ""
r.squared <- NA

# constants
global.degrees.freedom <- 10
global.strain.filter <- 0.2
global.grid.interval <- 0.0001

# Calculates Young's Modulus for a bone using three methods:
# 1. Global max slope
# 2. Slope at the inflection point
# 3. First local max of the first derivative spline fit
ym.calculate <- function(bone) {
  name <<- getName(bone)
  bone <- filter(bone, Strain < global.strain.filter)
  
  spline.fit.result <- fitStressSpline(bone, fitFirstDeriv = FALSE)
  if (is.null(spline.fit.result)) return(NULL)
  
  firstSecondDerivResult <- calculateFirstSecondDerivatives(spline.fit.result$strainGrid$Strain, spline.fit.result$coefs)
  # not great but we need a different spline fit and first and second derivatives for the fds method
  fds.spline.fit.result <- fitStressSpline(numericDifferentiation(bone), fitFirstDeriv = TRUE)
  if (is.null(fds.spline.fit.result)) return(NULL)
  fds.firstSecondDerivResult <- calculateFirstSecondDerivatives(fds.spline.fit.result$strainGrid$Strain, fds.spline.fit.result$coefs)
  
  return(
    c(
      max = globalMax(
        firstSecondDerivResult$first.deriv,
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
      name = name,
      r.squared = r.squared
    )
  )
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
      predictions <- suppressWarnings(predict(model, newdata = strain)) # what warnings?
      return(list(model = model, predictions = predictions))
    },
    error = function(e) {
      return(NULL)
    }
  )
  return(result)
}

# creates a grid with Strain points spaced argument increment apart.
# arg interval: grid spacing
# returns tibble with one gridded Strain column.
createGrid <- function(bone, interval = global.grid.interval) {
  return(data.frame(Strain = seq(from = 0, to = max(bone$Strain), by = interval)))
}

# fits stress and/or stress first derivative splines
# returns coefficients, strainGrid, and r-squared
fitStressSpline <- function(bone, fitFirstDeriv = FALSE) {
  strainGrid <- createGrid(bone)
  
  # fit and predict stress spline
  stress.spline.fit <- fitSpline(formula = Stress ~ bSpline(Strain, df = global.degrees.freedom), bone, strainGrid)
  if (is.null(stress.spline.fit)) {
    message(name, ": Failed to fit spline to stress")
    return(NULL)
  }
  strainGrid$stress.spline.fit <- stress.spline.fit$predictions
  
  result <- list(coefs = coef(stress.spline.fit$model),
                 strainGrid = strainGrid)
  
  # optionally fit and predict first derivative spline. this is used only by the FDS method.
  if (fitFirstDeriv) {
    deriv.spline.fit <- fitSpline(formula = first.deriv ~ bSpline(Strain, df = global.degrees.freedom), bone, strainGrid)
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
  first.deriv.basis.mat <- dbs(strain, df = global.degrees.freedom, derivs = 1)
  second.deriv.basis.mat <- dbs(strain, df = global.degrees.freedom, derivs = 2)
  
  return(list(first.deriv = first.deriv.basis.mat %*% coefficients[-1], # coefficients[1] is the intercept
         second.deriv = second.deriv.basis.mat %*% coefficients[-1]))
}

# Slope Variability Index evaluates the variance of slopes +- 200 ticks (0.0001) away from the selected index.
# we theorize that a "good" young's modulus position has an SVI near 0 which indicates low regional variance.
# arg first.deriv: values which to calculate SVI from a grid with intervals 0.0001.
# arg n: window size is 2 * n + 1 NOTE may be lower if you reach the bounds of the first.deriv vector
# returns: absolute value of a modified coefficient of variation score that uses the first derivative value at the selected
# index as the "anchor" to compare the deviation to. 
SVI <- function(index, first.deriv, n = 200) {
  selected.first.deriv <- first.deriv[index]
  start.index <- max(1, index - n) # cap start and end at index 1 and strain length
  end.index <- min(length(first.deriv), index + n)
  window <- first.deriv[start.index:end.index]
  
  # Since we can not guarantee that the first derivative values in the window 
  # have a constant slope (really a symmetric distribution), using the usual standard deviation formula is not ok.
  # Our modified deviation measure uses the distance from the selected first derivative value instead of the distance from the window mean. 
  # This modified variation method is analogous to the error of a regression model.
  deviation <- sqrt(mean((first.deriv - selected.first.deriv)^2, na.rm = TRUE))
  return(abs(deviation / selected.first.deriv))
}

# finds global max of first derivatives
# returns: first derivative value at the  global max and corresponding strain value and slope score
globalMax <- function(first.deriv, strain) {
  d1.max.index <- which.max(first.deriv)
  return(
    list(
      slope = first.deriv[d1.max.index],
      strain = strain[d1.max.index],
      score = SVI(d1.max.index, first.deriv)
    )
  )
}

# finds the inflection point of arg 2 (second deriv)
# returns: first derivative value at the inflection point and corresponding strain value and slope score
inflectionPoint <- function(first.deriv, second.deriv, strain) {
  inflectionIndex <- function(second.deriv) {
    if (anyNA(second.deriv)) {
      return(NULL)
    }
    for (i in 2:(length(second.deriv) - 1)) {
      if (sign(second.deriv[i]) > sign(second.deriv[i + 1])) {
        return(i)
      }
    }
    return (NULL)
  }
  
  d2.inflection.index <- inflectionIndex(second.deriv)
  if (is.null(d2.inflection.index)) {
    message(name, ": Failed to find second derivative inflection point")
    return(NULL)
  }
  return(
    list(
      slope = first.deriv[[d2.inflection.index]],
      strain = strain[[d2.inflection.index]],
      score = SVI(d2.inflection.index, first.deriv)
    )
  )
}


# finds first maximum of the first derivative spline fit
# returns: first derivative at first derivative local max and corresponding strain value and slope score
localMax <- function(first.deriv.spline.fit, first.deriv, strain) {
  localMaxIndex <- function(first.deriv.spline.fit) {
    start.index <- 100
    
    first.derivatives.differences <- diff(first.deriv.spline.fit[start.index:length(first.deriv.spline.fit)])
    local.max.index <- which(first.derivatives.differences[-1] <= 0 & first.derivatives.differences[-length(first.derivatives.differences)] > 0)
    
    return(local.max.index[1] + start.index)
  }
  
  d1.localMax.index <- localMaxIndex(first.deriv.spline.fit)
  return(
    list(
      slope = first.deriv[d1.localMax.index],
      strain = strain[d1.localMax.index],
      score = SVI(d1.localMax.index, first.deriv)
    )
  )
}
