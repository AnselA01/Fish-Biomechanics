library(splines2)

name <- ""

# Calculates Young's Modulus for a bone with one of three methods:
# 1. Global max slope ("max")
# 2. Slope at the inflection point ("inflection")
# 3. First local max of the first derivative spline fit ("fds")
ym.calculate <- function(bone, method = c("max", "inflection", "fds")) {
  name <<- paste0(bone$Individual[[1]], tolower(bone$Segment[[1]]), bone$Trial[[1]])
  method <- match.arg(method) # match.arg ensures method is one of the three values in the default argument list.
  bone <- filter(bone, Strain < 0.2)
  
  spline.fit.result <- fitStressSpline(bone, fitFirstDeriv = FALSE)
  if (is.null(spline.fit.result)) return(NULL)
  
  firstSecondDerivResult <- calculateFirstSecondDerivatives(spline.fit.result$strainGrid$Strain, spline.fit.result$coefs)

  return(switch(method,
    "max" = {
      globalMax(
        firstSecondDerivResult$first.deriv,
        firstSecondDerivResult$second.deriv,
        spline.fit.result$strainGrid$Strain
      )
    },
    "inflection" = {
      inflectionPoint(
        firstSecondDerivResult$first.deriv,
        firstSecondDerivResult$second.deriv,
        spline.fit.result$strainGrid$Strain
      )
    },
    "fds" = {
      bone <- numericDifferentiation(bone)
      spline.fit.result <- fitStressSpline(bone, fitFirstDeriv = TRUE)
      firstSecondDerivResult <- calculateFirstSecondDerivatives(spline.fit.result$strainGrid$Strain, spline.fit.result$coefs)
    
      localMax(
        spline.fit.result$first.deriv.spline.fit,
        firstSecondDerivResult$first.deriv,
        spline.fit.result$strainGrid$Strain
      )
    }))
}

# creates a grid with Strain points spaced `increment` apart.
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

# fit a spline with an abstract formula.
# TODO CHANGE THIS TO MARS PACKAGE METHOD
fitSpline <- function(formula, bone, strain) {
  result <- tryCatch(
    {
      model <- lm(formula, data = bone)
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
fitStressSpline <- function(bone, fitFirstDeriv = FALSE) {
  strainGrid <- createGrid(bone)
  
  # fit and predict stress spline
  stress.spline.fit <- fitSpline(Stress ~ bSpline(Strain, df = 10), bone, strainGrid)
  if (is.null(stress.spline.fit)) {
    message(paste0(name, ": ",  "Failed to fit spline to stress"))
    return(NULL)
  }
  strainGrid$stress.spline.fit <- stress.spline.fit$predictions
  
  result <- list(coefs = coef(stress.spline.fit$model), strainGrid = strainGrid)
  
  # optionally fit and predict first derivative spline. this is used by the FDS method.
  if (fitFirstDeriv) {
    deriv.spline.fit <- fitSpline(first.deriv ~ bSpline(Strain, df = 10), bone, strainGrid)
    if (is.null(deriv.spline.fit)) {
      message(paste0(name, ": ",  "Failed to fit spline to first derivatives"))
      return(NULL)
    }
    result$first.deriv.spline.fit <- deriv.spline.fit$predictions
    result$first.deriv.coefs <- coef(deriv.spline.fit$model)
  }
  
  return(result)
}

# calculates the first and second derivatives of `strain` values using the coefficients from a spline fit.
# arg strain: strain values to calculate derivatives for
# arg coefficients: the coefficients of a spline fit to original Stress/Strain data
calculateFirstSecondDerivatives <- function(strain, coefficients) {
  first.deriv.basis.mat <- dbs(strain, df = 10, derivs = 1)
  second.deriv.basis.mat <- dbs(strain, df = 10, derivs = 2)
  
  # TODO pf15cp2 has two NA coefficients probably due to overfitting. Parse the warning message and refit with n-1 df

  return(list(first.deriv = first.deriv.basis.mat %*% coefficients[-1], # coefficients[1] is the intercept
         second.deriv = second.deriv.basis.mat %*% coefficients[-1]))
}

inflectionIndex <- function(second.deriv) {
  if (anyNA(second.deriv)) {
    return(NULL)
  }
  for (i in 2:(length(second.deriv) - 1)) {
    if (sign(second.deriv[i]) != sign(second.deriv[i + 1])) {
      return(i)
    }
  }
  return (NULL)
}

# finds global max of arg 1 (first deriv)
# returns: first.deriv global max and corresponding second deriv and strain values.
globalMax <- function(first.deriv, second.deriv, strain) {
  d1MaxIndex <- which.max(first.deriv)
  return(list(slope = first.deriv[d1MaxIndex], second.deriv = second.deriv[d1MaxIndex], strain = strain[d1MaxIndex], name = name))
}

# finds the inflection point of arg 2 (second deriv)
# returns: first derivative and strain values at the inflection point
inflectionPoint <- function(first.deriv, second.deriv, strain) {
  d2InflectionIndex <- inflectionIndex(second.deriv)
  if (is.null(d2InflectionIndex)) return(NULL)
  return(list(slope = first.deriv[[d2InflectionIndex]], strain = strain[[d2InflectionIndex]], name = name))
}


# finds first maximum of arg 1 (first deriv spline fit)
# returns: first.deriv value at index of spline.fit first local max and corresponding strain value
localMax <- function(first.deriv.spline.fit, first.deriv, strain) {
  d1LocalMaxIdx <- min(which.min(diff(first.deriv.spline.fit) > 0))
  return(list(slope = first.deriv[d1LocalMaxIdx], strain = strain[d1LocalMaxIdx], name = name))
}

