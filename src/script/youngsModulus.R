# Finds Young's Modulus and corresponding strain value for Stress/Strain data using the max slope of Stress spline fit method.
ym.maxSlope <- function(bone) {
  # steps:
  # fit spline to stress/strain
  # make grid
  # calculate first and second derivatives of spline fit
  # look up first.deriv with index of second deriv where it crosses 0?
}

# Finds Young's Modulus and corresponding strain value for Stress/Strain data using the local maximum of Stress/Strain 1st derivatives spline fit.
ym.FDS <- function(bone) {
  bone <- filter(bone, Strain < 0.2)
  bone <- numericDifferentiation(bone)
  
  splineFitResult <- fitStressSpline(bone)
  boneGrid <- calculateSplineFirstDerivative(splineFitResult$boneGrid, splineFitResult$coefs)
  return(
    localMax(
      boneGrid$first.deriv.spline.fit,
      boneGrid$first.deriv,
      boneGrid$Strain
    )
  )
}


# creates a grid with Strain points spaced `increment` apart.
# arg increment: grid spacing
# returns tibble with gridded Strain column.
createGrid <- function(bone, increment = 0.004) {
  start <- min(bone$Strain)
  end <- max(bone$Strain)
  return(data.frame(Strain = seq(start, end, by = increment)))
}

# performs numeric differentiation on a bone's Stress and Strain values
# returns: original bone with two new columns: first.deriv and second.deriv
numericDifferentiation <- function(bone) {
  lag2Strain <- lag(bone$Strain, 2) # is used multiple times
  return(bone |>
           mutate(
             first.deriv = (Stress - lag(Stress, 2)) / (Strain - lag2Strain),
             second.deriv = (first.deriv - lag(first.deriv, 2)) / (Strain - lag2Strain)
           ) |>
           filter(!is.na(first.deriv), is.finite(first.deriv)))
}

# fits splines for Stress/Strain and Stress/Strain first derivatives
# returns: list 1. coefficients of Stress spline fit. 2. bone data appended with 1st derivative spline fit and Stress spline fit.
fitStressSpline <- function(bone) {
  ## TODO replace spline fit with method from MARS package
  first.deriv.spline.fit <- lm(first.deriv ~ bSpline(Strain, df = 10), data = bone)
  stress.spline.fit <- lm(Stress ~ bSpline(Strain, df = 10), data = bone)
  
  boneGrid <- createGrid(bone)
  boneGrid$first.deriv.spline.fit <- predict(first.deriv.spline.fit, newdata = boneGrid)
  boneGrid$stress.spline.fit <- predict(stress.spline.fit, newdata = boneGrid)
  
  return(list(
    coefs = coef(stress.spline.fit),
    bone = bone,
    boneGrid = boneGrid
  ))
}

# finds first derivatives of a spline fit
calculateSplineFirstDerivative <- function(bone, stress.spline.fit.coefficients) {
  first.deriv.mat <- dbs(bone$Strain, df = 10, derivs = 1) # dbs is the derivative of a spline fit
  bone$first.deriv.spline.fit <- first.deriv.mat %*% stress.spline.fit.coefficients[-1] # [1] is the intercept.
  return(bone)
}

# finds first maximum in v1; returns list of corresponding values in v2 and v3
# arg v1: FDS passes a first derivative spline fit, maxSlope passes a
# arg v2: FDS passes a first derivative, maxSlope passes a
# arg v3: both methods pass Strain
# returns: first.deriv value at index of spline.fit first local max
localMax <- function(v1, v2, v3) {
  v1MaxIndex <- min(which.min(diff(v1) > 0))
  return(list(slope = v2[v1MaxIndex], strain = v3[v1MaxIndex]))
}
