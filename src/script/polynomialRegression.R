# Polynomial regression on stress, strain
# * arg bone: bone
# * arg deg: degree of polynomial
# * returns lm of class polynomial
#
polyreg.plm <- function(bone, deg) {
  return(lm(Stress ~ poly(Strain, deg, raw = TRUE), data = bone, x = TRUE))
}

# Calculates first derivative of a polynomial function
# * arg model: polynomial lm model
# * arg strain: strain value
# * returns number representing 1st derivative
#
polyreg.first_derivative <- function(model, strain) {
  return(sum(sapply(1:model$rank-1, function(i) i * coef(model)[i + 1] * strain^(i - 1))))
}

# Calculates second derivative of a polynomial function
# * arg model: polynomial lm model
# * arg strain: strain value
# * returns number representing 2nd derivative
#
polyreg.second_derivative <- function(model, strain) {
  (sum(sapply(1:model$rank-1, function(i) i * (i - 1) * coef(model)[i + 1] * strain^(i - 2))))
}

# slope location of the two second derivative values that straddle 0
polyreg.slope_location <- function(derivatives, strains) {
  sorted <- sort(derivatives)
  print(sorted)
  
  neg_idx <- max(which(sorted < 0))
  pos_idx <- min(which(sorted > 0))
  

  # The average of the two strain values at these straddling locations
  return(mean(c(strains[neg_idx], strains[pos_idx])))
}