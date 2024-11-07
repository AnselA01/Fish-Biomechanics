concavitySlope <- function(bone, interpolate) {
  spline_fit <- smooth.spline(bone$Strain, bone$Stress)
  d1_vals <- predict(spline_fit, bone$Strain, deriv = 1)$y
  d2_vals <- predict(spline_fit, bone$Strain, deriv = 2)$y

  return(find_slope_at_sign_change(d1_vals, d2_vals, interpolate))
  
}

find_slope_at_sign_change <- function(d1_vals, d2_vals, interpolate) {
  d2_sign_change_indices <- find_sign_change_indices(d2_vals)
  
  d2_sign_change_vals <- c(d2_vals[d2_sign_change_indices[[1]]],
                           d2_vals[d2_sign_change_indices[[2]]])
  d1_sign_change_vals <- c(d1_vals[d2_sign_change_indices[[1]]],
                           d1_vals[d2_sign_change_indices[[2]]])
  
  return(ifelse(interpolate, 
                interpolate(d1_sign_change_vals, d2_sign_change_vals),
                mean(d1_sign_change_vals)))
}

find_sign_change_indices <- function(d2_vals) {
  for (i in 2:(length(d2_vals) - 1)) {
    if (sign(d2_vals[i]) != sign(d2_vals[i + 1])) {
      return(c(i, i + 1))
    }
  }
  return (NULL)
}


# linear interpolation between two d1 vals given two d2 values to relate
interpolate <- function(d1_vals, d2_vals) {
  d1_1 <- d1_vals[[1]]
  d1_2 <- d1_vals[[2]]
  d2_1 <- d2_vals[[1]]
  d2_2 <- d2_vals[[2]]

  proportion <- (0 - d2_1) / (d2_2 - d2_1)
  return(d1_1 + proportion * (d1_2 - d1_1))

}