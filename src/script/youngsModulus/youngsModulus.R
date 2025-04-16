
library(splines2)
library(gridExtra)
library(cowplot)

source("./src/script/helpers/general.R")

# per-bone variables
global.name <- ""
r.squared <- NA
SVI.plots <- list()

# constants
global.max.df <- 10 # the starting number of degrees of freedom when fitting splines
global.strain.filter <- 0.2
global.grid.interval <- 0.0001
global.svi.window.size <- 500

# Calculates Young's Modulus for a bone using three methods:
# 1. Global max slope
# 2. Slope at the inflection point
# 3. First local max of the first derivative spline fit
ym.calculate <- function(bone) {
  global.name <<- getName(bone)
  bone <- filter(bone, Strain < global.strain.filter)
  
  spline.fit.result <- fitStressSpline(bone, fitFirstDeriv = FALSE)
  if (is.null(spline.fit.result)) {
    return(NULL)
  }
  firstSecondDerivResult <- calculateFirstSecondDerivatives(
    spline.fit.result$strainGrid$Strain,
    spline.fit.result$coefs
  )
  # not great but we need a different spline fit and first and second derivatives for the fds method
  fds.spline.fit.result <- fitStressSpline(numericDifferentiation(bone), fitFirstDeriv = TRUE)
  if (is.null(fds.spline.fit.result)) {
    return(NULL)
  }
  fds.firstSecondDerivResult <- calculateFirstSecondDerivatives(
    fds.spline.fit.result$strainGrid$Strain,
    fds.spline.fit.result$coefs
  )
  
  results <- c(
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
    name = global.name,
    r.squared = r.squared
  )
  
  # uncomment for coefficient of variation (score) plots lol
  # gridExtra::grid.arrange(grobs = SVI.plots,
  #                         ncol = 3,
  #                         top = textGrob(getName(bone, sep = " "), gp = gpar(fontface = "bold", fontsize = 28)))
  return(results)
}

# performs numeric differentiation on Stress and Strain values
# returns: original bone with two new columns: first.deriv and second.deriv
numericDifferentiation <- function(bone) {
  return(bone |>
    dplyr::mutate(
      lag2Strain = lag(Strain, 2),
      first.deriv = (Stress - lag(Stress, 2)) / (Strain - lag2Strain),
      second.deriv = (first.deriv - lag(first.deriv, 2)) / (Strain - lag2Strain)
    ) |>
    dplyr::filter(!is.na(first.deriv), is.finite(first.deriv)) |>
    dplyr::select(-lag2Strain))
}

# fit a spline model
# returns list of model, predictions, and coefficients
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

# creates a grid with Strain points spaced argument interval apart.
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
    message(global.name, ": Failed to fit spline to stress")
    return(NULL)
  }
  strainGrid$stress.spline.fit <- stress.spline.fit$predictions
  
  result <- list(coefs = coef(stress.spline.fit$model),
                 strainGrid = strainGrid)
  
  # optionally fit and predict first derivative spline. this is used only by the FDS method.
  if (fitFirstDeriv) {
    deriv.spline.fit <- fitSpline(formula = first.deriv ~ bSpline(Strain, df = global.degrees.freedom), bone, strainGrid)
    if (is.null(deriv.spline.fit)) {
      message(global.name,  ": Failed to fit spline to first derivatives")
      return(NULL)
    }
    result$first.deriv.spline.fit <- deriv.spline.fit$predictions
    result$first.deriv.coefs <- coef(deriv.spline.fit$model)
    if (anyNA(result$first.deriv.coefs)) {
      message(global.name, ": Failed to fit all knots in first derivative spline fit")
      return(NULL)
    }
  }
  
  return(result)
}


# calculates the first and second derivatives of `strain` values using the coefficients from a spline fit.
# arg strain: strain values to calculate derivatives for
# arg coefficients: the coefficients of a spline fit to original Stress/Strain data
calculateFirstSecondDerivatives <- function(strain, coefficients) {
  first.deriv.basis.mat <- dbs(strain, df = global.max.df, derivs = 1)
  second.deriv.basis.mat <- dbs(strain, df = global.max.df, derivs = 2)
  
  return(list(first.deriv = first.deriv.basis.mat %*% coefficients[-1], # coefficients[1] is the intercept
         second.deriv = second.deriv.basis.mat %*% coefficients[-1]))
}

# 
subset.strain <- function(strain, index, n = global.svi.window.size) {
  half_window <- floor(window_size / 2)
  start_idx <- max(1, idx - half_window)
  end_idx <- min(length(strain), idx + half_window)
  return(strain[start_idx:end_idx])
}

# saves SVI plot 
plot.svi <- function(name, cov, data) {
  plot <- ggplot(data, aes(x = strain, y = slope)) + 
    geom_point() + 
    labs(x = "Strain", y = if_else(name == "Max", "First derivative", ""), title = name, subtitle = paste("CV:", round(cov, 2))) + 
    theme_classic() + 
    scale_x_continuous(
      limits = c(min(data$strain), max(data$strain)),
      breaks = c(min(data$strain), max(data$strain)),
      labels = scales::label_number(accuracy = 0.001)
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 32),
      plot.subtitle = element_text(face = "bold", size = 30),
      axis.title.x = element_text(size = 28),
      axis.text.x = element_text(size = 24),
      axis.title.y = element_text(size = 28),
      axis.text.y = element_text(size = 24),
    )
  
  if (name == "Max") {
    SVI.plots[[1]] <<- plot
  }
  if (name == "Inflection") {
    SVI.plots[[2]] <<- plot
  }
  if (name == "FDS") {
    SVI.plots[[3]] <<- plot
  }
  return(NA)
}

# Slope Variability Index evaluates the variance of slopes +- n strain ticks away from the selected index.
# we theorize that a "good" young's modulus position has an SVI near 0, indicating low regional variance.
# arg strain: strain values for x axis labels
# arg first.deriv: first derivative values which to calculate SVI
# arg n: window size is 2 * n + 1 NOTE may be lower if you reach the bounds of the first.deriv vector
# returns: absolute value of a modified coefficient of variation score that uses the first derivative value at the selected
# index as the "anchor" to compare the deviation to. 
SVI <- function(index, strain, first.deriv, name = NULL, n = global.svi.window.size) {
  selected.first.deriv <- first.deriv[index]
  start.index <- max(1, index - n) # cap start and end at index 1 and strain length
  end.index <- min(length(first.deriv), index + n)
  
  first.derivative.window <- first.deriv[start.index:end.index]
  strain.window <- strain[start.index:end.index]
  
  # Since we can not guarantee that the first derivative values in the window 
  # have a normally distributed first derivative value, using the usual standard deviation formula is not ok.
  # Our modified deviation measure uses the distance from the selected first derivative value instead of the distance from the window mean. 
  # This modified variation method is analogous to the error of a regression model.
  deviation <- sqrt(mean((first.derivative.window - selected.first.deriv)^2, na.rm = TRUE))
  
  cov <- abs(deviation / selected.first.deriv)

  # plotting SVI
  if (length(name)) plot.svi(name, cov, data =  data.frame(strain = strain.window, slope = first.derivative.window))
  
  return(cov)
}


# finds global max of first derivatives
# returns: first derivative value at the  global max and corresponding strain value and slope score
globalMax <- function(first.deriv, strain) {
  d1.max.index <- which.max(first.deriv)
  return(
    list(
      slope = first.deriv[d1.max.index],
      strain = strain[d1.max.index],
      score = SVI(d1.max.index, strain, first.deriv, "Max")
    )
  )
}

# arg which - take the nth inflection point
inflectionIndex <- function(second.deriv) {
  if (anyNA(second.deriv)) {
    return(NULL)
  }
  for (i in 2:(length(second.deriv) - 1)) {
    if (sign(second.deriv[i]) > sign(second.deriv[i + 1])) {
      return(i)
    }
  }
  return(NULL)
}

# finds the inflection point of arg 2 (second deriv)
# returns: first derivative value at the inflection point and corresponding strain value and slope score
inflectionPoint <- function(first.deriv, second.deriv, strain) {
  d2.inflection.index <- inflectionIndex(second.deriv)
  
  if (is.null(d2.inflection.index)) {
    message(global.name, ": Failed to find second derivative inflection point")
    return(NULL)
  }
  
  return(
    list(
      slope = first.deriv[[d2.inflection.index]],
      strain = strain[[d2.inflection.index]],
      score = SVI(d2.inflection.index, strain, first.deriv, "Inflection")
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
      score = SVI(d1.localMax.index, strain, first.deriv, "FDS")
    )
  )
}
