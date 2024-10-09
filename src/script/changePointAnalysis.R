# segments fish df with nseg segments
# * arg df: fish df
# * arg npointsmax: max number of break points
# * arg refit: try to refit model with n-1 break points. Default is FALSE. Will increase run time.
# returns a segmented model
cpa.segment <- function(df, npointsmax, refit = FALSE) {
  if (refit) {
    npoints <- npointsmax
    while (npoints > 1) {
      tryCatch({
        fit <- segmented::segmented(lm(Stress ~ Strain, data = df), seg.Z = ~Strain, npsi = npoints)
        # segmented will return an "lm" model if execution fails. We do not want that lm model. Try again with n-1 break points.
        stopifnot(inherits(fit, "segmented")) # stop if fit does not inherit "segmented". The alternative is inherits "lm" which we do not want.
        return(fit)
      }, error = function(e) {
        message("Error: Refitting segmented model with ", npoints - 1, " breakpoints.")
        npoints <<- npoints - 1
      })
    }
    stop("Failed to fit segmented model after ", npointsmax, " attempts. Returning model fit with 1 break point. ")
    return(segmented::segmented(lm(Stress ~ Strain, data = df), seg.Z = ~Strain, npsi = 1))
  }
  return(segmented::segmented(lm(Stress ~ Strain, data = df), seg.Z = ~Strain, npsi = npointsmax)) # go ahead fail see if i care
}

# Determines segment count for segmented model. NOTE it has been returning 0 for all fish. Even if this is "correct", I don't like it.
# * arg fit_lm: linear model.
determine_nsegments <- function(fit_lm) {
  return(selgmented(fit_lm, type = "bic", Kmax = 10, return.fit = FALSE))
}

# Change point detection using cpt.var
cpa.changepoint <- function(df) {
  df %>% dplyr::select(Strain, Stress) %>% 
    sapply(function(x) as.numeric(as.character(x))) %>% 
    return(changepoint::cpt.var(class = TRUE))
}

# finds the nearest observed point to each predicted breakpoint. Uses euclidean distance.
# * arg df: data used to fit arg 2
# * arg fit_segmented: fitted segmented model
cpa.nearest_points <- function(df, fit_segmented) {
  breakpoints <- fit_segmented$psi[,2]
  return(df[apply(crossdist(breakpoints, predict(fit_segmented, newdata = data.frame(Strain = breakpoints)), df$Strain, df$Stress), 1, FUN = which.min),] %>% na.omit())
}


# calculates the slope between adjacent pairs of points
cpa.slope <- function(points) {
  led <- lead(points)
  return(with(points, (lead(Stress) - Stress) / (lead(Strain) - Strain)) %>% na.omit())
}


