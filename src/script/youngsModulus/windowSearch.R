global.window.size = 5
library(slider)

# tbl is a subset of a bone. 
# returns lm fit of tbl with formula Stress ~ Strain
fitLm <- function(tbl) {
  return(lm(data = tbl, formula = Stress ~ Strain))
}

rmse <- function(model) {
  return(sqrt(mean(residuals(model)^2)))
}

# determines strain index and slope of "best" window
findBestSlope <- function(bone) {
  view(bone)
  return()
  nRun <- 0
  
  minRmse <- Inf
  maxSlope <- -Inf
  
  bestModel <- NULL
  bestSlope <- NULL
  bestStrain <- NULL
  
  index <- 1
  
  bone <- bone %>%  
    mutate(slope = map_dbl(window.fit, ~ coefficients(.x)[[2]])) %>% 
    na.omit()

  sd <- sd(bone$slope)
  
  for (model in bone$window.fit) {
    name <- getName(bone)
    rmse <- rmse(model)
    slope <- coefficients(model)[[2]]

    if (is.na(slope)) {
      next
    }
    
    if (slope < 0) {
      next
    }
    
    if (slope <= 2 * sd) {
      if (slope > maxSlope) {
        minRmse <- rmse
        bestModel <- model
        maxSlope <- slope
        bestStrain <- bone$Strain[[index]]
      }
    }
    index <- index + 1
  }
  
  return(
    list(
      name = name,
      rmse = minRmse,
      model = bestModel,
      slope = maxSlope,
      strain = bestStrain
    )
  )
}

fitWindow <- function(tbl, window.size) {
  
}

ws.calculate <- function(bone, window.sizes = 2:15) {
  bone %>% 
    distinct(Strain, .keep_all = TRUE) %>%
    filter(Strain < 0.2) %>% 
    expand_grid(window.size = window.sizes) %>%
    mutate(
      window.fit = map(window.size, ~ slide(
        .x = bone,
        .before = 0,
        .after = .x - 1, 
        .step = 1,
        .f = ~ fitLm(.x)
      ))
    ) %>% 
    unnest(window.fit) %>%
    findBestSlope()
}








