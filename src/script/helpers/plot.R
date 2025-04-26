library(ggtext)

source("./src/script/helpers/general.R")
  
#plots strain vs. stress
# layers argument is a list of ggplot layers like geom_vline(), annotate()... Add anything!
plot.plot <- function(data, layers = c()) {
  
  base.plot <- ggplot(data, aes(x = Strain, y = Stress)) +
    geom_line(size = 1) +
    labs(
      title = getName(data, sep = " "),
      x = "Strain",
      y = "Stress"
    ) +
    theme_classic()
  # add custom layers 
  for (layer in layers) {
    base.plot <- base.plot + layer
  }
  return(base.plot)
}

# fetches and plots subject.name. also returns the subject
# arg subject.name: a fish id in the form "<fish number><segment><trial>"
# returns subject
plot.subject <- function(subject.name) {
  library(stringi)
  source("./src/script/helpers/data.R")
  source("./src/script/helpers/general.R")
  
  subject <- data.fetch(subject.name = subject.name)
  if (!length(subject)) {
    message("Could not find subject", subject.name)
    return(NULL)
  }
  
  print(plot.plot(subject))
  return(subject)
}

# Plot Strain and Stress with predicted break points overlaid
# * arg df: fish
# * arg     segmented_fit: segmented model
plot.predicted <- function(df, segmented_fit, points) {
  predicted_values <- data.frame(Strain = df$Strain, predicted = segmented_fit$fitted.values)
  
  ggplot() +
    geom_point(data = df, aes(x = Strain, y = Stress)) + 
    geom_point(data = predicted_values, aes(x = Strain, y = predicted), size = 0.5, color = "red", alpha = 0.5) +
    geom_point(data = points, aes(x = Strain, y = Stress), color = "green", size = 1) +
    theme_minimal()
}

# plot a histogram. Needs original, non-filtered, fish
# arg bins: bin count
plot.histogram <- function(df, bins) {
  df %>% 
    ggplot(aes(x = Stress)) + 
    geom_histogram(bins = bins) + 
    theme_minimal()
}

getYoungsModulusDecisionColors <- function(choice) {
  return(list(
    max = ifelse(choice$method == "max", "forestgreen", "tomato"),
    inflection = ifelse(choice$method == "inflection", "forestgreen", "tomato"),
    fds = ifelse(choice$method == "fds", "forestgreen", "tomato")
  ))
}

makeYoungsModulusDecisionSubtitle <- function(decisionColors, result, choice) {
  plot.subtitle <- paste(
    "<b style='color:", decisionColors$max, ";'>Max:</b>",
    round(result$max.slope, 1),
    "<br>",
    "<b style='color:", decisionColors$inflection, ";'>Inflection:</b>",
    round(result$inflection.slope, 1),
    "<br>",
    "<b style='color:", decisionColors$fds, ";'>FDS:</b>",
    round(result$fds.slope, 1)
  )
}

makeDecisionAnnotationGeoms <- function(bone, decisionColors, result) {
  return(list(
    geom_segment(
      x = result$max.strain,
      xend = result$max.strain,
      y = 0,
      yend = max(bone$Stress * 0.80),
      color = decisionColors$max
    ),
    geom_segment(
      x = result$inflection.strain,
      xend = result$inflection.strain,
      y = 0,
      yend = max(bone$Stress * 0.80),
      color = decisionColors$inflection
    ),
    geom_segment(
      x = result$fds.strain,
      xend = result$fds.strain,
      y = 0,
      yend = max(bone$Stress * 0.80),
      color = decisionColors$fds
    ),
    annotate(
      "text",
      x = result$max.strain,
      y = max(bone$Stress),
      label = "Max",
      angle = 0,
      color = decisionColors$max,
      size = 3,
      fontface = "bold"
    ),
    annotate(
      "text",
      x = result$inflection.strain,
      y = max(bone$Stress * 0.925),
      label = "Inflection",
      angle = 0,
      color = decisionColors$inflection,
      size = 3,
      fontface = "bold"
    ),
    annotate(
      "text",
      x = result$fds.strain,
      y = max(bone$Stress * 0.85),
      label = "FDS",
      angle = 0,
      color = decisionColors$fds,
      size = 3,
      fontface = "bold"
    )
  ))
}

plot.youngsModulusDecision <- function(bone, result, choice) {
  decisionColors <- getYoungsModulusDecisionColors(choice)
  plot.subtitle <- makeYoungsModulusDecisionSubtitle(decisionColors, result, choice)
  plot.annotations <- makeDecisionAnnotationGeoms(bone, decisionColors, result)
  
  return(plot.plot(
    bone,
    layers = list(
      plot.annotations,
      labs(subtitle = plot.subtitle),
      theme(plot.subtitle = element_markdown())
    )
  ))
}
