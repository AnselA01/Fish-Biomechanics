
#plots strain vs. stress
# layers argument is a list of ggplot layers like geom_vline(), annotate()... Add anything!
plot.plot <- function(data, layers = c()) {
  source("./src/script/helpers/general.R")
  library(ggtext)
  
  base.plot <- ggplot(data, aes(x = Strain, y = Stress)) +
    geom_line(size = 1.5) +
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
