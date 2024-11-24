
#plots strain vs. stress
# layers argument is a list of ggplot layers like geom_vline(), annotate()... Add anything!
plot.plot <- function(data, layers = c()) {
  source("./src/script/helpers/general.R")
  library(ggtext)
  
  
  base.plot <- ggplot(data, aes(x = Strain, y = Stress)) +
    geom_line(size = 1) +
    labs(
      caption = getName(data, sep = " "),
      x = "Strain",
      y = "Stress"
    ) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0, size = 7), 
          plot.subtitle = element_markdown(hjust = 0, size = 7),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7) 
          )
  
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
  source("./src/script/data.R")
  source("./src/script/helpers/general.R")
  
  # parsing identification data
  fish_number <- parse_number(str_remove(subject.name, "^0")) # extract first number and remove leading 0 if present
  segment <- gsub("[^a-zA-Z]", "", x = subject.name)
  trial <- parse_number(str_remove(stri_reverse(subject.name), "^0")) # extract first number after reversal. leading 0 again
  # fetching and plotting
  subject <- data.fetch(fish_numbers = c(fish_number), segments = c(segment), trials = c(trial))
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
