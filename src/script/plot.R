#plots a cleaned data set for strain vs. stress
plot <- function(data) {
  return(ggplot(data, aes(x = Strain, y = Stress)) +
           geom_point() +        # Points for the data
           labs(caption = paste("Fish number: ", data$Individual, ", Segment: ",  data$Segment, ", Trial Number: ", data$Trial_Number, sep = ""),
                x = "Strain",
                y = "Stress") +
           theme_minimal()) +
    plot.caption = element_text(hjust = 0, size = 10)
}

# Plot Strain and Stress with predicted break points overlaid
# * arg df: fish
# * arg     segmented_fit: segmented model
plot.predicted <- function(df, segmented_fit, x) {
  breakpoints <- data.frame(xintercept = segmented_fit$psi[,2])
  
  breakpoint_locations <- data.frame(
    Strain = breakpoints$xintercept,
    Stress = predict(segmented_fit, newdata = data.frame(Strain = breakpoints$xintercept))
  )
  predicted_values <- data.frame(Strain = df$Strain, predicted = segmented_fit$fitted.values)
  
  ggplot() +
    geom_point(data = df, aes(x = Strain, y = Stress)) + 
    geom_point(data = predicted_values, aes(x = Strain, y = predicted), size = 0.5, color = "red", alpha = 0.5) +
    geom_point(data = breakpoint_locations, aes(x = Strain, y = Stress), color = "red", size = 2.5) +
    geom_point(data = x, aes(x = Strain, y = Stress), color = "green", size = 0.5) +
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

# * arg fish_numbers: list of numbers  fish number 0-21 to plot bones for.
# * arg segment:      boolean,         show breakpoints for each bone.
# * arg npointsmax:   number           max breakpoint count. 
# * arg refit:        boolean          Recommended. Will try to refit n-1 break points if fitting n break points fails. This may increase runtime.
# * arg grid.by:      string           Variable to grid by. Accepts "Trial" or "Segment".
plot.grid <- function(fish_numbers, segment, npointsmax, refit, grid_by = c("Trial", "Segment")) {
  grid_by <- match.arg(grid_by)

  return(lapply(fish_numbers, function(fish_number) {
    combined_data <- collect(data.generator(data_directory, fish_number)) %>% # collect all values from the data generator into a list
      map_dfr(function(bone) { # enumerates bone list and rbinds the result to combined_data
        bone$nearest <- if (segment) bone$Reading %in% cpa.nearest_points(bone, cpa.segment(bone, npointsmax, refit))$Reading else FALSE
        return(bone)
      })
    
    color_var <- c(Trial = "Segment", Segment = "Trial")[[grid_by]] # map color_var to the opposite of grid_by
    return(ggplot(data = combined_data, aes_string(x = "Strain", y = "Stress", color = color_var)) + 
           geom_line(size = 1) + 
           geom_point(aes_string(size = "ifelse(nearest, 1.5, NA)", fill = color_var), color = "black", shape = 21, show.legend = FALSE) +
           facet_wrap(as.formula(paste("~", grid_by))) + 
           labs(title = paste("Fish", fish_number)) +
           scale_size_identity() +
           theme_bw() + 
           theme(strip.background = element_blank()))
    })
  )
}