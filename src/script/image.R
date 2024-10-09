# save a ggplot
# * arg df fish with metadata to pull
# * arg plot: ggplot object to save
image.save <- function(df, plot, directory) {
  image_directory <- "./img"
  directory <- paste(image_directory, df$Individual[1], sep = "/")
  filepath <- paste(directory, "/", df$Individual[1], df$Segment[1], df$Trial_Number[1], ".png", sep = "")
  ggsave(filepath, plot = plot) # save last plot
}