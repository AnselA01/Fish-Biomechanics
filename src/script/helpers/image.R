# save a ggplot
# * arg df fish with metadata to pull
# * arg plot: ggplot object to save
# this has not been maintained. not recommended. use the only below instead.
image.save <- function(df, plot, directory) {
  image_directory <- "./img"
  directory <- paste(image_directory, directory, df$Individual[1], sep = "/")
  filepath <- paste(directory, "/", df$Individual[1], df$Segment[1], df$Trial[1], ".svg", sep = "")
  ggsave(filepath, plot = plot, width = 7, height = 6, units = "in")
}

# use this one instead
image.save2 <- function(plot, path) {
  ggsave(path, plot, width = 4, height = 3)
}