library(pracma)
savgolFilter <- function(stress) {
  return(savgol(stress, fl = 11, forder = 3))
}