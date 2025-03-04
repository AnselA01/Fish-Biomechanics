source("./src/script/helpers/general.R")
source("./src/script/youngsModulus/youngsModulus.R")

decisions <- read_csv("results/youngsModulus/decisions.csv")

# per-bone variables
name <- ""
r.squared <- NA

# constants
global.degrees.freedom <- 10
global.strain.filter <- 0.2
global.grid.interval <- 0.0001
global.intersection.limit <- 0.01


#current this function calculates to the x1 value, will switch it to go beyond that soon
ys.calculate <- function(bone) {
  name <<- getName(bone)
  bone.orig <- bone
  bone <- dplyr::filter(bone, Strain < global.strain.filter)
  
  rm(gridsplinea)
  
  # is there a decision for the bone?
  # ex: the spline of pf09ut4 could not be fit and is not in the decisions table.
  if (!(name %in% decisions$names)) {
    message("Yield stress: could not find decision for ", name) 
    return(NULL)
  }
  

  gridsplinea <- fitStressSpline(bone.orig)[[2]]
  xa <- decisions$strain[which(decisions$names == name)]
  
  ma <- decisions$slope[which(decisions$names == name)]
  x1a <- xa * 1.002
  
  stress.spline.indexa <- as.integer(str_replace_all(as.character(xa), "0|\\.", "")) + 1 # add 1 for indexing
  y1a <- gridsplinea$stress.spline.fit[stress.spline.indexa]
  
  gridsplinea <- gridsplinea %>%
    mutate(yieldline = ma*(Strain - x1a) + y1a)
  
  #highest_rowa <- as.numeric(row.names(gridsplinea[which.min(abs(gridsplinea$stress.spline.fit - gridsplinea$yieldline)), ]))
  
  highest_rowa <- max(as.numeric(row.names(gridsplinea[abs(gridsplinea$stress.spline.fit - gridsplinea$yieldline) < global.intersection.limit, ])))
  
  yield.stress <- gridsplinea$stress.spline.fit[highest_rowa]
  yield.strain <- gridsplinea$Strain[highest_rowa]
  
  plot <- ggplot(gridsplinea) + 
    geom_line(aes(x = Strain, y = stress.spline.fit), color = "blue") + 
    geom_line(aes(x = Strain, y = yieldline), color = "red") +
   # #abline(v=yield.strain, col="green") +
    xlim(c(0, yield.strain + 0.02)) +
    ylim(c(0, yield.stress + 1))
  
  plot2 <- ggplot(gridsplinea) + 
    geom_line(aes(x = Strain, y = stress.spline.fit), color = "blue") + 
    geom_line(aes(x = Strain, y = yieldline), color = "red")
  
  #print(plot)
  
  return(list(yield.stress = yield.stress, yield.strain = yield.strain, plot = plot, plot2 = plot2))
}

