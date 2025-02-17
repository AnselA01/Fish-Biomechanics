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
  bone <- filter(bone, Strain < global.strain.filter)
  
  gridsplinea <- fitStressSpline(bone.orig)[[2]]
  xa <- decisions$strain[which(decisions$names == name)]
  
  ma <- decisions$slope[which(decisions$names == name)]
  x1a <- xa + 0.002*max(bone.orig$Strain)
  
  #stress.spline.indexa <- as.integer(str_replace_all(as.character(xa), "0|\\.", "")) + 1 # add 1 for indexing
  #y1a <- gridsplinea$stress.spline.fit[[stress.spline.indexa]]
  
 # gridsplinea <- gridsplinea %>%
   # mutate(yieldline = ma*(Strain - x1a) + y1a)
  
  #plotggplot(gridspline) + 
   # geom_line(aes(x = Strain, y = stress.spline.fit), color = "blue") + 
   # geom_line(aes(x = Strain, y = yieldline), color = "red") +
    #xlim(c(0, max(pf04ut01$Strain))) 
  
  
  #highest_rowa <- max(as.numeric(row.names(gridsplinea[abs(gridsplinea$stress.spline.fit - gridsplinea$yieldline) < global.intersection.limit, ])))
  
 # yield.stress <- gridsplinea$stress.spline.fit[highest_row]
 # yield.strain <- gridsplinea$Strain[highest_row]
  
  return(x1a)
    #list(
      #yield.stress,
      #yield.strain
    #)
  #)
  
}
