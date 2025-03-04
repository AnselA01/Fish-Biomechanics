library(dbscan)
ym.dbScan <- function(bone) {
  bone <- numericDifferentiation(bone)
  clustering <- dbscan(bone[, c("Strain", "first.deriv")], eps = 7, minPts = 7, weights = 1 / bone$Strain)
  bone$cluster <- as.factor(clustering$cluster)
  return(bone)
}

ym.identifyElasticRegion <- function(bone) {
  bone <- ym.dbScan(bone)
  valid.clusters <- unique(bone$cluster)
  elastic.cluster <- valid.clusters[which.min(sapply(valid.clusters, function(cl) mean(bone$Strain[bone$cluster == cl])))]
  elastic.data <- subset(bone, cluster == elastic.cluster)
  print(plot.clusters(bone))
  return(elastic.data)
}

plot.clusters <- function(bone) {
  ggplot(bone, aes(x = Strain, y = Stress, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    theme_minimal()
}