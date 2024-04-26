MixClusterSizes = function (split_data, gamma) {
  n0 <- split_data$n0
  n1 <- split_data$n1
  k <- gamma$k
  cluster_sizes <- rep(0, k)
  if (n0 > 0) {
    gamma0 <- gamma$gamma0
    cluster_sizes <- cluster_sizes + apply(gamma0, 2, sum)
  }
  if (n1 > 0) {
    gamma1 <- gamma$gamma1
    cluster_sizes <- cluster_sizes + apply(gamma1, 2, sum)
  }
  return(cluster_sizes)
}

