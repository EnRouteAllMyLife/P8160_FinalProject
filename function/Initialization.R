# k = 2
Initialization = function (n0, n1, k,data_comp, data_incomp,d) {
  theta0 <- list()
  
  if (n0 == 0) {
      stop("If no observations are complete, initial values are required for all parameters.")
  }
  # based on complete set  
  k_means <- stats::kmeans(x = data_comp, 
                           centers = k, 
                           iter.max = 100,
                           nstart = 100)
  # nstart, if centers is a number, how many random sets should be chosen?
  cluster_assignment <- k_means$cluster
  # init_means, init_covs, init_props is null
  means <- k_means$centers
  theta0$means <- lapply(seq_len(k), function(i) {means[i, ] })
  theta0$covs <- lapply(seq_len(k), function(i) {
        clust <- data_comp[cluster_assignment == i, , 
                           drop = FALSE]
        return(cov(clust))
      })
  # might need double check on the cov
  
  # proportion of distribution
  theta0$pi <- as.numeric(table(cluster_assignment))/n0
  
  eigen_values <- unlist(lapply(seq_len(k), function(i) {
    eigen_value = eigen(theta0$covs[[i]])
    return(eigen_value$values)
  }))
  
  if (min(eigen_values) <= 0) {
    stop("Initial covariance matrices are not all positive definite.")
  }
  
 
  theta0$gamma <- Responsibility(n0, n1, k,data_comp, data_incomp, theta0$means, 
                                 theta0$covs, theta0$pi)
  return(theta0)
}
