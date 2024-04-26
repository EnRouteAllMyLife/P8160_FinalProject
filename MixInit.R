# intializing the primary mean and covariance

MixInit = function (split_data, k, init_means, init_covs, init_props) 
{
  theta0 <- list()
  n0 <- split_data$n0 #nrow(data_comp)
  d <- split_data$n_col #ncol(data)
  data_comp <- split_data$data_comp 
  
  
  if (all(!is.null(init_means), !is.null(init_covs), !is.null(init_props))) {
    theta0$means <- init_means
    theta0$covs <- init_covs
    theta0$pi <- init_props
  }
  else {
    if (n0 == 0) {
      stop("If no observations are complete, initial values are required for all parameters.")
    }
    k_means <- stats::kmeans(data_comp, k, iter.max = 100, 
                             nstart = 100)
    cluster_assignment <- k_means$cluster
    if (is.null(init_means)) {
      means <- k_means$centers
      theta0$means <- lapply(seq_len(k), function(i) {
        means[i, ]
      })
    }
    else {
      theta0$means <- init_means
    }
    if (is.null(init_covs)) {
      theta0$covs <- lapply(seq_len(k), function(i) {
        clust <- data_comp[cluster_assignment == i, , 
                           drop = FALSE]
        return(matCov(clust, clust))
      })
    }
    else {
      theta0$covs <- init_covs
    }
    if (is.null(init_props)) {
      theta0$pi <- as.numeric(table(cluster_assignment))/n0
    }
    else {
      theta0$pi <- init_props
    }
  }
  eigen_values <- unlist(lapply(theta0$covs, FUN = eigSym))
  if (min(eigen_values) <= 0) {
    stop("Initial covariance matrices are not all positive definite.")
  }
  theta0$gamma <- Responsibility(split_data, theta0$means, 
                                 theta0$covs, theta0$pi)
  return(theta0)
}