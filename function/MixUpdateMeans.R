MixUpdateMeans = function (split_data, means, covs, gamma) 
{
  n0 <- split_data$n0
  n1 <- split_data$n1
  k <- length(means)
  cluster_sizes <- MixClusterSizes(split_data, gamma)
  new_means <- lapply(seq_len(k), function(j) {
    total <- 0
    if (n0 > 0) {
      total <- total + apply(gamma$gamma0[, j] * split_data$data_comp, 
                             2, sum)
    }
    if (n1 > 0) {
      working_response <- WorkResp(split_data$data_incomp, 
                                   means[[j]], covs[[j]], gamma$gamma1[, j])
      total <- total + apply(working_response, 2, sum)
    }
    new_mean <- (total)/cluster_sizes[j]
    names(new_mean) <- split_data$orig_col_names
    return(new_mean)
  })
  return(new_means)
}
