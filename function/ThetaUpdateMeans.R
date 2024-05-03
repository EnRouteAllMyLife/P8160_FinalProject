ThetaUpdateMeans = function (n0,n1,k, data_comp, data_incomp,means, covs, gamma,cluster_sizes) 
{
  new_means <- lapply(seq_len(k), function(j) {
    total <- 0
    if (n0 > 0) {
      total <- total + apply(gamma$gamma0[, j] * data_comp, 2, sum)
    }
    if (n1 > 0) {
      working_response <- WorkResp(data_incomp, means[[j]], covs[[j]], gamma$gamma1[, j])
      total <- total + apply(working_response, 2, sum)
    }
    new_mean <- (total)/cluster_sizes[j]

    return(new_mean)
  })
  return(new_means)
}

