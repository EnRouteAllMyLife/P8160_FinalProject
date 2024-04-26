MixUpdate = function (split_data, theta) 
{
  old_means <- theta$means
  old_covs <- theta$covs
  old_pi <- theta$pi
  old_gamma <- theta$gamma
  old_cluster_sizes <- MixClusterSizes(split_data, old_gamma)
  old_resid_ops <- MixResidOP(split_data, old_means, old_means, 
                              old_covs, old_gamma)
  old_obj <- MixEMObj(old_cluster_sizes, old_pi, old_covs, 
                      old_resid_ops)
  
  new_means <- MixUpdateMeans(split_data, old_means, old_covs, 
                                old_gamma)
  
  new_resid_ops <- MixResidOP(split_data, new_means, old_means, 
                              old_covs, old_gamma)
  k <- theta$gamma$k
  new_covs <- lapply(seq_len(k), function(j) {
    new_cov <- new_resid_ops[[j]]/old_cluster_sizes[[j]]
    dimnames(new_cov) <- list(split_data$orig_col_names, 
                              split_data$orig_col_names)
    return(new_cov)
  })
  new_gamma <- Responsibility(split_data, new_means, new_covs, 
                              old_pi)
  new_pi <- old_cluster_sizes/sum(old_cluster_sizes)
  new_obj <- MixEMObj(old_cluster_sizes, new_pi, new_covs, 
                      new_resid_ops)
  delta <- new_obj - old_obj
  out <- list()
  out$means <- new_means
  out$covs <- new_covs
  out$pi <- new_pi
  out$gamma <- new_gamma
  out$new_obj <- new_obj
  out$old_obj <- old_obj
  out$delta <- delta
  return(out)
}
