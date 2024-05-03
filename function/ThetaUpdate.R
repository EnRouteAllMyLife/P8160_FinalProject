ThetaUpdate = function (n0, n1, k,d, data_comp, data_incomp,theta)
{
  old_means <- theta$means
  old_covs <- theta$covs
  old_pi <- theta$pi
  old_gamma <- theta$gamma
  
  
  old_cluster_sizes <- rep(0, k)
  if (n0 > 0) {
    gamma0 <- old_gamma$gamma0
    old_cluster_sizes <- old_cluster_sizes + apply(gamma0, 2, sum)
  }
  if (n1 > 0) {
    gamma1 <- old_gamma$gamma1
    old_cluster_sizes <- old_cluster_sizes + apply(gamma1, 2, sum)
  }
  
  
  old_resid_ops <- ResidOP(n0,n1,d,k, data_comp, data_incomp, old_means, old_means, 
                              old_covs, old_gamma)
  
  old_obj <- MixEMObj(old_cluster_sizes, old_pi, old_covs,  old_resid_ops)
  
  new_means <- ThetaUpdateMeans(n0,n1,k, data_comp, data_incomp, old_means, old_covs, old_gamma,old_cluster_sizes)
  
  new_resid_ops <- ResidOP(n0,n1,d,k, data_comp, data_incomp, new_means, old_means, 
                              old_covs, old_gamma)
  
  
  new_covs <- lapply(seq_len(k), function(j) {
    new_cov <- new_resid_ops[[j]]/old_cluster_sizes[[j]]

    return(new_cov)
  })
  new_gamma <- Responsibility(n0, n1, k,data_comp, data_incomp, new_means, new_covs,old_pi)
  new_pi <- old_cluster_sizes/sum(old_cluster_sizes)
  new_obj <- MixEMObj(old_cluster_sizes, new_pi, new_covs,new_resid_ops)
  delta <- new_obj - old_obj
  
  # organize the output
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
