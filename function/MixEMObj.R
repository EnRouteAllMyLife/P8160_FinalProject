# EM Objective for a Gaussian Mixture Model

# cluster_sizes Cluster sizes.
# pi Cluster proportions
# covs List of component covariances. 
# resid_ops List of residual outer products.

MixEMObj = function (cluster_sizes, pi, covs, resid_ops) 
{
  k <- length(pi)
  pi_term <- sum(cluster_sizes * log(pi))
  det_term <- lapply(1:k, function(j) {
    cluster_sizes[j] * log(det(covs[[j]]))
  })
  det_term <- do.call(sum, det_term)
  trace_term <- lapply(1:k, function(j) {
    sum(diag((MASS::ginv(covs[[j]])%*% (resid_ops[[j]]))))
  })
  trace_term <- do.call(sum, trace_term)
  obj <- pi_term - det_term - trace_term
  return(obj)
}

# the generalized inverse (pseudoinverse) of a matrix MASS::ginv(A)