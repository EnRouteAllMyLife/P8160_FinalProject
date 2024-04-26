MixResidOP = function (split_data, new_means, old_means, covs, gamma) 
{
  n0 <- split_data$n0
  n1 <- split_data$n1
  d <- split_data$n_col
  k <- gamma$k
  out <- lapply(seq_len(k), function(j) {
    resid_op <- array(0, dim = c(d, d))
    if (n0 > 0) {
      mean_mat <- matrix(data = new_means[[j]], nrow = n0, 
                         ncol = d, byrow = TRUE)
      resid <- split_data$data_comp - mean_mat
      resid_op <- resid_op + t(resid) %*%(gamma$gamma0[,j] * resid)
    }
    if (n1 > 0) {
      resid_op <- resid_op + ExpResidOP(split_data$data_incomp, 
                                        new_means[[j]], old_means[[j]], covs[[j]], gamma$gamma1[, 
                                                                                                j])
    }
    return(resid_op)
  })
  return(out)
}
