ResidOP = function (n0,n1,d,k, data_comp, data_incomp,new_means, old_means, covs, gamma) 
{
  out <- lapply(seq_len(k), function(j) {
    resid_op <- array(0, dim = c(d, d))
    if (n0 > 0) {
      mean_mat <- matrix(data = new_means[[j]], nrow = n0, 
                         ncol = d, byrow = TRUE)
      resid <- data_comp - mean_mat
      resid_op <- resid_op + t(resid) %*%(gamma$gamma0[,j] * resid)
    }
    if (n1 > 0) {
      resid_op <- resid_op + 
        ExpResidOP(data_incomp, new_means[[j]], old_means[[j]], covs[[j]], gamma$gamma1[, j])
    }
    return(resid_op)
  })
  return(out)
}
