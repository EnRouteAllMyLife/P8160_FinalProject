#-------------------------------------------------------------------------------
# Expected Residual Outer Product. 
#-------------------------------------------------------------------------------

ExpResidOP <- function (data_incomp, new_mean, old_mean, old_cov, gamma = NULL) 
{
  d <- ncol(data_incomp)
  n <- nrow(data_incomp)
  if (is.null(gamma)) {
    gamma <- rep(1, n)
  }
  out <- lapply(seq_len(n), function(i) {
    y <- data_incomp[i, ]
    is_mis <- is.na(y)
    is_obs <- !is_mis
    perm <- c(which(is_obs), which(is_mis))
    rev_perm <- order(perm)
    n_obs <- sum(is_obs)
    var_mis_mis <- old_cov[is_mis, is_mis, drop = FALSE]
    cov_mis_obs <- old_cov[is_mis, is_obs, drop = FALSE]
    var_obs_obs <- old_cov[is_obs, is_obs, drop = FALSE]
    var_obs_obs_inv <- MASS::ginv(var_obs_obs)
    
    pre_mis_mis_inv <- var_mis_mis - cov_mis_obs %*% solve(var_obs_obs, t(cov_mis_obs))
    obs_ele <- matrix(y[is_obs], ncol = 1)
    miss_ele <- old_mean[is_mis] + cov_mis_obs%*%(var_obs_obs_inv%*%(obs_ele - old_mean[is_obs]))
    working_response <- rbind(obs_ele, miss_ele)
    residual <- working_response - new_mean[perm]
    resid_OP <- residual%*% t(residual)
    idx <- seq(from = n_obs + 1, to = d)
    resid_OP[idx, idx] <- resid_OP[idx, idx] + pre_mis_mis_inv
    resid_OP <- resid_OP[rev_perm, rev_perm]
    out <- gamma[i] * resid_OP
    return(out)
  })
  out <- Reduce("+", out)
  return(out)
}
