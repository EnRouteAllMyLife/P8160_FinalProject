
WorkRespIndiv = function (y, mean, cov, gamma) 
{
  is_mis <- is.na(y)
  is_obs <- !is_mis
  if (!any(is_mis)) {
    return(y)
  }
  perm <- c(which(is_obs), which(is_mis))
  rev_perm <- order(perm)
  cov_mis_obs <- cov[is_mis, is_obs, drop = FALSE]
  var_mis_mis <- cov[is_obs, is_obs, drop = FALSE]
  var_mis_mis_inv <- MASS::ginv(var_mis_mis)
  obs_ele <- matrix(y[is_obs], ncol = 1)
  mis_ele <- mean[is_mis] + cov_mis_obs %*%(var_mis_mis_inv %*%(obs_ele - mean[is_obs]))
  working_response <- rbind(obs_ele, mis_ele)
  out <- gamma * working_response[rev_perm]
  return(out)
}
