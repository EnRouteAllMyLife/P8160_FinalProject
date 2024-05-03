EvalDensIncompObs = function (y, means, covs, pi) 
{
  k <- length(pi)
  is_obs <- !is.na(y)
  obs_ele = y[is_obs]
  obs_dens <- lapply(seq_len(k), function(j) {
    obs_mean <- means[[j]][is_obs]
    obs_cov <- covs[[j]][is_obs, is_obs]
    out <- mvnfast::dmvn(X = obs_ele, mu = obs_mean, sigma = obs_cov) * pi[j]
    return(out)
  })
  obs_dens <- unlist(obs_dens)
  return(obs_dens)
}

#y = data_incomp[1,]
