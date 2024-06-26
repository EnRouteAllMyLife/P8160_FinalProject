# Calculates the posterior probability of cluster membership given the observed data

Responsibility = function (n0, n1, k,data_comp, data_incomp, means, covs, pi) {
  
  out <- list()
  
  if (n0 > 0) {
    dens_eval0 <- lapply(seq_len(k), function(j) {
      mvnfast::dmvn(X = data_comp, mu = means[[j]], 
                    sigma = covs[[j]]) * pi[j]
    })
    dens_eval0 <- do.call(cbind, dens_eval0)
    # standarized to 1
    gamma0 <- plyr::aaply(.data = dens_eval0, .margins = 1, 
                          .fun = function(x) {
                            x/sum(x)
                          }, .drop = FALSE)
    # rename
    colnames(dens_eval0) <- colnames(gamma0) <- paste0("k",seq_len(k))

    out$dens_eval0 <- dens_eval0
    out$gamma0 <- gamma0
  }
  if (n1 > 0) {
    
    dens_eval1 <- plyr::aaply(.data = data_incomp, .margins = 1, 
                              .fun = function(x) {
                                EvalDensIncompObs(x, means, covs, pi)
                              }, .drop = FALSE)
    gamma1 <- plyr::aaply(.data = dens_eval1, .margins = 1, 
                          .fun = function(x) {
                            x/sum(x)
                          }, .drop = FALSE)
    colnames(dens_eval1) <- colnames(gamma1) <- paste0("k", seq(1:k))
    rownames(dens_eval1) <- rownames(gamma1) <- seq(1:n1)
    out$dens_eval1 <- dens_eval1
    out$gamma1 <- gamma1
  }
  return(out)
}
