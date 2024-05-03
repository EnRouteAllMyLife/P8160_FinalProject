WorkResp = function (data_incomp, mean, cov, gamma = NULL) 
{
  n <- nrow(data_incomp)
  if (is.null(gamma)) {
    gamma <- rep(1, n)
  }
  out <- lapply(seq_len(n), function(i) {
    WorkRespIndiv(data_incomp[i, ], mean, cov, gamma[i])
  })
  out <- do.call(rbind, out)
  return(out)
}

