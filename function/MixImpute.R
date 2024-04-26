MixImpute = function (split_data, theta) 
{
  n0 <- split_data$n0
  n1 <- split_data$n1
  d <- split_data$n_col
  k <- theta$gamma$k
  out <- matrix(NA, nrow = 0, ncol = d)
  if (n0 > 0) {
    out <- rbind(out, split_data$data_comp)
  }
  if (n1 > 0) {
    data_imp <- lapply(seq_len(k), function(j) {
      working_response <- WorkResp(split_data$data_incomp, 
                                   theta$means[[j]], theta$covs[[j]], theta$gamma$gamma1[, 
                                                                                         j])
      return(working_response)
    })
    data_imp <- Reduce("+", data_imp)
    out <- rbind(out, data_imp)
  }
  
  init_order <- split_data$init_order
  out <- out[order(init_order), , drop = FALSE]
  rownames(out) <- split_data$orig_row_names
  colnames(out) <- split_data$orig_col_names
  return(out)
}
