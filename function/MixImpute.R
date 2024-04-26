MixImpute = function (n0,n1,d,k,data_comp,data_incomp,init_order, theta) 
{
  
  out <- matrix(NA, nrow = 0, ncol = d)
  if (n0 > 0) {
    out <- rbind(out, data_comp)
  }
  if (n1 > 0) {
    data_imp <- lapply(seq_len(k), function(j) {
      working_response <- WorkResp(data_incomp, theta$means[[j]], theta$covs[[j]], theta$gamma$gamma1[,j])
      return(working_response)
    })
    data_imp <- Reduce("+", data_imp)
    out <- rbind(out, data_imp)
  }

  out <- out[order(init_order), , drop = FALSE]
 # rownames(out) <- split_data$orig_row_names
 # colnames(out) <- split_data$orig_col_names
  return(out)
}
