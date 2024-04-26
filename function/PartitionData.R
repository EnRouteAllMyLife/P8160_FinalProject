# Split the data into complete, incomplete, (completely) empty
PartitionData = function (data) 
{
  d <- ncol(data)
  idx <- seq(1:nrow(data))
  is_comp <- stats::complete.cases(data)

  data_comp <- data[is_comp, , drop = FALSE]
  data_incomp <- data[!is_comp, , drop = FALSE]
  
  idx_comp <- idx[is_comp]
  idx_incomp <- idx[!is_comp]
  
  out <- list()
  out$n_row <- nrow(data)
  out$n_col <- ncol(data)
  out$n0 <- nrow(data_comp)
  out$n1 <- nrow(data_incomp)
  out$data_comp <- data_comp
  out$data_incomp <- data_incomp
  out$idx_comp <- idx_comp
  out$idx_incomp <- idx_incomp
  out$init_order <- c(idx_comp, idx_incomp)
  return(out)
}

