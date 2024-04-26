MixClusterAssign= function(split_data, theta) 
{
 # n2 <- split_data$n2
  d <- split_data$n_col
  k <- theta$gamma$k
  resp <- rbind(theta$gamma$gamma0, theta$gamma$gamma1)
  dens <- rbind(theta$gamma$dens_eval0, theta$gamma$dens_eval1)
  map_assign <- apply(resp, 1, which.max)
  init_order <- split_data$init_order
  map_assign <- map_assign[order(init_order)]
  names(map_assign) <- split_data$orig_row_names
 
  resp <- resp[order(init_order), ]
  rownames(resp) <- NULL
  entropy <- plyr::aaply(.data = resp, .margins = 1, .fun = function(x) {
    -sum(x * log(x))/log(k)
  })
  
  dens <- dens[order(init_order), ]
  rownames(dens) <- NULL
  assign <- cbind(Assignments = map_assign, Entropy = entropy)
  rownames(assign) <- split_data$orig_row_names
  rownames(resp) <- split_data$orig_row_names
  rownames(dens) <- split_data$orig_row_names
  out <- list()
  out$Assignments <- assign
  out$Responsibilities <- resp
  out$Density <- dens
  return(out)
}
