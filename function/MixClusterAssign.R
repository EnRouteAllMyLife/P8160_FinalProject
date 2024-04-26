MixClusterAssign= function(d,k,init_order, theta) 
{
 
  resp <- rbind(theta$gamma$gamma0, theta$gamma$gamma1)
  dens <- rbind(theta$gamma$dens_eval0, theta$gamma$dens_eval1)
  map_assign <- apply(resp, 1, which.max)
  #init_order <- split_data$init_order
  map_assign <- map_assign[order(init_order)]
  
  resp <- resp[order(init_order), ]
  rownames(resp) <- NULL
  
  entropy <- plyr::aaply(.data = resp, .margins = 1, .fun = function(x) {
    -sum(x * log(x))/log(k)
  })
  
  dens <- dens[order(init_order), ]
  rownames(dens) <- NULL
  assign <- cbind(Assignments = map_assign, Entropy = entropy)

  out <- list()
  out$Assignments <- assign
  out$Responsibilities <- resp
  out$Density <- dens
  return(out)
}
