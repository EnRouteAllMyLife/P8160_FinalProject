
GMM=function (data, k = 2, maxit = 1000, eps = 1e-06,  report = TRUE) 
{
  d <- ncol(data)
  idx <- seq(1:nrow(data))
  is_comp <- stats::complete.cases(data)
  
  data_comp <- data[is_comp, , drop = FALSE]
  data_incomp <- data[!is_comp, , drop = FALSE]
  
  idx_comp <- idx[is_comp]
  idx_incomp <- idx[!is_comp]
  init_order <- c(idx_comp, idx_incomp)
  
  n_row <- nrow(data)
  n_col <- ncol(data)
  n0 <- nrow(data_comp)
  n1 <- nrow(data_incomp)
  
# Initialization using k-means
  theta0 <- Initialization(n0, n1, k,data_comp, data_incomp,d)
# Expectation
  Update <- function(theta) {
    ThetaUpdate(n0, n1, k,d,data_comp, data_incomp,theta)
  }
# Maximization  
  theta1 <- Maximization(theta0, Update, maxit, eps, report)
# Assign Clusters
  resp <- rbind(theta1$gamma$gamma0, theta1$gamma$gamma1)
  dens <- rbind(theta1$gamma$dens_eval0, theta1$gamma$dens_eval1)
  map_assign <- apply(resp, 1, which.max)
  map_assign <- map_assign[order(init_order)]
  
  resp <- resp[order(init_order), ]
  dens <- dens[order(init_order), ]
  
  imputed <- Impute(n0,n1,d,k,data_comp,data_incomp,init_order, theta1)
  
  
  
  
  out <- list(Data = cbind(df_raw,data.frame(assignments = map_assign)),
              Density = dens,
              Responsibilities = resp,
              Complete_data = cbind(imputed,data.frame(assignments = map_assign)),
              Means = theta1$means, 
              Covariances = theta1$covs, 
              Objective = theta1$new_obj,
              Proportions = theta1$pi,
              BIC = log(n_row)* (d * k) - 2*theta1$new_obj )
  return(out)
}
