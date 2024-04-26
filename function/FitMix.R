
FitMix=function (data, k = 2, maxit = 1000, eps = 1e-06,  report = TRUE) 
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
  
# Initialization
  theta0 <- MixInit(n0, n1, k,data_comp, data_incomp,d)
# Expectation
  Update <- function(theta) {
    MixUpdate(n0, n1, k,d,data_comp, data_incomp,theta)
  }
# Maximization  
  theta1 <- Maximization(theta0, Update, maxit, eps, report)

  assign <- MixClusterAssign(d,k,init_order, theta1)
  #imputed <- MixImpute(n0,n1,d,k,data_comp,data_incomp,init_order, theta1)
  
  
  
  
  out <- list(Assignments = assign$Assignments, 
              Density = assign$Density,
              Responsibilities = assign$Responsibilities,
              Covariances = theta1$covs, 
              Proportions = theta1$pi)
  return(out)
}
