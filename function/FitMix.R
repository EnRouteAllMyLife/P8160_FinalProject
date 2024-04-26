
FitMix=function (data, k = 2, maxit = 100, eps = 1e-06,  report = FALSE) 
{
  split_data <- PartitionData(data)
  theta0 <- MixInit(split_data, k)
  Update <- function(theta) {
    MixUpdate(split_data, theta)
  }
  
  theta1 <- Maximization(theta0, Update, maxit, eps, report)
  assign <- MixClusterAssign(split_data, theta1)
  imputed <- MixImpute(split_data, theta1)
  
  
  
  
  out <- list(Assignments = assign$Assignments, 
                      Completed = imputed, Components = k, Covariances = theta1$covs, 
                      Data = data, Density = assign$Density, Means = theta1$means, 
                      Objective = theta1$new_obj, Proportions = theta1$pi, 
                      Responsibilities = assign$Responsibilities)
  return(out)
}
