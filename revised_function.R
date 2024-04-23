library(MASS)
set.seed(123123)
Sigma = matrix(c(1,0.5,0.5,1),2,2)
x1 = mvrnorm(n = 200, mu=c(0,0), Sigma)
Sigma = matrix(c(2,0.5,0.5,2),2,2)
x2 = mvrnorm(n = 200, mu=c(0,5), Sigma)
Sigma = matrix(c(3,0.5,0.5,3),2,2)
x3 = mvrnorm(n = 200, mu=c(6,4), Sigma)
xx = rbind(x1,x2,x3)
plot(xx)
xx[1,2] = NA
xx[3,1] = NA

ncluster = 3
data = xx

# add convergence
# add checks for possible singular matrices in the EM algorithm
# add count, BIC

EM_MG_algrm = function(data, ncluster,tolerance = 1e-6 ){
  
  #setting 
  # -1~1
  data = as.matrix(data) %>% scale()
  N = nrow(data)
  q = ncol(data)
  p_j = rep(1/ncluster, ncluster)
  mu =  data[sample(N, ncluster),  ] %>% as.matrix()   
  covmat = diag(ncol(data))
  
  covList = list()
  for(i in 1:ncluster){
    covList[[i]] = covmat
  }
  
  count = 1
  converged = FALSE
  prev_mu = mu
  while(!converged && count <1000){
    mu0 = mu
    # E-step: Evaluate posterior probability, gamma
    gamma = c()
    
    # Check if the covariance matrix is positive definite
    for(j in 1:ncluster){
     
      if (!all(eigen(covList[[j]])$values > 0)) {
        stop("Non-positive definite covariance matrix encountered")
      }
      gamma2 = dmvnorm(data, mean = mu[j,], sigma = covList[[j]])
      #gamma2 = apply(as.matrix(data),1, dmvnorm, mean = mu[j,], sigma = covList[[j]]) # 1 indicated rows
      gamma = cbind(gamma, gamma2)
    }
    
    # M- step: Calculate mu
    tempmat = matrix(rep(p_j,N),nrow=N,byrow = T)
    r = (gamma * tempmat) / rowSums(gamma * tempmat)  
    mu = t(r) %*% data / colSums(r) 
    
    # M- step: Calculate Sigma and p
    for(j in 1:ncluster){
      sigma = matrix(rep(0,q^2),ncol=q)
      for(i in 1:N){
        sigma = sigma + r[i,j] * (data[i,]-mu0[j,]) %*% t(data[i,]-mu0[j,])   
      }
      covList[[j]] = sigma/sum(r[,j]) 
    }
    p_j = colSums(r)/N
    # Check convergence based on the mean vectors
    if (max(abs(mu - prev_mu)) < tolerance) {
      converged <- TRUE
    }
    prev_mu <- mu  # Update previous mu
    count = count + 1
  }
  
  cluster = which(r == apply(r, 1, max), arr.ind = T)
  cluster = cluster[order(cluster[,1]),]
  log_likelihood = sum(log(rowSums(gamma * tempmat)))
  k = ncluster * q + ncluster * (q * (q + 1) / 2) + (ncluster - 1)  # Total number of parameters
  BIC = k * log(N) - 2 * log_likelihood
  
  return(list(mu = mu,covList = covList, p_j = p_j,cluster = cluster, iteration = count, BIC = BIC))
}


res2 = EM_MG_algrm(xx, 3)
