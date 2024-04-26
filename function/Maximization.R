Maximization = function (theta0, Update, maxit, eps, report) 
{
  for (i in 1:maxit) {
    theta1 <- Update(theta0)
    if (theta1$delta > 0) {
      theta0 <- theta1
      if (report) {
        cat("Objective increment: ", signif(theta1$d, 
                                            digits = 3), "\n")
      }
    }
    if (theta1$delta < eps) {
      if (i == 1) {
        theta0$new_obj <- theta1$old_obj
      }
      break
    }
  }
  if (report) {
    if (i < maxit) {
      cat(paste0(i - 1, " update(s) performed before reaching tolerance limit.\n\n"))
    }
    else {
      cat(paste0(i, " update(s) performed without reaching tolerance limit.\n\n"))
    }
  }
  return(theta0)
}
