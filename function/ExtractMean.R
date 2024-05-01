ExtractMean = function(fit, k){
  df_mean = NULL
  for (i in 1:k){
    df_mean = rbind(df_mean,fit$Means[[i]])
  }
  df_mean = t(df_mean)
  colnames(df_mean) = paste0('Cluster_',1:k)
  df_mean = data.frame(df_mean)
  df_mean$predictor = rownames(df_mean)
  rownames(df_mean) = NULL
  df_mean = df_mean |>pivot_longer(cols = starts_with("Cluster"), names_to = "subgroup", values_to = "mean_value")
  
  return(df_mean)
  # colnames(df_mean)
}
