EvalFunc = function(fit, k){
  df_auc = NULL
  df_tpr= NULL
  
  df_complete = fit$Complete_data |>mutate(outcome = df_raw[,'outcome'])
  for (i in 0:k){
    print(i)
    if (i == 0){
      data = df_complete|> dplyr::select(-assignments)
      actuals = df_complete |> pull(outcome) 
      cluster_name = "Total"
    }
    else{
      data = df_complete|> filter(assignments == i) |> dplyr::select(-assignments)
      actuals = df_complete|> filter(assignments == i) |> pull(outcome)
      cluster_name = paste0("Cluster_",i)}
    
    
    glm.fit = glm(outcome ~ ., data = data, family = binomial)
    # Predict on the test data
    probabilities = predict(glm.fit, newdata = data, type = "response")
    
    # Calculate AUC
    roc_result = roc(actuals, probabilities)
    print(auc(roc_result))
    roc_data = data.frame( FPR = roc_result$specificities,
                           TPR = roc_result$sensitivities)
    roc_data$subgroup = cluster_name
    df_auc = rbind(df_auc, roc_data)
    
    # Confusion Matrix
    cm = table(actuals,probabilities>0.5)
    tpr_data = data.frame(Cluster = cluster_name,
                          Cnt = sum(cm),
                          Prevalence = (cm[2,2] + cm[2,1])/sum(cm),
                          TPR = cm[2,2]/(cm[2,2]+cm[1,2]),
                          TNR = cm[1,1]/(cm[1,1]+cm[2,1]),
                          AUC = auc(roc_result))
    df_tpr = rbind(df_tpr,tpr_data)
  }
  
  p = ggplot(df_auc, aes(x = 1 - FPR, y = TPR, col = subgroup)) +
    geom_line() +
    geom_abline(linetype = "dashed") +
    labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") 
  
  return(list(auc_plot = p,
              df_tpr = df_tpr))
}
