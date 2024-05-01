MeanPlot = function(df_mean_fit){
  p = df_mean_fit |> ggplot(aes(x = predictor, y = mean_value,
                                shape = subgroup, col = subgroup)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, col = "grey",lty = 2) +
    ylim(-1,1) + ylab("Mean Value") +  xlab("") +
    coord_flip()
  return(p)
}