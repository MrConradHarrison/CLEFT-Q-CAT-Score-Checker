radarPlot <- function(data, scores_df) {

  radar_data =
    data %>%
    group_by(Scale) %>%
    summarise(median = median(Score, na.rm = TRUE)) %>%
    full_join(scores_df, by = "Scale") %>%
    mutate(median = replace_na(median, 0)) %>%
    mutate(max = 100, min = 0) %>%
    pivot_longer(-Scale, names_to = "type") %>%
    pivot_wider(names_from = Scale, values_from = value) %>%
    arrange(factor(type, levels = c("max", "min", "median", "score")), desc(type)) %>%
    select(c("Face", "Nose", "Nostrils", "Jaws", "Lips", "Teeth", "Scar", "School",
             "Social Function", "Psychological Function", "Speech function", "Speech distress"))

  radarchart(as.data.frame(radar_data),
             axistype = 1, caxislabels = c("0", "25", "50", "75", "100"), maxmin = T,
             pcol = c("red", "darkmagenta"), pfcol = c(alpha("red", 0.000001), alpha("darkmagenta", 0.2)), plwd = 2, plty = c(2, 1),
             # custom the grid
             cglcol = "lightgrey", cglty = 1, axislabcol = "black", cglwd = 0.8,
             # custom labels
             vlcex = 1.1, xaxs = "i"
  )
  legend(x = 1, legend = c("Median scores", "Respondent"), y = -0.95, bty = "n", lty = c(2, 1), lwd = 2, col = c("red", "darkmagenta"), text.col = "black", cex = 1.1, pt.cex = 1)
}
