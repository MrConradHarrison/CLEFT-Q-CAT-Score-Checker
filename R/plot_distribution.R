makePlot <- function(score_distribution, score, percentile) {
  ggplot(score_distribution) +
    geom_density(aes(x = Score), alpha = 0.1, colour = "darkmagenta", fill = "darkmagenta") +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    ) +
    ylab(glue::glue("Frequency (n = {nrow(score_distribution)})")) +
    geom_vline(aes(xintercept = score), color = "darkblue", linetype = "longdash") +
    annotate(geom = "label",
             label = glue::glue("Percentile {percentile}"),
             x = score + positionLabelx(score),
             y = 0.75 * (max((density(score_distribution$Score)$y))),
             color = "black",
             size = 5,
             label.size = 0.35,
             label.padding = unit(0.55, "lines"))
}
