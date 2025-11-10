# Function to compute simple ROC
simple_roc <- function(labels, scores) {
  df <- data.frame(labels = labels, scores = scores)
  df <- df[order(-df$scores), ]
  df$TP <- cumsum(df$labels)
  df$FP <- cumsum(!df$labels)
  TPR <- df$TP / sum(df$labels)
  FPR <- df$FP / sum(!df$labels)
  data.frame(TPR = TPR, FPR = FPR)
}

set.seed(1)
P <- 0.01
Ns <- seq(100, 5000, by = 500)
roc_frames <- lapply(Ns, function(N) {
  labels <- sample(c(TRUE, FALSE), N, replace = TRUE, prob = c(P, 1 - P))
  scores <- rep(0, N)  # degenerate predictor
  roc <- simple_roc(labels, scores)
  auc_val <- auc(labels, scores)
  roc$N <- N
  roc$AUC <- round(auc_val, 3)
  roc$frame <- paste0("N = ", N, " | AUC = ", round(auc_val, 3))
  roc
})

roc_df <- bind_rows(roc_frames)

# Plot and animate
p <- ggplot(roc_df, aes(x = FPR, y = TPR, group = frame)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve as N Increases\n{closest_state}",
       x = "False Positive Rate", y = "True Positive Rate") +
  transition_states(frame, transition_length = 2, state_length = 1) +
  ease_aes('linear')
p
# Save as GIF
anim <- animate(p, nframes = length(Ns) * 5, fps = 10, width = 600, height = 500, renderer = gifski_renderer())
anim_save("roc_animation.gif", animation = anim)

