## ============================================================
## Breed Prediction Accuracy Plot (with Wilson 95% CI error bars)
## ============================================================

## ---- 0. Packages ----
required_pkgs <- c("ggplot2", "dplyr")
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)

library(ggplot2)
library(dplyr)

## ---- 1. Read your data ----
## Expects (at minimum) two columns:
##   FID              = TRUE breed of the sample
##   Predicted_Label  = breed predicted by the model
csv_path <- "Blind_XGB_Validation_Predictions.csv"   # <-- change path if needed
df <- read.csv(csv_path, stringsAsFactors = FALSE)

## Drop any fully blank trailing rows (can happen with Excel-exported CSVs)
df <- df[df$FID != "" & !is.na(df$FID), ]

## ---- 2. Wilson 95% CI function for a binomial proportion ----
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  phat   <- x / n
  denom  <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  margin <- z * sqrt((phat * (1 - phat) / n) + (z^2 / (4 * n^2))) / denom
  data.frame(
    accuracy = phat,
    lower    = pmax(0, center - margin),
    upper    = pmin(1, center + margin)
  )
}

## ---- 3. Compute per-breed accuracy + CI ----
acc_df <- df %>%
  group_by(FID) %>%
  summarise(
    n       = n(),
    correct = sum(Predicted_Label == FID),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    accuracy = wilson_ci(correct, n)$accuracy,
    lower    = wilson_ci(correct, n)$lower,
    upper    = wilson_ci(correct, n)$upper
  ) %>%
  ungroup() %>%
  rename(Breed = FID)

## Order breeds alphabetically (Sahiwal/any new breed just slots in automatically)
acc_df$Breed <- factor(acc_df$Breed, levels = sort(unique(acc_df$Breed)))

## ---- 4. Color palette (extend/edit as needed for your breed count) ----
breed_colors <- c(
  "#F4847B", "#D99828", "#9DA836", "#4CA64C",
  "#2EA6A0", "#3E8FE0", "#9B7FE0", "#E06FA8",
  "#7A6BD9", "#D9C23E", "#5BBF8A"
)
n_breeds <- nlevels(acc_df$Breed)
plot_colors <- breed_colors[seq_len(n_breeds)]
names(plot_colors) <- levels(acc_df$Breed)

## ---- 5. Plot ----
p <- ggplot(acc_df, aes(x = Breed, y = accuracy, color = Breed)) +
  geom_point(size = 12) +
  ## error bar drawn AFTER the point, so the line sits on top of the dot
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, color = "black", linewidth = 0.9) +
  scale_color_manual(values = plot_colors, guide = "none") +
  scale_y_continuous(limits = c(0.2, 1), breaks = c(0.2, 0.4, 0.6, 0.8, 1.0),
                     labels = c("0.2", "0.4", "0.6", "0.8", "1.0")) +
  labs(x = "Breed", y = "Accuracy") +
  theme_classic(base_size = 28) +
  theme(
    axis.title       = element_text(face = "bold", size = 30),
    axis.text.x      = element_text(face = "bold", angle = 45, hjust = 1, color = "black", size = 26),
    axis.text.y      = element_text(face = "bold", color = "black", size = 26),
    axis.line        = element_line(color = "black", linewidth = 0.8),
    axis.ticks       = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.25)
  )

print(p)

## ---- 6. Save ----
ggsave("breed_accuracy_plot.png", p, width = 8, height = 6, dpi = 300)
