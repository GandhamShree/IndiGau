# ---------------------------------------------------------------
# Breed-wise XGBoost Heatmap (R / ggplot2 version)
#
# Required packages:
#   install.packages(c("dplyr", "tidyr", "ggplot2", "RColorBrewer", "scales"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

# ---------- 1. Load & aggregate ----------
df <- read.csv("Blind_XGB_Validation_Predictions.csv",
               stringsAsFactors = FALSE)

agg <- df %>%
  group_by(FID) %>%
  summarise(
    n             = n(),
    Top1_Prob     = mean(Top1_Prob),
    Margin        = mean(Margin),
    Top2_Prob     = mean(Top2_Prob),
    Norm_Entropy  = mean(Norm_Entropy),
    Admixed_Score = mean(Admixed_Score),
    Pure_pct       = mean(Confidence_Tier == "Pure") * 100,
    Borderline_pct = mean(Confidence_Tier == "Uncertain - Borderline") * 100,
    Admixed_pct    = mean(Confidence_Tier == "Likely Admixed") * 100,
    HighConf_pct   = mean(Confidence_Tier == "High Confidence Admixed") * 100
  ) %>%
  arrange(desc(Admixed_Score))  # top row = highest score

breeds   <- agg$FID
n_breeds <- nrow(agg)
agg$row  <- seq_len(n_breeds)            # 1 = top row
agg$y0   <- n_breeds - agg$row           # bottom edge (ggplot y goes up)
agg$y1   <- agg$y0 + 1
agg$yc   <- agg$y0 + 0.5

# ---------- 2. Column metadata (mirrors the Python `left_cols`/`right_cols`) ----------
left_cols <- list(
  list(col = "Top1_Prob",     label = "Top-1\nProb.",         pal = "Blues",  vmin = 0, vmax = 1),
  list(col = "Margin",        label = "Margin",               pal = "Blues",  vmin = 0, vmax = 1),
  list(col = "Top2_Prob",     label = "Top-2\nProb.",         pal = "Purples",vmin = 0, vmax = max(agg$Top2_Prob)),
  list(col = "Norm_Entropy",  label = "Norm.\nEntropy",       pal = "Reds",   vmin = 0, vmax = 1),
  list(col = "Admixed_Score", label = "Admixed\nScore (/6)",  pal = "Reds",   vmin = 0, vmax = 6)
)

right_cols <- list(
  list(col = "Pure_pct",       label = "Pure\n%",              pal = "Greens", vmin = 0, vmax = 100, hdr_col = "#1a7a3c"),
  list(col = "Borderline_pct", label = "Borderline\n%",        pal = "YlOrBr", vmin = 0, vmax = 100, hdr_col = "#b8860b"),
  list(col = "Admixed_pct",    label = "Admixed\n%",           pal = "Oranges",vmin = 0, vmax = 100, hdr_col = "#d2691e"),
  list(col = "HighConf_pct",   label = "High Conf.\nAdmixed %",pal = "Reds",   vmin = 0, vmax = 100, hdr_col = "#b22222")
)

# ---------- 3. Layout: x-boundaries for each column ----------
left_w  <- 1.0
gap     <- 0.5
right_w <- 1.0

left_bounds <- list()
x <- 0
for (i in seq_along(left_cols)) {
  left_bounds[[i]] <- c(x, x + left_w)
  x <- x + left_w
}
x_after_left <- x
x <- x + gap

right_bounds <- list()
for (i in seq_along(right_cols)) {
  right_bounds[[i]] <- c(x, x + right_w)
  x <- x + right_w
}
total_w <- x
sep_x   <- x_after_left + gap / 2

# ---------- 4. Helper: map a value -> hex color from a brewer palette ----------
get_color <- function(value, pal_name, vmin, vmax) {
  ramp <- colorRampPalette(brewer.pal(9, pal_name))(101)
  pos  <- round(rescale(value, to = c(0, 100), from = c(vmin, vmax)))
  pos  <- pmin(100, pmax(0, pos))
  ramp[pos + 1]
}

# luminance-based text color for contrast against a fill color
text_color_for <- function(hex, threshold = 0.6) {
  rgb_val <- col2rgb(hex) / 255
  lum <- 0.299 * rgb_val[1, ] + 0.587 * rgb_val[2, ] + 0.114 * rgb_val[3, ]
  ifelse(lum < threshold, "white", "#222222")
}

# ---------- 5. Build the long data frame of cells ----------
cells <- list()

for (i in seq_along(left_cols)) {
  spec <- left_cols[[i]]
  xb   <- left_bounds[[i]]
  vals <- agg[[spec$col]]
  fillcol <- get_color(vals, spec$pal, spec$vmin, spec$vmax)
  cells[[length(cells) + 1]] <- data.frame(
    xmin = xb[1], xmax = xb[2], ymin = agg$y0, ymax = agg$y1,
    fill = fillcol,
    label = sprintf("%.2f", vals),
    textcol = text_color_for(fillcol, 0.6),
    yc = agg$yc,
    part = "left"
  )
}

for (i in seq_along(right_cols)) {
  spec <- right_cols[[i]]
  xb   <- right_bounds[[i]]
  vals <- agg[[spec$col]]
  fillcol <- get_color(vals, spec$pal, spec$vmin, spec$vmax)
  lbl <- ifelse(vals >= 0.5, sprintf("%.0f%%", vals), "")
  cells[[length(cells) + 1]] <- data.frame(
    xmin = xb[1], xmax = xb[2], ymin = agg$y0, ymax = agg$y1,
    fill = fillcol,
    label = lbl,
    textcol = text_color_for(fillcol, 0.55),
    yc = agg$yc,
    part = "right"
  )
}

cells_df <- bind_rows(cells)

# breed row labels
row_labels <- data.frame(
  x = -0.15, y = agg$yc,
  label = sprintf("%s  (n=%d)", agg$FID, agg$n)
)

# ---------- 6. Header labels ----------
header_y <- n_breeds + 0.55
left_headers <- data.frame(
  x = sapply(left_bounds, function(b) mean(b)),
  y = header_y,
  label = sapply(left_cols, function(s) s$label),
  col = "#1f4e8c"
)
right_headers <- data.frame(
  x = sapply(right_bounds, function(b) mean(b)),
  y = header_y,
  label = sapply(right_cols, function(s) s$label),
  col = sapply(right_cols, function(s) s$hdr_col)
)

# ---------- 7. Plot ----------
p <- ggplot() +
  geom_rect(data = cells_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
            color = "white", linewidth = 1) +
  scale_fill_identity() +
  geom_text(data = subset(cells_df, part == "left"),
            aes(x = (xmin + xmax) / 2, y = yc, label = label, color = textcol),
            size = 5.2) +
  geom_text(data = subset(cells_df, part == "right"),
            aes(x = (xmin + xmax) / 2, y = yc, label = label, color = textcol),
            size = 5.6, fontface = "bold") +
  scale_color_identity() +
  geom_text(data = row_labels, aes(x = x, y = y, label = label),
            hjust = 1, fontface = "bold", size = 6.3) +
  geom_text(data = left_headers, aes(x = x, y = y, label = label, color = col),
            fontface = "bold", size = 4.6, lineheight = 0.9) +
  geom_text(data = right_headers, aes(x = x, y = y, label = label, color = col),
            fontface = "bold", size = 4.6, lineheight = 0.9) +
  annotate("text", x = mean(unlist(left_bounds[c(1, length(left_bounds))])),
           y = n_breeds + 1.05, label = "Classifier Confidence Metrics",
           fontface = "bold", size = 6.2) +
  annotate("text", x = mean(unlist(right_bounds[c(1, length(right_bounds))])),
           y = n_breeds + 1.05, label = "Confidence Category Composition",
           fontface = "bold", size = 6.2) +
  coord_cartesian(xlim = c(-2.6, total_w + 0.3), ylim = c(-0.15, n_breeds + 1.75), clip = "off") +
  labs(title = "Probability-based Uncertainty Reference Evaluation: PURE") +
  theme_void() +
  theme(
    plot.margin = margin(15, 5, 5, 5),
    plot.title  = element_text(face = "bold", size = 18, hjust = 0.5,
                                margin = margin(b = 12))
  )

ggsave("breed_admixture_heatmap_R.png", p,
       width = 13.8, height = 8.5, dpi = 200, bg = "white")

cat("Saved to:", file.path(getwd(), "breed_admixture_heatmap_R.png"), "\n")
