# ==========================================================
# 🧬 Enhanced Multi-Metric Heatmap for Vechur vs All
# Showing r, CI_lower, CI_upper, p_value, RMSE, MAE
# ==========================================================

library(tidyverse)
library(patchwork)
library(ragg)
library(Cairo)

# === 1️⃣ Load & prepare data ===
df0 <- read.csv("Vechur_vs_All_Correlation.csv", check.names = FALSE)
if (!"File" %in% names(df0)) names(df0)[1] <- "File"

# Detect columns automatically
col_r    <- names(df0)[str_detect(names(df0), regex("pearson", ignore_case = TRUE))][1]
col_rmse <- names(df0)[str_detect(names(df0), regex("rmse", ignore_case = TRUE))][1]
col_mae  <- names(df0)[str_detect(names(df0), regex("mae",  ignore_case = TRUE))][1]
col_p    <- names(df0)[str_detect(names(df0), regex("p[_-]?val", ignore_case = TRUE))][1]
col_low  <- names(df0)[str_detect(names(df0), regex("lower", ignore_case = TRUE))][1]
col_up   <- names(df0)[str_detect(names(df0), regex("upper", ignore_case = TRUE))][1]

# Rename & tidy
df <- df0 %>%
  select(File, all_of(c(col_r, col_p, col_low, col_up, col_rmse, col_mae))) %>%
  rename(
    Pearson_r = all_of(col_r),
    p_value   = all_of(col_p),
    CI_lower  = all_of(col_low),
    CI_upper  = all_of(col_up),
    RMSE      = all_of(col_rmse),
    MAE       = all_of(col_mae)
  ) %>%
  mutate(
    Breed = str_replace(File, ".*range_([^./\\\\]+).*", "\\1"),
    Breed = if_else(Breed == File, tools::file_path_sans_ext(basename(File)), Breed)
  )

# === 2️⃣ Clean breed names ===
df$Breed <- df$Breed %>%
  str_replace_all("_", " ") %>%
  str_to_title()

df$Breed <- case_when(
  str_detect(df$Breed, regex("Krishna", ignore_case = TRUE)) ~ "Krishna Valley",
  str_detect(df$Breed, regex("Red Sindhi", ignore_case = TRUE)) ~ "Red Sindhi",
  str_detect(df$Breed, regex("Malnad", ignore_case = TRUE)) ~ "Malnad Gidda",
  str_detect(df$Breed, regex("Konkan", ignore_case = TRUE)) ~ "Konkan Kapila",
  TRUE ~ df$Breed
)

# === 3️⃣ Split into 3 panels ordered by r ===
df <- df %>%
  arrange(Pearson_r) %>%
  mutate(Panel = paste0("Panel ", ceiling(row_number() / ceiling(nrow(.) / 3)))) %>%
  group_by(Panel) %>%
  mutate(Breed = factor(Breed, levels = Breed[order(Pearson_r)])) %>%
  ungroup()

# === 4️⃣ Reshape for plotting ===
# New metric order — places CI and p near r
metrics_order <- c("MAE", "CI_lower", "Pearson_r", "CI_upper", "p_value", "RMSE")
metric_labels <- c("MAE", "CI", "r", "CI", "p", "RMSE")

plot_df <- df %>%
  select(Panel, Breed, all_of(metrics_order)) %>%
  pivot_longer(cols = all_of(metrics_order),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = metrics_order))

make_heatmap <- function(panel_label, data, ratio = 1.0, base_text_size = 3.6) {
  sub_df <- data %>% filter(Panel == panel_label)
  n_breeds <- nrow(sub_df)
  
  text_size <- if (n_breeds <= 10) base_text_size + 1.5
  else if (n_breeds <= 20) base_text_size + 0.5
  else if (n_breeds <= 30) base_text_size
  else if (n_breeds <= 40) base_text_size - 0.5
  else base_text_size - 1
  
  # 🧩 Format text: split p-values into two lines if in scientific notation
  sub_df <- sub_df %>%
    mutate(
      Label = case_when(
        Metric == "p_value" & !is.na(Value) ~ {
          # Split scientific notation into two lines (e.g. 3.01\nE−04)
          formatted <- format(Value, scientific = TRUE, digits = 2)
          gsub("e", "\ne", formatted, fixed = TRUE)
        },
        TRUE ~ ifelse(is.na(Value), "",
                      ifelse(abs(Value) < 0.001, format(Value, scientific = TRUE, digits = 2),
                             sprintf("%.2f", Value)))
      )
    )
  
  ggplot(sub_df, aes(x = Metric, y = Breed, fill = Value)) +
    geom_tile(color = "black", linewidth = 0.1) +
    geom_text(aes(label = Label),
              size = text_size, lineheight = 0.9, na.rm = TRUE) +
    
    scale_fill_gradientn(
      colours = c("#0072B2", "#F0F0F0", "#E69F00"),
      values = scales::rescale(c(-1, 0, 1)),
      limits = c(-1, 1),
      name = ""
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = 1.2,
        barheight = 25,
        frame.colour = "black",
        ticks.colour = "black",
        label.theme = element_text(size = 14, face = "bold"),
        title.theme = element_text(size = 14, face = "bold")
      )
    ) +
    scale_x_discrete(labels = c("MAE", "CI-", "r", "CI+", "p", "RMSE")) +
    coord_fixed(ratio = ratio) +
    labs(title = panel_label, x = NULL, y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.text.x = element_text(face = "bold", size = 16, color = "black"),
      axis.text.y = element_text(face = "bold", size = 16, color = "black"),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
    )
}
# === 6️⃣ Create three panels ===
p1 <- make_heatmap("Panel 1", plot_df, ratio = 1.0, base_text_size = 5.2)
p2 <- make_heatmap("Panel 2", plot_df, ratio = 1.0, base_text_size = 5.2)
p3 <- make_heatmap("Panel 3", plot_df, ratio = 1.0, base_text_size = 5.2)

final_plot <- p1 + p2 + p3 + plot_layout(nrow = 1, guides = "collect") &
  theme(legend.position = "right")

# === 7️⃣ Save high-quality output ===
ggsave("Vechur_vs_All_Panels_with_CI_Boxes.png", final_plot,
       width = 16, height = 8, dpi = 400, device = ragg::agg_png)
ggsave("Vechur_vs_All_Panels_with_CI_Boxes.pdf", final_plot,
       width = 16, height = 8, device = cairo_pdf)

cat("✅ Saved heatmap with CI_lower, r, CI_upper, p-value, RMSE, and MAE as separate boxes\n")