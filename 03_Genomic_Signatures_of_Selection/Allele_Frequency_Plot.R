library(ggplot2)

# === Step 1: Load data ===
a <- read.table("AFMdd.txt", header = TRUE)

# === Step 2: Base plot ===
gg <- ggplot(a, aes(x = Bp)) +
  geom_line(aes(y = Milk, color = "Milk"), linewidth = 0.75) +
  geom_line(aes(y = Dual, color = "Dual"), linewidth = 0.75) +
  geom_line(aes(y = Draft, color = "Draft"), linewidth = 0.75) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1.00)) +
  scale_x_continuous(breaks = seq(16.65, 17.35, by = 0.10)) +
  theme_classic() +
  labs(x = "Chromosome 6 (Mb)", y = "Allele frequency", color = NULL) +
  theme(legend.position = "top") +
  scale_color_manual(values = c("Milk" = "blue", "Dual" = "black", "Draft" = "red"))

# === Step 3: Styling ===
gg1 <- gg +
  theme(
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    axis.text.y = element_text(face = "bold", size = 12, color = "black"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.0),
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12, color = "black")
  )

# === Step 4: Highlight regions ===
gg2 <- gg1 +
  # Main gene regions
  annotate('rect', xmin = 16.65, xmax = 16.725571, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') + # COL25A1
  annotate('rect', xmin = 16.761040, xmax = 16.779409, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') + # ETNPPL
  annotate('rect', xmin = 16.840088, xmax = 16.851177, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') + # OSTC
  annotate('rect', xmin = 17.284328, xmax = 17.35, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') +     # LEF1
  # Left margin shading
  annotate('rect', xmin = -Inf, xmax = 16.65, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') +
  # Right margin shading
  annotate('rect', xmin = 17.35, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .1, fill = 'black') +
  # Vertical markers
  geom_vline(xintercept = 16.670875, col = "black", lty = 3, lwd = 0.75) +
  geom_vline(xintercept = 16.813733, col = "black", lty = 3, lwd = 0.75)

# === Step 5: Save plot ===
ggsave("AF_81fn.pdf", gg2, width = 8, height = 3.25, units = "in", bg = "white")