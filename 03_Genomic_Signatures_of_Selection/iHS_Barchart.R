# Load library
library(ggplot2)

# Data
df <- data.frame(
  Breed = c("Kankrej","Gir","Ongole","Malnad Gidda","Vechur"),
  Value = c(-0.948269,-0.48434,0.0375313,1.24142,2.57526)
)

# Define custom border colors
my_colors <- c(
  "Vechur" = "red",
  "Ongole" = "black",
  "Gir" = "blue",
  "Kankrej" = "yellow3",
  "Red Sindhi" = "purple",
  "Malnad Gidda" = "dodgerblue"
)


# Reorder Breeds by Value (increasing order)
df$Breed <- factor(df$Breed, levels = df$Breed[order(df$Value)])

# Format values with 2 decimals
df$Label <- formatC(df$Value, format = "f", digits = 2)

# Mark which need an asterisk
df$Star <- ifelse(abs(df$Value) > 2, "*", "")

# Open PDF
pdf("CORIN_BarChart.pdf", width = 6, height = )

# Plot
ggplot(df, aes(x = Breed, y = Value, color = Breed)) +   
  geom_bar(stat = "identity", width = 0.65, fill = NA, size = 2.5) +
  
  # Values
  geom_text(aes(label = Label, color = Breed),
            vjust = ifelse(df$Value >= 0, -0.7, 1.6),
            size = 7.5, fontface = "bold", show.legend = FALSE) +
  
  # Bigger asterisks
  geom_text(aes(label = Star, color = Breed),
            vjust = ifelse(df$Value >= 0, -0.7, 2.2),  
            size = 12, fontface = "bold", show.legend = FALSE) +
  
  scale_color_manual(values = my_colors) +
  scale_y_continuous(
    limits = c(-1.5, 3.0),               
    breaks = seq(-3.5, 3.0, 0.5)         
  ) +
  labs(
    x = "Breeds",
    y = "Normalized iHS values"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.title.x = element_text(size = 22, face = "bold"),
    axis.title.y = element_text(size = 22, face = "bold"),
    axis.text.x  = element_text(size = 20, color ="black", face = "bold", angle = 45, hjust = 1),
    axis.text.y  = element_text(size = 20, color ="black", face = "bold"),
    legend.position = "none",
    panel.border   = element_rect(colour = "black", fill = NA, linewidth = 2),
    axis.ticks     = element_line(linewidth = 1.2),
    axis.line      = element_line(linewidth = 1.2, colour = "black")
  )

# Close PDF
dev.off()