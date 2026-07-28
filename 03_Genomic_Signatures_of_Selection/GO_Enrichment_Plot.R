library(ggplot2)
library(stringr)
data <- read.delim("fa.txt", header = TRUE, sep = "\t")
gg <- ggplot(data, aes(x = -log10(PValue), y = GO_Term, size = Ratio, fill = -log10(PValue))) +
  geom_point(shape = 21, alpha = 0.7) +
  #labs(title = "GO Term Enrichment", x = "Enrichment", y = "GO Term") +
  scale_size_continuous(range = c(2, 12), guide = guide_legend(override.aes = list(fill = "black"))) + 
  scale_fill_gradient(low = "red", high = "green4", name = "Significance") + 
  scale_y_discrete(labels = function(y) str_wrap(y, width = 30)) +
  theme_minimal() +
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12, face = "bold",colour = "Black"), axis.text.y = element_text(size = 10, face = "bold",colour = "Black"),
        legend.margin = margin(b = 5),panel.border = element_rect(color = "black", fill = NA, size = 1.0),axis.ticks = element_line(),axis.title.x=element_blank(),axis.title.y=element_blank())

gg
ggsave("G_Inf_CD4_bubble.pdf", gg, width = 8, height = 9, units = "in", bg = "white")
