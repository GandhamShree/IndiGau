# Load libraries
library(tidyverse)
library(ggplot2)
library(Cairo)
library(forcats)

# Step 1: Load haplotype data
haplotype_data <- read.delim("haplotype_summary_MDD-top3.txt", header = TRUE, stringsAsFactors = FALSE)

# Step 2: Parse and prepare
haplotype_data <- haplotype_data %>%
  mutate(HapID = row_number()) %>%
  separate(Haplotype, into = c("Left", "Core", "Right"), sep = "\\|") %>%
  mutate(
    FullSeq   = paste0(Left, Core, Right),
    CoreIndex = nchar(Left) + 1
  )
haplotype_data$Breed <- trimws(haplotype_data$Breed)  # Remove leading/trailing spaces
# Step 3: Expand to base-by-base format
haplo_expanded <- haplotype_data %>%
  select(HapID, Breed, FullSeq, CoreIndex, Frequency) %>%
  rowwise() %>% mutate(Split = list(strsplit(FullSeq, "")[[1]])) %>% ungroup() %>%
  mutate(Length = lengths(Split)) %>%
  unnest_longer(Split, values_to = "Base", indices_to = "Position") %>%
  mutate(IsCore = Position == CoreIndex)

# Step 4: Preserve original HapID order
haplotype_data <- haplotype_data %>%
  mutate(HapID = fct_rev(factor(HapID, levels = HapID)))
haplo_expanded$HapID <- factor(haplo_expanded$HapID,
                               levels = levels(haplotype_data$HapID))

# Step 5: Layout helpers
max_pos <- max(haplo_expanded$Position)
x_freq  <- max_pos + 2
top_y   <- length(levels(haplo_expanded$HapID)) + 1.5

# Step 6: Breed label positions
breed_labels <- haplotype_data %>%
  group_by(Breed) %>%
  summarize(y = mean(as.numeric(HapID)), .groups = "drop") %>%
  mutate(x = -0.8)

# Step 7: Assign custom breed colors manually
custom_breed_colors <- tibble(
  Breed = c("Draft", "Milk", "Dual"),
  BreedColor = c("red", "blue", "black")
)

# Merge custom colors with data
haplotype_data <- haplotype_data %>% left_join(custom_breed_colors, by = "Breed")
haplo_expanded <- haplo_expanded %>% left_join(custom_breed_colors, by = "Breed")

# Step 8: Define fill color map
nuc_colors  <- c(A = "white", T = "lightgrey", G = "lightgrey", C = "white")
fill_colors <- c(
  setNames(custom_breed_colors$BreedColor, custom_breed_colors$Breed),  # Breed strip colors
  nuc_colors  # Nucleotide base colors
)

# Step 9: Prepare SNP core background fill as tiles (optional)
core_fill <- haplo_expanded %>%
  filter(Position == unique(haplotype_data$CoreIndex)) %>%
  select(Position, HapID)

# Step 10: Build plot
p <- ggplot() +
  # Breed color strip
  geom_tile(data = haplo_expanded,
            aes(x = 0, y = HapID, fill = Breed),
            width = 0.6, height = 1,
            color = NA,
            show.legend = FALSE) +
  
  # Main haplotype block
  geom_tile(data = haplo_expanded,
            aes(x = Position, y = HapID, fill = Base),
            width = 1, height = 1,
            color = "black", linewidth = 0) +
  
  # Base letters
  geom_text(data = haplo_expanded,
            aes(x = Position, y = HapID, label = Base),
            size = 2.0, fontface = "plain", color = "black", family = "Arial") +
  
  # Frequency values with 2 decimal places
  geom_text(data = haplotype_data,
            aes(x = x_freq, y = HapID, label = sprintf("%.2f", Frequency)),
            inherit.aes = FALSE, size = 1.8, fontface = "plain", family = "Helvetica") +
  
  # Frequency header
  geom_text(data = tibble(x = x_freq, y = top_y, label = "Freq (%)"),
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE, size = 2.15, fontface = "bold", family = "Helvetica") +
  
  # Breed name labels
  geom_text(data = breed_labels,
            aes(x = x, y = y, label = Breed),
            inherit.aes = FALSE, hjust = 1,
            size = 3.0, fontface = "bold", family = "Helvetica") +
  
  # Apply combined fill colors for both Breed and Base
  scale_fill_manual(values = fill_colors) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.4))) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  coord_fixed(ratio = 1, clip = "off") +
  theme_minimal(base_size = 10) +
  theme(
    axis.title      = element_blank(),
    axis.text.x     = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks      = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "none",
    plot.margin     = margin(3, 3, 2, 45)
  )

# Step 11: Save plot
ggsave("haplotype_matrix_milk-top3.tiff",
       plot = p,
       width = 30, height = 15, units = "cm",
       dpi = 600)