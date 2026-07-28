library(LDheatmap)
library(vcfR)
library(grid)

# Read VCF
vcf <- read.vcfR("chr6_range.vcf")

# Read sample lists
Gir <- read.table("Gir.txt", header = FALSE)
Kankrej <- read.table("Kankrej.txt", header = FALSE)
Tharparkar <- read.table("Tharparkar.txt", header = FALSE)

# Convert to SnpMatrix
list_Gir <- vcfR2SnpMatrix(vcf, subjects = Gir[,1])
list_Kankrej <- vcfR2SnpMatrix(vcf, subjects = Kankrej[,1])
list_Tharparkar <- vcfR2SnpMatrix(vcf, subjects = Tharparkar[,1])

# Color palette
rgb.palette <- colorRampPalette(rev(c("ivory", "orange", "red")), space = "rgb")

# Open PDF ONCE
pdf("LDheatmap_chr6_Gir_Kankrej_Tharparkar_clean.pdf",
    width = 7, height = 7)

# ---------------- GIR ----------------
LDheatmap(
  list_Gir$data,
  list_Gir$genetic.distance,
  title = "Gir",
  color = rgb.palette(18),
  add.map = TRUE,
  flip = TRUE
)

# ---------------- KANKREJ ----------------
LDheatmap(
  list_Kankrej$data,
  list_Kankrej$genetic.distance,
  title = "Kankrej",
  color = rgb.palette(18),
  add.map = TRUE,
  flip = TRUE
)

# ---------------- THARPARkar ----------------
LDheatmap(
  list_Tharparkar$data,
  list_Tharparkar$genetic.distance,
  title = "Tharparkar",
  color = rgb.palette(18),
  add.map = TRUE,
  flip = TRUE
)

# Close PDF
dev.off()