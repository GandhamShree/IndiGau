# ----------------------------------------------------------------------
# ADMIXTURE-style bar plot for XGBoost blind validation predictions,
# showing ONLY Top1 (predicted breed) probability per individual.
#
# Input:  Blind_XG_Validation_Pure.csv
# Output: admixture_style_blind_validation_top1.pdf
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# 1. Breed -> color lookup (sampled from the reference ADMIXTURE figure)
# ----------------------------------------------------------------------
BREED_COLORS_RAW <- c(
  Amritmahal     = "#1C9E78", Bachaur        = "#D86000",
  Bargur         = "#756FB2", Belahi         = "#E6298B",
  Binjharpuri    = "#64A61E", Deoni          = "#E5AA02",
  Gangatiri      = "#A5761C", Gaolao         = "#666666",
  Ghumsari       = "#8CD3C7", Gir            = "#FB7F71",
  Hallikar       = "#BEBADB", Hariana        = "#FCB45F",
  Kangayam       = "#B2E066", Kankrej        = "#C29108",
  Kenkatha       = "#D9D9D9", Khariar        = "#BD80BB",
  Kherigarh      = "#CBEBC2", Konkan_Kapila  = "#FFED6D",
  Kosali         = "#68C1A3", Krishna_Valley = "#F98E62",
  Ladakhi        = "#80A700", Lakhimi        = "#EB87C5",
  Malnad_Gidda   = "#04C165", Malvi          = "#FED732",
  Mewati         = "#E4C491", Motu           = "#B3B4AF",
  Nagori         = "#FF8000", Nimari         = "#FE999D",
  Ongole         = "#07C1CE", Ponwar         = "#A2CFE2",
  Pulikulam      = "#1F7AB1", Punganur       = "#CAB2D6",
  Rathi          = "#7CB2D4", Red_Kandhari   = "#FA8170",
  Red_Sindhi     = "#B0E08A", Sahiwal        = "#FB8200",
  Siri           = "#00B1FB", Tharparkar     = "#E5C491",
  Umblachery     = "#C87AFF", Vechur         = "#FE50D5"
)
OTHER_COLOR <- "#BFBFBF"   # residual probability mass outside Top1-3

# normalize a breed name for matching across naming conventions
norm <- function(name) {
  tolower(gsub("[^a-zA-Z0-9]", "", as.character(name)))
}

COLOR_LOOKUP <- setNames(unname(BREED_COLORS_RAW), norm(names(BREED_COLORS_RAW)))

get_color <- function(breed_name) {
  key <- norm(breed_name)
  out <- COLOR_LOOKUP[key]
  out[is.na(out)] <- OTHER_COLOR
  unname(out)
}

# ----------------------------------------------------------------------
# 2. Load data (input file must be in the same folder this script is run from)
# ----------------------------------------------------------------------
df <- read.csv("Blind_XG_Validation_Pure.csv", stringsAsFactors = FALSE)

# ----------------------------------------------------------------------
# 3. Order breed groups (largest first, matching reference figure style)
#    then order individuals within each group by confidence (Top1_Prob desc)
#    NOTE: in this file FID is a per-individual sample ID, not a breed
#    group, so grouping uses Top1_Breed instead.
# ----------------------------------------------------------------------
group_counts <- sort(table(df$Top1_Breed), decreasing = TRUE)
group_order  <- names(group_counts)

ordered_chunks <- lapply(group_order, function(grp) {
  sub <- df[df$Top1_Breed == grp, ]
  sub[order(-sub$Top1_Prob), ]
})
plot_df <- do.call(rbind, ordered_chunks)
rownames(plot_df) <- NULL

n <- nrow(plot_df)
x <- 0:(n - 1)   # 0-indexed, matches np.arange(n)

# ----------------------------------------------------------------------
# 4. Figure out group boundaries (for labels / arrows)
# ----------------------------------------------------------------------
boundaries <- list()
start <- 0
for (grp in group_order) {
  cnt <- sum(plot_df$Top1_Breed == grp)
  boundaries[[length(boundaries) + 1]] <- list(grp = grp, s = start, e = start + cnt)
  start <- start + cnt
}

# ----------------------------------------------------------------------
# 5. Build the bars (full-height ADMIXTURE-style bars using Top1 breed color)
# ----------------------------------------------------------------------
top1_colors <- get_color(plot_df$Top1_Breed)

pdf("probability_top1_pure.pdf", width = 20, height = 6.5)

# Larger outer margins to accommodate bigger axis text/labels.
par(mar = c(3.5, 8.0, 1.0, 0.5), xpd = FALSE)

WIDE_THRESHOLD <- 40
wide_groups   <- Filter(function(b) (b$e - b$s) >= WIDE_THRESHOLD, boundaries)
narrow_groups <- Filter(function(b) (b$e - b$s) <  WIDE_THRESHOLD, boundaries)

CHAR_W <- 7.0   # approx data-units per bold-12pt character at this figure size
PAD    <- 10.0  # extra padding between adjacent labels

assign_label_x <- function(items) {
  if (length(items) == 0) return(numeric(0))
  labels <- sapply(items, function(b) gsub("_", " ", b$grp))
  widths <- nchar(labels) * CHAR_W
  placed <- numeric(length(items))
  for (i in seq_along(items)) {
    cx <- (items[[i]]$s + items[[i]]$e) / 2
    if (i > 1) {
      min_x <- placed[i - 1] + widths[i - 1] / 2 + widths[i] / 2 + PAD
      cx <- max(cx, min_x)
    }
    placed[i] <- cx
  }
  placed
}

# Narrow-group labels are now placed INSIDE the plot zone (no more
# above/below arrow callouts sticking out of the plot), so the only
# x-spacing we still need is among the narrow labels themselves so
# adjacent thin slivers' labels don't collide horizontally.
narrow_label_x <- assign_label_x(narrow_groups)

xlim_max <- n

# Set up the plotting region first (axes drawn manually like matplotlib).
# ylim is now tight to [0, 1] -- no extra headroom above/below the bars --
# which removes the white space that used to sit between the 0.00 tick
# and the bottom of the plotted bars, and the same above 1.00.
plot(NA, NA, xlim = c(0, xlim_max), ylim = c(0, 1),
     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")

# ---- bars ----
# Full-height bars so no white gaps appear above the colored portion.
# NOTE: matplotlib's ax.bar(x, height, width=1.0) uses align='center' by
# default, so each bar spans [x-0.5, x+0.5], not [x, x+1]. Replicated here
# for pixel-faithful alignment with the group boundary / label logic.
rect(xleft = x - 0.5, xright = x + 0.5, ybottom = 0, ytop = 1,
     col = top1_colors, border = top1_colors, lwd = 0.5)

# ---- axes (left + bottom only, matching ax.spines top/right hidden) ----
# Axis text and numbers enlarged (cex 2.0) per request; las=1 keeps the
# y-axis numbers horizontal (never vertical).
axis(2, at = c(0, 0.25, 0.5, 0.75, 1.0), las = 1, lwd = 1.4, cex.axis = 2.0)
axis(1, at = NULL, labels = FALSE, lwd = 1.4, tick = TRUE, lwd.ticks = 0)
mtext("Probability", side = 2, line = 5.3, cex = 2.2, font = 2)
box(bty = "l", lwd = 1.4)  # left + bottom spines only

xpd_old <- par("xpd")
par(xpd = TRUE)

# ----------------------------------------------------------------------
# 6. Group labels: bold text for wide groups; narrow-group labels are
#    ALSO drawn inside the plot zone now (no arrows / no text outside the
#    axes), staggered between two heights so neighbouring thin slivers
#    don't overlap. All text is horizontal (srt = 0), never vertical.
# ----------------------------------------------------------------------
EMPHASIZE_BREEDS <- c("gir", "ongole", "kankrej")

for (b in wide_groups) {
  cx <- (b$s + b$e) / 2
  label <- gsub("_", " ", b$grp)
  label_cex <- if (norm(b$grp) %in% EMPHASIZE_BREEDS) 2.8 else 2.0
  text(cx, 0.5, label, cex = label_cex, font = 2, col = "black", srt = 0)
}

# Narrow-breed groups: per request, their names are no longer printed on
# the plot at all (their colors still appear in the bars and in the
# legend, but no in-plot label or connector line is drawn for them).

par(xpd = xpd_old)

# ----------------------------------------------------------------------
# 7. Legend (show ALL 40 breeds, not only breeds appearing in Top1)
# ----------------------------------------------------------------------
all_breeds <- names(BREED_COLORS_RAW)
legend_labels <- gsub("_", " ", all_breeds)
legend_colors <- unname(BREED_COLORS_RAW[all_breeds])

par(xpd = NA)
legend(x = xlim_max * 1.04, y = 1.0,
       legend = legend_labels, fill = legend_colors, border = NA,
       ncol = 2, bty = "n", cex = 1.3, title = "Breed",
       x.intersp = 0.6, y.intersp = 1.15)

dev.off()
cat("done\n")
