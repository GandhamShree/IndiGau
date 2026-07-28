# Load the necessary libraries
library(ggplot2)
library(reshape2)

# Read the confusion matrix CSV file

data <- read.csv("Confusion_Matrix_TestSet_90_10.csv", header = TRUE, row.names = 1)

# Check the structure of the data to ensure it's loaded correctly
head(data)

# --- IMPORTANT: Skipped Filtering Steps ---
# The following lines are intentionally REMOVED to include all rows and columns:
# data_filtered_rows <- data[rowSums(data != 0) > 0, ]
# data_filtered <- data_filtered_rows[, colSums(data_filtered_rows != 0) > 0]
# Instead, we will work directly with the 'data' object after handling row names.

# Convert row names into a column for 'True Class' or 'Breed'
data$True_Class <- rownames(data)

# --- Add spaces to specific breed names ---
# Replace "RedKandhari" with "Red Khandhari" in True_Class column
data$True_Class <- gsub("Red_kandhari", "Red Khandhari", data$True_Class)

# Replace "MalnadGidda" with "Malnad Gidda" in True_Class column
data$True_Class <- gsub("Malnad_gidda", "Malnad Gidda", data$True_Class)

# Replace "RedSindhi" with "Red Sindhi" in True_Class column
data$True_Class <- gsub("Red_sindhi", "Red Sindhi", data$True_Class)

# Replace "RedSindhi" with "Red Sindhi" in True_Class column
data$True_Class <- gsub("Konkan_kapila", "Konkan kapila", data$True_Class)

# Replace "RedSindhi" with "Red Sindhi" in True_Class column
data$True_Class <- gsub("Krishna_valley", "Krishna valley", data$True_Class)

# Now, apply the same changes to the column names (predicted classes)
# Get current column names
colnames_original <- colnames(data)



# Assign the new column names back to the data (apply the same renaming used for True_Class)
colnames_new <- colnames_original
colnames_new <- gsub("Red_kandhari", "Red Khandhari", colnames_new)
colnames_new <- gsub("Malnad_gidda", "Malnad Gidda", colnames_new)
colnames_new <- gsub("Red_sindhi", "Red Sindhi", colnames_new)
colnames_new <- gsub("Konkan_kapila", "Konkan kapila", colnames_new)
colnames_new <- gsub("Krishna_valley", "Krishna valley", colnames_new)
colnames(data) <- colnames_new
# --- End of name changes ---


# Print the modified data to inspect it (now includes all original rows/cols with updated names)
print(head(data))


# --- Calculate Percentages ---
# Create a copy of the data for percentage calculation
data_percentages <- data

# Exclude the 'True_Class' column for percentage calculation
# Note: The column names here will already have the spaces from the above lines
data_for_percentage <- data_percentages[, !(names(data_percentages) %in% "True_Class")]

# Calculate row sums to normalize by true class (breed) totals
# Handle cases where a row sum might be zero to avoid division by zero
row_sums <- rowSums(data_for_percentage)
# Replace 0 row sums with 1 to avoid NaN/Inf when dividing (these rows will result in 0% anyway)
row_sums[row_sums == 0] <- 1

# Convert to percentages: Divide each cell by its row sum and multiply by 100
# Use apply to perform row-wise division
percentage_matrix <- as.data.frame(t(apply(data_for_percentage, 1, function(x) (x / sum(x)) * 100)))

# Add the 'True_Class' column back to the percentage matrix
# This True_Class column already has the spaces from previous modification
percentage_matrix$True_Class <- data$True_Class

# Reshape the percentage data into a long format suitable for ggplot
data_melted_percentages <- melt(percentage_matrix, id.vars = "True_Class")

# Check the structure of the melted percentage data
head(data_melted_percentages)

# Plot the heatmap using ggplot with percentages
heatmap_plot_percentages <- ggplot(data_melted_percentages, aes(x = True_Class, y = variable, fill = value)) +
  geom_tile(color = "grey60", linewidth = 0.2) + # Adds grey borders around the tiles
  scale_fill_gradient(low = "white", high = "green3") +
  theme_minimal() +
  labs(x = "True Class", y = "Predicted Class", fill = "Percentage") + # Updated fill legend title
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 22, color = "black",face = "bold"), # X-axis labels
    axis.text.y = element_text(size = 22, color = "black",face = "bold"), # Y-axis labels
    axis.title.x = element_text(size = 26, face = "bold", color = "black"), # Increased size and bold for x-axis title
    axis.title.y = element_text(size = 26, face = "bold", color = "black") # Increased size and bold for y-axis title
  ) +
  # Add percentages inside the heatmap boxes, formatted based on value
  geom_text(aes(label = ifelse(value == 0, "0", sprintf("%.0f", value))), color = "black", size = 7) + # Custom label formatting (no '%' for non-zero)
  # Fine grid lines
  theme(
    panel.grid.major = element_line(color = "grey60", linewidth = 0.3), # Major grid lines
    panel.grid.minor = element_line(color = "gray60", linewidth = 0.1)  # Minor grid lines
  ) +
  # Reduce the size of the boxes (aspect ratio adjustment)
  coord_fixed(ratio = 0.75) + # You can adjust this ratio
  # Reduce legend size
  theme(
    legend.key.size = unit(0.9, "cm"),   # Increase the size of the legend keys
    legend.text = element_text(size = 14), # Increase the font size of the legend text
    legend.title = element_text(size = 16, face = "bold") # Increase legend title size
  )

# Save the plot to a PDF file
ggsave("Confusion_Matrix_Heatmap_Complete_NoSymbols.pdf", plot = heatmap_plot_percentages, width = 15, height = 15) # Increased width/height for complete matrix

# Print the plot in R
print(heatmap_plot_percentages)
