import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# ==============================
# 1. Read the tab-separated file
# ==============================
df = pd.read_csv("For_Tajima.txt", sep="\t")

# ==============================
# 2. Data Summary
# ==============================
print("--- Data Summary ---")
print(f"Initial shape: {df.shape}")
print(f"Missing values (nan) per column: \n{df.isna().sum()}\n")

# ==============================
# 3. Save cleaned values + counts to file
# ==============================
out_file = "cleaned_values_per_breed.txt"

with open(out_file, "w") as f:
    for column in df.columns:
        clean_data = df[column].dropna()
        count = clean_data.shape[0]

        # Print to console
        print(f"\n--- {column} ---")
        print(f"Count: {count}")
        print(clean_data.to_list())

        # Write to file
        f.write(f"--- {column} ---\n")
        f.write(f"Count: {count}\n")
        f.write("Values: " + ", ".join(map(str, clean_data.to_list())) + "\n\n")

print(f"\nCleaned values and counts saved in '{out_file}'\n")

# ==============================
# 4. Plotting
# ==============================
plt.style.use('seaborn-v0_8-whitegrid')
fig, ax = plt.subplots(figsize=(10, 6))

ax.set_xlabel("Tajima's D", fontsize=24, fontweight='bold')
ax.set_ylabel("Density", fontsize=24, fontweight='bold')

# Bold tick labels
ax.tick_params(axis='both', which='major', labelsize=22)
for tick in ax.get_xticklabels():
    tick.set_fontweight('bold')
for tick in ax.get_yticklabels():
    tick.set_fontweight('bold')

# ==============================
# Custom Colors for Breeds
# ==============================
custom_palette = {
    "Draft": "red",
    "Dual": "black",
    "Milk": "blue",
}

# Plot KDE per breed with custom colors
for column in df.columns:
    clean_data = df[column].dropna()
    sns.kdeplot(
        clean_data,
        ax=ax,
        label=column,
        fill=True,
        alpha=0.3,  # transparency for fill
        linewidth=2,
        color=custom_palette[column]   # custom color
    )

# Legend
ax.legend(
    title='',
    loc='upper right',
    prop={'size': 18, 'weight': 'bold'},
    labelspacing=0.2
)

# Reference line at 0
ax.axvline(0, color='grey', linestyle='--', alpha=0.6)

# Add border around axes
for spine in ax.spines.values():
    spine.set_edgecolor('black')
    spine.set_linewidth(2)

# Save
plt.tight_layout()
plt.savefig('tajimas_d_density_plot_colored.png', dpi=300)

print("Density plot saved as 'tajimas_d_density_plot_colored.png'")
