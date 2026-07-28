# -*- coding: utf-8 -*-
import pysam
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec

# ===== USER CONFIG =====
vcf_files = ["Milk_81_Beagle.vcf.gz", "Dual_81_Beagle.vcf.gz", "Draft_81_Beagle.vcf.gz"]
group_labels = ["Milk", "Dual", "Draft"]
core_chr = "NC_040081.1"
start_pos = 16650000
end_pos   = 17350000

# ===== FUNCTIONS =====

def extract_snps_between(vcf_path, core_chr, start_pos, end_pos):
    vcf = pysam.VariantFile(vcf_path)
    snps = []
    for rec in vcf.fetch(core_chr, start_pos, end_pos):
        if len(rec.ref) != 1 or len(rec.alts) != 1:
            continue
        snps.append((rec.pos, rec))
    return snps

def get_major_allele_only(snps):
    pos_list = []
    values = []
    alleles = []

    for pos, rec in snps:
        gt_list = []
        for sample in rec.samples.values():
            gt = sample.get('GT')
            if gt is not None and len(gt) == 2:
                gt_list.extend(gt)

        if not gt_list:
            values.append(np.nan)
            alleles.append("")
            pos_list.append(pos)
            continue

        ref_count = gt_list.count(0)
        alt_count = gt_list.count(1)
        total = ref_count + alt_count

        if total == 0:
            values.append(np.nan)
            alleles.append("")
        elif ref_count >= alt_count:
            values.append(float(ref_count) / total)
            alleles.append(rec.ref)
        else:
            values.append(float(alt_count) / total)
            alleles.append(rec.alts[0])
        pos_list.append(pos)

    return np.array(values), alleles, pos_list

def plot_vertical_4panel(data_list, allele_lists, snp_positions, labels, outfile):
    data = np.vstack(data_list)
    n_snps = data.shape[1]
    quarter = n_snps // 4

    fig = plt.figure(figsize=(20, 12))
    gs = gridspec.GridSpec(4, 2, width_ratios=[20, 1], height_ratios=[1, 1, 1, 1], wspace=0.05, hspace=0.5)

    axes = [plt.subplot(gs[i, 0]) for i in range(4)]
    cbar_ax = plt.subplot(gs[:, 1])

    for i in range(4):
        start = i * quarter
        end = (i + 1) * quarter if i < 3 else n_snps
        ax = axes[i]

        data_part = data[:, start:end]
        labels_part = [row[start:end] for row in allele_lists]
        xticks = ["%.2f" % (pos / 1e6) for pos in snp_positions[start:end]]

        annot_vals = []
        for row_vals, row_alleles in zip(data_part, labels_part):
            formatted_row = []
            for val, allele in zip(row_vals, row_alleles):
                formatted_row.append("" if np.isnan(val) else allele)
            annot_vals.append(formatted_row)

        sns.heatmap(
            data_part,
            cmap="cividis",
            center=0.5,
            annot=annot_vals,
            fmt="",
            cbar=(i == 3),
            cbar_ax=cbar_ax if i == 3 else None,
            linewidths=0.0,
            xticklabels=xticks,
            yticklabels=labels,
            ax=ax,
            annot_kws={"fontsize": 18, "weight": "bold", "fontname": "Arial"}
        )

        # Only show x-axis label on the last subplot
        if i == 3:
            ax.set_xlabel("Chromosome 6 (Mb)", fontsize=21, fontweight='bold', fontname='Arial')
        else:
            ax.set_xlabel("")

        # Center y-tick labels vertically
        ax.set_yticks(np.arange(len(labels)) + 0.5)
        ax.set_yticklabels(labels, rotation=90, fontsize=14, fontweight='bold', fontname='Arial', va='center')

        for label in ax.get_xticklabels():
            label.set_rotation(45)
            label.set_fontname("Arial")
            label.set_fontweight("bold")
            label.set_fontsize(12)

    # Colorbar formatting
    cbar_ax.tick_params(labelsize=14)
    for label in cbar_ax.get_yticklabels():
        label.set_fontname("Arial")
        label.set_fontweight("bold")
        label.set_fontsize(18)
      

    #plt.suptitle("Major Allele Frequencies Across SNPs", fontsize=16, fontweight='bold', fontname='Arial')
    plt.tight_layout(rect=[0, 0, 0.98, 0.95])
    plt.savefig(outfile, dpi=300)
    print("[INFO] 4-panel vertical heatmap saved to {}".format(outfile))
    plt.close()

# ===== MAIN SCRIPT =====

print("[INFO] Extracting SNPs from region {}:{}-{}".format(core_chr, start_pos, end_pos))
snps_ref = extract_snps_between(vcf_files[0], core_chr, start_pos, end_pos)
if not snps_ref:
    raise ValueError("No SNPs found in region!")

snp_positions = [pos for pos, _ in snps_ref]

data_matrix = []
allele_matrix = []

for vcf in vcf_files:
    snps = extract_snps_between(vcf, core_chr, start_pos, end_pos)
    freqs, alleles, _ = get_major_allele_only(snps)
    data_matrix.append(freqs)
    allele_matrix.append(alleles)

plot_vertical_4panel(data_matrix, allele_matrix, snp_positions, group_labels, "haplo_heatmap_4panel_verticalfn.png")
