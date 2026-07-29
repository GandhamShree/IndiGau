This repository is collection of codes used to generate results in the manuscript " Genomic footprints of selection and adaptation across 40 breeds of Indian Zebu cattle "
Please contact gandham71@gmail.com (corresponding authors) if you have any questions on these analyses and codes.

One representative script per analysis/plot type used in the manuscript, organized by section. Where a script (e.g. the EHH step plot or GWAS Manhattan plot) was reused multiple times across different traits/loci/climatic zones, only a single template copy is kept here, swap in the relevant input file names to regenerate any specific panel.


## 01_Population_Structure_Fst/
`Fst_Heatmap_Dendrogram.R` | Pairwise Fst heat map with hierarchical clustering dendrogram across breeds |
`Average_Fst_Dotplot.R` | Dot plot of average Fst per breed |
`Admixture` | Scripts for ADMIXTURE analysis and visualization |
`BovineHapmapPCAplots` | Scripts for principal component analysis (PCA) of cattle populations |
`EFFECTIVEpopulaationsize` | Scripts for estimating effective population size (Ne) |
`Corr_MAE_RMSE_CI.R` | Multi-panel heatmap of Pearson's r, 95% CI, p-value, RMSE, and MAE for Vechur vs other cattle breeds |
## 02_Genome-wide_Selection_Manhattan_QQ/
`GWAS_Manhattan_QQplot.R` | GWAS/selection-scan Manhattan plot with QQ plot |

## 03_Genomic_Signatures_of_Selection/
`EHH_Step_Plot.R` | Extended haplotype homozygosity (EHH) step-function plot |
`iHS_Density_Plot.R` | iHS score density distribution |
`iHS_Barchart.R` | iHS bar chart by breed at a selected locus |
`iHS_Manhattan_Plot.R` | Manhattan plot of iHS values across the genome |
`Allele_Frequency_Plot.R` | Allele frequency trajectory plot |
`Haplotype_Matrix_Plot.R` | Haplotype matrix/grid plot |
`TajimasD_Density_Plot.py` | Tajima's D density plot |
`GO_Enrichment_Plot.R` | GO term enrichment bubble plot |
`Allele_frq_Haplotype_Heatmap.py` | Runs-of-homozygosity / major allele frequency heat map |
`LD_heatmap.R` | Pairwise linkage disequilibrium (LD) heat map |

## 04_Machine_Learning_Breed_Classification/
`Breed_Accuracy_Plot.R` | Per-breed classification accuracy plot |
`Confusion_Matrix_Heatmap.R` | Confusion matrix heat map |
`Breed_Admixture_Heatmap.R` | Breed admixture/purity heat map |
`Prediction_Probability_Barplot.R` | Top-1 predicted-breed probability bar plot |



Note: Input data files, output figures (PDF/TIFF/PNG), and OS metadata files were excluded — this repository contains scripts only.
