##For changing the name into number along with removing NW and p.value with "?"
library(stringr)
file_list <- list.files(pattern = "*.csv")
for (file in file_list) {output_file <-sub(".csv",".txt", file)
gwas <- read.csv(file, header=T)
gwas$Chromosome[gwas$Chromosome=="NC_040076.1"]<-"1"
gwas$Chromosome[gwas$Chromosome=="NC_040077.1"]<-"2"
gwas$Chromosome[gwas$Chromosome=="NC_040078.1"]<-"3"
gwas$Chromosome[gwas$Chromosome=="NC_040079.1"]<-"4"
gwas$Chromosome[gwas$Chromosome=="NC_040080.1"]<-"5"
gwas$Chromosome[gwas$Chromosome=="NC_040081.1"]<-"6"
gwas$Chromosome[gwas$Chromosome=="NC_040082.1"]<-"7"
gwas$Chromosome[gwas$Chromosome=="NC_040083.1"]<-"8"
gwas$Chromosome[gwas$Chromosome=="NC_040084.1"]<-"9"
gwas$Chromosome[gwas$Chromosome=="NC_040085.1"]<-"10"
gwas$Chromosome[gwas$Chromosome=="NC_040086.1"]<-"11"
gwas$Chromosome[gwas$Chromosome=="NC_040087.1"]<-"12"
gwas$Chromosome[gwas$Chromosome=="NC_040088.1"]<-"13"
gwas$Chromosome[gwas$Chromosome=="NC_040089.1"]<-"14"
gwas$Chromosome[gwas$Chromosome=="NC_040090.1"]<-"15"
gwas$Chromosome[gwas$Chromosome=="NC_040091.1"]<-"16"
gwas$Chromosome[gwas$Chromosome=="NC_040092.1"]<-"17"
gwas$Chromosome[gwas$Chromosome=="NC_040093.1"]<-"18"
gwas$Chromosome[gwas$Chromosome=="NC_040094.1"]<-"19"
gwas$Chromosome[gwas$Chromosome=="NC_040095.1"]<-"20"
gwas$Chromosome[gwas$Chromosome=="NC_040096.1"]<-"21"
gwas$Chromosome[gwas$Chromosome=="NC_040097.1"]<-"22"
gwas$Chromosome[gwas$Chromosome=="NC_040098.1"]<-"23"
gwas$Chromosome[gwas$Chromosome=="NC_040099.1"]<-"24"
gwas$Chromosome[gwas$Chromosome=="NC_040100.1"]<-"25"
gwas$Chromosome[gwas$Chromosome=="NC_040101.1"]<-"26"
gwas$Chromosome[gwas$Chromosome=="NC_040102.1"]<-"27"
gwas$Chromosome[gwas$Chromosome=="NC_040103.1"]<-"28"
gwas$Chromosome[gwas$Chromosome=="NC_040104.1"]<-"29"
gwas$Chromosome[gwas$Chromosome=="NC_040105.1"]<-"30"
gwas <-subset(gwas,Chromosome %in% seq(1,30) & P.Value != "?")
write.table(gwas, file = output_file, sep="\t", quote = FALSE, row.names=FALSE)}

##For manhattan plot
#library(qqman)
file_list <- list.files(pattern = "*.txt")
for (file in file_list) {
  output_file <- sub(".txt", ".tiff", file)
  man <- read.table(file, header = TRUE)
  tiff(filename = output_file, res = 100, units = "cm", width = 145, height = 35)
  d <- manhattan(man, chr = 'Chromosome', bp='Position',p='P.Value',snp='Marker', col = c("black", "#666666", "#CC6600"), logp = TRUE, ylab = '', xlab = '', cex = 2.5, cex.lab = 1.25, cex.axis = 2.6, ylim = c(0.0, 17.0), font.axis = 2, lwd = 2)
  dev.off()
}

##For qqPLOT
#library(qqman)
file_list <- list.files(pattern = "*.txt")
for (file in file_list) {
  output_file <- sub(".txt", "_qqplot.tiff", file)
  tiff(filename = output_file, res = 100, units = "cm",width = 15, height =10 )
  man <- read.table(file, header = TRUE)
  qq(man$P.Value, pch = 19, col = "blue4", cex = 0.25, las = 2)
  dev.off()
}



##For Plambda
library(QCEWAS)
file_list <- list.files(pattern = "*.txt")
for (file in file_list) {
  output_file <- sub(".txt", "_p_lambda.txt", file)
  man <- read.table(file, header = TRUE)
  a<- P_lambda(man$P.Value) 
  write.table(a, file = output_file, sep="\t", quote = FALSE, row.names=FALSE)}
