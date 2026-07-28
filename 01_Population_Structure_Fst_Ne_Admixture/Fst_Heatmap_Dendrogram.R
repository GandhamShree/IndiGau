library(ggplot2)
library(tidyverse)
library(vegan)
library(plotly)
library(ggdendro)
library(gplots)

z<-read.table("Fst_all_breeds1fnfn.txt",header = TRUE, sep = '\t', row.names = 1)
my_palette <- colorRampPalette(c("white", "lightpink", "deeppink1"))(n = 299)
#rc <- rainbow(nrow(z), start=0, end=.3)
lmat = rbind(c(0,3,0),c(2,1,0),c(4,0,0))
#lmat = rbind(c(0,3),c(2,1),c(0,4))
#lwid = c(1.5,4)
lwid=c(1.5,4,0.5)
#lhei = c(1.5,4,1)
lhei = c(1.5,4,1)
z1<-apply(as.matrix.noquote(z),2,as.numeric)
name<-c("Gir","Sahiwal","Tharparkar","Red_Sindhi","Malnad_gidda","Kangayam","Amritmahal","Kankrej","Pulikulam","Umblachery","Siri","Punganur","Ongole","Hariana","Red_kandhari","Nimari","Ponwar","Mewati","Nagori","Rathi","Motu","Ladakhi","Kosali","Malvi","Khariar","Kherigarh","Binjharpuri","Hallikar","Bachaur","Bargur","Belahi","Deoni","Gaolao","Ghumsari","Gangatiri","Kenkatha","Vechur","Konkan_kapila","Krishna_valley","Lakhimi")
row.names(z1) <- name
heatmap.2(z1,trace = "none",density.info = "none", col = bluered(100), dendrogram = "column", sepcolor="grey",sepwidth=c(0.0005,0.0005), colsep=1:ncol(z1),rowsep=1:nrow(z1),lmat = lmat, lwid = lwid, lhei = lhei, cexRow = 1, cexCol = 1.05, offsetRow =0.05,offsetCol=0.05, key.title=NA, key.xlab =NA,key.ylab =NA)
z1

#colors = c(seq(0,0.02,length=100),seq(0.02,0.05,length=100),seq(0.05,6,length=100))
heatmap.2(z1,trace = "none",density.info = "none", col = my_palette, dendrogram = "column", sepcolor="black",sepwidth=c(0.00005,0.00005), colsep=1:ncol(z1),rowsep=1:nrow(z1),lmat = lmat, lwid = lwid, lhei = lhei, cexRow = 1.0, cexCol = 1.0)
z1

heatmap.2(z1,trace = "none",density.info = "none", col = bluered(100), dendrogram = "column", sepcolor="black",sepwidth=c(0.0005,0.0005), colsep=1:ncol(z1),rowsep=1:nrow(z1),lmat = lmat, lwid = lwid, lhei = lhei, cexRow = 1, cexCol = 1.05, offsetRow =0.05,offsetCol=0.05, key.title=NA, key.xlab =NA,key.ylab =NA)
z1
