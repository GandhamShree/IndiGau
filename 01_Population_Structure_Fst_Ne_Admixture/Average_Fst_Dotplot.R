a<-read.table("Fst_breeds.txt",header = TRUE)
dotchart(a$Fst)
dotchart(a$Fst, labels = a$Breeds,cex=0.75, pch = 19, pt.cex = 1.5, color = "blue", xlab = "Fst",cex.lab = 1.5)
