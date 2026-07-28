#Create data for the step curves
pdf("Height_Primary.pdf", width = 6, height = 5, bg = "white")
Sp <- scan("Vechur_ihs.txt")
Position<- scan("Position.txt")
Mx<-scan("Gir_ihs.txt")
Du<-scan("Ongole_ihs.txt")
Kj<-scan("Kankrej_ihs.txt")
Mn<-scan("Mng_ihs.txt")

# Create step functions for Y1 and Y2
step_function_Y1 <- stepfun(Position, Sp)
step_function_Y2 <- stepfun(Position, Mx)
step_function_Y3 <- stepfun(Position, Du)
step_function_Y4 <- stepfun(Position, Kj)
step_function_Y5 <- stepfun(Position, Mn)

# Plot both step curves on the same plot
plot(step_function_Y1, main="", xlab="", ylab="", col="red", ylim=c(0,1.0),cex = 0.0, lwd=2.5, yaxt="n",xaxt="n" )
plot(step_function_Y2, col="blue",cex=0.0,lwd=2.5,add = TRUE)
plot(step_function_Y3, col="black",cex=0.0,lwd=2.5,add = TRUE)
plot(step_function_Y4, col="yellow2",cex=0.0,lwd=2.5,add = TRUE)
plot(step_function_Y5, col="dodgerblue",cex=0.0,lwd=2.5,add = TRUE)

abline(v=0.0, col="black",lty=1,lwd=2.50) # AX-512660516_center
#abline(v=-0.195697, col="red",lty=2,lwd=5.50) # COL25A1_start
#abline(v=-0.142696, col="red",lty=2,lwd=5.50) # COL25A1_start
#abline(v=-0.-0.195697, col="black",lty=3,lwd=2.50) # ROH start
#abline(v=-0.052839, col="black",lty=3,lwd=2.50) # ROH end

rect(xleft = -0.003605, xright = 0.009169, ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("black", alpha = 0.2)) # ALOX2

rect(xleft = -0.300421,xright = -0.25435, ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("black", alpha = 0.2)) #MINK

rect(xleft = 0.011731	, xright = 0.013261, ybottom = par("usr")[3], ytop = par("usr")[4], 
     border = NA, col = adjustcolor("black", alpha = 0.2)) #RNASEK


#legend("topright", legend=c("Draft Validation", "Dual Validation", "Milk Validation"), col=c("red", "black", "blue"), lty=1, cex=2.20,lwd=4.00,box.lwd = 3, text.font=2)

# === Horizontal Legend ===
legend("top", inset=-0.23, xpd=TRUE, horiz= FALSE,
       legend=c("Vechur Primary", "Ongole Primary", "Gir Primary","Kankrej Primary","Malnad Gidda Primary"),
       col=c("red", "black", "blue","yellow3","dodgerblue"), lty=1, lwd=4,
       cex=1.0, box.lwd=2.5, text.font=2, bty="n", ncol = 2)

box(col = "black",lwd = 3)

# Adjust margins and axis label positions
par( mgp = c(1.5, 0.3, 0.0))  # ← adjust label distance

axis(side = 1,cex.axis=1.25,padj=0.75,tck=-0.020, font.axis = 2)
axis(side = 2,cex.axis=1.25, tck=-0.020, font.axis = 2)

# === Axis Titles ===
mtext("Chromosome 19 (Mb)", side = 1, line = 1.8, font = 2, cex = 1.2)
mtext("Extended Haplotype homozygosity", side = 2, line = 1.50, font = 2, cex = 1.2)

dev.off()

