# Batch result POFLABMA_ValidRun 1-10

##################################################################################################################################
##################################################################################################################################

rm(list=ls())
repet <- 5
simulation_length <- 15
plot <- 'POFLABMA'
numsubplots <- 1
species <- c("ABCO","CADE" ,"PILA", "PIPO" ,"QUCH" ,"QUKE", "PICO" ,"PIMO", "PIJE", "ABMA")
c_ABCO <- 'blue4'; c_ABMA <- 'forestgreen'; c_CADE <- 'deeppink2'; c_PICO <- 'chocolate4'
c_PIJE <- 'grey51'; c_PILA <- 'deepskyblue'; c_PIMO <- 'lightcoral'; c_PIPO <- 'orangered3'
c_QUCH <- 'darkmagenta'; c_QUKE <- 'darkgoldenrod1'

# to store the results
results <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(results)[2] <- list(species)
dimnames(results)[3] <- list(1:(simulation_length+1))
resultsDens <- results


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA/Output/POFLABMA_RA_",i,".out")
  
  data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot 1 is the extracted center 100mx100m of whole plot
  
  for (who in species)
  {
    # tree = Sapl + Adult
    results[i,who,] <- data[,paste("Adult.Abs.BA.",who,sep=".")] + 
      data[,paste("Sapl.Abs.BA.",who,sep=".")]
    resultsDens[i,who,] <- data[,paste("Adult.Abs.Den.",who,sep=".")] +
      data[,paste("Sapl.Abs.Den.",who,sep=".")]
  }
}

#######################################################################
# field data

field_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treedata.txt",header=T)
info_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treeyears.txt",header=T,row.names = 1)
info_plot <- read.table("C:/Users/ucmuser/SORTIE2/models/data/PlotInfo.txt",header=T,sep="\t",row.names = 1)

### basal area for all species in POFLABMA
# "ABMA" "PICO"

ABMAinterBA <- apply(results[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA <- apply(results[,'ABMA',],2,median)
ABMAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABMA',]

PICOinterBA <- apply(results[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedBA <- apply(results[,'PICO',],2,median)
PICOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PICO',]

ABMAfdDBH <- ABMAfd[,substr(colnames(field_data),1,3)=='DBH']
PICOfdDBH <- PICOfd[,substr(colnames(field_data),1,3)=='DBH']

colnames(ABMAfdDBH) <- colnames(PICOfdDBH) <- info_data[plot,]
ABMAfdDBH <- ABMAfdDBH[,substr(colnames(ABMAfdDBH),1,2)!='NA']
PICOfdDBH <- PICOfdDBH[,substr(colnames(PICOfdDBH),1,2)!='NA']

ABMABA <- pi*(ABMAfdDBH/200)^2
ABMAtotBA <- apply(ABMABA, 2, sum,na.rm=T)
ABMAfdDBH[!is.na(ABMAfdDBH)] = 1

PICOBA <- pi*(PICOfdDBH/200)^2
PICOtotBA <- apply(PICOBA, 2, sum,na.rm=T)
PICOfdDBH[!is.na(PICOfdDBH)] = 1

# plot
yrs <- as.double(names(ABMAfdDBH))
rangePlotBA <- range(c(ABMAinterBA,ABMAmedBA,ABMAtotBA,PICOinterBA,PICOmedBA,PICOtotBA))
yrsSim <- (0:simulation_length)+yrs[2]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedBA,type='l',ylim=rangePlotBA,main=paste(plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA[1,],rev(ABMAinterBA[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotBA,col=c_ABMA,pch=19)

lines(yrsSim,PICOmedBA,col=c_PICO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA[1,],rev(PICOinterBA[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)
points(yrs,PICOtotBA,col=c_PICO,pch=19)

legend("right",c('Actual ABMA', 'Actual PICO', 'Simulated ABMA','Simulated PICO'), inset=c(-0.32,0), bty='n',
       col=c(c_ABMA, c_PICO, c_ABMA, c_PICO),pch = c(19, 19, NA, NA), lwd= c(NA, NA, 2, 2),cex=1)

dev.off()


# only ABMA
rangePlotBA <- range(c(ABMAinterBA,ABMAmedBA,ABMAtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_ABMA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedBA,type='l',ylim=rangePlotBA,main=paste('ABMA Basal area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA[1,],rev(ABMAinterBA[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotBA,col=c_ABMA,pch=19)

legend("topright",c('Simulated ABMA', 'Actual ABMA'), inset=c(-0.32,0),
       col=c(c_ABMA, c_ABMA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PICO
rangePlotBA <- range(c(PICOinterBA,PICOmedBA,PICOtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_PICO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PICOmedBA,type='l',ylim=rangePlotBA,main=paste('Basal area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_PICO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA[1,],rev(PICOinterBA[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)
points(yrs,PICOtotBA,col=c_PICO,pch=19)

legend("topright",c('Simulated PICO', 'Actual PICO'), inset=c(-0.32,0),
       col=c(c_PICO, c_PICO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

###################################
### Density of all species in POFLABMA

ABMAinterDens <- apply(resultsDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens <- apply(resultsDens[,'ABMA',],2,median)
ABMAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABMA',]

PICOinterDens <- apply(resultsDens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens <- apply(resultsDens[,'PICO',],2,median)
PICOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PICO',]

ABMAfdDBH1 <- ABMAfd[,substr(colnames(field_data),1,3)=='DBH']
ABMAfdDBH <- ABMAfdDBH1[,2:7]
PICOfdDBH1 <- PICOfd[,substr(colnames(field_data),1,3)=='DBH']
PICOfdDBH <- PICOfdDBH1[,2:7]

colnames(ABMAfdDBH) <- colnames(PICOfdDBH) <- info_data[plot,2:7]

ABMAfdDBH <- ABMAfdDBH[,substr(colnames(ABMAfdDBH),1,2)!='NA']
PICOfdDBH <- PICOfdDBH[,substr(colnames(PICOfdDBH),1,2)!='NA']

ABMAfdDBH[!is.na(ABMAfdDBH)] = 1
PICOfdDBH[!is.na(PICOfdDBH)] = 1

ABMAtotDens <- apply(ABMAfdDBH, 2, sum,na.rm=T)
PICOtotDens <- apply(PICOfdDBH, 2, sum,na.rm=T)

####
# plot
yrs <- as.double(names(ABMAfdDBH))
rangePlotDens <- range(c(ABMAinterDens,ABMAmedDens,ABMAtotDens,PICOinterDens,PICOmedDens,PICOtotDens))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_Dens.png",width = 3000, height = 2000, units = "px",res=300)
par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangePlotDens,main=paste(plot),lwd=2, ylab = "Density",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotDens,col=c_ABMA,pch=19)

lines(yrsSim,PICOmedDens,col=c_PICO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens[1,],rev(PICOinterDens[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)
points(yrs,PICOtotDens,col=c_PICO,pch=19)

legend("right",c('Actual ABMA', 'Actual PICO', 'Simulated ABMA','Simulated PICO'), inset=c(-0.32,0), bty='n',
       col=c(c_ABMA, c_PICO, c_ABMA, c_PICO),pch = c(19, 19, NA, NA), lwd= c(NA, NA, 2, 2),cex=1)

dev.off()

# only ABMA
rangePlotDens <- range(c(ABMAinterDens,ABMAmedDens,ABMAtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_ABMA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotDens,col=c_ABMA,pch=19)

legend("topright",c('Simulated ABMA', 'Actual ABMA'), inset=c(-0.32,0),
       col=c(c_ABMA, c_ABMA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only PICO
rangePlotDens <- range(c(PICOinterDens,PICOmedDens,PICOtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/POFLABMA//POFLABMA_PICO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PICOmedDens,type='l',ylim=rangePlotDens,main=paste('Density in', plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PICO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens[1,],rev(PICOinterDens[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)
points(yrs,PICOtotDens,col=c_PICO,pch=19)

legend("topright",c('Simulated PICO', 'Actual PICO'), inset=c(-0.32,0),
       col=c(c_PICO, c_PICO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()



