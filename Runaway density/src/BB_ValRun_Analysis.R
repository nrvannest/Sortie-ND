# Batch result BBBPIPO_RA 1-20 10-24-19

##################################################################################################################################
##################################################################################################################################

rm(list=ls())
repet <- 5
simulation_length <- 16
plot <- 'BBBPIPO'
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
   our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/Output/BBBPIPO_RA_",i,".out")
    data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
  
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
field_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treedata.txt",header=T)
info_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treeyears.txt",header=T,row.names = 1)
info_plot <- read.table("C:/Users/ucmuser/SORTIE2/models/data/PlotInfo.txt",header=T,sep="\t",row.names = 1)
#######################################################################

#######################################################################
### Runaway Basal Area for all species in BBBPIPO
# "ABCO" "CADE" "PILA" "PIPO "QUCH" "QUKE"

ABCOinterBA <- apply(results[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA <- apply(results[,'ABCO',],2,median)
ABCOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABCO',]

CADEinterBA <- apply(results[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA <- apply(results[,'CADE',],2,median)
CADEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='CADE',]

PILAinterBA <- apply(results[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA <- apply(results[,'PILA',],2,median)
PILAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PILA',]

PIPOinterBA <- apply(results[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedBA <- apply(results[,'PIPO',],2,median)
PIPOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PIPO',]

QUCHinterBA <- apply(results[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedBA <- apply(results[,'QUCH',],2,median)
QUCHfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUCH',]

QUKEinterBA <- apply(results[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA <- apply(results[,'QUKE',],2,median)
QUKEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUKE',]

ABCOfdDBH1 <- ABCOfd[,substr(colnames(field_data),1,3)=='DBH']
CADEfdDBH1 <- CADEfd[,substr(colnames(field_data),1,3)=='DBH']
PILAfdDBH1 <- PILAfd[,substr(colnames(field_data),1,3)=='DBH']
PIPOfdDBH1 <- PIPOfd[,substr(colnames(field_data),1,3)=='DBH']
QUCHfdDBH1 <- QUCHfd[,substr(colnames(field_data),1,3)=='DBH']
QUKEfdDBH1 <- QUKEfd[,substr(colnames(field_data),1,3)=='DBH']

ABCOfdDBH <- ABCOfdDBH1[,2:7]
CADEfdDBH <- CADEfdDBH1[,2:7]
PILAfdDBH <- PILAfdDBH1[,2:7]
PIPOfdDBH <- PIPOfdDBH1[,2:7]
QUCHfdDBH <- QUCHfdDBH1[,2:7]
QUKEfdDBH <- QUKEfdDBH1[,2:7]

simnames1 <- info_data[plot,]
simnames <- simnames1[2:7]
colnames(ABCOfdDBH) <- colnames(CADEfdDBH) <- colnames(PILAfdDBH) <- colnames(PIPOfdDBH) <- colnames(QUCHfdDBH) <- colnames(QUKEfdDBH) <- simnames
ABCOfdDBH <- ABCOfdDBH[,substr(colnames(ABCOfdDBH),1,2)!='NA']
CADEfdDBH <- CADEfdDBH[,substr(colnames(CADEfdDBH),1,2)!='NA']
PILAfdDBH <- PILAfdDBH[,substr(colnames(PILAfdDBH),1,2)!='NA']
PIPOfdDBH <- PIPOfdDBH[,substr(colnames(PIPOfdDBH),1,2)!='NA']
QUCHfdDBH <- QUCHfdDBH[,substr(colnames(QUCHfdDBH),1,2)!='NA']
QUKEfdDBH <- QUKEfdDBH[,substr(colnames(QUKEfdDBH),1,2)!='NA']

ABCOBA <- pi*(ABCOfdDBH/200)^2
ABCOtotBA <- apply(ABCOBA, 2, sum,na.rm=T)
ABCOfdDBH[!is.na(ABCOfdDBH)] = 1

CADEBA <- pi*(CADEfdDBH/200)^2
CADEtotBA <- apply(CADEBA, 2, sum,na.rm=T)
CADEfdDBH[!is.na(CADEfdDBH)] = 1

PILABA <- pi*(PILAfdDBH/200)^2
PILAtotBA <- apply(PILABA, 2, sum,na.rm=T)
PILAfdDBH[!is.na(PILAfdDBH)] = 1

PIPOBA <- pi*(PIPOfdDBH/200)^2
PIPOtotBA <- apply(PIPOBA, 2, sum,na.rm=T)
PIPOfdDBH[!is.na(PIPOfdDBH)] = 1

QUCHBA <- pi*(QUCHfdDBH/200)^2
QUCHtotBA <- apply(QUCHBA, 2, sum,na.rm=T)
QUCHfdDBH[!is.na(QUCHfdDBH)] = 1

QUKEBA <- pi*(QUKEfdDBH/200)^2
QUKEtotBA <- apply(QUKEBA, 2, sum,na.rm=T)
QUKEfdDBH[!is.na(QUKEfdDBH)] = 1

# plot
yrs <- as.double(names(ABCOfdDBH))
rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOtotBA,CADEinterBA,CADEmedBA,CADEtotBA,
                       PILAinterBA,PILAmedBA,PILAtotBA,PIPOinterBA,PIPOmedBA,PIPOtotBA,
                       QUCHinterBA,QUCHmedBA,QUCHtotBA,QUKEinterBA,QUKEmedBA,QUKEtotBA))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste(plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotBA,col=c_ABCO,pch=19)

lines(yrsSim,CADEmedBA,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotBA,col=c_CADE,pch=19)

lines(yrsSim,PILAmedBA,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotBA,col=c_PILA,pch=19)

lines(yrsSim,PIPOmedBA,col=c_PIPO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA[1,],rev(PIPOinterBA[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)
points(yrs,PIPOtotBA,col=c_PIPO,pch=19)

lines(yrsSim,QUCHmedBA,col=c_QUCH,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA[1,],rev(QUCHinterBA[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)
points(yrs,QUCHtotBA,col=c_QUCH,pch=19)

lines(yrsSim,QUKEmedBA,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotBA,col=c_QUKE,pch=19)


legend("topright",c('Simulated ABCO', 'Simulated CADE', 'Simulated PILA', 'Simulated PIPO', 'Simulated QUCH', 'Simulated QUKE', 'Actual ABCO',
                    'Actual CADE', 'Actual PILA', 'Actual PIPO', 'Actual QUCH', 'Actual QUKE'), inset=c(-0.32,0),lty=1,
       col=c(c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE, c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE),
              lwd= c(2, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA),
       pch = c(NA, NA, NA, NA, NA, NA, 19, 19, 19, 19, 19, 19), bty='n',cex=1)

dev.off()


# only ABCO
rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_ABCO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste('ABCO Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotBA,col=c_ABCO,pch=19)

legend("topright",c('Simulated ABCO', 'Actual ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO, c_ABCO),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)

dev.off()

# only CADE
rangePlotBA <- range(c(CADEinterBA,CADEmedBA,CADEtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_CADE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedBA,type='l',ylim=rangePlotBA,main=paste('CADE Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotBA,col=c_CADE,pch=19)

legend("topright",c('Simulated CADE', 'Actual CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)

dev.off()

# only PILA
rangePlotBA <- range(c(PILAinterBA,PILAmedBA,PILAtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_PILA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedBA,type='l',ylim=rangePlotBA,main=paste('PILA Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col= c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotBA,col=c_PILA, pch=19)

legend("topright",c('Simulated PILA', 'Actual PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)

dev.off()

# only PIPO
rangePlotBA <- range(c(PIPOinterBA,PIPOmedBA,PIPOtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_PIPO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedBA,type='l',ylim=rangePlotBA,main=paste('PIPO Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col=c_PIPO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA[1,],rev(PIPOinterBA[2,])), col=adjustcolor("orangered3",alpha.f=0.2), border = NA)
points(yrs,PIPOtotBA,col="orangered3",pch=19)

legend("topright",c('Simulated PIPO', 'Actual PIPO'), inset=c(-0.32,0),
       col=c(c_PIPO, c_PIPO),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)
dev.off()

# only QUCH
rangePlotBA <- range(c(QUCHinterBA,QUCHmedBA,QUCHtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_QUCH_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedBA,type='l',ylim=rangePlotBA,main=paste('QUCH Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col=c_QUCH,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA[1,],rev(QUCHinterBA[2,])), col=adjustcolor("darkmagenta",alpha.f=0.2), border = NA)
points(yrs,QUCHtotBA,col="darkmagenta",pch=19)

legend("topright",c('Simulated QUCH', 'Actual QUCH'), inset=c(-0.32,0),
       col=c(c_QUCH, c_QUCH),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)

dev.off()

# only QUKE
rangePlotBA <- range(c(QUKEinterBA,QUKEmedBA,QUKEtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_QUKE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedBA,type='l',ylim=rangePlotBA,main=paste('QUKE Validation Run Basal Area in',plot),lwd=2,ylab = "Validation Run Basal Area",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor("darkgoldenrod1",alpha.f=0.2), border = NA)
points(yrs,QUKEtotBA,col="darkgoldenrod1",pch=19)

legend("topright",c('Simulated QUKE', 'Actual QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd= c(2, NA), pch = c(NA, 19),bty='n',cex=1)

dev.off()

###################################
### Density of all species in BBBPIPO

ABCOinterDens <- apply(resultsDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens <- apply(resultsDens[,'ABCO',],2,median)
ABCOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABCO',]

CADEinterDens <- apply(resultsDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens <- apply(resultsDens[,'CADE',],2,median)
CADEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='CADE',]

PILAinterDens <- apply(resultsDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens <- apply(resultsDens[,'PILA',],2,median)
PILAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PILA',]

PIPOinterDens <- apply(resultsDens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens <- apply(resultsDens[,'PIPO',],2,median)
PIPOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PIPO',]

QUCHinterDens <- apply(resultsDens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens <- apply(resultsDens[,'QUCH',],2,median)
QUCHfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUCH',]

QUKEinterDens <- apply(resultsDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens <- apply(resultsDens[,'QUKE',],2,median)
QUKEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUKE',]

ABCOfdDBH1 <- ABCOfd[,substr(colnames(field_data),1,3)=='DBH']
ABCOfdDBH <- ABCOfdDBH1[,2:7]
CADEfdDBH1 <- CADEfd[,substr(colnames(field_data),1,3)=='DBH']
CADEfdDBH <- CADEfdDBH1[,2:7]
PILAfdDBH1 <- PILAfd[,substr(colnames(field_data),1,3)=='DBH']
PILAfdDBH <- PILAfdDBH1[,2:7]
PIPOfdDBH1 <- PIPOfd[,substr(colnames(field_data),1,3)=='DBH']
PIPOfdDBH <- PIPOfdDBH1[,2:7]
QUCHfdDBH1 <- QUCHfd[,substr(colnames(field_data),1,3)=='DBH']
QUCHfdDBH <- QUCHfdDBH1[,2:7]
QUKEfdDBH1 <- QUKEfd[,substr(colnames(field_data),1,3)=='DBH']
QUKEfdDBH <- QUKEfdDBH1[,2:7]

colnames(ABCOfdDBH) <- colnames(CADEfdDBH) <- colnames(PILAfdDBH) <- colnames(PIPOfdDBH) <- colnames(QUCHfdDBH) <- colnames(QUKEfdDBH) <-  info_data[plot,2:7]

ABCOfdDBH <- ABCOfdDBH[,substr(colnames(ABCOfdDBH),1,2)!='NA']
CADEfdDBH <- CADEfdDBH[,substr(colnames(CADEfdDBH),1,2)!='NA']
PILAfdDBH <- PILAfdDBH[,substr(colnames(PILAfdDBH),1,2)!='NA']
PIPOfdDBH <- PIPOfdDBH[,substr(colnames(PIPOfdDBH),1,2)!='NA']
QUCHfdDBH <- QUCHfdDBH[,substr(colnames(QUCHfdDBH),1,2)!='NA']
QUKEfdDBH <- QUKEfdDBH[,substr(colnames(QUKEfdDBH),1,2)!='NA']

ABCOfdDBH[!is.na(ABCOfdDBH)] = 1
CADEfdDBH[!is.na(CADEfdDBH)] = 1
PILAfdDBH[!is.na(PILAfdDBH)] = 1
PIPOfdDBH[!is.na(PIPOfdDBH)] = 1
QUCHfdDBH[!is.na(QUCHfdDBH)] = 1
QUKEfdDBH[!is.na(QUKEfdDBH)] = 1

ABCOtotDens <- apply(ABCOfdDBH, 2, sum,na.rm=T)
CADEtotDens <- apply(CADEfdDBH, 2, sum,na.rm=T)
PILAtotDens <- apply(PILAfdDBH, 2, sum,na.rm=T)
PIPOtotDens <- apply(PIPOfdDBH, 2, sum,na.rm=T)
QUCHtotDens <- apply(QUCHfdDBH, 2, sum,na.rm=T)
QUKEtotDens <- apply(QUKEfdDBH, 2, sum,na.rm=T)


####
# plot
yrs <- as.double(names(ABCOfdDBH))
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens,ABCOtotDens, CADEinterDens,CADEmedDens,CADEtotDens,
                         PILAinterDens,PILAmedDens,PILAtotDens, PIPOinterDens,PIPOmedDens,PIPOtotDens,
                         QUCHinterDens,QUCHmedDens,QUCHtotDens, QUKEinterDens,QUKEmedDens,QUKEtotDens))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste(plot),lwd=2, ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotDens,col=c_ABCO,pch=19)

lines(yrsSim,CADEmedDens,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotDens,col=c_CADE,pch=19)

lines(yrsSim,PILAmedDens,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotDens,col=c_PILA,pch=19)

lines(yrsSim,PIPOmedDens,col=c_PIPO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)
points(yrs,PIPOtotDens,col=c_PIPO,pch=19)

lines(yrsSim,QUCHmedDens,col=c_QUCH,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)
points(yrs,QUCHtotDens,col=c_QUCH,pch=19)

lines(yrsSim,QUKEmedDens,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotDens,col=c_QUKE,pch=19)


legend("topright",c('Simulated ABCO', 'Simulated CADE', 'Simulated PILA', 'Simulated PIPO', 'Simulated QUCH', 'Simulated QUKE', 'Actual ABCO',
                    'Actual CADE', 'Actual PILA', 'Actual PIPO', 'Actual QUCH', 'Actual QUKE'), inset=c(-0.32,0),lty=1,
      col=c(c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE, c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA),
       pch = c(NA, NA, NA, NA, NA, NA, 19, 19, 19, 19, 19, 19), bty='n',cex=1)

dev.off()

# only ABCO
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens,ABCOtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_ABCO_Dens2.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotDens,col=c_ABCO,pch=19)

legend("topright",c('Simulated ABCO', 'Actual ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO, c_ABCO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only CADE
rangePlotDens <- range(c(CADEinterDens,CADEmedDens,CADEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_CADE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotDens,col=c_CADE,pch=19)

legend("topright",c('Simulated CADE', 'Actual CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PILA
rangePlotDens <- range(c(PILAinterDens,PILAmedDens,PILAtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_PILA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotDens,col=c_PILA,pch=19)

legend("topright",c('Simulated PILA', 'Actual PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PIPO
rangePlotDens <- range(c(PIPOinterDens,PIPOmedDens,PIPOtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_PIPO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PIPO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)
points(yrs,PIPOtotDens,col=c_PIPO,pch=19)

legend("topright",c('Simulated PIPO', 'Actual PIPO'), inset=c(-0.32,0),
       col=c(c_PIPO, c_PIPO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only QUCH
rangePlotDens <- range(c(QUCHinterDens,QUCHmedDens,QUCHtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_QUCH_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUCH,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)
points(yrs,QUCHtotDens,col=c_QUCH,pch=19)

legend("topright",c('Simulated QUCH', 'Actual QUCH'), inset=c(-0.32,0),
       col=c(c_QUCH, c_QUCH),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only QUKE
rangePlotDens <- range(c(QUKEinterDens,QUKEmedDens,QUKEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/BBBPIPO/BBBPIPO_QUKE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedDens,type='l',ylim=rangePlotDens,main=paste('Validation Run Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotDens,col=c_QUKE,pch=19)

legend("topright",c('Simulated QUKE', 'Actual QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()




