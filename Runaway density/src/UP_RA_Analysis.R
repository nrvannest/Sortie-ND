# Batch result UPLOG_ValidRun 1-10 

##################################################################################################################################
##################################################################################################################################

rm(list=ls())
repet <- 5
simulation_length <- 16
plot <- 'UPLOG'
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
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/Output/UPLOG_RA_",i,".out")
 
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
# field data
field_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treedata.txt",header=T)
info_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treeyears.txt",header=T,row.names = 1)
info_plot <- read.table("C:/Users/ucmuser/SORTIE2/models/data/PlotInfo.txt",header=T,sep="\t",row.names = 1)

#######################################################################
### Basal Area for all species in UPLOG
# "ABCO" "ABMA" "CADE" "PIJE" "PILA" "QUKE"

ABCOinterBA <- apply(results[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA <- apply(results[,'ABCO',],2,median)
ABCOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABCO',]

ABMAinterBA <- apply(results[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA <- apply(results[,'ABMA',],2,median)
ABMAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABMA',]

CADEinterBA <- apply(results[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA <- apply(results[,'CADE',],2,median)
CADEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='CADE',]

PIJEinterBA <- apply(results[,'PIJE',],2,quantile,c(0.025,0.975))
PIJEmedBA <- apply(results[,'PIJE',],2,median)
PIJEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PIJE',]

PILAinterBA <- apply(results[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA <- apply(results[,'PILA',],2,median)
PILAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PILA',]

QUKEinterBA <- apply(results[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA <- apply(results[,'QUKE',],2,median)
QUKEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUKE',]

ABCOfdDBH1 <- ABCOfd[,substr(colnames(field_data),1,3)=='DBH']
ABMAfdDBH1 <- ABMAfd[,substr(colnames(field_data),1,3)=='DBH']
CADEfdDBH1 <- CADEfd[,substr(colnames(field_data),1,3)=='DBH']
PIJEfdDBH1 <- PIJEfd[,substr(colnames(field_data),1,3)=='DBH']
PILAfdDBH1 <- PILAfd[,substr(colnames(field_data),1,3)=='DBH']
QUKEfdDBH1 <- QUKEfd[,substr(colnames(field_data),1,3)=='DBH']

ABCOfdDBH <- ABCOfdDBH1[,3:6]
ABMAfdDBH <- ABMAfdDBH1[,3:6]
CADEfdDBH <- CADEfdDBH1[,3:6]
PIJEfdDBH <- PIJEfdDBH1[,3:6]
PILAfdDBH <- PILAfdDBH1[,3:6]
QUKEfdDBH <- QUKEfdDBH1[,3:6]

simnames1 <- info_data[plot,]
simnames <- simnames1[3:6]
colnames(ABCOfdDBH) <- colnames(ABMAfdDBH) <- colnames(CADEfdDBH) <- colnames(PIJEfdDBH) <- colnames(PILAfdDBH) <- colnames(QUKEfdDBH) <- simnames
ABCOfdDBH <- ABCOfdDBH[,substr(colnames(ABCOfdDBH),1,2)!='NA']
ABMAfdDBH <- ABMAfdDBH[,substr(colnames(ABMAfdDBH),1,2)!='NA']
CADEfdDBH <- CADEfdDBH[,substr(colnames(CADEfdDBH),1,2)!='NA']
PIJEfdDBH <- PIJEfdDBH[,substr(colnames(PIJEfdDBH),1,2)!='NA']
PILAfdDBH <- PILAfdDBH[,substr(colnames(PILAfdDBH),1,2)!='NA']
QUKEfdDBH <- QUKEfdDBH[,substr(colnames(QUKEfdDBH),1,2)!='NA']

ABCOBA <- pi*(ABCOfdDBH/200)^2
ABCOtotBA <- apply(ABCOBA, 2, sum,na.rm=T)
ABCOfdDBH[!is.na(ABCOfdDBH)] = 1

ABMABA <- pi*(ABMAfdDBH/200)^2
ABMAtotBA <- apply(ABMABA, 2, sum,na.rm=T)
ABMAfdDBH[!is.na(ABMAfdDBH)] = 1

CADEBA <- pi*(CADEfdDBH/200)^2
CADEtotBA <- apply(CADEBA, 2, sum,na.rm=T)
CADEfdDBH[!is.na(CADEfdDBH)] = 1

PIJEBA <- pi*(PIJEfdDBH/200)^2
PIJEtotBA <- apply(PIJEBA, 2, sum,na.rm=T)
PIJEfdDBH[!is.na(PIJEfdDBH)] = 1

PILABA <- pi*(PILAfdDBH/200)^2
PILAtotBA <- apply(PILABA, 2, sum,na.rm=T)
PILAfdDBH[!is.na(PILAfdDBH)] = 1

QUKEBA <- pi*(QUKEfdDBH/200)^2
QUKEtotBA <- apply(QUKEBA, 2, sum,na.rm=T)
QUKEfdDBH[!is.na(QUKEfdDBH)] = 1

# plot
yrs <- as.double(names(ABCOfdDBH))
rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOtotBA,ABMAinterBA,ABMAmedBA,ABMAtotBA,
                       CADEinterBA,CADEmedBA,CADEtotBA,PIJEinterBA,PIJEmedBA,PIJEtotBA,
                       PILAinterBA,PILAmedBA,PILAtotBA,QUKEinterBA,QUKEmedBA,QUKEtotBA))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste(plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotBA,col=c_ABCO,pch=19)

lines(yrsSim,ABMAmedBA,col=c_ABMA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA[1,],rev(ABMAinterBA[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotBA,col=c_ABMA,pch=19)

lines(yrsSim,CADEmedBA,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotBA,col=c_CADE,pch=19)

lines(yrsSim,PIJEmedBA,col=c_PIJE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIJEinterBA[1,],rev(PIJEinterBA[2,])), col=adjustcolor(c_PIJE,alpha.f=0.2), border = NA)
points(yrs,PIJEtotBA,col=c_PIJE,pch=19)

lines(yrsSim,PILAmedBA,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotBA,col=c_PILA,pch=19)

lines(yrsSim,QUKEmedBA,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotBA,col=c_QUKE,pch=19)


legend("topright",c('Simulated ABCO', 'Simulated ABMA', 'Simulated CADE', 'Simulated PIJE', 'Simulated PILA', 'Simulated QUKE', 'Actual ABCO',
                    'Actual ABMA', 'Actual CADE', 'Actual PIJE', 'Actual PILA', 'Actual QUKE'), inset=c(-0.32,0),lty=1,
       col=c(c_ABCO, c_ABMA, c_CADE, c_PIJE, c_PILA,  c_QUKE, c_ABCO, c_ABMA, c_CADE, c_PIJE, c_PILA, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA),
       pch = c(NA, NA, NA, NA, NA, NA, 19, 19, 19, 19, 19, 19), bty='n',cex=1)

dev.off()

# only ABCO
rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_ABCO_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_ABCO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotBA,col=c_ABCO,pch=19)

legend("topright",c('Simulated ABCO', 'Actual ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO, c_ABCO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only ABMA
rangePlotBA <- range(c(ABMAinterBA,ABMAmedBA,ABMAtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_ABMA_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_ABMA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA[1,],rev(ABMAinterBA[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotBA,col=c_ABMA,pch=19)

legend("topright",c('Simulated ABMA', 'Actual ABMA'), inset=c(-0.32,0),
       col=c(c_ABMA, c_ABMA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only CADE
rangePlotBA <- range(c(CADEinterBA,CADEmedBA,CADEtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_CADE_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_CADE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotBA,col=c_CADE,pch=19)

legend("topright",c('Simulated CADE', 'Actual CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PILA
rangePlotBA <- range(c(PILAinterBA,PILAmedBA,PILAtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_PILA_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_PILA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotBA,col=c_PILA,pch=19)

legend("topright",c('Simulated PILA', 'Actual PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PIJE
rangePlotBA <- range(c(PIJEinterBA,PIJEmedBA,PIJEtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_PIJE_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_PIJE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIJEmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_PIJE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIJEinterBA[1,],rev(PIJEinterBA[2,])), col=adjustcolor(c_PIJE,alpha.f=0.2), border = NA)
points(yrs,PIJEtotBA,col=c_PIJE,pch=19)

legend("topright",c('Simulated PIJE', 'Actual PIJE'), inset=c(-0.32,0),
       col=c(c_PIJE, c_PIJE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()


# only QUKE
rangePlotBA <- range(c(QUKEinterBA,QUKEmedBA,QUKEtotBA))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_QUKE_BA.png",width = 3000, height = 2000, units = "px",res=300)
#png(filename = "/Users/nikolevannest/Desktop/VR analysis/UPLOG/Analysis/Graphs/UPLOG_QUKE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedBA,type='l',ylim=rangePlotBA,main=paste('BAity in',plot),lwd=2,ylab = "BAity",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotBA,col=c_QUKE,pch=19)

legend("topright",c('Simulated QUKE', 'Actual QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()



#######################################################################
### Density for all species in UPLOG
# "ABCO" "ABMA" "CADE" "PIJE" "PILA" "QUKE"

ABCOinterDens <- apply(resultsDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens <- apply(resultsDens[,'ABCO',],2,median)
ABCOfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABCO',]

ABMAinterDens <- apply(resultsDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens <- apply(resultsDens[,'ABMA',],2,median)
ABMAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='ABMA',]

CADEinterDens <- apply(resultsDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens <- apply(resultsDens[,'CADE',],2,median)
CADEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='CADE',]

PIJEinterDens <- apply(resultsDens[,'PIJE',],2,quantile,c(0.025,0.975))
PIJEmedDens <- apply(resultsDens[,'PIJE',],2,median)
PIJEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PIJE',]

PILAinterDens <- apply(resultsDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens <- apply(resultsDens[,'PILA',],2,median)
PILAfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='PILA',]

QUKEinterDens <- apply(resultsDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens <- apply(resultsDens[,'QUKE',],2,median)
QUKEfd <- field_data[field_data$PLOT==plot & field_data$SppCode=='QUKE',]

ABCOfdDBH1 <- ABCOfd[,substr(colnames(field_data),1,3)=='DBH']
ABMAfdDBH1 <- ABMAfd[,substr(colnames(field_data),1,3)=='DBH']
CADEfdDBH1 <- CADEfd[,substr(colnames(field_data),1,3)=='DBH']
PIJEfdDBH1 <- PIJEfd[,substr(colnames(field_data),1,3)=='DBH']
PILAfdDBH1 <- PILAfd[,substr(colnames(field_data),1,3)=='DBH']
QUKEfdDBH1 <- QUKEfd[,substr(colnames(field_data),1,3)=='DBH']

ABCOfdDBH <- ABCOfdDBH1[,3:6]
ABMAfdDBH <- ABMAfdDBH1[,3:6]
CADEfdDBH <- CADEfdDBH1[,3:6]
PIJEfdDBH <- PIJEfdDBH1[,3:6]
PILAfdDBH <- PILAfdDBH1[,3:6]
QUKEfdDBH <- QUKEfdDBH1[,3:6]

simnames1 <- info_data[plot,]
simnames <- simnames1[3:6]
colnames(ABCOfdDBH) <- colnames(ABMAfdDBH) <- colnames(CADEfdDBH) <- colnames(PIJEfdDBH) <- colnames(PILAfdDBH) <- colnames(QUKEfdDBH) <- simnames
ABCOfdDBH <- ABCOfdDBH[,substr(colnames(ABCOfdDBH),1,2)!='NA']
ABMAfdDBH <- ABMAfdDBH[,substr(colnames(ABMAfdDBH),1,2)!='NA']
CADEfdDBH <- CADEfdDBH[,substr(colnames(CADEfdDBH),1,2)!='NA']
PIJEfdDBH <- PIJEfdDBH[,substr(colnames(PIJEfdDBH),1,2)!='NA']
PILAfdDBH <- PILAfdDBH[,substr(colnames(PILAfdDBH),1,2)!='NA']
QUKEfdDBH <- QUKEfdDBH[,substr(colnames(QUKEfdDBH),1,2)!='NA']

ABCOfdDBH[!is.na(ABCOfdDBH)] = 1
ABMAfdDBH[!is.na(ABMAfdDBH)] = 1
CADEfdDBH[!is.na(CADEfdDBH)] = 1
PIJEfdDBH[!is.na(PIJEfdDBH)] = 1
PILAfdDBH[!is.na(PILAfdDBH)] = 1
QUKEfdDBH[!is.na(QUKEfdDBH)] = 1

ABCOtotDens <- apply(ABCOfdDBH, 2, sum,na.rm=T)
ABMAtotDens <- apply(ABMAfdDBH, 2, sum,na.rm=T)
CADEtotDens <- apply(CADEfdDBH, 2, sum,na.rm=T)
PIJEtotDens <- apply(PIJEfdDBH, 2, sum,na.rm=T)
PILAtotDens <- apply(PILAfdDBH, 2, sum,na.rm=T)
QUKEtotDens <- apply(QUKEfdDBH, 2, sum,na.rm=T)

# plot
yrs <- as.double(names(ABCOfdDBH))
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens,ABCOtotDens,ABMAinterDens,ABMAmedDens,ABMAtotDens,
                       CADEinterDens,CADEmedDens,CADEtotDens,PIJEinterDens,PIJEmedDens,PIJEtotDens,
                       PILAinterDens,PILAmedDens,PILAtotDens,QUKEinterDens,QUKEmedDens,QUKEtotDens))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste(plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotDens,col=c_ABCO,pch=19)

lines(yrsSim,ABMAmedDens,col=c_ABMA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotDens,col=c_ABMA,pch=19)

lines(yrsSim,CADEmedDens,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotDens,col=c_CADE,pch=19)

lines(yrsSim,PIJEmedDens,col=c_PIJE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIJEinterDens[1,],rev(PIJEinterDens[2,])), col=adjustcolor(c_PIJE,alpha.f=0.2), border = NA)
points(yrs,PIJEtotDens,col=c_PIJE,pch=19)

lines(yrsSim,PILAmedDens,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotDens,col=c_PILA,pch=19)

lines(yrsSim,QUKEmedDens,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotDens,col=c_QUKE,pch=19)


legend("topright",c('Simulated ABCO', 'Simulated ABMA', 'Simulated CADE', 'Simulated PIJE', 'Simulated PILA', 'Simulated QUKE', 'Actual ABCO',
                    'Actual ABMA', 'Actual CADE', 'Actual PIJE', 'Actual PILA', 'Actual QUKE'), inset=c(-0.32,0),lty=1,
       col=c(c_ABCO, c_ABMA, c_CADE, c_PIJE, c_PILA,  c_QUKE, c_ABCO, c_ABMA, c_CADE, c_PIJE, c_PILA, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2, NA, NA, NA, NA, NA, NA),
       pch = c(NA, NA, NA, NA, NA, NA, 19, 19, 19, 19, 19, 19), bty='n',cex=1)

dev.off()


# only ABCO
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens,ABCOtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_ABCO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)
points(yrs,ABCOtotDens,col=c_ABCO,pch=19)

legend("topright",c('Simulated ABCO', 'Actual ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO, c_ABCO),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only ABMA
rangePlotDens <- range(c(ABMAinterDens,ABMAmedDens,ABMAtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_ABMA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)
points(yrs,ABMAtotDens,col=c_ABMA,pch=19)

legend("topright",c('Simulated ABMA', 'Actual ABMA'), inset=c(-0.32,0),
       col=c(c_ABMA, c_ABMA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)

dev.off()

# only CADE
rangePlotDens <- range(c(CADEinterDens,CADEmedDens,CADEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_CADE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)
points(yrs,CADEtotDens,col=c_CADE,pch=19)

legend("topright",c('Simulated CADE', 'Actual CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PILA
rangePlotDens <- range(c(PILAinterDens,PILAmedDens,PILAtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_PILA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)
points(yrs,PILAtotDens,col=c_PILA,pch=19)

legend("topright",c('Simulated PILA', 'Actual PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

# only PIJE
rangePlotDens <- range(c(PIJEinterDens,PIJEmedDens,PIJEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_PIJE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIJEmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PIJE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIJEinterDens[1,],rev(PIJEinterDens[2,])), col=adjustcolor(c_PIJE,alpha.f=0.2), border = NA)
points(yrs,PIJEtotDens,col=c_PIJE,pch=19)

legend("topright",c('Simulated PIJE', 'Actual PIJE'), inset=c(-0.32,0),
       col=c(c_PIJE, c_PIJE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()


# only QUKE
rangePlotDens <- range(c(QUKEinterDens,QUKEmedDens,QUKEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Runaway Density/UPLOG/UPLOG_QUKE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedDens,type='l',ylim=rangePlotDens,main=paste('Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)
points(yrs,QUKEtotDens,col=c_QUKE,pch=19)

legend("topright",c('Simulated QUKE', 'Actual QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd=c(3, NA), pch = c(NA, 19), bty='n',cex=1)
dev.off()

