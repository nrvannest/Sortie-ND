# Comparison of results from BBBPIPO_Current and other climate sequences

# Scroll down to percent.change function to get average %change over all timesteps

##################################################################################################################################
##################################################################################################################################

rm(list=ls())
repet <- 10
simulation_length <- 93
plot <- 'BBBPIPO'
numsubplots <- 1
species <- c("ABCO","CADE" ,"PILA", "PIPO" ,"QUCH" ,"QUKE", "PICO" ,"PIMO", "PIJE", "ABMA")
c_ABCO <- 'blue4'; c_ABMA <- 'forestgreen'; c_CADE <- 'deeppink2'; c_PICO <- 'chocolate4'
c_PIJE <- 'grey51'; c_PILA <- 'deepskyblue'; c_PIMO <- 'lightcoral'; c_PIPO <- 'orangered3'
c_QUCH <- 'darkmagenta'; c_QUKE <- 'darkgoldenrod1'
c_current <- "forestgreen" ; c_cnrm <- "purple" ; c_ccsm <- "orangered" ; c_miroc <- "turquoise2";


# to store the results Current
results <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(results)[2] <- list(species)
dimnames(results)[3] <- list(1:(simulation_length+1))
resultsDens <- results


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Current/Output/BBBPIPO_Current_",i,".out")
  
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

# to store the results CNRM
resultsCNRM <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(resultsCNRM)[2] <- list(species)
dimnames(resultsCNRM)[3] <- list(1:(simulation_length+1))
resultsCNRMDens <- resultsCNRM


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Output/BBBPIPO_CNRM_",i,".out")
  
  data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
  
  for (who in species)
  {
    # tree = Sapl + Adult
    resultsCNRM[i,who,] <- data[,paste("Adult.Abs.BA.",who,sep=".")] + 
      data[,paste("Sapl.Abs.BA.",who,sep=".")]
    resultsCNRMDens[i,who,] <- data[,paste("Adult.Abs.Den.",who,sep=".")] +
      data[,paste("Sapl.Abs.Den.",who,sep=".")]
  }
}

# to store the results CCSM
resultsCCSM <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(resultsCCSM)[2] <- list(species)
dimnames(resultsCCSM)[3] <- list(1:(simulation_length+1))
resultsCCSMDens <- resultsCCSM


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CCSM/Output/BBBPIPO_CCSM_",i,".out")
  
  data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
  
  for (who in species)
  {
    # tree = Sapl + Adult
    resultsCCSM[i,who,] <- data[,paste("Adult.Abs.BA.",who,sep=".")] + 
      data[,paste("Sapl.Abs.BA.",who,sep=".")]
    resultsCCSMDens[i,who,] <- data[,paste("Adult.Abs.Den.",who,sep=".")] +
      data[,paste("Sapl.Abs.Den.",who,sep=".")]
  }
}

# to store the results MIROC
resultsMIROC <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(resultsMIROC)[2] <- list(species)
dimnames(resultsMIROC)[3] <- list(1:(simulation_length+1))
resultsMIROCDens <- resultsMIROC


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/MIROC/Output/BBBPIPO_MIROC_",i,".out")
  
  data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
  
  for (who in species)
  {
    # tree = Sapl + Adult
    resultsMIROC[i,who,] <- data[,paste("Adult.Abs.BA.",who,sep=".")] + 
      data[,paste("Sapl.Abs.BA.",who,sep=".")]
    resultsMIROCDens[i,who,] <- data[,paste("Adult.Abs.Den.",who,sep=".")] +
      data[,paste("Sapl.Abs.Den.",who,sep=".")]
  }
}


info_data <- read.table("C:/Users/ucmuser/SORTIE2/models/data/treeyears.txt",header=T,row.names = 1)
info_plot <- read.table("C:/Users/ucmuser/SORTIE2/models/data/PlotInfo.txt",header=T,sep="\t",row.names = 1)

#########################################################
#********update to match functionality of validation runs analysis R files. 

#######################################################################
### basal area for all species in BBBPIPO
# "ABCO" "CADE" "PILA" "PIPO "QUCH" "QUKE"
# calculating probability over all 20 runs

ABCOinterBA <- apply(results[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA <- apply(results[,'ABCO',],2,median)

CADEinterBA <- apply(results[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA <- apply(results[,'CADE',],2,median)

PILAinterBA <- apply(results[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA <- apply(results[,'PILA',],2,median)

PIPOinterBA <- apply(results[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedBA <- apply(results[,'PIPO',],2,median)

QUCHinterBA <- apply(results[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedBA <- apply(results[,'QUCH',],2,median)

QUKEinterBA <- apply(results[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA <- apply(results[,'QUKE',],2,median)

simnames1 <- info_data[plot,]
simnames <- simnames1[2:7]

### CNRM
ABCOinterBA_CNRM <- apply(resultsCNRM[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA_CNRM <- apply(resultsCNRM[,'ABCO',],2,median)

CADEinterBA_CNRM <- apply(resultsCNRM[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA_CNRM <- apply(resultsCNRM[,'CADE',],2,median)

PILAinterBA_CNRM <- apply(resultsCNRM[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA_CNRM <- apply(resultsCNRM[,'PILA',],2,median)

PIPOinterBA_CNRM <- apply(resultsCNRM[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedBA_CNRM <- apply(resultsCNRM[,'PIPO',],2,median)

QUCHinterBA_CNRM <- apply(resultsCNRM[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedBA_CNRM <- apply(resultsCNRM[,'QUCH',],2,median)

QUKEinterBA_CNRM <- apply(resultsCNRM[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA_CNRM <- apply(resultsCNRM[,'QUKE',],2,median)

### CCSM
ABCOinterBA_CCSM <- apply(resultsCCSM[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA_CCSM <- apply(resultsCCSM[,'ABCO',],2,median)

CADEinterBA_CCSM <- apply(resultsCCSM[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA_CCSM <- apply(resultsCCSM[,'CADE',],2,median)

PILAinterBA_CCSM <- apply(resultsCCSM[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA_CCSM <- apply(resultsCCSM[,'PILA',],2,median)

PIPOinterBA_CCSM <- apply(resultsCCSM[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedBA_CCSM <- apply(resultsCCSM[,'PIPO',],2,median)

QUCHinterBA_CCSM <- apply(resultsCCSM[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedBA_CCSM <- apply(resultsCCSM[,'QUCH',],2,median)

QUKEinterBA_CCSM <- apply(resultsCCSM[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA_CCSM <- apply(resultsCCSM[,'QUKE',],2,median)

### MIROC
ABCOinterBA_MIROC <- apply(resultsMIROC[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedBA_MIROC <- apply(resultsMIROC[,'ABCO',],2,median)

CADEinterBA_MIROC <- apply(resultsMIROC[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedBA_MIROC <- apply(resultsMIROC[,'CADE',],2,median)

PILAinterBA_MIROC <- apply(resultsMIROC[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedBA_MIROC <- apply(resultsMIROC[,'PILA',],2,median)

PIPOinterBA_MIROC <- apply(resultsMIROC[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedBA_MIROC <- apply(resultsMIROC[,'PIPO',],2,median)

QUCHinterBA_MIROC <- apply(resultsMIROC[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedBA_MIROC <- apply(resultsMIROC[,'QUCH',],2,median)

QUKEinterBA_MIROC <- apply(resultsMIROC[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedBA_MIROC <- apply(resultsMIROC[,'QUKE',],2,median)


#### plot each spp all sequences####
#ABCO
yrs <- as.double(na.exclude(as.double(simnames)))
rangeABCOBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOinterBA_CNRM,ABCOmedBA_CNRM,
                       ABCOinterBA_CCSM,ABCOmedBA_CCSM,ABCOinterBA_MIROC,ABCOmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_ABCO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangeABCOBA,main=paste('ABCO Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA_CNRM[1,],rev(ABCOinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA_CCSM[1,],rev(ABCOinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA_MIROC[1,],rev(ABCOinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#CADE
yrs <- as.double(na.exclude(as.double(simnames)))
rangeCADEBA <- range(c(CADEinterBA,CADEmedBA,CADEinterBA_CNRM,CADEmedBA_CNRM,
                       CADEinterBA_CCSM,CADEmedBA_CCSM,CADEinterBA_MIROC,CADEmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_CADE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedBA,type='l',ylim=rangeCADEBA,main=paste('CADE Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA_CNRM[1,],rev(CADEinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA_CCSM[1,],rev(CADEinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA_MIROC[1,],rev(CADEinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#PILA
yrs <- as.double(na.exclude(as.double(simnames)))
rangePILABA <- range(c(PILAinterBA,PILAmedBA,PILAinterBA_CNRM,PILAmedBA_CNRM,
                       PILAinterBA_CCSM,PILAmedBA_CCSM,PILAinterBA_MIROC,PILAmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_PILA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedBA,type='l',ylim=rangePILABA,main=paste('Comparing PILA Basal Area of Several Climate Sequences in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA_CNRM[1,],rev(PILAinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA_CCSM[1,],rev(PILAinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA_MIROC[1,],rev(PILAinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#PIPO
yrs <- as.double(na.exclude(as.double(simnames)))
rangePIPOBA <- range(c(PIPOinterBA,PIPOmedBA,PIPOinterBA_CNRM,PIPOmedBA_CNRM,
                       PIPOinterBA_CCSM,PIPOmedBA_CCSM,PIPOinterBA_MIROC,PIPOmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_PIPO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedBA,type='l',ylim=rangePIPOBA,main=paste('Comparing PIPO Basal Area of Several Climate Sequences in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA[1,],rev(PIPOinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA_CNRM[1,],rev(PIPOinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA_CCSM[1,],rev(PIPOinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA_MIROC[1,],rev(PIPOinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ),
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#QUCH
yrs <- as.double(na.exclude(as.double(simnames)))
rangeQUCHBA <- range(c(QUCHinterBA,QUCHmedBA,QUCHinterBA_CNRM,QUCHmedBA_CNRM,
                       QUCHinterBA_CCSM,QUCHmedBA_CCSM,QUCHinterBA_MIROC,QUCHmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_QUCH_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedBA,type='l',ylim=rangeQUCHBA,main=paste('Comparing QUCH Basal Area of Several Climate Sequences in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA[1,],rev(QUCHinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA_CNRM[1,],rev(QUCHinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA_CCSM[1,],rev(QUCHinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA_MIROC[1,],rev(QUCHinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#QUKE
yrs <- as.double(na.exclude(as.double(simnames)))
rangeQUKEBA <- range(c(QUKEinterBA,QUKEmedBA,QUKEinterBA_CNRM,QUKEmedBA_CNRM,
                       QUKEinterBA_CCSM,QUKEmedBA_CCSM,QUKEinterBA_MIROC,QUKEmedBA_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_QUKE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedBA,type='l',ylim=rangeQUKEBA,main=paste('QUKE Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA_CNRM[1,],rev(QUKEinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA_CCSM[1,],rev(QUKEinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA_MIROC[1,],rev(QUKEinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#### plot all spp one sequence ####
yrs <- as.double(na.exclude(as.double(simnames)))

rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA, CADEinterBA,CADEmedBA,
                       PILAinterBA,PILAmedBA,PIPOinterBA,PIPOmedBA,
                       QUCHinterBA,QUCHmedBA,QUKEinterBA,QUKEmedBA,
                       ABCOinterBA_CNRM,ABCOmedBA_CNRM,CADEinterBA_CNRM,CADEmedBA_CNRM,
                       PILAinterBA_CNRM,PILAmedBA_CNRM,PIPOinterBA_CNRM,PIPOmedBA_CNRM,
                       QUCHinterBA_CNRM,QUCHmedBA_CNRM,QUKEinterBA_CNRM,QUKEmedBA_CNRM))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste('Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA,col=c_PIPO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA[1,],rev(PIPOinterBA[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedBA,col=c_QUCH,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA[1,],rev(QUCHinterBA[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedBA_CNRM,col=c_ABCO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA_CNRM[1,],rev(ABCOinterBA_CNRM[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA_CNRM,col=c_CADE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA_CNRM[1,],rev(CADEinterBA_CNRM[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA_CNRM,col=c_PILA,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA_CNRM[1,],rev(PILAinterBA_CNRM[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA_CNRM,col=c_PIPO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA_CNRM[1,],rev(PIPOinterBA_CNRM[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedBA_CNRM,col=c_QUCH,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA_CNRM[1,],rev(QUCHinterBA_CNRM[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA_CNRM,col=c_QUKE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA_CNRM[1,],rev(QUKEinterBA_CNRM[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)



legend("topright",c('Current ABCO', 'Current CADE', 'Current PILA', 'Current PIPO', 'Current QUCH', 'Current QUKE', 'Actual ABCO',
                    'Future ABCO', 'Future CADE', 'Future PILA', 'Future PIPO', 'Future QUCH', 'Future QUKE'), inset=c(-0.32,0),
       col=c(c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE, c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
       lty = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), bty='n',cex=1)

dev.off()

#### plot all spp one at a time####
# only ABCO
rangePlotBA <- range(c(ABCOinterBA,ABCOmedBA,ABCOinterBA_CNRM, ABCOmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_ABCO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedBA,type='l',ylim=rangePlotBA,main=paste('ABCO Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA[1,],rev(ABCOinterBA[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedBA_CNRM,col=c_ABCO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterBA_CNRM[1,],rev(ABCOinterBA_CNRM[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)


legend("topright",c('Current ABCO', 'Future ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO, c_ABCO),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only CADE
rangePlotBA <- range(c(CADEinterBA,CADEmedBA,CADEinterBA_CNRM, CADEmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_CADE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedBA,type='l',ylim=rangePlotBA,main=paste('CADE Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA[1,],rev(CADEinterBA[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedBA_CNRM,col=c_CADE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterBA_CNRM[1,],rev(CADEinterBA_CNRM[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)


legend("topright",c('Current CADE', 'Future CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only PILA
rangePlotBA <- range(c(PILAinterBA,PILAmedBA,PILAinterBA_CNRM, PILAmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_PILA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedBA,type='l',ylim=rangePlotBA,main=paste('PILA Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA[1,],rev(PILAinterBA[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedBA_CNRM,col=c_PILA,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterBA_CNRM[1,],rev(PILAinterBA_CNRM[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)


legend("topright",c('Current PILA', 'Future PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only PIPO
rangePlotBA <- range(c(PIPOinterBA,PIPOmedBA,PIPOinterBA_CNRM, PIPOmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_PIPO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedBA,type='l',ylim=rangePlotBA,main=paste('PIPO Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_PIPO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA[1,],rev(PIPOinterBA[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedBA_CNRM,col=c_PIPO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterBA_CNRM[1,],rev(PIPOinterBA_CNRM[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)


legend("topright",c('Current PIPO', 'Future PIPO'), inset=c(-0.32,0),
       col=c(c_PIPO, c_PIPO, c_PIPO),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only QUCH
rangePlotBA <- range(c(QUCHinterBA,QUCHmedBA,QUCHinterBA_CNRM, QUCHmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_QUCH_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedBA,type='l',ylim=rangePlotBA,main=paste('QUCH Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_QUCH,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA[1,],rev(QUCHinterBA[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)


lines(yrsSim,QUCHmedBA_CNRM,col=c_QUCH,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterBA_CNRM[1,],rev(QUCHinterBA_CNRM[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)


legend("topright",c('Current QUCH',  'Future QUCH'), inset=c(-0.32,0),
       col=c(c_QUCH, c_QUCH),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only QUKE
rangePlotBA <- range(c(QUKEinterBA,QUKEmedBA,QUKEinterBA_CNRM, QUKEmedBA_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_QUKE_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedBA,type='l',ylim=rangePlotBA,main=paste('QUKE Comparing CNRM and Current Basal Area in',plot),lwd=2,ylab = "Basal area",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA[1,],rev(QUKEinterBA[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedBA_CNRM,col=c_QUKE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterBA_CNRM[1,],rev(QUKEinterBA_CNRM[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)


legend("topright",c('Current QUKE',  'Future QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

###################################
### Density of all species in BBBPIPO########

ABCOinterDens <- apply(resultsDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens <- apply(resultsDens[,'ABCO',],2,median)

CADEinterDens <- apply(resultsDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens <- apply(resultsDens[,'CADE',],2,median)

PILAinterDens <- apply(resultsDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens <- apply(resultsDens[,'PILA',],2,median)

PIPOinterDens <- apply(resultsDens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens <- apply(resultsDens[,'PIPO',],2,median)

QUCHinterDens <- apply(resultsDens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens <- apply(resultsDens[,'QUCH',],2,median)

QUKEinterDens <- apply(resultsDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens <- apply(resultsDens[,'QUKE',],2,median)

simnames1 <- info_data[plot,]
simnames <- simnames1[2:7]

### CNRM
ABCOinterDens_CNRM <- apply(resultsCNRMDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens_CNRM <- apply(resultsCNRMDens[,'ABCO',],2,median)

CADEinterDens_CNRM <- apply(resultsCNRMDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens_CNRM <- apply(resultsCNRMDens[,'CADE',],2,median)

PILAinterDens_CNRM <- apply(resultsCNRMDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens_CNRM <- apply(resultsCNRMDens[,'PILA',],2,median)

PIPOinterDens_CNRM <- apply(resultsCNRMDens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens_CNRM <- apply(resultsCNRMDens[,'PIPO',],2,median)

QUCHinterDens_CNRM <- apply(resultsCNRMDens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens_CNRM <- apply(resultsCNRMDens[,'QUCH',],2,median)

QUKEinterDens_CNRM <- apply(resultsCNRMDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens_CNRM <- apply(resultsCNRMDens[,'QUKE',],2,median)

### CCSM
ABCOinterDens_CCSM <- apply(resultsCCSMDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens_CCSM <- apply(resultsCCSMDens[,'ABCO',],2,median)

CADEinterDens_CCSM <- apply(resultsCCSMDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens_CCSM <- apply(resultsCCSMDens[,'CADE',],2,median)

PILAinterDens_CCSM <- apply(resultsCCSMDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens_CCSM <- apply(resultsCCSMDens[,'PILA',],2,median)

PIPOinterDens_CCSM <- apply(resultsCCSMDens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens_CCSM <- apply(resultsCCSMDens[,'PIPO',],2,median)

QUCHinterDens_CCSM <- apply(resultsCCSMDens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens_CCSM <- apply(resultsCCSMDens[,'QUCH',],2,median)

QUKEinterDens_CCSM <- apply(resultsCCSMDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens_CCSM <- apply(resultsCCSMDens[,'QUKE',],2,median)

### MIROC
ABCOinterDens_MIROC <- apply(resultsMIROCDens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens_MIROC <- apply(resultsMIROCDens[,'ABCO',],2,median)

CADEinterDens_MIROC <- apply(resultsMIROCDens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens_MIROC <- apply(resultsMIROCDens[,'CADE',],2,median)

PILAinterDens_MIROC <- apply(resultsMIROCDens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens_MIROC <- apply(resultsMIROCDens[,'PILA',],2,median)

PIPOinterDens_MIROC <- apply(resultsMIROCDens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens_MIROC <- apply(resultsMIROCDens[,'PIPO',],2,median)

QUCHinterDens_MIROC <- apply(resultsMIROCDens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens_MIROC <- apply(resultsMIROCDens[,'QUCH',],2,median)

QUKEinterDens_MIROC <- apply(resultsMIROCDens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens_MIROC <- apply(resultsMIROCDens[,'QUKE',],2,median)

#### plot each spp all sequences####
#ABCO
yrs <- as.double(na.exclude(as.double(simnames)))
rangeABCODens <- range(c(ABCOinterDens,ABCOmedDens,ABCOinterDens_CNRM,ABCOmedDens_CNRM,
                         ABCOinterDens_CCSM,ABCOmedDens_CCSM,ABCOinterDens_MIROC,ABCOmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_ABCO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangeABCODens,main=paste('ABCO Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens_CNRM[1,],rev(ABCOinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens_CCSM[1,],rev(ABCOinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens_MIROC[1,],rev(ABCOinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#CADE
yrs <- as.double(na.exclude(as.double(simnames)))
rangeCADEDens <- range(c(CADEinterDens,CADEmedDens,CADEinterDens_CNRM,CADEmedDens_CNRM,
                         CADEinterDens_CCSM,CADEmedDens_CCSM,CADEinterDens_MIROC,CADEmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_CADE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedDens,type='l',ylim=rangeCADEDens,main=paste('CADE Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens_CNRM[1,],rev(CADEinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens_CCSM[1,],rev(CADEinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens_MIROC[1,],rev(CADEinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#PILA
yrs <- as.double(na.exclude(as.double(simnames)))
rangePILADens <- range(c(PILAinterDens,PILAmedDens,PILAinterDens_CNRM,PILAmedDens_CNRM,
                         PILAinterDens_CCSM,PILAmedDens_CCSM,PILAinterDens_MIROC,PILAmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_PILA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedDens,type='l',ylim=rangePILADens,main=paste('Comparing PILA Density of Several Climate Sequences in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens_CNRM[1,],rev(PILAinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens_CCSM[1,],rev(PILAinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens_MIROC[1,],rev(PILAinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ),
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#PIPO
yrs <- as.double(na.exclude(as.double(simnames)))
rangePIPODens <- range(c(PIPOinterDens,PIPOmedDens,PIPOinterDens_CNRM,PIPOmedDens_CNRM,
                         PIPOinterDens_CCSM,PIPOmedDens_CCSM,PIPOinterDens_MIROC,PIPOmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_PIPO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedDens,type='l',ylim=rangePIPODens,main=paste('Comparing PIPO Density of Several Climate Sequences in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens_CNRM[1,],rev(PIPOinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens_CCSM[1,],rev(PIPOinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens_MIROC[1,],rev(PIPOinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#QUCH
yrs <- as.double(na.exclude(as.double(simnames)))
rangeQUCHDens <- range(c(QUCHinterDens,QUCHmedDens,QUCHinterDens_CNRM,QUCHmedDens_CNRM,
                         QUCHinterDens_CCSM,QUCHmedDens_CCSM,QUCHinterDens_MIROC,QUCHmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_QUCH_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedDens,type='l',ylim=rangeQUCHDens,main=paste('Comparing QUCH Density of Several Climate Sequences in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens_CNRM[1,],rev(QUCHinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens_CCSM[1,],rev(QUCHinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens_MIROC[1,],rev(QUCHinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ),
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

#QUKE
yrs <- as.double(na.exclude(as.double(simnames)))
rangeQUKEDens <- range(c(QUKEinterDens,QUKEmedDens,QUKEinterDens_CNRM,QUKEmedDens_CNRM,
                         QUKEinterDens_CCSM,QUKEmedDens_CCSM,QUKEinterDens_MIROC,QUKEmedDens_MIROC))
yrsSim <- (0:simulation_length)+yrs[1]
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_seq_QUKE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedDens,type='l',ylim=rangeQUKEDens,main=paste('QUKE Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens_CNRM[1,],rev(QUKEinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens_CCSM[1,],rev(QUKEinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens_MIROC[1,],rev(QUKEinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c('Current', 'CNRM 8.5', 'CCSM 8.5', 'MIROC 8.5' ), 
       inset=c(-0.25,0),
       col= c(c_current, c_cnrm, c_ccsm, c_miroc),
       lwd= c(2, 2, 2, 2),
       title = "Climate Sequence")

dev.off()

####plot all spp in one sequence####
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens,CADEinterDens,CADEmedDens,
                         PILAinterDens,PILAmedDens,PIPOinterDens,PIPOmedDens,
                         QUCHinterDens,QUCHmedDens,QUKEinterDens,QUKEmedDens,
                         ABCOinterDens_CNRM,ABCOmedDens_CNRM,CADEinterDens_CNRM,CADEmedDens_CNRM,
                         PILAinterDens_CNRM,PILAmedDens_CNRM,PIPOinterDens_CNRM,PIPOmedDens_CNRM,
                         QUCHinterDens_CNRM,QUCHmedDens_CNRM,QUKEinterDens_CNRM,QUKEmedDens_CNRM))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens,col=c_PIPO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens,col=c_QUCH,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedDens_CNRM,col=c_ABCO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens_CNRM[1,],rev(ABCOinterDens_CNRM[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens_CNRM,col=c_CADE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens_CNRM[1,],rev(CADEinterDens_CNRM[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens_CNRM,col=c_PILA,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens_CNRM[1,],rev(PILAinterDens_CNRM[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens_CNRM,col=c_PIPO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens_CNRM[1,],rev(PIPOinterDens_CNRM[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens_CNRM,col=c_QUCH,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens_CNRM[1,],rev(QUCHinterDens_CNRM[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens_CNRM,col=c_QUKE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens_CNRM[1,],rev(QUKEinterDens_CNRM[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)



legend("topright",c('Current ABCO', 'Current CADE', 'Current PILA', 'Current PIPO', 'Current QUCH', 'Current QUKE', 'Actual ABCO',
                    'Future ABCO', 'Future CADE', 'Future PILA', 'Future PIPO', 'Future QUCH', 'Future QUKE'), inset=c(-0.32,0),
       col=c(c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE, c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
       lty = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2), bty='n',cex=1)

dev.off()

####one spp per climate sequence####
# only ABCO
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens, ABCOinterDens_CNRM, ABCOmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_ABCO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('ABCO Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,ABCOmedDens_CNRM,col=c_ABCO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens_CNRM[1,],rev(ABCOinterDens_CNRM[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)


legend("topright", c('Current ABCO', 'Future ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO,c_ABCO),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only CADE
rangePlotDens <- range(c(CADEinterDens,CADEmedDens,CADEinterDens_CNRM, CADEmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_CADE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedDens,type='l',ylim=rangePlotDens,main=paste('CADE Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens_CNRM,col=c_CADE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens_CNRM[1,],rev(CADEinterDens_CNRM[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)


legend("topright",c('Current CADE', 'Future CADE'), inset=c(-0.32,0),
       col=c(c_CADE, c_CADE),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only PILA
rangePlotDens <- range(c(PILAinterDens,PILAmedDens,PILAinterDens_CNRM, PILAmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_PILA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedDens,type='l',ylim=rangePlotDens,main=paste('PILA Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)


lines(yrsSim,PILAmedDens_CNRM,col=c_PILA,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens_CNRM[1,],rev(PILAinterDens_CNRM[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)


legend("topright",c('Current PILA', 'Future PILA'), inset=c(-0.32,0),
       col=c(c_PILA, c_PILA),lwd= c(2, 2),  lty = c(1, 2), bty='n',cex=1)

dev.off()

# only PIPO
rangePlotDens <- range(c(PIPOinterDens,PIPOmedDens, PIPOinterDens_CNRM, PIPOmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_PIPO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedDens,type='l',ylim=rangePlotDens,main=paste('PIPO Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PIPO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens_CNRM,col=c_PIPO,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens_CNRM[1,],rev(PIPOinterDens_CNRM[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)


legend("topright",c('Current PIPO',  'Future PIPO'), inset=c(-0.32,0),
       col=c(c_PIPO, c_PIPO),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only QUCH
rangePlotDens <- range(c(QUCHinterDens,QUCHmedDens, QUCHinterDens_CNRM, QUCHmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_QUCH_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedDens,type='l',ylim=rangePlotDens,main=paste('QUCH Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUCH,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens_CNRM,col=c_QUCH,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens_CNRM[1,],rev(QUCHinterDens_CNRM[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)


legend("topright",c('Current QUCH', 'Future QUCH'), inset=c(-0.32,0),
       col=c(c_QUCH, c_QUCH),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()

# only QUKE
rangePlotDens <- range(c(QUKEinterDens,QUKEmedDens, QUKEinterDens_CNRM, QUKEmedDens_CNRM))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/BB_FC_QUKE_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedDens,type='l',ylim=rangePlotDens,main=paste('QUKE Comparing CNRM and Current Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens_CNRM,col=c_QUKE,lwd=2, lty = 2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens_CNRM[1,],rev(QUKEinterDens_CNRM[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)


legend("topright",c('Current QUKE', 'Future QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE, c_QUKE),lwd= c(2, 2), lty = c(1, 2), bty='n',cex=1)

dev.off()
###################################
### CNRM Climate Sequence Density of SEEDLINGS in all species in BBBPIPO

ABCOinterDens <- apply(sdl_Dens[,'ABCO',],2,quantile,c(0.025,0.975))
ABCOmedDens <- apply(sdl_Dens[,'ABCO',],2,median)

CADEinterDens <- apply(sdl_Dens[,'CADE',],2,quantile,c(0.025,0.975))
CADEmedDens <- apply(sdl_Dens[,'CADE',],2,median)

PILAinterDens <- apply(sdl_Dens[,'PILA',],2,quantile,c(0.025,0.975))
PILAmedDens <- apply(sdl_Dens[,'PILA',],2,median)

PIPOinterDens <- apply(sdl_Dens[,'PIPO',],2,quantile,c(0.025,0.975))
PIPOmedDens <- apply(sdl_Dens[,'PIPO',],2,median)

QUCHinterDens <- apply(sdl_Dens[,'QUCH',],2,quantile,c(0.025,0.975))
QUCHmedDens <- apply(sdl_Dens[,'QUCH',],2,median)

QUKEinterDens <- apply(sdl_Dens[,'QUKE',],2,quantile,c(0.025,0.975))
QUKEmedDens <- apply(sdl_Dens[,'QUKE',],2,median)


####
# plot
yrs <- as.double(names(ABCOfdDBH))
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens, CADEinterDens,CADEmedDens,
                         PILAinterDens,PILAmedDens, PIPOinterDens,PIPOmedDens,
                         QUCHinterDens,QUCHmedDens, QUKEinterDens,QUKEmedDens))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_sdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of', plot),lwd=2, ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)

lines(yrsSim,CADEmedDens,col=c_CADE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)

lines(yrsSim,PILAmedDens,col=c_PILA,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)

lines(yrsSim,PIPOmedDens,col=c_PIPO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)

lines(yrsSim,QUCHmedDens,col=c_QUCH,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)

lines(yrsSim,QUKEmedDens,col=c_QUKE,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)

legend("topright",c('Simulated ABCO', 'Simulated CADE', 'Simulated PILA', 'Simulated PIPO', 'Simulated QUCH', 'Simulated QUKE'), 
       inset=c(-0.32,0),lty=1,
       col=c(c_ABCO, c_CADE, c_PILA, c_PIPO, c_QUCH, c_QUKE),
       lwd= c(2, 2, 2, 2, 2, 2),
       bty='n',cex=1)

dev.off()

# only ABCO
rangePlotDens <- range(c(ABCOinterDens,ABCOmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_ABCOsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABCOmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABCO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABCOinterDens[1,],rev(ABCOinterDens[2,])), col=adjustcolor(c_ABCO,alpha.f=0.2), border = NA)


legend("topright",c('Simulated ABCO'), inset=c(-0.32,0),
       col=c(c_ABCO),lwd=c(3), bty='n',cex=1)

dev.off()

# only CADE
rangePlotDens <- range(c(CADEinterDens,CADEmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_CADEsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,CADEmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_CADE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(CADEinterDens[1,],rev(CADEinterDens[2,])), col=adjustcolor(c_CADE,alpha.f=0.2), border = NA)


legend("topright",c('Simulated CADE'), inset=c(-0.32,0),
       col=c(c_CADE),lwd=c(3), bty='n',cex=1)
dev.off()

# only PILA
rangePlotDens <- range(c(PILAinterDens,PILAmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_PILAsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PILAmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PILA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PILAinterDens[1,],rev(PILAinterDens[2,])), col=adjustcolor(c_PILA,alpha.f=0.2), border = NA)


legend("topright",c('Simulated PILA'), inset=c(-0.32,0),
       col=c(c_PILA),lwd=c(3), bty='n',cex=1)
dev.off()

# only PIPO
rangePlotDens <- range(c(PIPOinterDens,PIPOmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_PIPOsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PIPOmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PIPO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PIPOinterDens[1,],rev(PIPOinterDens[2,])), col=adjustcolor(c_PIPO,alpha.f=0.2), border = NA)


legend("topright",c('Simulated PIPO'), inset=c(-0.32,0),
       col=c(c_PIPO),lwd=c(3), bty='n',cex=1)
dev.off()

# only QUCH
rangePlotDens <- range(c(QUCHinterDens,QUCHmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_QUCHsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUCHmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUCH,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUCHinterDens[1,],rev(QUCHinterDens[2,])), col=adjustcolor(c_QUCH,alpha.f=0.2), border = NA)


legend("topright",c('Simulated QUCH'), inset=c(-0.32,0),
       col=c(c_QUCH),lwd=c(3), bty='n',cex=1)
dev.off()

# only QUKE
rangePlotDens <- range(c(QUKEinterDens,QUKEmedDens,QUKEtotDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/CNRM/Graphs 20x/BBBPIPO_QUKEsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,QUKEmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Climate Sequence Density of',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_QUKE,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(QUKEinterDens[1,],rev(QUKEinterDens[2,])), col=adjustcolor(c_QUKE,alpha.f=0.2), border = NA)


legend("topright",c('Simulated QUKE'), inset=c(-0.32,0),
       col=c(c_QUKE),lwd=c(3), bty='n',cex=1)
dev.off()





#####################LOOKING AT TOTAL BASAL AREA AND DENSITY IN EACH SEQUENCE##############################

percent.change <- function(first, last){
  ((last-first)/first)*100 
}
slices <- c("BA Control", "BA CNRM", "BA CCSM", "BA MIROC", "Dens Control",  "Dens CNRM", 
             "Dens CCSM",  "Dens MIROC")


total_array <- array(NA, dim = c(20, 94, 8),
                     dimnames = list(1:20,
                                     1:94,
                                     slices))

for(i in 1:(simulation_length)){
      total_array[,i,1] <- rowSums(results[,,i])
      total_array[,i,2] <- rowSums(resultsCNRM[,,i])
      total_array[,i,3] <- rowSums(resultsCCSM[,,i])
      total_array[,i,4] <- rowSums(resultsMIROC[,,i])
      total_array[,i,5] <- rowSums(resultsDens[,,i])
      total_array[,i,6] <- rowSums(resultsCNRMDens[,,i])
      total_array[,i,7] <- rowSums(resultsCCSMDens[,,i])
      total_array[,i,8] <- rowSums(resultsMIROCDens[,,i])
  }

changeperc <- matrix(0, 6, 8)
colnames(changeperc) <- slices
rownames(changeperc) <- c("Total", "ABCO", "CADE", "PILA", "PIPO", "QUKE")

changeperc["Total", "BA Control"] <- percent.change(mean(total_array[,1,"BA Control"]), mean(total_array[,93,"BA Control"]))
changeperc["Total", "BA CNRM"] <- percent.change(mean(total_array[,1,"BA CNRM"]), mean(total_array[,93,"BA CNRM"]))
changeperc["Total", "BA CCSM"] <-percent.change(mean(total_array[,1,"BA CCSM"]), mean(total_array[,93,"BA CCSM"]))
changeperc["Total", "BA MIROC"] <- percent.change(mean(total_array[,1,"BA MIROC"]), mean(total_array[,93,"BA MIROC"]))
changeperc["Total", "Dens Control"] <- percent.change(mean(total_array[,1,"Dens Control"]), mean(total_array[,93,"Dens Control"]))
changeperc["Total", "Dens CNRM"] <- percent.change(mean(total_array[,1,"Dens CNRM"]), mean(total_array[,93,"Dens CNRM"]))
changeperc["Total", "Dens CCSM"] <- percent.change(mean(total_array[,1,"Dens CCSM"]), mean(total_array[,93,"Dens CCSM"]))
changeperc["Total", "Dens MIROC"] <- percent.change(mean(total_array[,1,"Dens MIROC"]), mean(total_array[,93,"Dens MIROC"]))


#####Now for each species individually####
#ABCO

ABCO_array <- array(NA, dim = c(20, 94, 8),
                     dimnames = list(1:20,
                                     1:94,
                                     slices))

for(i in 1:(simulation_length)){
  ABCO_array[,i,1] <- results[,"ABCO",i]
  ABCO_array[,i,2] <- resultsCNRM[,"ABCO",i]
  ABCO_array[,i,3] <- resultsCCSM[,"ABCO",i]
  ABCO_array[,i,4] <- resultsMIROC[,"ABCO",i]
  ABCO_array[,i,5] <- resultsDens[,"ABCO",i]
  ABCO_array[,i,6] <- resultsCNRMDens[,"ABCO",i]
  ABCO_array[,i,7] <- resultsCCSMDens[,"ABCO",i]
  ABCO_array[,i,8] <- resultsMIROCDens[,"ABCO",i]
}

changeperc["ABCO", "BA Control"] <- percent.change(mean(ABCO_array[,1,"BA Control"]), mean(ABCO_array[,93,"BA Control"]))
changeperc["ABCO", "BA CNRM"] <- percent.change(mean(ABCO_array[,1,"BA CNRM"]), mean(ABCO_array[,93,"BA CNRM"]))
changeperc["ABCO", "BA CCSM"] <-percent.change(mean(ABCO_array[,1,"BA CCSM"]), mean(ABCO_array[,93,"BA CCSM"]))
changeperc["ABCO", "BA MIROC"] <- percent.change(mean(ABCO_array[,1,"BA MIROC"]), mean(ABCO_array[,93,"BA MIROC"]))
changeperc["ABCO", "Dens Control"] <- percent.change(mean(ABCO_array[,1,"Dens Control"]), mean(ABCO_array[,93,"Dens Control"]))
changeperc["ABCO", "Dens CNRM"] <- percent.change(mean(ABCO_array[,1,"Dens CNRM"]), mean(ABCO_array[,93,"Dens CNRM"]))
changeperc["ABCO", "Dens CCSM"] <- percent.change(mean(ABCO_array[,1,"Dens CCSM"]), mean(ABCO_array[,93,"Dens CCSM"]))
changeperc["ABCO", "Dens MIROC"] <- percent.change(mean(ABCO_array[,1,"Dens MIROC"]), mean(ABCO_array[,93,"Dens MIROC"]))

print(changeperc)

#CADE

CADE_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  CADE_array[,i,1] <- results[,"CADE",i]
  CADE_array[,i,2] <- resultsCNRM[,"CADE",i]
  CADE_array[,i,3] <- resultsCCSM[,"CADE",i]
  CADE_array[,i,4] <- resultsMIROC[,"CADE",i]
  CADE_array[,i,5] <- resultsDens[,"CADE",i]
  CADE_array[,i,6] <- resultsCNRMDens[,"CADE",i]
  CADE_array[,i,7] <- resultsCCSMDens[,"CADE",i]
  CADE_array[,i,8] <- resultsMIROCDens[,"CADE",i]
}

changeperc["CADE", "BA Control"] <- percent.change(mean(CADE_array[,1,"BA Control"]), mean(CADE_array[,93,"BA Control"]))
changeperc["CADE", "BA CNRM"] <- percent.change(mean(CADE_array[,1,"BA CNRM"]), mean(CADE_array[,93,"BA CNRM"]))
changeperc["CADE", "BA CCSM"] <-percent.change(mean(CADE_array[,1,"BA CCSM"]), mean(CADE_array[,93,"BA CCSM"]))
changeperc["CADE", "BA MIROC"] <- percent.change(mean(CADE_array[,1,"BA MIROC"]), mean(CADE_array[,93,"BA MIROC"]))
changeperc["CADE", "Dens Control"] <- percent.change(mean(CADE_array[,1,"Dens Control"]), mean(CADE_array[,93,"Dens Control"]))
changeperc["CADE", "Dens CNRM"] <- percent.change(mean(CADE_array[,1,"Dens CNRM"]), mean(CADE_array[,93,"Dens CNRM"]))
changeperc["CADE", "Dens CCSM"] <- percent.change(mean(CADE_array[,1,"Dens CCSM"]), mean(CADE_array[,93,"Dens CCSM"]))
changeperc["CADE", "Dens MIROC"] <- percent.change(mean(CADE_array[,1,"Dens MIROC"]), mean(CADE_array[,93,"Dens MIROC"]))

print(changeperc)

#PILA

PILA_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  PILA_array[,i,1] <- results[,"PILA",i]
  PILA_array[,i,2] <- resultsCNRM[,"PILA",i]
  PILA_array[,i,3] <- resultsCCSM[,"PILA",i]
  PILA_array[,i,4] <- resultsMIROC[,"PILA",i]
  PILA_array[,i,5] <- resultsDens[,"PILA",i]
  PILA_array[,i,6] <- resultsCNRMDens[,"PILA",i]
  PILA_array[,i,7] <- resultsCCSMDens[,"PILA",i]
  PILA_array[,i,8] <- resultsMIROCDens[,"PILA",i]
}

changeperc["PILA", "BA Control"] <- percent.change(mean(PILA_array[,1,"BA Control"]), mean(PILA_array[,93,"BA Control"]))
changeperc["PILA", "BA CNRM"] <- percent.change(mean(PILA_array[,1,"BA CNRM"]), mean(PILA_array[,93,"BA CNRM"]))
changeperc["PILA", "BA CCSM"] <-percent.change(mean(PILA_array[,1,"BA CCSM"]), mean(PILA_array[,93,"BA CCSM"]))
changeperc["PILA", "BA MIROC"] <- percent.change(mean(PILA_array[,1,"BA MIROC"]), mean(PILA_array[,93,"BA MIROC"]))
changeperc["PILA", "Dens Control"] <- percent.change(mean(PILA_array[,1,"Dens Control"]), mean(PILA_array[,93,"Dens Control"]))
changeperc["PILA", "Dens CNRM"] <- percent.change(mean(PILA_array[,1,"Dens CNRM"]), mean(PILA_array[,93,"Dens CNRM"]))
changeperc["PILA", "Dens CCSM"] <- percent.change(mean(PILA_array[,1,"Dens CCSM"]), mean(PILA_array[,93,"Dens CCSM"]))
changeperc["PILA", "Dens MIROC"] <- percent.change(mean(PILA_array[,1,"Dens MIROC"]), mean(PILA_array[,93,"Dens MIROC"]))

print(changeperc)

#PIPO

PIPO_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  PIPO_array[,i,1] <- results[,"PIPO",i]
  PIPO_array[,i,2] <- resultsCNRM[,"PIPO",i]
  PIPO_array[,i,3] <- resultsCCSM[,"PIPO",i]
  PIPO_array[,i,4] <- resultsMIROC[,"PIPO",i]
  PIPO_array[,i,5] <- resultsDens[,"PIPO",i]
  PIPO_array[,i,6] <- resultsCNRMDens[,"PIPO",i]
  PIPO_array[,i,7] <- resultsCCSMDens[,"PIPO",i]
  PIPO_array[,i,8] <- resultsMIROCDens[,"PIPO",i]
}

changeperc["PIPO", "BA Control"] <- percent.change(mean(PIPO_array[,1,"BA Control"]), mean(PIPO_array[,93,"BA Control"]))
changeperc["PIPO", "BA CNRM"] <- percent.change(mean(PIPO_array[,1,"BA CNRM"]), mean(PIPO_array[,93,"BA CNRM"]))
changeperc["PIPO", "BA CCSM"] <-percent.change(mean(PIPO_array[,1,"BA CCSM"]), mean(PIPO_array[,93,"BA CCSM"]))
changeperc["PIPO", "BA MIROC"] <- percent.change(mean(PIPO_array[,1,"BA MIROC"]), mean(PIPO_array[,93,"BA MIROC"]))
changeperc["PIPO", "Dens Control"] <- percent.change(mean(PIPO_array[,1,"Dens Control"]), mean(PIPO_array[,93,"Dens Control"]))
changeperc["PIPO", "Dens CNRM"] <- percent.change(mean(PIPO_array[,1,"Dens CNRM"]), mean(PIPO_array[,93,"Dens CNRM"]))
changeperc["PIPO", "Dens CCSM"] <- percent.change(mean(PIPO_array[,1,"Dens CCSM"]), mean(PIPO_array[,93,"Dens CCSM"]))
changeperc["PIPO", "Dens MIROC"] <- percent.change(mean(PIPO_array[,1,"Dens MIROC"]), mean(PIPO_array[,93,"Dens MIROC"]))

print(changeperc)

#QUKE

QUKE_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  QUKE_array[,i,1] <- results[,"QUKE",i]
  QUKE_array[,i,2] <- resultsCNRM[,"QUKE",i]
  QUKE_array[,i,3] <- resultsCCSM[,"QUKE",i]
  QUKE_array[,i,4] <- resultsMIROC[,"QUKE",i]
  QUKE_array[,i,5] <- resultsDens[,"QUKE",i]
  QUKE_array[,i,6] <- resultsCNRMDens[,"QUKE",i]
  QUKE_array[,i,7] <- resultsCCSMDens[,"QUKE",i]
  QUKE_array[,i,8] <- resultsMIROCDens[,"QUKE",i]
}

changeperc["QUKE", "BA Control"] <- percent.change(mean(QUKE_array[,1,"BA Control"]), mean(QUKE_array[,93,"BA Control"]))
changeperc["QUKE", "BA CNRM"] <- percent.change(mean(QUKE_array[,1,"BA CNRM"]), mean(QUKE_array[,93,"BA CNRM"]))
changeperc["QUKE", "BA CCSM"] <-percent.change(mean(QUKE_array[,1,"BA CCSM"]), mean(QUKE_array[,93,"BA CCSM"]))
changeperc["QUKE", "BA MIROC"] <- percent.change(mean(QUKE_array[,1,"BA MIROC"]), mean(QUKE_array[,93,"BA MIROC"]))
changeperc["QUKE", "Dens Control"] <- percent.change(mean(QUKE_array[,1,"Dens Control"]), mean(QUKE_array[,93,"Dens Control"]))
changeperc["QUKE", "Dens CNRM"] <- percent.change(mean(QUKE_array[,1,"Dens CNRM"]), mean(QUKE_array[,93,"Dens CNRM"]))
changeperc["QUKE", "Dens CCSM"] <- percent.change(mean(QUKE_array[,1,"Dens CCSM"]), mean(QUKE_array[,93,"Dens CCSM"]))
changeperc["QUKE", "Dens MIROC"] <- percent.change(mean(QUKE_array[,1,"Dens MIROC"]), mean(QUKE_array[,93,"Dens MIROC"]))

changeperc <- round(changeperc, 2)

print(changeperc)

write.table(changeperc, "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/BBBPIPO/Graphs/7.6.20/Percent_change.txt", sep = "\t", quote = F)
