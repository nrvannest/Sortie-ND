# Batch result POFLABMA runs and Percent change analysis

##################################################################################################################################
##################################################################################################################################

rm(list=ls())
repet <- 10
simulation_length <- 93
plot <- 'POFLABMA'
numsubplots <- 1
species <- c("ABCO","CADE" ,"PILA", "PIPO" ,"QUCH" ,"QUKE", "PICO" ,"PIMO", "PIJE", "ABMA")
c_ABCO <- 'blue4'; c_ABMA <- 'forestgreen'; c_CADE <- 'deeppink2'; c_PICO <- 'chocolate4'
c_PIJE <- 'grey51'; c_PILA <- 'deepskyblue'; c_PIMO <- 'lightcoral'; c_PIPO <- 'orangered3'
c_QUCH <- 'darkmagenta'; c_QUKE <- 'darkgoldenrod1'
c_current <- "forestgreen" ; c_cnrm <- "purple" ; c_ccsm <- "orangered" ; c_miroc <- "turquoise2";


# to store the results
results <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(results)[2] <- list(species)
dimnames(results)[3] <- list(1:(simulation_length+1))
resultsDens <- results


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Current/Output/POFLABMA_CNTRL_",i,".out")
  
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

# to store the results CNRM
resultsCNRM <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(resultsCNRM)[2] <- list(species)
dimnames(resultsCNRM)[3] <- list(1:(simulation_length+1))
resultsCNRMDens <- resultsCNRM


for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Output/PO_CNRM_",i,".out")
  
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
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CCSM/Output/PO_CCSM_",i,".out")
  
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
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/MIROC/Output/POFLABMA_MIROC_",i,".out")
  
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


#######################################################################
### basal area for all species in POFLABMA
# "ABMA" "PICO"

#Current
ABMAinterBA <- apply(results[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA <- apply(results[,'ABMA',],2,median)

PICOinterBA <- apply(results[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedBA <- apply(results[,'PICO',],2,median)

#CNRM
ABMAinterBA_CNRM <- apply(resultsCNRM[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA_CNRM <- apply(resultsCNRM[,'ABMA',],2,median)

PICOinterBA_CNRM <- apply(resultsCNRM[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedBA_CNRM <- apply(resultsCNRM[,'PICO',],2,median)

#CCSM
ABMAinterBA_CCSM <- apply(resultsCCSM[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA_CCSM <- apply(resultsCCSM[,'ABMA',],2,median)

PICOinterBA_CCSM <- apply(resultsCCSM[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedBA_CCSM <- apply(resultsCCSM[,'PICO',],2,median)

#MIROC
ABMAinterBA_MIROC <- apply(resultsMIROC[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedBA_MIROC <- apply(resultsMIROC[,'ABMA',],2,median)

PICOinterBA_MIROC <- apply(resultsMIROC[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedBA_MIROC <- apply(resultsMIROC[,'PICO',],2,median)

simnames1 <- info_data[plot,]
simnames <- simnames1[2:7]

##### plot all clim seq with each spp individually######

#ABMA
yrs <- as.double(na.exclude(as.double(simnames)))
yrsSim <- (0:simulation_length)+yrs[1]

rangeABMABA <- range(c(ABMAinterBA, ABMAmedBA, ABMAinterBA_CNRM, ABMAmedBA_CNRM, 
                       ABMAinterBA_CCSM, ABMAmedBA_CCSM, ABMAinterBA_MIROC, ABMAmedBA_MIROC))
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Graphs/7.6.20/POFLABMA_ABMA_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedBA,type='l',ylim=rangeABMABA,main=paste('ABMA Basal Area in',plot, 'Under Climate Scenarios'),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA[1,],rev(ABMAinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA_CNRM[1,],rev(ABMAinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA_CCSM[1,],rev(ABMAinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterBA_MIROC[1,],rev(ABMAinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c("Current", "CNRM 8.5", "CCSM 8.5", "MIROC 8.5"), 
       inset=c(-0.25,0), 
       bty='n',
       col=c(c_current, c_cnrm, c_ccsm, c_miroc), 
       lwd= c(2, 2),
       cex=1)

dev.off()

#PICO

rangePICOBA <- range(c(PICOinterBA, PICOmedBA, PICOinterBA_CNRM, PICOmedBA_CNRM, 
                       PICOinterBA_CCSM, PICOmedBA_CCSM, PICOinterBA_MIROC, PICOmedBA_MIROC))
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Graphs/7.6.20/POFLABMA_PICO_BA.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PICOmedBA,type='l',ylim=rangePICOBA,main=paste('PICO Basal Area in',plot, 'Under Climate Scenarios'),lwd=2,ylab = "Basal area",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA[1,],rev(PICOinterBA[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedBA_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA_CNRM[1,],rev(PICOinterBA_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedBA_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA_CCSM[1,],rev(PICOinterBA_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedBA_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterBA_MIROC[1,],rev(PICOinterBA_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c("Current", "CNRM 8.5", "CCSM 8.5", "MIROC 8.5"), 
       inset=c(-0.25,0), 
       bty='n',
       col=c(c_current, c_cnrm, c_ccsm, c_miroc), 
       lwd= c(2, 2),
       cex=1)

dev.off()

###################################
### Density of all species in POFLABMA

ABMAinterDens <- apply(resultsDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens <- apply(resultsDens[,'ABMA',],2,median)

PICOinterDens <- apply(resultsDens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens <- apply(resultsDens[,'PICO',],2,median)

ABMAinterDens_CNRM <- apply(resultsCNRMDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens_CNRM <- apply(resultsCNRMDens[,'ABMA',],2,median)

PICOinterDens_CNRM <- apply(resultsCNRMDens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens_CNRM <- apply(resultsCNRMDens[,'PICO',],2,median)

ABMAinterDens_CCSM <- apply(resultsCCSMDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens_CCSM <- apply(resultsCCSMDens[,'ABMA',],2,median)

PICOinterDens_CCSM <- apply(resultsCCSMDens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens_CCSM <- apply(resultsCCSMDens[,'PICO',],2,median)

ABMAinterDens_MIROC <- apply(resultsMIROCDens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens_MIROC <- apply(resultsMIROCDens[,'ABMA',],2,median)

PICOinterDens_MIROC <- apply(resultsMIROCDens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens_MIROC <- apply(resultsMIROCDens[,'PICO',],2,median)
####one spp with all clim seq####
#ABMA
yrs <- as.double(na.exclude(as.double(simnames)))
yrsSim <- (0:simulation_length)+yrs[1]

rangeABMADens <- range(c(ABMAinterDens, ABMAmedDens, ABMAinterDens_CNRM, ABMAmedDens_CNRM, 
                       ABMAinterDens_CCSM, ABMAmedDens_CCSM, ABMAinterDens_MIROC, ABMAmedDens_MIROC))
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Graphs/7.6.20/POFLABMA_ABMA_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangeABMADens,main=paste('ABMA Density in',plot, 'Under Climate Scenarios'),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens_CNRM[1,],rev(ABMAinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens_CCSM[1,],rev(ABMAinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,ABMAmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens_MIROC[1,],rev(ABMAinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c("Current", "CNRM 8.5", "CCSM 8.5", "MIROC 8.5"), 
       inset=c(-0.25,0), 
       bty='n',
       col=c(c_current, c_cnrm, c_ccsm, c_miroc), 
       lwd= c(2, 2),
       cex=1)

dev.off()

#PICO

rangePICODens <- range(c(PICOinterDens, PICOmedDens, PICOinterDens_CNRM, PICOmedDens_CNRM, 
                       PICOinterDens_CCSM, PICOmedDens_CCSM, PICOinterDens_MIROC, PICOmedDens_MIROC))
png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Graphs/7.6.20/POFLABMA_PICO_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PICOmedDens,type='l',ylim=rangePICODens,main=paste('PICO Density in',plot, 'Under Climate Scenarios'),lwd=2,ylab = "Density",xlab = "Year",col=c_current,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens[1,],rev(PICOinterDens[2,])), col=adjustcolor(c_current,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedDens_CNRM,col=c_cnrm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens_CNRM[1,],rev(PICOinterDens_CNRM[2,])), col=adjustcolor(c_cnrm,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedDens_CCSM,col=c_ccsm,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens_CCSM[1,],rev(PICOinterDens_CCSM[2,])), col=adjustcolor(c_ccsm,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedDens_MIROC,col=c_miroc,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens_MIROC[1,],rev(PICOinterDens_MIROC[2,])), col=adjustcolor(c_miroc,alpha.f=0.2), border = NA)

legend("topright",c("Current", "CNRM 8.5", "CCSM 8.5", "MIROC 8.5"), 
       inset=c(-0.25,0), 
       bty='n',
       col=c(c_current, c_cnrm, c_ccsm, c_miroc), 
       lwd= c(2, 2),
       cex=1)

dev.off()

###################################
############Sdl Density#############
  # to store the results
sdl_Dens <- array(data=NA,dim=c(repet,length(species),simulation_length+1))
dimnames(sdl_Dens)[2] <- list(species)
dimnames(sdl_Dens)[3] <- list(1:(simulation_length+1))



for (i in 1:repet)
{
  our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Output 20x/PO_CNRM_",i,".out")
  #our_file <- paste0("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Output 4/POFLABMA_CNRM4.out")
  data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
  data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
  
  for (who in species)
  {
    # tree = sdl
    sdl_Dens[i,who,] <- data[,paste("Sdl.Abs.Den.",who,sep=".")] 
  }
}

### Density of SEEDLINGS in all species in POFLABMA

ABMAinterDens <- apply(sdl_Dens[,'ABMA',],2,quantile,c(0.025,0.975))
ABMAmedDens <- apply(sdl_Dens[,'ABMA',],2,median)

PICOinterDens <- apply(sdl_Dens[,'PICO',],2,quantile,c(0.025,0.975))
PICOmedDens <- apply(sdl_Dens[,'PICO',],2,median)


####
# plot
yrs <- c(1999:2012)
rangePlotDens <- range(c(ABMAinterDens,ABMAmedDens, PICOinterDens,PICOmedDens))
yrsSim <- (0:simulation_length)+yrs[1]

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Graphs 20x/POFLABMA_sdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Density in', plot),lwd=2, ylab = "Density",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)

lines(yrsSim,PICOmedDens,col=c_PICO,lwd=2)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens[1,],rev(PICOinterDens[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)

legend("topright",c('Simulated ABMA', 'Simulated PICO'), 
       inset=c(-0.32,0),lty=1,
       col=c(c_ABMA, c_PICO),
       lwd= c(2, 2),
       bty='n',cex=1)

dev.off()

# only ABMA
rangePlotDens <- range(c(ABMAinterDens,ABMAmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Graphs 20x/POFLABMA_ABMAsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,ABMAmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_ABMA,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(ABMAinterDens[1,],rev(ABMAinterDens[2,])), col=adjustcolor(c_ABMA,alpha.f=0.2), border = NA)


legend("topright",c('Simulated ABMA'), inset=c(-0.32,0),
       col=c(c_ABMA),lwd=c(3), bty='n',cex=1)

dev.off()


# only PICO
rangePlotDens <- range(c(PICOinterDens,PICOmedDens))

png(filename = "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/CNRM/Graphs 20x/POFLABMA_PICOsdl_Dens.png",width = 3000, height = 2000, units = "px",res=300)

par(mar=c(5,5,5,12), xpd=TRUE)

plot(yrsSim,PICOmedDens,type='l',ylim=rangePlotDens,main=paste('CNRM Density in',plot),lwd=2,ylab = "Density",xlab = "Year",col=c_PICO,cex.lab=1,cex.main=1)
polygon(x = c(yrsSim,rev(yrsSim)), y = c(PICOinterDens[1,],rev(PICOinterDens[2,])), col=adjustcolor(c_PICO,alpha.f=0.2), border = NA)


legend("topright",c('Simulated PICO'), inset=c(-0.32,0),
       col=c(c_PICO),lwd=c(3), bty='n',cex=1)
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

###Change matrix size based on number of species####
changeperc <- matrix(0, 3, 8)
colnames(changeperc) <- slices
rownames(changeperc) <- c("Total", "ABMA", "PICO")

changeperc["Total", "BA Control"] <- percent.change(mean(total_array[,1,"BA Control"]), mean(total_array[,93,"BA Control"]))
changeperc["Total", "BA CNRM"] <- percent.change(mean(total_array[,1,"BA CNRM"]), mean(total_array[,93,"BA CNRM"]))
changeperc["Total", "BA CCSM"] <-percent.change(mean(total_array[,1,"BA CCSM"]), mean(total_array[,93,"BA CCSM"]))
changeperc["Total", "BA MIROC"] <- percent.change(mean(total_array[,1,"BA MIROC"]), mean(total_array[,93,"BA MIROC"]))
changeperc["Total", "Dens Control"] <- percent.change(mean(total_array[,1,"Dens Control"]), mean(total_array[,93,"Dens Control"]))
changeperc["Total", "Dens CNRM"] <- percent.change(mean(total_array[,1,"Dens CNRM"]), mean(total_array[,93,"Dens CNRM"]))
changeperc["Total", "Dens CCSM"] <- percent.change(mean(total_array[,1,"Dens CCSM"]), mean(total_array[,93,"Dens CCSM"]))
changeperc["Total", "Dens MIROC"] <- percent.change(mean(total_array[,1,"Dens MIROC"]), mean(total_array[,93,"Dens MIROC"]))


#####Now for each species individually####
#ABMA

ABMA_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  ABMA_array[,i,1] <- results[,"ABMA",i]
  ABMA_array[,i,2] <- resultsCNRM[,"ABMA",i]
  ABMA_array[,i,3] <- resultsCCSM[,"ABMA",i]
  ABMA_array[,i,4] <- resultsMIROC[,"ABMA",i]
  ABMA_array[,i,5] <- resultsDens[,"ABMA",i]
  ABMA_array[,i,6] <- resultsCNRMDens[,"ABMA",i]
  ABMA_array[,i,7] <- resultsCCSMDens[,"ABMA",i]
  ABMA_array[,i,8] <- resultsMIROCDens[,"ABMA",i]
}

changeperc["ABMA", "BA Control"] <- percent.change(mean(ABMA_array[,1,"BA Control"]), mean(ABMA_array[,93,"BA Control"]))
changeperc["ABMA", "BA CNRM"] <- percent.change(mean(ABMA_array[,1,"BA CNRM"]), mean(ABMA_array[,93,"BA CNRM"]))
changeperc["ABMA", "BA CCSM"] <-percent.change(mean(ABMA_array[,1,"BA CCSM"]), mean(ABMA_array[,93,"BA CCSM"]))
changeperc["ABMA", "BA MIROC"] <- percent.change(mean(ABMA_array[,1,"BA MIROC"]), mean(ABMA_array[,93,"BA MIROC"]))
changeperc["ABMA", "Dens Control"] <- percent.change(mean(ABMA_array[,1,"Dens Control"]), mean(ABMA_array[,93,"Dens Control"]))
changeperc["ABMA", "Dens CNRM"] <- percent.change(mean(ABMA_array[,1,"Dens CNRM"]), mean(ABMA_array[,93,"Dens CNRM"]))
changeperc["ABMA", "Dens CCSM"] <- percent.change(mean(ABMA_array[,1,"Dens CCSM"]), mean(ABMA_array[,93,"Dens CCSM"]))
changeperc["ABMA", "Dens MIROC"] <- percent.change(mean(ABMA_array[,1,"Dens MIROC"]), mean(ABMA_array[,93,"Dens MIROC"]))

print(changeperc)




#PICO

PICO_array <- array(NA, dim = c(20, 94, 8),
                    dimnames = list(1:20,
                                    1:94,
                                    slices))

for(i in 1:(simulation_length)){
  PICO_array[,i,1] <- results[,"PICO",i]
  PICO_array[,i,2] <- resultsCNRM[,"PICO",i]
  PICO_array[,i,3] <- resultsCCSM[,"PICO",i]
  PICO_array[,i,4] <- resultsMIROC[,"PICO",i]
  PICO_array[,i,5] <- resultsDens[,"PICO",i]
  PICO_array[,i,6] <- resultsCNRMDens[,"PICO",i]
  PICO_array[,i,7] <- resultsCCSMDens[,"PICO",i]
  PICO_array[,i,8] <- resultsMIROCDens[,"PICO",i]
}

changeperc["PICO", "BA Control"] <- percent.change(mean(PICO_array[,1,"BA Control"]), mean(PICO_array[,93,"BA Control"]))
changeperc["PICO", "BA CNRM"] <- percent.change(mean(PICO_array[,1,"BA CNRM"]), mean(PICO_array[,93,"BA CNRM"]))
changeperc["PICO", "BA CCSM"] <-percent.change(mean(PICO_array[,1,"BA CCSM"]), mean(PICO_array[,93,"BA CCSM"]))
changeperc["PICO", "BA MIROC"] <- percent.change(mean(PICO_array[,1,"BA MIROC"]), mean(PICO_array[,93,"BA MIROC"]))
changeperc["PICO", "Dens Control"] <- percent.change(mean(PICO_array[,1,"Dens Control"]), mean(PICO_array[,93,"Dens Control"]))
changeperc["PICO", "Dens CNRM"] <- percent.change(mean(PICO_array[,1,"Dens CNRM"]), mean(PICO_array[,93,"Dens CNRM"]))
changeperc["PICO", "Dens CCSM"] <- percent.change(mean(PICO_array[,1,"Dens CCSM"]), mean(PICO_array[,93,"Dens CCSM"]))
changeperc["PICO", "Dens MIROC"] <- percent.change(mean(PICO_array[,1,"Dens MIROC"]), mean(PICO_array[,93,"Dens MIROC"]))
changeperc <- round(changeperc, 2)

print(changeperc)

write.table(changeperc, "C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Testing Climate Scenarios/Climate Tests/POFLABMA/Percent_change.txt", sep = "\t", quote = F)

