##Creation and synthesis of data frames associated with SEKI_graph_creation doc
#Pulled and organized from SEKI_analysis_updated

plots <- c("SJM", "SJ", "SP", "SJP")
runs <- 10
timesteps <- 92
species <- c("ABCO","CADE" ,"PILA", "PIPO" ,"QUCH" ,"QUKE", "PICO" ,"PIMO", "PIJE", "ABMA")
clim_seq<- c("CCSM4", "CNRM", "MIROC", "CNTRL")
# Give colors printer friendly hues
spp_colors <- c("#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494", "black")
one <- "#c7e9b4"
two <- "#7fcdbb"
three <- "#41b6c4"
four <- "#2c7fb8"
five <- "#253494"

CCSM4 <- "#A6611A"
CNRM <- "black"
MIROC <- "#80CDC1"
CNTRL <- "#018571"

library(tidyverse)
library(directlabels)
library(gridExtra)
library(data.table)
library(scales)
library(reshape2)
library(RColorBrewer)
##############Function to extract adult and sapling basal area###################
###Use site in quotes and caps to get info ie: "SJM"

Basal_area <- function(site, clim_seq){
  results <- array(data = NA, dim = c(runs, length(species), timesteps + 1))
  dimnames(results)[2] <- list(species)
  dimnames(results)[3] <- list(1:(timesteps + 1))
  
  for (i in 1:runs){   
    our_file <- paste0(site, "/", site, "_", clim_seq, "_", i,".out")
    data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
    data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
    
    for (who in species){ # tree = Sapl + Adult
      results[i, who, ] <- data[,paste("Adult.Abs.BA.",who,sep=".")] + 
        data[ , paste("Sapl.Abs.BA.", who, sep=".")]
    }
  }
  return(results)
}
#####################Function to extract adult and sapling density####################
Density <- function(site, clim_seq){
  results <- array(data = NA, dim = c(runs, length(species), timesteps + 1))
  dimnames(results)[2] <- list(species)
  dimnames(results)[3] <- list(1:(timesteps + 1))
  
  for (i in 1:runs){
    our_file <- paste0(site, "/", site, "_", clim_seq, "_", i,".out")
    data1 <- read.table(file = our_file,skip=6,header=TRUE,sep = "\t")
    data <- data1[data1$Subplot == 1,] ### Subplot #1 is the extracted center 100m x 100m
    
    for (who in species){ # tree = Sapl + Adult
      results[i, who, ] <- data[,paste("Adult.Abs.Den.",who,sep=".")] + 
        data[ , paste("Sapl.Abs.Den.", who, sep=".")]
    }
  }
  return(results)
}

###########Analyzing the data#########################

Years_sim <- 2007:2099

################Median#############################
###Function to find the median basal area for each species for each timestep within each site

Median_BA <- function(site, clim_seq){
  Median_BA <- matrix(NA, nrow = length(species), ncol = timesteps + 1)
  rownames(Median_BA) <- species
  for(i in species){
    temp <- apply(Basal_area(site, clim_seq)[, i, ], 2, median)
    Median_BA[i, ] <- round(temp, 3)
  }
  
  Median <- Median_BA[which(rowSums(Median_BA) > 0),]
  Median_BA <- transpose(as.data.frame(Median))
  colnames(Median_BA) <- rownames(Median)
  Median_BA <- cbind(data.frame("Years" = Years_sim), Median_BA)
  
  return(Median_BA)
}

###Function to find the median density for each species for each timestep within each site

Median_Dens <- function(site, clim_seq){
  Median_Dens <- matrix(NA, nrow = length(species), ncol = timesteps + 1)
  rownames(Median_Dens) <- species
  for(i in species){
    temp <- apply(Density(site, clim_seq)[, i, ], 2, median)
    Median_Dens[i, ] <- round(temp, 3)
  }
  
  Median <- Median_Dens[which(rowSums(Median_Dens) > 0),]
  Median_Dens <- transpose(as.data.frame(Median))
  colnames(Median_Dens) <- rownames(Median)
  Median_Dens <- cbind(data.frame("Years" = Years_sim), Median_Dens)
  
  return(Median_Dens)
}


######################Quantile######################

Quant_BA <- function(site, clim_seq){
  Quant_BA <- matrix(0, 1, ncol = timesteps + 1)
  for(i in species){
    temp <- apply(Basal_area(site, clim_seq)[,i,],2,quantile,c(0.025,0.975))
    temp <- round(temp, 3)
    rownames(temp) <- c(paste0(i, " 2.5%"), paste0(i, " 97.5%"))
    Quant_BA <- rbind(temp, Quant_BA)
  }
  Quant_BA_temp <- Quant_BA[which(rowSums(Quant_BA) > 0), ]
  Quant_BA <- transpose(as.data.frame(Quant_BA_temp))
  colnames(Quant_BA) <- rownames(Quant_BA_temp)
  Quant_BA <- cbind(data.frame("Years" = Years_sim), Quant_BA)
  
  return(Quant_BA)
}



###
Quant_Dens <- function(site, clim_seq){
  Quant_Dens <- matrix(0, 1, ncol = timesteps + 1)
  for(i in species){
    temp <- apply(Density(site, clim_seq)[,i,],2,quantile,c(0.025,0.975))
    temp <- round(temp, 3)
    rownames(temp) <- c(paste0(i, " 2.5%"), paste0(i, " 97.5%"))
    Quant_Dens <- rbind(temp, Quant_Dens)
  }
  Quant_Dens_temp <- Quant_Dens[which(rowSums(Quant_Dens) > 0), ]
  Quant_Dens <- transpose(as.data.frame(Quant_Dens_temp))
  colnames(Quant_Dens) <- rownames(Quant_Dens_temp)
  Quant_Dens <- cbind(data.frame("Years" = Years_sim), Quant_Dens)
  
  return(Quant_Dens)
}
# 
# ggplot(Median_BA("SJP", "CNTRL"), aes(x = Median_BA("SJP", "CNTRL")$Year)) +
#   geom_line(aes(y = Median_BA("SJP", "CNTRL")$ABCO)) +
#   geom_line(aes(y = Median_BA("SJP", "MIROC")$ABCO), color = "blue")

##Use if want to save plots by site using 1 function


# #Species in plots names(Median_BA(site, clim_seq))
# By_site <- function(clim_seq){
#   SJ.BA.plot <- ggplot(Median_BA("SJ", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJ", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_BA("SJ", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJ", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_BA("SJ", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJ", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_BA("SJ", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIJE, colour = "PIJE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJ", clim_seq)[, "PIJE 2.5%"],
#         ymax = Quant_BA("SJ", clim_seq)[, "PIJE 97.5%"],
#         fill = "PIJE"
#       ),
#       alpha = 0.3,
#       show.legend = F 
#     ) +
#     labs(y = "Basal Area", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJ ", clim_seq, " Basal Area"))
#   
#   
#   SJ.Dens.plot <- ggplot(Median_Dens("SJ", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJ", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_Dens("SJ", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJ", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_Dens("SJ", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJ", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_Dens("SJ", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIJE, colour = "PIJE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJ", clim_seq)[, "PIJE 2.5%"],
#         ymax = Quant_Dens("SJ", clim_seq)[, "PIJE 97.5%"],
#         fill = "PIJE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     labs(y = "Density", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJ ", clim_seq, " Density"))
#   
#   SJM.BA.plot <- ggplot(Median_BA("SJM", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJM", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_BA("SJM", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJM", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_BA("SJM", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PICO, colour = "PICO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJM", clim_seq)[, "PICO 2.5%"],
#         ymax = Quant_BA("SJM", clim_seq)[, "PICO 97.5%"],
#         fill = "PICO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIMO, colour = "PIMO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJM", clim_seq)[, "PIMO 2.5%"],
#         ymax = Quant_BA("SJM", clim_seq)[, "PIMO 97.5%"],
#         fill = "PIMO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     )  +
#     labs(y = "Basal Area", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJM", clim_seq, " Basal Area"))
#   
#   
#   SJM.Dens.plot <- ggplot(Median_Dens("SJM", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJM", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_Dens("SJM", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJM", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_Dens("SJM", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PICO, colour = "PICO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJM", clim_seq)[, "PICO 2.5%"],
#         ymax = Quant_Dens("SJM", clim_seq)[, "PICO 97.5%"],
#         fill = "PICO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIMO, colour = "PIMO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJM", clim_seq)[, "PIMO 2.5%"],
#         ymax = Quant_Dens("SJM", clim_seq)[, "PIMO 97.5%"],
#         fill = "PIMO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     )  +
#     labs(y = "Density", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJM", clim_seq, " Density"))
#   
#   SJP.BA.plot <- ggplot(Median_BA("SJP", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIJE, colour = "PIJE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "PIJE 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "PIJE 97.5%"],
#         fill = "PIJE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIPO, colour = "PIPO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "PIPO 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "PIPO 97.5%"],
#         fill = "PIPO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = CADE, colour = "CADE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SJP", clim_seq)[, "CADE 2.5%"],
#         ymax = Quant_BA("SJP", clim_seq)[, "CADE 97.5%"],
#         fill = "CADE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     labs(y = "Basal Area", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJP ", clim_seq, " Basal Area")) 
#   
#   SJP.Dens.plot <- ggplot(Median_Dens("SJP", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = ABMA, colour = "ABMA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "ABMA 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "ABMA 97.5%"],
#         fill = "ABMA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIJE, colour = "PIJE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "PIJE 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "PIJE 97.5%"],
#         fill = "PIJE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIPO, colour = "PIPO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "PIPO 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "PIPO 97.5%"],
#         fill = "PIPO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = CADE, colour = "CADE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SJP", clim_seq)[, "CADE 2.5%"],
#         ymax = Quant_Dens("SJP", clim_seq)[, "CADE 97.5%"],
#         fill = "CADE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     labs(y = "Density", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SJP ", clim_seq, " Density"))
#   
#   SP.BA.plot <- ggplot(Median_BA("SP", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = QUCH, colour = "QUCH")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "QUCH 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "QUCH 97.5%"],
#         fill = "QUCH"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = QUKE, colour = "QUKE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "QUKE 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "QUKE 97.5%"],
#         fill = "QUKE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIPO, colour = "PIPO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "PIPO 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "PIPO 97.5%"],
#         fill = "PIPO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = CADE, colour = "CADE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "CADE 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "CADE 97.5%"],
#         fill = "CADE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     labs(y = "Basal Area", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SP ", clim_seq, " Basal Area"))
#   
#   SP.Dens.plot <- ggplot(Median_Dens("SP", clim_seq), aes(x = Years)) +
#     geom_line(aes(y = ABCO, colour = "ABCO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SP", clim_seq)[, "ABCO 2.5%"],
#         ymax = Quant_Dens("SP", clim_seq)[, "ABCO 97.5%"],
#         fill = "ABCO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = QUCH, colour = "QUCH")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SP", clim_seq)[, "QUCH 2.5%"],
#         ymax = Quant_Dens("SP", clim_seq)[, "QUCH 97.5%"],
#         fill = "QUCH"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PILA, colour = "PILA")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SP", clim_seq)[, "PILA 2.5%"],
#         ymax = Quant_Dens("SP", clim_seq)[, "PILA 97.5%"],
#         fill = "PILA"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = QUKE, colour = "QUKE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_Dens("SP", clim_seq)[, "QUKE 2.5%"],
#         ymax = Quant_Dens("SP", clim_seq)[, "QUKE 97.5%"],
#         fill = "QUKE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = PIPO, colour = "PIPO")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "PIPO 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "PIPO 97.5%"],
#         fill = "PIPO"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     ) +
#     geom_line(aes(y = CADE, colour = "CADE")) +
#     geom_ribbon(
#       aes(
#         ymin = Quant_BA("SP", clim_seq)[, "CADE 2.5%"],
#         ymax = Quant_BA("SP", clim_seq)[, "CADE 97.5%"],
#         fill = "CADE"
#       ),
#       alpha = 0.3,
#       show.legend = F
#     )+
#     labs(y = "Density", colour = "Species") +
#     scale_x_continuous(breaks = pretty_breaks(n = 5)) +
#     ggtitle(paste0("SP ", clim_seq, " Density"))
#   
#   ggsave(paste0("SJ.BA.", clim_seq, ".png"), SJ.BA.plot)
#   ggsave(paste0("SJ.Dens.", clim_seq, ".png"), SJ.Dens.plot)
#   ggsave(paste0("SJM.BA.", clim_seq, ".png"), SJM.BA.plot)
#   ggsave(paste0("SJM.Dens.", clim_seq, ".png"), SJM.Dens.plot)
#   ggsave(paste0("SJP.BA.", clim_seq, ".png"), SJP.BA.plot)
#   ggsave(paste0("SJP.Dens.", clim_seq, ".png"), SJP.Dens.plot)
#   ggsave(paste0("SP.BA.", clim_seq, ".png"), SP.BA.plot)
#   ggsave(paste0("SP.Dens.", clim_seq, ".png"), SP.Dens.plot)
# }

#####################By site all sequences one species################
# Putting into data frames to melt into 1 large data frame to make graphing easier

#SJ : ABCO, ABMA, PIJE, PILA

SJ.ABCO.df <- data.frame("Year" = Median_BA("SJ", "MIROC")[, "Years"],
                         "Plot" = "SJ",
                         "Species" = "ABCO",
                      "CCSM4_BA" = Median_BA("SJ", "CCSM4")[, "ABCO"],
                      "CCSM4_Dens" = Median_Dens("SJ", "CCSM4")[, "ABCO"],
                      "CCSM4_2.5_BA" = Quant_BA("SJ", "CCSM4")[, "ABCO 2.5%"],
                      "CCSM4_2.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "ABCO 2.5%"],
                      "CCSM4_97.5_BA" = Quant_BA("SJ", "CCSM4")[, "ABCO 97.5%"],
                      "CCSM4_97.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "ABCO 97.5%"],
                      "CNRM_BA" = Median_BA("SJ", "CNRM")[, "ABCO"],
                      "CNRM_Dens" = Median_Dens("SJ", "CNRM")[, "ABCO"],
                      "CNRM_2.5_BA" = Quant_BA("SJ", "CNRM")[, "ABCO 2.5%"],
                      "CNRM_2.5_Dens" = Quant_Dens("SJ", "CNRM")[, "ABCO 2.5%"],
                      "CNRM_97.5_BA" = Quant_BA("SJ", "CNRM")[, "ABCO 97.5%"],
                      "CNRM_97.5_Dens" = Quant_Dens("SJ", "CNRM")[, "ABCO 97.5%"],
                      "MIROC_BA" = Median_BA("SJ", "MIROC")[, "ABCO"],
                      "MIROC_Dens" = Median_Dens("SJ", "MIROC")[, "ABCO"],
                      "MIROC_2.5_BA" = Quant_BA("SJ", "MIROC")[, "ABCO 2.5%"],
                      "MIROC_2.5_Dens" = Quant_Dens("SJ", "MIROC")[, "ABCO 2.5%"],
                      "MIROC_97.5_BA" = Quant_BA("SJ", "MIROC")[, "ABCO 97.5%"],
                      "MIROC_97.5_Dens" = Quant_Dens("SJ", "MIROC")[, "ABCO 97.5%"],
                      "CNTRL_BA" = Median_BA("SJ", "CNTRL")[, "ABCO"],
                      "CNTRL_Dens" = Median_Dens("SJ", "CNTRL")[, "ABCO"],
                      "CNTRL_2.5_BA" = Quant_BA("SJ", "CNTRL")[, "ABCO 2.5%"],
                      "CNTRL_2.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "ABCO 2.5%"],
                      "CNTRL_97.5_BA" = Quant_BA("SJ", "CNTRL")[, "ABCO 97.5%"],
                      "CNTRL_97.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "ABCO 97.5%"])

SJ.ABMA.df <- data.frame("Year" = Median_BA("SJ", "MIROC")[, "Years"],
                         "Plot" = "SJ",
                         "Species" = "ABMA",
                         "CCSM4_BA" = Median_BA("SJ", "CCSM4")[, "ABMA"],
                         "CCSM4_Dens" = Median_Dens("SJ", "CCSM4")[, "ABMA"],
                         "CCSM4_2.5_BA" = Quant_BA("SJ", "CCSM4")[, "ABMA 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "ABMA 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SJ", "CCSM4")[, "ABMA 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "ABMA 97.5%"],
                         "CNRM_BA" = Median_BA("SJ", "CNRM")[, "ABMA"],
                         "CNRM_Dens" = Median_Dens("SJ", "CNRM")[, "ABMA"],
                         "CNRM_2.5_BA" = Quant_BA("SJ", "CNRM")[, "ABMA 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SJ", "CNRM")[, "ABMA 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SJ", "CNRM")[, "ABMA 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SJ", "CNRM")[, "ABMA 97.5%"],
                         "MIROC_BA" = Median_BA("SJ", "MIROC")[, "ABMA"],
                         "MIROC_Dens" = Median_Dens("SJ", "MIROC")[, "ABMA"],
                         "MIROC_2.5_BA" = Quant_BA("SJ", "MIROC")[, "ABMA 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SJ", "MIROC")[, "ABMA 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SJ", "MIROC")[, "ABMA 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SJ", "MIROC")[, "ABMA 97.5%"],
                         "CNTRL_BA" = Median_BA("SJ", "CNTRL")[, "ABMA"],
                         "CNTRL_Dens" = Median_Dens("SJ", "CNTRL")[, "ABMA"],
                         "CNTRL_2.5_BA" = Quant_BA("SJ", "CNTRL")[, "ABMA 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "ABMA 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SJ", "CNTRL")[, "ABMA 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "ABMA 97.5%"])


SJ.PIJE.df <- data.frame("Year" = Median_BA("SJ", "MIROC")[, "Years"],
                         "Plot" = "SJ",
                         "Species" = "PIJE",
                         "CCSM4_BA" = Median_BA("SJ", "CCSM4")[, "PIJE"],
                         "CCSM4_Dens" = Median_Dens("SJ", "CCSM4")[, "PIJE"],
                         "CCSM4_2.5_BA" = Quant_BA("SJ", "CCSM4")[, "PIJE 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "PIJE 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SJ", "CCSM4")[, "PIJE 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "PIJE 97.5%"],
                         "CNRM_BA" = Median_BA("SJ", "CNRM")[, "PIJE"],
                         "CNRM_Dens" = Median_Dens("SJ", "CNRM")[, "PIJE"],
                         "CNRM_2.5_BA" = Quant_BA("SJ", "CNRM")[, "PIJE 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SJ", "CNRM")[, "PIJE 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SJ", "CNRM")[, "PIJE 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SJ", "CNRM")[, "PIJE 97.5%"],
                         "MIROC_BA" = Median_BA("SJ", "MIROC")[, "PIJE"],
                         "MIROC_Dens" = Median_Dens("SJ", "MIROC")[, "PIJE"],
                         "MIROC_2.5_BA" = Quant_BA("SJ", "MIROC")[, "PIJE 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SJ", "MIROC")[, "PIJE 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SJ", "MIROC")[, "PIJE 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SJ", "MIROC")[, "PIJE 97.5%"],
                         "CNTRL_BA" = Median_BA("SJ", "CNTRL")[, "PIJE"],
                         "CNTRL_Dens" = Median_Dens("SJ", "CNTRL")[, "PIJE"],
                         "CNTRL_2.5_BA" = Quant_BA("SJ", "CNTRL")[, "PIJE 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "PIJE 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SJ", "CNTRL")[, "PIJE 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "PIJE 97.5%"])

SJ.PILA.df <- data.frame("Year" = Median_BA("SJ", "MIROC")[, "Years"],
                         "Plot" = "SJ",
                         "Species" = "PILA",
                         "CCSM4_BA" = Median_BA("SJ", "CCSM4")[, "PILA"],
                         "CCSM4_Dens" = Median_Dens("SJ", "CCSM4")[, "PILA"],
                         "CCSM4_2.5_BA" = Quant_BA("SJ", "CCSM4")[, "PILA 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "PILA 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SJ", "CCSM4")[, "PILA 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SJ", "CCSM4")[, "PILA 97.5%"],
                         "CNRM_BA" = Median_BA("SJ", "CNRM")[, "PILA"],
                         "CNRM_Dens" = Median_Dens("SJ", "CNRM")[, "PILA"],
                         "CNRM_2.5_BA" = Quant_BA("SJ", "CNRM")[, "PILA 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SJ", "CNRM")[, "PILA 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SJ", "CNRM")[, "PILA 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SJ", "CNRM")[, "PILA 97.5%"],
                         "MIROC_BA" = Median_BA("SJ", "MIROC")[, "PILA"],
                         "MIROC_Dens" = Median_Dens("SJ", "MIROC")[, "PILA"],
                         "MIROC_2.5_BA" = Quant_BA("SJ", "MIROC")[, "PILA 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SJ", "MIROC")[, "PILA 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SJ", "MIROC")[, "PILA 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SJ", "MIROC")[, "PILA 97.5%"],
                         "CNTRL_BA" = Median_BA("SJ", "CNTRL")[, "PILA"],
                         "CNTRL_Dens" = Median_Dens("SJ", "CNTRL")[, "PILA"],
                         "CNTRL_2.5_BA" = Quant_BA("SJ", "CNTRL")[, "PILA 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "PILA 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SJ", "CNTRL")[, "PILA 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SJ", "CNTRL")[, "PILA 97.5%"])
##################################################################
#SJP: ABCO, CADE, PIJE, PILA, PIPO, ABMA

SJP.ABCO.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "ABCO",
                         "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "ABCO"],
                         "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "ABCO"],
                         "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "ABCO 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "ABCO 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "ABCO 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "ABCO 97.5%"],
                         "CNRM_BA" = Median_BA("SJP", "CNRM")[, "ABCO"],
                         "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "ABCO"],
                         "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "ABCO 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "ABCO 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "ABCO 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "ABCO 97.5%"],
                         "MIROC_BA" = Median_BA("SJP", "MIROC")[, "ABCO"],
                         "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "ABCO"],
                         "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "ABCO 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "ABCO 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "ABCO 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "ABCO 97.5%"],
                         "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "ABCO"],
                         "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "ABCO"],
                         "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "ABCO 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "ABCO 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "ABCO 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "ABCO 97.5%"])

SJP.ABMA.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "ABMA",
                          "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "ABMA"],
                          "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "ABMA"],
                          "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "ABMA 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "ABMA 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "ABMA 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "ABMA 97.5%"],
                          "CNRM_BA" = Median_BA("SJP", "CNRM")[, "ABMA"],
                          "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "ABMA"],
                          "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "ABMA 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "ABMA 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "ABMA 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "ABMA 97.5%"],
                          "MIROC_BA" = Median_BA("SJP", "MIROC")[, "ABMA"],
                          "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "ABMA"],
                          "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "ABMA 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "ABMA 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "ABMA 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "ABMA 97.5%"],
                          "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "ABMA"],
                          "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "ABMA"],
                          "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "ABMA 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "ABMA 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "ABMA 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "ABMA 97.5%"])

SJP.PIJE.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "PIJE",
                          "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "PIJE"],
                          "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "PIJE"],
                          "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "PIJE 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PIJE 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "PIJE 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PIJE 97.5%"],
                          "CNRM_BA" = Median_BA("SJP", "CNRM")[, "PIJE"],
                          "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "PIJE"],
                          "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "PIJE 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PIJE 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "PIJE 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PIJE 97.5%"],
                          "MIROC_BA" = Median_BA("SJP", "MIROC")[, "PIJE"],
                          "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "PIJE"],
                          "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "PIJE 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PIJE 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "PIJE 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PIJE 97.5%"],
                          "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "PIJE"],
                          "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "PIJE"],
                          "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "PIJE 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PIJE 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "PIJE 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PIJE 97.5%"])

SJP.PILA.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "PILA",
                          "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "PILA"],
                          "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "PILA"],
                          "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "PILA 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PILA 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "PILA 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PILA 97.5%"],
                          "CNRM_BA" = Median_BA("SJP", "CNRM")[, "PILA"],
                          "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "PILA"],
                          "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "PILA 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PILA 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "PILA 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PILA 97.5%"],
                          "MIROC_BA" = Median_BA("SJP", "MIROC")[, "PILA"],
                          "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "PILA"],
                          "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "PILA 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PILA 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "PILA 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PILA 97.5%"],
                          "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "PILA"],
                          "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "PILA"],
                          "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "PILA 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PILA 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "PILA 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PILA 97.5%"])

SJP.CADE.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "CADE",
                          "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "CADE"],
                          "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "CADE"],
                          "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "CADE 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "CADE 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "CADE 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "CADE 97.5%"],
                          "CNRM_BA" = Median_BA("SJP", "CNRM")[, "CADE"],
                          "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "CADE"],
                          "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "CADE 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "CADE 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "CADE 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "CADE 97.5%"],
                          "MIROC_BA" = Median_BA("SJP", "MIROC")[, "CADE"],
                          "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "CADE"],
                          "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "CADE 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "CADE 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "CADE 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "CADE 97.5%"],
                          "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "CADE"],
                          "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "CADE"],
                          "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "CADE 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "CADE 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "CADE 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "CADE 97.5%"])

SJP.PIPO.df <- data.frame("Year" = Median_BA("SJP", "MIROC")[, "Years"],
                          "Plot" = "SJP",
                          "Species" = "PIPO",
                          "CCSM4_BA" = Median_BA("SJP", "CCSM4")[, "PIPO"],
                          "CCSM4_Dens" = Median_Dens("SJP", "CCSM4")[, "PIPO"],
                          "CCSM4_2.5_BA" = Quant_BA("SJP", "CCSM4")[, "PIPO 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PIPO 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJP", "CCSM4")[, "PIPO 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJP", "CCSM4")[, "PIPO 97.5%"],
                          "CNRM_BA" = Median_BA("SJP", "CNRM")[, "PIPO"],
                          "CNRM_Dens" = Median_Dens("SJP", "CNRM")[, "PIPO"],
                          "CNRM_2.5_BA" = Quant_BA("SJP", "CNRM")[, "PIPO 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PIPO 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJP", "CNRM")[, "PIPO 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJP", "CNRM")[, "PIPO 97.5%"],
                          "MIROC_BA" = Median_BA("SJP", "MIROC")[, "PIPO"],
                          "MIROC_Dens" = Median_Dens("SJP", "MIROC")[, "PIPO"],
                          "MIROC_2.5_BA" = Quant_BA("SJP", "MIROC")[, "PIPO 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PIPO 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJP", "MIROC")[, "PIPO 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJP", "MIROC")[, "PIPO 97.5%"],
                          "CNTRL_BA" = Median_BA("SJP", "CNTRL")[, "PIPO"],
                          "CNTRL_Dens" = Median_Dens("SJP", "CNTRL")[, "PIPO"],
                          "CNTRL_2.5_BA" = Quant_BA("SJP", "CNTRL")[, "PIPO 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PIPO 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJP", "CNTRL")[, "PIPO 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJP", "CNTRL")[, "PIPO 97.5%"])

##################################################################
#SJM: ABCO, ABMA, PICO, PIMO

SJM.ABCO.df <- data.frame("Year" = Median_BA("SJM", "MIROC")[, "Years"],
                          "Plot" = "SJM",
                          "Species" = "ABCO",
                          "CCSM4_BA" = Median_BA("SJM", "CCSM4")[, "ABCO"],
                          "CCSM4_Dens" = Median_Dens("SJM", "CCSM4")[, "ABCO"],
                          "CCSM4_2.5_BA" = Quant_BA("SJM", "CCSM4")[, "ABCO 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "ABCO 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJM", "CCSM4")[, "ABCO 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "ABCO 97.5%"],
                          "CNRM_BA" = Median_BA("SJM", "CNRM")[, "ABCO"],
                          "CNRM_Dens" = Median_Dens("SJM", "CNRM")[, "ABCO"],
                          "CNRM_2.5_BA" = Quant_BA("SJM", "CNRM")[, "ABCO 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJM", "CNRM")[, "ABCO 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJM", "CNRM")[, "ABCO 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJM", "CNRM")[, "ABCO 97.5%"],
                          "MIROC_BA" = Median_BA("SJM", "MIROC")[, "ABCO"],
                          "MIROC_Dens" = Median_Dens("SJM", "MIROC")[, "ABCO"],
                          "MIROC_2.5_BA" = Quant_BA("SJM", "MIROC")[, "ABCO 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJM", "MIROC")[, "ABCO 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJM", "MIROC")[, "ABCO 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJM", "MIROC")[, "ABCO 97.5%"],
                          "CNTRL_BA" = Median_BA("SJM", "CNTRL")[, "ABCO"],
                          "CNTRL_Dens" = Median_Dens("SJM", "CNTRL")[, "ABCO"],
                          "CNTRL_2.5_BA" = Quant_BA("SJM", "CNTRL")[, "ABCO 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "ABCO 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJM", "CNTRL")[, "ABCO 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "ABCO 97.5%"])

SJM.ABMA.df <- data.frame("Year" = Median_BA("SJM", "MIROC")[, "Years"],
                          "Plot" = "SJM",
                          "Species" = "ABMA",
                          "CCSM4_BA" = Median_BA("SJM", "CCSM4")[, "ABMA"],
                          "CCSM4_Dens" = Median_Dens("SJM", "CCSM4")[, "ABMA"],
                          "CCSM4_2.5_BA" = Quant_BA("SJM", "CCSM4")[, "ABMA 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "ABMA 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJM", "CCSM4")[, "ABMA 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "ABMA 97.5%"],
                          "CNRM_BA" = Median_BA("SJM", "CNRM")[, "ABMA"],
                          "CNRM_Dens" = Median_Dens("SJM", "CNRM")[, "ABMA"],
                          "CNRM_2.5_BA" = Quant_BA("SJM", "CNRM")[, "ABMA 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJM", "CNRM")[, "ABMA 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJM", "CNRM")[, "ABMA 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJM", "CNRM")[, "ABMA 97.5%"],
                          "MIROC_BA" = Median_BA("SJM", "MIROC")[, "ABMA"],
                          "MIROC_Dens" = Median_Dens("SJM", "MIROC")[, "ABMA"],
                          "MIROC_2.5_BA" = Quant_BA("SJM", "MIROC")[, "ABMA 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJM", "MIROC")[, "ABMA 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJM", "MIROC")[, "ABMA 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJM", "MIROC")[, "ABMA 97.5%"],
                          "CNTRL_BA" = Median_BA("SJM", "CNTRL")[, "ABMA"],
                          "CNTRL_Dens" = Median_Dens("SJM", "CNTRL")[, "ABMA"],
                          "CNTRL_2.5_BA" = Quant_BA("SJM", "CNTRL")[, "ABMA 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "ABMA 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJM", "CNTRL")[, "ABMA 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "ABMA 97.5%"])

SJM.PIMO.df <- data.frame("Year" = Median_BA("SJM", "MIROC")[, "Years"],
                          "Plot" = "SJM",
                          "Species" = "PIMO",
                          "CCSM4_BA" = Median_BA("SJM", "CCSM4")[, "PIMO"],
                          "CCSM4_Dens" = Median_Dens("SJM", "CCSM4")[, "PIMO"],
                          "CCSM4_2.5_BA" = Quant_BA("SJM", "CCSM4")[, "PIMO 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "PIMO 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJM", "CCSM4")[, "PIMO 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "PIMO 97.5%"],
                          "CNRM_BA" = Median_BA("SJM", "CNRM")[, "PIMO"],
                          "CNRM_Dens" = Median_Dens("SJM", "CNRM")[, "PIMO"],
                          "CNRM_2.5_BA" = Quant_BA("SJM", "CNRM")[, "PIMO 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJM", "CNRM")[, "PIMO 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJM", "CNRM")[, "PIMO 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJM", "CNRM")[, "PIMO 97.5%"],
                          "MIROC_BA" = Median_BA("SJM", "MIROC")[, "PIMO"],
                          "MIROC_Dens" = Median_Dens("SJM", "MIROC")[, "PIMO"],
                          "MIROC_2.5_BA" = Quant_BA("SJM", "MIROC")[, "PIMO 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJM", "MIROC")[, "PIMO 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJM", "MIROC")[, "PIMO 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJM", "MIROC")[, "PIMO 97.5%"],
                          "CNTRL_BA" = Median_BA("SJM", "CNTRL")[, "PIMO"],
                          "CNTRL_Dens" = Median_Dens("SJM", "CNTRL")[, "PIMO"],
                          "CNTRL_2.5_BA" = Quant_BA("SJM", "CNTRL")[, "PIMO 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "PIMO 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJM", "CNTRL")[, "PIMO 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "PIMO 97.5%"])

SJM.PICO.df <- data.frame("Year" = Median_BA("SJM", "MIROC")[, "Years"],
                          "Plot" = "SJM",
                          "Species" = "PICO",
                          "CCSM4_BA" = Median_BA("SJM", "CCSM4")[, "PICO"],
                          "CCSM4_Dens" = Median_Dens("SJM", "CCSM4")[, "PICO"],
                          "CCSM4_2.5_BA" = Quant_BA("SJM", "CCSM4")[, "PICO 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "PICO 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SJM", "CCSM4")[, "PICO 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SJM", "CCSM4")[, "PICO 97.5%"],
                          "CNRM_BA" = Median_BA("SJM", "CNRM")[, "PICO"],
                          "CNRM_Dens" = Median_Dens("SJM", "CNRM")[, "PICO"],
                          "CNRM_2.5_BA" = Quant_BA("SJM", "CNRM")[, "PICO 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SJM", "CNRM")[, "PICO 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SJM", "CNRM")[, "PICO 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SJM", "CNRM")[, "PICO 97.5%"],
                          "MIROC_BA" = Median_BA("SJM", "MIROC")[, "PICO"],
                          "MIROC_Dens" = Median_Dens("SJM", "MIROC")[, "PICO"],
                          "MIROC_2.5_BA" = Quant_BA("SJM", "MIROC")[, "PICO 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SJM", "MIROC")[, "PICO 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SJM", "MIROC")[, "PICO 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SJM", "MIROC")[, "PICO 97.5%"],
                          "CNTRL_BA" = Median_BA("SJM", "CNTRL")[, "PICO"],
                          "CNTRL_Dens" = Median_Dens("SJM", "CNTRL")[, "PICO"],
                          "CNTRL_2.5_BA" = Quant_BA("SJM", "CNTRL")[, "PICO 2.5%"],
                          "CNTRL_2.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "PICO 2.5%"],
                          "CNTRL_97.5_BA" = Quant_BA("SJM", "CNTRL")[, "PICO 97.5%"],
                          "CNTRL_97.5_Dens" = Quant_Dens("SJM", "CNTRL")[, "PICO 97.5%"])


##################################################################
#SP: ABCO, CADE, PILA, PIPO, QUCH, QUKE

SP.ABCO.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "ABCO",
                          "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "ABCO"],
                          "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "ABCO"],
                          "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "ABCO 2.5%"],
                          "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "ABCO 2.5%"],
                          "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "ABCO 97.5%"],
                          "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "ABCO 97.5%"],
                          "CNRM_BA" = Median_BA("SP", "CNRM")[, "ABCO"],
                          "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "ABCO"],
                          "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "ABCO 2.5%"],
                          "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "ABCO 2.5%"],
                          "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "ABCO 97.5%"],
                          "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "ABCO 97.5%"],
                          "MIROC_BA" = Median_BA("SP", "MIROC")[, "ABCO"],
                          "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "ABCO"],
                          "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "ABCO 2.5%"],
                          "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "ABCO 2.5%"],
                          "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "ABCO 97.5%"],
                          "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "ABCO 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "ABCO"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "ABCO"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "ABCO 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "ABCO 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "ABCO 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "ABCO 97.5%"])

SP.CADE.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "CADE",
                         "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "CADE"],
                         "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "CADE"],
                         "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "CADE 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "CADE 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "CADE 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "CADE 97.5%"],
                         "CNRM_BA" = Median_BA("SP", "CNRM")[, "CADE"],
                         "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "CADE"],
                         "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "CADE 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "CADE 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "CADE 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "CADE 97.5%"],
                         "MIROC_BA" = Median_BA("SP", "MIROC")[, "CADE"],
                         "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "CADE"],
                         "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "CADE 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "CADE 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "CADE 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "CADE 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "CADE"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "CADE"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "CADE 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "CADE 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "CADE 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "CADE 97.5%"])

SP.PIPO.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "PIPO",
                         "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "PIPO"],
                         "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "PIPO"],
                         "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "PIPO 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "PIPO 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "PIPO 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "PIPO 97.5%"],
                         "CNRM_BA" = Median_BA("SP", "CNRM")[, "PIPO"],
                         "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "PIPO"],
                         "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "PIPO 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "PIPO 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "PIPO 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "PIPO 97.5%"],
                         "MIROC_BA" = Median_BA("SP", "MIROC")[, "PIPO"],
                         "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "PIPO"],
                         "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "PIPO 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "PIPO 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "PIPO 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "PIPO 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "PIPO"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "PIPO"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "PIPO 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "PIPO 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "PIPO 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "PIPO 97.5%"])

SP.PILA.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "PILA",
                         "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "PILA"],
                         "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "PILA"],
                         "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "PILA 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "PILA 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "PILA 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "PILA 97.5%"],
                         "CNRM_BA" = Median_BA("SP", "CNRM")[, "PILA"],
                         "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "PILA"],
                         "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "PILA 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "PILA 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "PILA 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "PILA 97.5%"],
                         "MIROC_BA" = Median_BA("SP", "MIROC")[, "PILA"],
                         "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "PILA"],
                         "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "PILA 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "PILA 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "PILA 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "PILA 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "PILA"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "PILA"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "PILA 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "PILA 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "PILA 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "PILA 97.5%"])

SP.QUCH.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "QUCH",
                         "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "QUCH"],
                         "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "QUCH"],
                         "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "QUCH 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "QUCH 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "QUCH 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "QUCH 97.5%"],
                         "CNRM_BA" = Median_BA("SP", "CNRM")[, "QUCH"],
                         "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "QUCH"],
                         "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "QUCH 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "QUCH 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "QUCH 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "QUCH 97.5%"],
                         "MIROC_BA" = Median_BA("SP", "MIROC")[, "QUCH"],
                         "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "QUCH"],
                         "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "QUCH 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "QUCH 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "QUCH 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "QUCH 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "QUCH"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "QUCH"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "QUCH 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "QUCH 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "QUCH 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "QUCH 97.5%"])

SP.QUKE.df <- data.frame("Year" = Median_BA("SP", "MIROC")[, "Years"],
                         "Plot" = "SP",
                         "Species" = "QUKE",
                         "CCSM4_BA" = Median_BA("SP", "CCSM4")[, "QUKE"],
                         "CCSM4_Dens" = Median_Dens("SP", "CCSM4")[, "QUKE"],
                         "CCSM4_2.5_BA" = Quant_BA("SP", "CCSM4")[, "QUKE 2.5%"],
                         "CCSM4_2.5_Dens" = Quant_Dens("SP", "CCSM4")[, "QUKE 2.5%"],
                         "CCSM4_97.5_BA" = Quant_BA("SP", "CCSM4")[, "QUKE 97.5%"],
                         "CCSM4_97.5_Dens" = Quant_Dens("SP", "CCSM4")[, "QUKE 97.5%"],
                         "CNRM_BA" = Median_BA("SP", "CNRM")[, "QUKE"],
                         "CNRM_Dens" = Median_Dens("SP", "CNRM")[, "QUKE"],
                         "CNRM_2.5_BA" = Quant_BA("SP", "CNRM")[, "QUKE 2.5%"],
                         "CNRM_2.5_Dens" = Quant_Dens("SP", "CNRM")[, "QUKE 2.5%"],
                         "CNRM_97.5_BA" = Quant_BA("SP", "CNRM")[, "QUKE 97.5%"],
                         "CNRM_97.5_Dens" = Quant_Dens("SP", "CNRM")[, "QUKE 97.5%"],
                         "MIROC_BA" = Median_BA("SP", "MIROC")[, "QUKE"],
                         "MIROC_Dens" = Median_Dens("SP", "MIROC")[, "QUKE"],
                         "MIROC_2.5_BA" = Quant_BA("SP", "MIROC")[, "QUKE 2.5%"],
                         "MIROC_2.5_Dens" = Quant_Dens("SP", "MIROC")[, "QUKE 2.5%"],
                         "MIROC_97.5_BA" = Quant_BA("SP", "MIROC")[, "QUKE 97.5%"],
                         "MIROC_97.5_Dens" = Quant_Dens("SP", "MIROC")[, "QUKE 97.5%"],
                         "CNTRL_BA" = Median_BA("SP", "CNTRL")[, "QUKE"],
                         "CNTRL_Dens" = Median_Dens("SP", "CNTRL")[, "QUKE"],
                         "CNTRL_2.5_BA" = Quant_BA("SP", "CNTRL")[, "QUKE 2.5%"],
                         "CNTRL_2.5_Dens" = Quant_Dens("SP", "CNTRL")[, "QUKE 2.5%"],
                         "CNTRL_97.5_BA" = Quant_BA("SP", "CNTRL")[, "QUKE 97.5%"],
                         "CNTRL_97.5_Dens" = Quant_Dens("SP", "CNTRL")[, "QUKE 97.5%"])

##Creating one big data frame to save
           
SJ.ABCO.melt <- SJ.ABCO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJ.ABMA.melt <- SJ.ABMA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJ.PIJE.melt <- SJ.PIJE.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJ.PILA.melt <- SJ.PILA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")

SJM.ABCO.melt <- SJM.ABCO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJM.ABMA.melt <- SJM.ABMA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJM.PICO.melt <- SJM.PICO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJM.PIMO.melt <- SJM.PIMO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")

SJP.ABCO.melt <- SJP.ABCO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJP.ABMA.melt <- SJP.ABMA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJP.CADE.melt <- SJP.CADE.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJP.PILA.melt <- SJP.PILA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJP.PIJE.melt <- SJP.PIJE.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SJP.PIPO.melt <- SJP.PIPO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")

SP.ABCO.melt <- SP.ABCO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SP.CADE.melt <- SP.CADE.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SP.PILA.melt <- SP.PILA.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SP.PIPO.melt <- SP.PIPO.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SP.QUCH.melt <- SP.QUCH.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")
SP.QUKE.melt <- SP.QUKE.df %>% 
  melt(id.vars = c("Year", "Plot", "Species"), value.name = "sequence")

melted_df <- rbind(SJ.ABCO.melt, SJ.PIJE.melt, SJ.PILA.melt, SJ.ABMA.melt,
      SJM.ABCO.melt, SJM.ABMA.melt, SJM.PICO.melt, SJM.PIMO.melt,
      SJP.ABCO.melt, SJP.ABMA.melt, SJP.CADE.melt, SJP.PIJE.melt,
      SJP.PILA.melt, SJP.PIPO.melt, SP.ABCO.melt, SP.CADE.melt,
      SP.PILA.melt, SP.PILA.melt, SP.PIPO.melt, SP.QUCH.melt, 
      SP.QUKE.melt)
## Nikole's attempt to make the graphing process more intuitive 

#write.table(melted_df, "SEKI_future_data.txt", sep = "\t")

# Function to find site, species, and sequence 

#This one gives the whole matrix as an output
find_matrix <- function(plt, spp = NULL, var) {
  if(missing(spp))
    new_mat <- melted_df %>%
      filter(Plot == plt,
             variable == var)
  else
  new_mat <- melted_df %>%
    filter(Plot == plt,
           Species == spp,
           variable == var)
  return(new_mat)
}

#This one gives a vector of the actual data as output
find_pull <- function(plt, spp = NULL, var) {
  if(missing(spp))
    new_pull <- melted_df %>%
      filter(Plot == plt,
             variable == var) %>%
      pull(sequence)
  else 
  new_pull <- melted_df %>%
    filter(Plot == plt,
           Species == spp,
           variable == var) %>%
    pull(sequence)
  return(new_pull)
}
#######GRAPHS###########################################################

########SJ BA############

#Functions to make graphs for single species and plot with all climate scenarios

#Basal Area
find_climseq_graph_BA <- function(plt, spp, title){
  ggplot(find_matrix(plt, spp, "CCSM4_BA"), aes(x = Year)) +
  geom_line(aes(y = sequence, color = "CCSM4")) +
  geom_ribbon(
    aes(
      ymin = find_pull(plt, spp, "CCSM4_2.5_BA"),
      ymax = find_pull(plt, spp, "CCSM4_97.5_BA")),
    alpha = 0.3,
    fill = CCSM4
  ) +
  geom_line(aes(y = find_pull(plt, spp, "CNRM_BA"), color = "CNRM")) +
  geom_ribbon(
    aes(
      ymin = find_pull(plt, spp, "CNRM_2.5_BA"),
      ymax = find_pull(plt, spp, "CNRM_97.5_BA")),
    alpha = 0.3,
    fill = CNRM
  )  +
  geom_line(aes(y = find_pull(plt, spp, "MIROC_BA"), color = "MIROC")) +
  geom_ribbon(
    aes(
      ymin = find_pull(plt, spp, "MIROC_2.5_BA"),
      ymax = find_pull(plt, spp, "MIROC_97.5_BA")),
    alpha = 0.3,
    fill = MIROC
  ) +
    geom_line(aes(y = find_pull(plt, spp, "CNTRL_BA"), colour = "CNTRL")) +
    geom_ribbon(
      aes(
        ymin = find_pull(plt, spp, "CNTRL_2.5_BA"),
        ymax = find_pull(plt, spp, "CNTRL_97.5_BA")),
      alpha = 0.3,
      fill = CNTRL
    ) +
  labs(y = "Basal Area", colour = "Climate Sequence") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(title) +
    scale_color_manual(name = "Sequence", 
                       values = c(CCSM4, CNRM, MIROC, CNTRL))
}

# Density
find_climseq_graph_Dens <- function(plt, spp, title){
  ggplot(find_matrix(plt, spp, "CCSM4_Dens"), aes(x = Year)) +
    geom_line(aes(y = sequence, color = "CCSM4")) +
    geom_ribbon(
      aes(
        ymin = find_pull(plt, spp, "CCSM4_2.5_Dens"),
        ymax = find_pull(plt, spp, "CCSM4_97.5_Dens")),
      alpha = 0.3,
      fill = CCSM4
    ) +
    geom_line(aes(y = find_pull(plt, spp, "CNRM_Dens"),
              color = "CNRM")) +
    geom_ribbon(
      aes(
        ymin = find_pull(plt, spp, "CNRM_2.5_Dens"),
        ymax = find_pull(plt, spp, "CNRM_97.5_Dens")),
      alpha = 0.3,
      fill = CNRM
    )  +
    geom_line(aes(y = find_pull(plt, spp, "MIROC_Dens"),
              color = "MIROC")) +
    geom_ribbon(
      aes(
        ymin = find_pull(plt, spp, "MIROC_2.5_Dens"),
        ymax = find_pull(plt, spp, "MIROC_97.5_Dens")),
      alpha = 0.3,
      fill = MIROC
    ) +
    geom_line(aes(y = find_pull(plt, spp, "CNTRL_Dens"),
              color = "CNTRL")) +
    geom_ribbon(
      aes(
        ymin = find_pull(plt, spp, "CNTRL_2.5_Dens"),
        ymax = find_pull(plt, spp, "CNTRL_97.5_Dens")),
      alpha = 0.3,
      fill = CNTRL
    ) +
    labs(y = "Density", colour = "Climate Sequence") +
    scale_x_continuous(breaks = pretty_breaks(n = 5)) +
    ggtitle(title) +
    scale_color_manual(name = "Sequence",
                       values = c(CCSM4, CNRM, MIROC, CNTRL))
}

# Functions to make graphs of single plot and climate scenario with all species
  
    SJ.BA.plot <- function(clim_seq){
      ggplot(Median_BA("SJ", clim_seq), aes(x = Years)) +
      geom_line(aes(y = ABCO, color = "ABCO"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_BA("SJ", clim_seq)[, "ABCO 2.5%"],
          ymax = Quant_BA("SJ", clim_seq)[, "ABCO 97.5%"]),
        alpha = 0.3,
        fill = one) +
      geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_BA("SJ", clim_seq)[, "ABMA 2.5%"],
          ymax = Quant_BA("SJ", clim_seq)[, "ABMA 97.5%"]),
        alpha = 0.3,
          fill = two) +
      geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_BA("SJ", clim_seq)[, "PILA 2.5%"],
          ymax = Quant_BA("SJ", clim_seq)[, "PILA 97.5%"]),
        alpha = 0.3,
        fill = three) +
      geom_line(aes(y = PIJE, colour = "PIJE")) +
      geom_ribbon(
        aes(
          ymin = Quant_BA("SJ", clim_seq)[, "PIJE 2.5%"],
          ymax = Quant_BA("SJ", clim_seq)[, "PIJE 97.5%"]),
        alpha = 0.3,
        fill = four) +
      labs(y = "Basal Area", colour = "Species") +
      scale_x_continuous(breaks = pretty_breaks(n = 5)) +
      ggtitle(paste0("SJ ", clim_seq, " Basal Area")) +
        scale_color_manual(name = "Species",
                           labels = c("ABCO", "ABMA", "PILA", "PIJE"),
                           values = spp_colors)
    }
    
SJ.Dens.plot <- function(clim_seq){
  ggplot(Median_Dens("SJ", clim_seq), aes(x = Years)) +
      geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_Dens("SJ", clim_seq)[, "ABCO 2.5%"],
          ymax = Quant_Dens("SJ", clim_seq)[, "ABCO 97.5%"]),
        alpha = 0.3,
          fill = one) +
      geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_Dens("SJ", clim_seq)[, "ABMA 2.5%"],
          ymax = Quant_Dens("SJ", clim_seq)[, "ABMA 97.5%"]),
        alpha = 0.3,
          fill = two) +
      geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_Dens("SJ", clim_seq)[, "PILA 2.5%"],
          ymax = Quant_Dens("SJ", clim_seq)[, "PILA 97.5%"]),
        alpha = 0.3,
          fill = three) +
      geom_line(aes(y = PIJE, colour = "PIJE"), size = 0.5) +
      geom_ribbon(
        aes(
          ymin = Quant_Dens("SJ", clim_seq)[, "PIJE 2.5%"],
          ymax = Quant_Dens("SJ", clim_seq)[, "PIJE 97.5%"]),
        alpha = 0.3,
        fill = four) +
      labs(y = "Density", colour = "Species") +
      scale_x_continuous(breaks = pretty_breaks(n = 5)) +
      ggtitle(paste0("SJ ", clim_seq, " Density")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "ABMA", "PILA", "PIJE"),
                       values = spp_colors)
    
}
  
#######SJM###############

SJM.BA.plot <- function(clim_seq){
  ggplot(Median_BA("SJM", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJM", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_BA("SJM", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJM", clim_seq)[, "ABMA 2.5%"],
      ymax = Quant_BA("SJM", clim_seq)[, "ABMA 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PICO, colour = "PICO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJM", clim_seq)[, "PICO 2.5%"],
      ymax = Quant_BA("SJM", clim_seq)[, "PICO 97.5%"]),
    alpha = 0.3,
    fill = three
  ) +
  geom_line(aes(y = PIMO, colour = "PIMO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJM", clim_seq)[, "PIMO 2.5%"],
      ymax = Quant_BA("SJM", clim_seq)[, "PIMO 97.5%"]),
    alpha = 0.3,
    fill = four
  )  +
  labs(y = "Basal Area", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SJM", clim_seq, " Basal Area")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "ABMA", "PICO", "PIMO"),
                       values = spp_colors)
}


SJM.Dens.plot <- function(clim_seq){
  ggplot(Median_Dens("SJM", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJM", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_Dens("SJM", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJM", clim_seq)[, "ABMA 2.5%"],
      ymax = Quant_Dens("SJM", clim_seq)[, "ABMA 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PICO, colour = "PICO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJM", clim_seq)[, "PICO 2.5%"],
      ymax = Quant_Dens("SJM", clim_seq)[, "PICO 97.5%"]),
    alpha = 0.3,
    fill = three
  ) +
  geom_line(aes(y = PIMO, colour = "PIMO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJM", clim_seq)[, "PIMO 2.5%"],
      ymax = Quant_Dens("SJM", clim_seq)[, "PIMO 97.5%"]),
    alpha = 0.3,
    fill = four
  )  +
  labs(y = "Density", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SJM ", clim_seq, " Density")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "ABMA", "PICO", "PIMO"),
                       values = spp_colors)
}

#######SJP PLOTS################

SJP.BA.plot <- function(clim_seq){
  ggplot(Median_BA("SJP", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "ABMA 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "ABMA 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "PILA 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "PILA 97.5%"]),
    alpha = 0.3,
    color = three
  ) +
  geom_line(aes(y = PIJE, colour = "PIJE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "PIJE 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "PIJE 97.5%"]),
    alpha = 0.3,
    fill = four
  ) +
  geom_line(aes(y = PIPO, colour = "PIPO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "PIPO 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "PIPO 97.5%"]),
    alpha = 0.3,
    fill = five
  ) +
  geom_line(aes(y = CADE, colour = "CADE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SJP", clim_seq)[, "CADE 2.5%"],
      ymax = Quant_BA("SJP", clim_seq)[, "CADE 97.5%"]),
    alpha = 0.3,
    fill = "black"
  ) +
  labs(y = "Basal Area", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SJP ", clim_seq, " Basal Area")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "ABMA", "PILA", "PIJE", "PIPO", "CADE"),
                       values = spp_colors)
}

SJP.Dens.plot <- function(clim_seq){
  ggplot(Median_Dens("SJP", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = ABMA, colour = "ABMA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "ABMA 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "ABMA 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "PILA 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "PILA 97.5%"]),
    alpha = 0.3,
    fill = three
  ) +
  geom_line(aes(y = PIJE, colour = "PIJE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "PIJE 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "PIJE 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PIPO, colour = "PIPO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "PIPO 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "PIPO 97.5%"]),
    alpha = 0.3,
    fill = five
  ) +
  geom_line(aes(y = CADE, colour = "CADE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SJP", clim_seq)[, "CADE 2.5%"],
      ymax = Quant_Dens("SJP", clim_seq)[, "CADE 97.5%"]),
    alpha = 0.3,
    fill = "black"
  ) +
  labs(y = "Density", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SJP ", clim_seq, " Density")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "ABMA", "PILA", "PIJE", "PIPO", "CADE"),
                       values = spp_colors)
}

#########SP PLOTS####################

SP.BA.plot <- function(clim_seq){
  ggplot(Median_BA("SP", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = QUCH, colour = "QUCH"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "QUCH 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "QUCH 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "PILA 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "PILA 97.5%"]),
    alpha = 0.3,
    fill = three
  ) +
  geom_line(aes(y = QUKE, colour = "QUKE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "QUKE 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "QUKE 97.5%"]),
    alpha = 0.3,
    fill = four
  ) +
  geom_line(aes(y = PIPO, colour = "PIPO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "PIPO 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "PIPO 97.5%"]),
    alpha = 0.3,
    fill = five
  ) +
  geom_line(aes(y = CADE, colour = "CADE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "CADE 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "CADE 97.5%"]),
    alpha = 0.3,
    fill = "black"
  ) +
  labs(y = "Basal Area", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SP ", clim_seq, " Basal Area")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "QUCH", "PILA", "QUKE", "PIPO", "CADE"),
                       values = spp_colors)
}

SP.Dens.plot <- function(clim_seq){
  ggplot(Median_Dens("SP", clim_seq), aes(x = Years)) +
  geom_line(aes(y = ABCO, colour = "ABCO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SP", clim_seq)[, "ABCO 2.5%"],
      ymax = Quant_Dens("SP", clim_seq)[, "ABCO 97.5%"]),
    alpha = 0.3,
    fill = one
  ) +
  geom_line(aes(y = QUCH, colour = "QUCH"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SP", clim_seq)[, "QUCH 2.5%"],
      ymax = Quant_Dens("SP", clim_seq)[, "QUCH 97.5%"]),
    alpha = 0.3,
    fill = two
  ) +
  geom_line(aes(y = PILA, colour = "PILA"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SP", clim_seq)[, "PILA 2.5%"],
      ymax = Quant_Dens("SP", clim_seq)[, "PILA 97.5%"]),
    alpha = 0.3,
    fill = three
  ) +
  geom_line(aes(y = QUKE, colour = "QUKE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("SP", clim_seq)[, "QUKE 2.5%"],
      ymax = Quant_Dens("SP", clim_seq)[, "QUKE 97.5%"]),
    alpha = 0.3,
    fill = four
  ) +
  geom_line(aes(y = PIPO, colour = "PIPO"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "PIPO 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "PIPO 97.5%"]),
    alpha = 0.3,
    fill = five
  ) +
  geom_line(aes(y = CADE, colour = "CADE"), size = 0.5) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("SP", clim_seq)[, "CADE 2.5%"],
      ymax = Quant_BA("SP", clim_seq)[, "CADE 97.5%"]),
    alpha = 0.3,
    fill = "black"
  )+
  labs(y = "Density", colour = "Species") +
  scale_x_continuous(breaks = pretty_breaks(n = 5)) +
  ggtitle(paste0("SP ", clim_seq, " Density")) +
    scale_color_manual(name = "Species",
                       labels = c("ABCO", "QUCH", "PILA", "QUKE", "PIPO", "CADE"),
                       values = spp_colors)
}

library(grid)
library(ggpubr)
library(gtable)
library(ggthemes)


# Use ggarrange to make multi-faceted figures

fig_4_1  <- ggarrange(find_climseq_graph_BA("SJP", "PIJE", "SJP PIJE") +
                        labs(x = c()) + theme(legend.title = element_text(size = 5),
                                              legend.text = element_text(size = 5)), 
                      find_climseq_graph_BA("SJP", "ABCO", "SJP ABCO") + 
                        theme(legend.title = element_text(size = 5),
                              legend.text = element_text(size = 5)),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")

fig_4_2  <- ggarrange(SJP.BA.plot("CNTRL") + 
                        labs(title = "SJP CNTRL",
                             y = c(),
                             x = c()) + 
                        theme(legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)), 
                      SJP.BA.plot("MIROC") +
                        labs(title = "SJP MIROC",
                             y = c()) + 
                        theme(legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")

fig_4 <- ggarrange(fig_4_1, 
                   fig_4_2,
                   ncol = 2,
                   nrow = 1)

ggsave("Figure4.png", fig_4)

fig_5_1  <- ggarrange(find_climseq_graph_BA("SP", "QUKE", "SP QUKE") +
                        labs(x = c()) + 
                        theme(legend.title = element_text(size = 6),
                              legend.text = element_text(size = 6)), 
                      find_climseq_graph_BA("SP", "PIPO", "SP PIPO") + 
                        theme(legend.title = element_text(size = 6),
                              legend.text = element_text(size = 6)),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")

fig_5_2  <- ggarrange(SP.BA.plot("CNTRL") +
                        labs(title = "SP CNTRL",
                             y = c(),
                             x = c()) +
                        theme(legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)), 
                      SP.BA.plot("MIROC") +
                        labs(title = "SP MIROC",
                             y = c()) +
                        theme(legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)),
                      ncol = 1, nrow = 2,
                      common.legend = TRUE,
                      legend = "bottom")

fig_5 <- ggarrange(fig_5_1, 
                   fig_5_2,
                   ncol = 2,
                   nrow = 1)

ggsave("Figure5.png", fig_5)

fig_6_1  <- ggarrange(find_climseq_graph_Dens("SP", "QUKE", "SP QUKE") +
                        labs(x = c()) + 
                        theme(legend.title = element_text(size = 6),
                              legend.text = element_text(size = 6)), 
                  find_climseq_graph_Dens("SP", "PIPO", "SP PIPO") + 
                    theme(legend.title = element_text(size = 6),
                          legend.text = element_text(size = 6)),
                  ncol = 1, nrow = 2,
                  common.legend = TRUE,
                  legend = "bottom")

fig_6_2  <- ggarrange(SP.Dens.plot("CNTRL") +
                        labs(title = "SP CNTRL",
                             x = c(),
                             y = c()) + 
                        theme(legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8)), 
          SP.Dens.plot("MIROC") +
            labs(title = "SP MIROC",
                 y = c()) + 
            theme(legend.title = element_text(size = 8),
                  legend.text = element_text(size = 8)),
          ncol = 1, nrow = 2,
          common.legend = TRUE,
          legend = "bottom")

fig_6 <- ggarrange(fig_6_1, 
                   fig_6_2,
                   ncol = 2,
                   nrow = 1)

ggsave("Figure6.png", fig_6)

