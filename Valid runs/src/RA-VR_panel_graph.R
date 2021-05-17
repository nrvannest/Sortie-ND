######R file that contains all overall Valid Run graphs so they can be combined into a single panel graph#####
##Can be used to graph runaway versions of model, just change #runs, file locations, and timesteps for RA scenarios are POFLABMA = 15, UPLOG and BBBPIPO = 16.

rm(list = ls())
#Runaway = 5, normal = 20
runs <- 20
numsubplots <- 1
species <-
  c("ABCO",
    "CADE" ,
    "PILA",
    "PIPO" ,
    "QUCH" ,
    "QUKE",
    "PICO" ,
    "PIMO",
    "PIJE",
    "ABMA")
Species_colors <- c(
  ABCO = "#3399FF",
  ABMA = "#009900",
  CADE = "#FF33CC",
  PICO = "#996633",
  PIJE = "#999999",
  PILA = "#009999",
  PIMO = "#FF6666",
  PIPO = "#FF6600",
  QUCH = "#6633FF",
  QUKE = "#FFCC00"
)

plots <- c("BBBPIPO", "POFLABMA", "UPLOG")

library(tidyverse)
library(directlabels)
library(gridExtra)
library(data.table)
library(scales)
library(reshape2)
library(RColorBrewer)
##############Function to extract adult and sapling basal area###################
###Use site in quotes and caps to get info ie: "BBBPIPO"
###Timesteps: BBBPIPO & UPLOG = 17, POFLABMA = 15

Basal_area <- function(site, timesteps) {
  results <-
    array(data = NA, dim = c(runs, length(species), timesteps + 1))
  dimnames(results)[2] <- list(species)
  dimnames(results)[3] <- list(1:(timesteps + 1))
  
  for (i in 1:runs) {
    our_file <- paste0(site, "/Output/", site, "_VR_", i, ".out")
    data1 <-
      read.table(
        file = our_file,
        skip = 6,
        header = TRUE,
        sep = "\t"
      )
    data <-
      data1[data1$Subplot == 1, ] ### Subplot #1 is the extracted center 100m x 100m
    
    for (who in species) {
      # tree = Sapl + Adult
      results[i, who,] <-
        data[, paste("Adult.Abs.BA.", who, sep = ".")] +
        data[, paste("Sapl.Abs.BA.", who, sep = ".")]
    }
  }
  return(results)
}

#####################Function to extract adult and sapling density####################
Density <- function(site, timesteps) {
  results <-
    array(data = NA, dim = c(runs, length(species), timesteps + 1))
  dimnames(results)[2] <- list(species)
  dimnames(results)[3] <- list(1:(timesteps + 1))
  
  for (i in 1:runs) {
    our_file <- paste0(site, "/Output/", site, "_VR_", i, ".out")
    data1 <-
      read.table(
        file = our_file,
        skip = 6,
        header = TRUE,
        sep = "\t"
      )
    data <-
      data1[data1$Subplot == 1, ] ### Subplot #1 is the extracted center 100m x 100m
    
    for (who in species) {
      # tree = Sapl + Adult
      results[i, who,] <-
        data[, paste("Adult.Abs.Den.", who, sep = ".")] +
        data[, paste("Sapl.Abs.Den.", who, sep = ".")]
    }
  }
  return(results)
}

###Field Data####
#######################################################################
field_data <- read.table("treedata.txt", header = T)
info_data <- read.table("treeyears.txt", header = T, row.names = 1)
info_plot <-
  read.table("PlotInfo.txt",
             header = T,
             sep = "\t",
             row.names = 1)
#######################################################################

####Determine years#####
#######################################################################

get_yr <- function(site, timesteps) {
  Years <- info_data[site,]
  Years <-
    Years[, apply(Years, 2, function(x)
      ! any(is.na(x)))] #removing NAs
  Years <-
    Years[findInterval(1999, Years):length(Years)] #Field data collection years (plot spp)
  First_year <- as.integer(Years[1])
  Years_sim <- seq(First_year, length.out = timesteps + 1)
  return(Years_sim)
}

###########Analyzing the data#########################
################Median#############################
###Function to find the median basal area for each species for each timestep within each site

Median_BA <- function(site, timesteps) {
  Median_BA <-
    matrix(NA, nrow = length(species), ncol = timesteps + 1)
  rownames(Median_BA) <- species
  for (i in species) {
    temp <- apply(Basal_area(site, timesteps)[, i,], 2, median)
    Median_BA[i,] <- round(temp, 3)
  }
  
  Median <- Median_BA[which(rowSums(Median_BA) > 0), ]
  Median_BA <- transpose(as.data.frame(Median))
  colnames(Median_BA) <- rownames(Median)
  Median_BA <-
    cbind(data.frame("Years" = get_yr(site, timesteps)), Median_BA)
  
  return(Median_BA)
}

###Function to find the median density for each species for each timestep within each site

Median_Dens <- function(site, timesteps) {
  Median_Dens <-
    matrix(NA, nrow = length(species), ncol = timesteps + 1)
  rownames(Median_Dens) <- species
  for (i in species) {
    temp <- apply(Density(site, timesteps)[, i,], 2, median)
    Median_Dens[i,] <- round(temp, 3)
  }
  
  Median <- Median_Dens[which(rowSums(Median_Dens) > 0), ]
  Median_Dens <- transpose(as.data.frame(Median))
  colnames(Median_Dens) <- rownames(Median)
  Median_Dens <-
    cbind(data.frame("Years" = get_yr(site, timesteps)), Median_Dens)
  
  return(Median_Dens)
}


######################Quantile######################

Quant_BA <- function(site, timesteps) {
  Quant_BA <- matrix(0, 1, ncol = timesteps + 1)
  for (i in species) {
    temp <-
      apply(Basal_area(site, timesteps)[, i, ], 2, quantile, c(0.025, 0.975))
    temp <- round(temp, 3)
    rownames(temp) <- c(paste0(i, " 2.5%"), paste0(i, " 97.5%"))
    Quant_BA <- rbind(temp, Quant_BA)
  }
  Quant_BA_temp <- Quant_BA[which(rowSums(Quant_BA) > 0),]
  Quant_BA <- transpose(as.data.frame(Quant_BA_temp))
  colnames(Quant_BA) <- rownames(Quant_BA_temp)
  Quant_BA <-
    cbind(data.frame("Years" = get_yr(site, timesteps)), Quant_BA)
  return(Quant_BA)
}


###
Quant_Dens <- function(site, timesteps) {
  Quant_Dens <- matrix(0, 1, ncol = timesteps + 1)
  for (i in species) {
    temp <-
      apply(Density(site, timesteps)[, i, ], 2, quantile, c(0.025, 0.975))
    temp <- round(temp, 3)
    rownames(temp) <- c(paste0(i, " 2.5%"), paste0(i, " 97.5%"))
    Quant_Dens <- rbind(temp, Quant_Dens)
  }
  Quant_Dens_temp <- Quant_Dens[which(rowSums(Quant_Dens) > 0),]
  Quant_Dens <- transpose(as.data.frame(Quant_Dens_temp))
  colnames(Quant_Dens) <- rownames(Quant_Dens_temp)
  Quant_Dens <-
    cbind(data.frame("Years" = get_yr(site, timesteps)), Quant_Dens)
  return(Quant_Dens)
}


#Note: array dimensions = row length, column length, matrix slice number
###Field data basal area for each species starting with First_year
fd_yrs <- function(site) {
  Years <- info_data[site,]
  Years <-
    Years[, apply(Years, 2, function(x)
      ! any(is.na(x)))] #removing NAs
  Years <-
    Years[findInterval(1999, Years):length(Years)] #Field data collection years (plot spp)
  return(Years)
}

fd_BA <- function(site, timesteps) {
  FD_clean <- matrix(0, length(species), length(fd_yrs(site)))
  rownames(FD_clean) <- species
  for (i in species) {
    temp <-
      field_data[field_data$PLOT == site & field_data$SppCode == i,]
    temp <- temp[, substr(colnames(field_data), 1, 3) == 'DBH']
    colnames(temp) <- info_data[site, ]
    temp <- temp[, substr(colnames(temp), 1, 2) != 'NA']
    temp <-
      temp[, which(colnames(temp) == get_yr(site, timesteps)[1]):length(temp)]
    temp <- pi * (temp / 200) ^ 2
    temp <- apply(temp, 2, sum, na.rm = T)
    FD_clean[i,] <- temp
    
  }
  FD_clean <- FD_clean[which(rowSums(FD_clean) > 0), ]
  FD_BA <- transpose(as.data.frame(FD_clean))
  colnames(FD_BA) <- rownames(FD_clean)
  FD_BA <-
    cbind(data.frame("Years" = as.numeric(fd_yrs(site))), FD_BA)
  
  return(FD_BA)
}

fd_Dens <- function(site, timesteps) {
  FD_Dens <- matrix(0, length(species), length(fd_yrs(site)))
  rownames(FD_Dens) <- species
  for (i in species) {
    temp2 <-
      field_data[field_data$PLOT == site & field_data$SppCode == i,]
    temp2 <- temp2[, substr(colnames(field_data), 1, 3) == 'DBH']
    colnames(temp2) <- info_data[site, ]
    temp2 <- temp2[, substr(colnames(temp2), 1, 2) != 'NA']
    temp2 <-
      temp2[, which(colnames(temp2) == get_yr(site, timesteps)[1]):length(temp2)]
    temp2[!is.na(temp2)] = 1
    temp2 <- apply(temp2, 2, sum, na.rm = T)
    FD_Dens[i,] <- temp2
  }
  FD_Dens_temp <- FD_Dens[which(rowSums(FD_Dens) > 0),]
  FD_Dens <- transpose(as.data.frame(FD_Dens_temp))
  colnames(FD_Dens) <- rownames(FD_Dens_temp)
  FD_Dens <-
    cbind(data.frame("Years" = as.numeric(fd_yrs(site))), FD_Dens)
  
  return(FD_Dens)
}

################GGPLOT Version###################################
library(ggplot2)
library(gtable)
library(gridExtra)

BBBPIPO_BA.plot <-
  ggplot(Median_BA("BBBPIPO", 17), aes(x = get_yr("BBBPIPO", 17))) +
  geom_line(aes(y = ABCO, colour = "ABCO")) +
  geom_line(aes(y = CADE, colour = "CADE")) +
  geom_line(aes(y = PILA, colour = "PILA")) +
  geom_line(aes(y = PIPO, colour = "PIPO")) +
  geom_line(aes(y = QUCH, colour = "QUCH")) +
  geom_line(aes(y = QUKE, colour = "QUKE")) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = ABCO, colour = "ABCO")
  ) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = CADE, colour = "CADE")
  ) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = PILA, colour = "PILA")
  ) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = PIPO, colour = "PIPO")
  ) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = QUCH, colour = "QUCH")
  ) +
  geom_point(
    data = fd_BA("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = QUKE, colour = "QUKE")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "ABCO 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "ABCO 97.5%"],
      fill = "ABCO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "CADE 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "CADE 97.5%"],
      fill = "CADE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "PILA 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "PILA 97.5%"],
      fill = "PILA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "PIPO 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "PIPO 97.5%"],
      fill = "PIPO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "QUCH 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "QUCH 97.5%"],
      fill = "QUCH"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("BBBPIPO", 17)[, "QUKE 2.5%"],
      ymax = Quant_BA("BBBPIPO", 17)[, "QUKE 97.5%"],
      fill = "QUKE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() +
  labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 9, face = "bold"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 10, face = "bold")
  ) +
  ggtitle("Basal Area") +
  ylab("BBBPIPO") 


UPLOG_BA.plot <-
  ggplot(Median_BA("UPLOG", 17), aes(x = get_yr("UPLOG", 17))) +
  geom_line(aes(y = ABCO, colour = "ABCO")) +
  geom_line(aes(y = CADE, colour = "CADE")) +
  geom_line(aes(y = PILA, colour = "PILA")) +
  geom_line(aes(y = PIJE, colour = "PIJE")) +
  geom_line(aes(y = ABMA, colour = "ABMA")) +
  geom_line(aes(y = QUKE, colour = "QUKE")) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = ABCO, colour = "ABCO")
  ) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = CADE, colour = "CADE")
  ) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = PILA, colour = "PILA")
  ) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = PIJE, colour = "PIJE")
  ) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = ABMA, colour = "ABMA")
  ) +
  geom_point(
    data = fd_BA("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = QUKE, colour = "QUKE")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "ABCO 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "ABCO 97.5%"],
      fill = "ABCO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "CADE 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "CADE 97.5%"],
      fill = "CADE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "PILA 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "PILA 97.5%"],
      fill = "PILA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "PIJE 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "PIJE 97.5%"],
      fill = "PIJE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "ABMA 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "ABMA 97.5%"],
      fill = "ABMA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("UPLOG", 17)[, "QUKE 2.5%"],
      ymax = Quant_BA("UPLOG", 17)[, "QUKE 97.5%"],
      fill = "QUKE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() + labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 9, face = "bold"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.title = element_blank()
  ) +
  ylab("UPLOG")

POFLABMA_BA.plot <-
  ggplot(Median_BA("POFLABMA", 15), aes(x = get_yr("POFLABMA", 15))) +
  geom_line(aes(y = ABMA, colour = "ABMA")) +
  geom_line(aes(y = PICO, colour = "PICO")) +
  geom_point(
    data = fd_BA("POFLABMA", 15),
    size = 1,
    mapping = aes(x = Years, y = ABMA, colour = "ABMA")
  ) +
  geom_point(
    data = fd_BA("POFLABMA", 15),
    size = 1,
    mapping = aes(x = Years, y = PICO, colour = "PICO")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("POFLABMA", 15)[, "ABMA 2.5%"],
      ymax = Quant_BA("POFLABMA", 15)[, "ABMA 97.5%"],
      fill = "ABMA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_BA("POFLABMA", 15)[, "PICO 2.5%"],
      ymax = Quant_BA("POFLABMA", 15)[, "PICO 97.5%"],
      fill = "PICO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() + labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.text = element_text(size = 6),
    axis.title.y = element_text(size = 9, face = "bold"),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.title = element_blank()
  ) +
  ylab("POFLABMA") 






##########Density###############

BBBPIPO_Dens.plot <-
  ggplot(Median_Dens("BBBPIPO", 17), aes(x = get_yr("BBBPIPO", 17))) +
  geom_line(aes(y = ABCO, colour = "ABCO")) +
  geom_line(aes(y = CADE, colour = "CADE")) +
  geom_line(aes(y = PILA, colour = "PILA")) +
  geom_line(aes(y = PIPO, colour = "PIPO")) +
  geom_line(aes(y = QUCH, colour = "QUCH")) +
  geom_line(aes(y = QUKE, colour = "QUKE")) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = ABCO, colour = "ABCO")
  ) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = CADE, colour = "CADE")
  ) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = PILA, colour = "PILA")
  ) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = PIPO, colour = "PIPO")
  ) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = QUCH, colour = "QUCH")
  ) +
  geom_point(
    data = fd_Dens("BBBPIPO", 17),
    size = 1,
    mapping = aes(x = Years, y = QUKE, colour = "QUKE")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "ABCO 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "ABCO 97.5%"],
      fill = "ABCO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "CADE 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "CADE 97.5%"],
      fill = "CADE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "PILA 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "PILA 97.5%"],
      fill = "PILA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "PIPO 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "PIPO 97.5%"],
      fill = "PIPO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "QUCH 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "QUCH 97.5%"],
      fill = "QUCH"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("BBBPIPO", 17)[, "QUKE 2.5%"],
      ymax = Quant_Dens("BBBPIPO", 17)[, "QUKE 97.5%"],
      fill = "QUKE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() + labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 6),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    plot.title = element_text(size = 10, face = "bold")
  ) +
  ggtitle("Density")

UPLOG_Dens.plot <-
  ggplot(Median_Dens("UPLOG", 17), aes(x = get_yr("UPLOG", 17))) +
  geom_line(aes(y = ABCO, colour = "ABCO")) +
  geom_line(aes(y = CADE, colour = "CADE")) +
  geom_line(aes(y = PILA, colour = "PILA")) +
  geom_line(aes(y = PIJE, colour = "PIJE")) +
  geom_line(aes(y = ABMA, colour = "ABMA")) +
  geom_line(aes(y = QUKE, colour = "QUKE")) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = ABCO, colour = "ABCO")
  ) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = CADE, colour = "CADE")
  ) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = PILA, colour = "PILA")
  ) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = PIJE, colour = "PIJE")
  ) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = ABMA, colour = "ABMA")
  ) +
  geom_point(
    data = fd_Dens("UPLOG", 17),
    size = 1,
    mapping = aes(x = Years, y = QUKE, colour = "QUKE")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "ABCO 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "ABCO 97.5%"],
      fill = "ABCO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "CADE 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "CADE 97.5%"],
      fill = "CADE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "PILA 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "PILA 97.5%"],
      fill = "PILA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "PIJE 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "PIJE 97.5%"],
      fill = "PIJE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "ABMA 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "ABMA 97.5%"],
      fill = "ABMA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("UPLOG", 17)[, "QUKE 2.5%"],
      ymax = Quant_Dens("UPLOG", 17)[, "QUKE 97.5%"],
      fill = "QUKE"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() + labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 6),
    plot.title = element_blank(),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7)
  )

POFLABMA_Dens.plot <-
  ggplot(Median_Dens("POFLABMA", 15), aes(x = get_yr("POFLABMA", 15))) +
  geom_line(aes(y = ABMA, colour = "ABMA")) +
  geom_line(aes(y = PICO, colour = "PICO")) +
  geom_point(
    data = fd_Dens("POFLABMA", 15),
    size = 1,
    mapping = aes(x = Years, y = ABMA, colour = "ABMA")
  ) +
  geom_point(
    data = fd_Dens("POFLABMA", 15),
    size = 1,
    mapping = aes(x = Years, y = PICO, colour = "PICO")
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("POFLABMA", 15)[, "ABMA 2.5%"],
      ymax = Quant_Dens("POFLABMA", 15)[, "ABMA 97.5%"],
      fill = "ABMA"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  geom_ribbon(
    aes(
      ymin = Quant_Dens("POFLABMA", 15)[, "PICO 2.5%"],
      ymax = Quant_Dens("POFLABMA", 15)[, "PICO 97.5%"],
      fill = "PICO"
    ),
    alpha = 0.1,
    show.legend = F
  ) +
  theme_light() + labs(color = "Species") +
  scale_color_manual(values = Species_colors) +
  scale_fill_manual(values = Species_colors) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 6),
    plot.title = element_blank(),
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7)
  )


#########Group plots together##############
library(grid)
library(ggpubr)

 BB_temp <- ggarrange(BBBPIPO_BA.plot, BBBPIPO_Dens.plot,
                      ncol = 2, nrow = 1,
                     common.legend = TRUE,
                      legend = "right")

 BB_temp <- annotate_figure(BB_temp,
                            left = text_grob("",
                                             rot = 90,
                                             size = 7,
                                             face = "bold"))

 UP_temp <- ggarrange(UPLOG_BA.plot, UPLOG_Dens.plot,
                      ncol = 2, nrow = 1,
                      legend = "right",
                      common.legend = TRUE)

 UP_temp <- annotate_figure(UP_temp,
                            left = text_grob("",
                                             rot = 90,
                                             size = 7,
                                             face = "bold"))

 PO_temp <- ggarrange(POFLABMA_BA.plot, POFLABMA_Dens.plot,
                      ncol = 2, nrow = 1,
                      common.legend = TRUE,
                     legend = "right")

 PO_temp <- annotate_figure(PO_temp,
                            bottom = text_grob("Year",
                                               size = 8,
                                               face = "bold")
 )

VR_graph <- ggarrange(BB_temp, UP_temp, PO_temp, nrow = 3, ncol = 1)




ggsave(
  "VR_graph.png",
  VR_graph,
  width = 18,
  height = 17,
  units = "in",
  dpi = 800,
  scale = 0.4,
  type = "cairo",
  antialias = "none"
)
dev.off



