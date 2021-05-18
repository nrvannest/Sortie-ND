############Creating maps for the SEKI plots############
# Seedlings >50 cm are recorded in sapling data set
# *********** Character arguments must be in "" for functions to work
rm(list = ls())

########################################################
############Seki plots are 80 x 100 so sortie map needs to be 240 x 300

#setwd("C:/Users/ucmuser/SORTIE2/runs/Simulation runs/Sequoia Valid Runs")


library("readxl")
library("reshape2")
library("reshape")
library("data.table")
library("tidyverse")
library(tidyr)



seedlings <- as.data.frame(read_excel("SEKI_seedling_data.xlsx"))
saplings <- as.data.frame(read_excel("SEKI_sapling_data.xlsx"))
adults <- read.csv("SEKI_tree_data_clean.txt", sep = "\t", header = T)

#Make 80 x 100 plot then replicate 9x to make a 3x3 plot
#Use adult tree actual coordinates
#Seedlings total by size class and x10.

# adult_test <- as.data.frame(read_excel("SEKI_tree_data.xlsx"))
# map_SJ <- read.table("SJ_map.txt", header = T)
# map_SJM <- read.table("SJM_map.txt", header = T)
# map_SJP <- read.table("SJP_map.txt", header = T)
# map_SP <- read.table("SP_map.txt", header = T)

###############################SEEDLINGS######################################

###Need to use data from first year (2016) - line up with climate data

Vars <- c("X","Y","Species","Type","Diam","Height", "ParamG1")
species <- c("ABCO","CADE" ,"PILA", "PIPO" ,"QUCH" ,"QUKE", "PICO" ,"PIMO", "PIJE", "ABMA")
First_yr <- "16"
sdl_vars <- c("10.25_16", "25.50_16")

#Seedlings by site
OrganizeSdls <- function(site){
  Sdls <- seedlings[seedlings$Site == site,
                    c("Site", "Species", "10.25_16", "25.50_16")]
  Sdls <- melt.data.frame(Sdls)
  Sdls <- Sdls[Sdls$value != 0, ]
  Sdls <- na.omit(Sdls)
  Sdls <- uncount(Sdls, weights = Sdls$value)
  Sdls <- Sdls[, c("Site","Species", "variable")]
  class25 <- Sdls[Sdls$variable == "10.25_16", ]
  class50 <- Sdls[Sdls$variable == "25.50_16", ]
  
  
  new25 <- data.frame("Size_2016" = replicate(nrow(class25), "25"))
  new50 <- data.frame("Size_2016" = replicate(nrow(class50), "50"))
  Sdls <- rbind(cbind(class25[, c("Site", "Species")], new25),
                cbind(class50[, c("Site", "Species")], new50))
  colnames(Sdls) <- c("Site", "Species", "Size Class")
  return(Sdls)
} 

#####################SAPLINGS###########################
##****Only found in SJ and SJM*************************

Clean_sap <- function(site){
  sap <- saplings[, c("Site", "Species", "Size_2016")] 
  sap <- sap[sap$Site == site, c("Site", "Species", "Size_2016")] #sort by site
  sap <- sap[sap$Size_2016 != "NA", ] #remove NAs
  sap <- sap[sap$Size_2016 > 140, ] #only seedlings <140 cm
  colnames(sap) <- c("Site", "Species", "Size Class")
  return(sap)
}

####"Saplings" (not actual saplings but recorded this way.. ht below 137 cm) are included in this step

All_sdls <- function(site){
  all <- rbind(OrganizeSdls(site), Clean_sap(site))
  return(all)
}

#############Adults##################
##Take DBH from SU_16 column since seedlings are also from 16


TreeMap <- function(site){
  
  # select trees that are in the site, at the first year
  map <- as.data.frame(adults[adults$Site == site, c("x", "y", "Site", "Species", "Su_16_DBH")])
  
  map <- map[complete.cases(map), ]
  
  map <- cbind.data.frame(map, 
                          data.frame("Type" = rep("Adult", nrow(map))), 
                          data.frame("Height" = rep(0, nrow(map))),
                          data.frame("ParamG1" = rep(0, nrow(map))))
  
  if(site == "SJ" || site == "SJM"){
    # give adults new coordinates to replicate 3x3
    x.1 <- as.numeric(map[, "x"]) + 80; x.2 <- as.numeric(map[, "x"]) + 160;
    y.1 <- as.numeric(map[, "y"]) + 100; y.2 <- as.numeric(map[, "y"]) + 200;
  } else{
    # give adults new coordinates to replicate 3x3
    x.1 <- as.numeric(map[, "x"]) + 100; x.2 <- as.numeric(map[, "x"]) + 200;
    y.1 <- as.numeric(map[, "y"]) + 80; y.2 <- as.numeric(map[, "y"]) + 160;
  }

  
  map0.1 <- map; map0.2 <- map; map1.0 <- map; map2.0 <- map;
  map1.1 <- map; map1.2 <- map; map2.1 <- map; map2.2 <- map;
  
  map0.1$x <- x.1; map0.2$x <- x.2; map1.0$y <- y.1; map2.0$y <- y.2;
  map1.1$x <- x.1; map1.1$y <- y.1; map1.2$x <- x.1; map1.2$y <- y.2;
  map2.1$x <- x.2; map2.1$y <- y.1; map2.2$x <- x.2; map2.2$y <- y.2;
  
  map <- rbind.data.frame(map, map0.1, map0.2,
                          map1.0, map1.1, map1.2,
                          map2.0, map2.1, map2.2)
  map <- map[, c("x", "y", "Species", "Type", "Su_16_DBH", "Height", "ParamG1")]
  
  colnames(map) <- c("X","Y","Species","Type","Diam","Height", "ParamG1")
  
  map$X <- round(map$X, 3)
  map$Y <- round(map$Y, 3)
  
  return(map)
  
}

#####Find proportion of ABXX or PIXX that need to be renamed########

########THIS FUNCTION TELLS YOU HOW MANY OF XX SPECIES ARE TO BE ACTUAL SPP... DOES NOT GIVE TOTAL########
###***important for ActualSdls function to work properly. 


#****************************************

FixSdlsXX <- function(site, spp){
  temp <- All_sdls("SJP")
   tbl <- table(temp[, "Species"])
  if(is.na(tbl["ABCO"]) == FALSE && tbl["ABCO"] > 0){
  abco <- tbl["ABCO"]
  } else {
    abco <- 0
  } 
  
  if(is.na(tbl["ABMA"]) == FALSE && tbl["ABMA"] > 0){
    abma <- tbl["ABMA"]
  } else {
    abma <- 0
  }
  
   if(is.na(tbl["ABXX"]) == FALSE && tbl["ABXX"] >0){
     abxx <- tbl["ABXX"]
   } else {
     abxx <- 0
   }
  
  if(is.na(tbl["PICO"]) == FALSE && tbl["PICO"] > 0){
    pico <- tbl["PICO"]
  } else {
    pico <- 0
  }
  
  if(is.na(tbl["PIJE"]) == FALSE && tbl["PIJE"] > 0){
    pije <- tbl["PIJE"]
  } else {
    pije <- 0
  }
  
  if(is.na(tbl["PILA"]) == FALSE && tbl["PILA"] > 0){
    pila <- tbl["PILA"]
  } else {
    pila <- 0
  }
  
  if(is.na(tbl["PIMO"]) == FALSE && tbl["PIMO"] > 0){
    pimo <- tbl["PIMO"]
  } else {
    pimo <- 0
  }
  
  if(is.na(tbl["PIPO"]) == FALSE && tbl["PIPO"] > 0){
  pipo <- tbl["PIPO"]
  } else {
    pipo <- 0
  }
  
   if(is.na(tbl["PIXX"]) == FALSE && tbl["PIXX"] > 0){
     pixx <- tbl["PIXX"]
   } else {
     pixx <- 0
   }
   
  if(abco + abma == 0 && abxx > 0){
    abco <- table(TreeMap("SJP")[, "Species"])["ABCO"]
    abma <- table(TreeMap("SJP")[, "Species"])["ABMA"]
  }
   
  if(spp == "ABCO"){
    if(abxx > 0){
      abco <- abxx*(abco/(abco + abma)) 
      abco <- round(abco, 0)
    } 
    if(is.na(abco) == T || is.nan(abco) == T){
        abco <- 0
      } else {
        abco <- round(abco, 0)
        return(as.vector(abco))
      }
  }
   
  if(spp == "ABMA"){
    if(abxx > 0){
      abma <- abxx*(abma/(abco + abma))
      abco <- round(abco, 0)
    }
    if(is.na(abma) == T || is.nan(abma) == T){
      abma <- 0
    } else {
      abma <- round(abma, 0)
      return(as.vector(abma))
    }
  }
   
   if(pico + pije + pila + pimo + pipo == 0 && pixx > 0){
     pico <- table(TreeMap("SJP")[, "Species"])["PICO"]
     pije <- table(TreeMap("SJP")[, "Species"])["PIJE"]
     pila <- table(TreeMap("SJP")[, "Species"])["PILA"]
     pimo <- table(TreeMap("SJP")[, "Species"])["PIMO"]
     pipo <- table(TreeMap("SJP")[, "Species"])["PIPO"]
   }
   
  if(spp == "PICO"){
    if(pixx > 0){
      pico <- pixx*(pico/(pico + pije + pila + pimo + pipo))
      pico <- round(pico, 0)
    }
    if(is.na(pico) == T || is.nan(pico) == T){
      pico <- 0
      } else {
        pico <- round(pico, 0)
        return(as.vector(pico))
    }
  }
  if(spp == "PIJE"){
    if(pixx > 0){
      pije <- pixx*(pije/(pico + pije + pila + pimo + pipo))
      pije <- round(pije, 0)
    }
    if(is.na(pije) == T || is.nan(pije) == T){
      pije <- 0
    } else {
      pije <- round(pije, 0)
      return(as.vector(pije))
    }
  }
  if(spp == "PILA"){
    if(pixx > 0){
      pila <- pixx*(pila/(pico + pije + pila + pimo + pipo)) 
      pila <- round(pila, 0)
    }
    if(is.na(pila) == T || is.nan(pila) == T){
      pila <- 0
    } else {
      pila <- round(pila, 0)
      return(as.vector(pila))
    }
  }
  if(spp == "PIMO"){
    if(pixx > 0){
      pimo <- pixx*(pimo/(pico + pije + pila + pimo + pipo)) 
      pimo <- round(pimo, 0)
    }
    if(is.na(pimo) == T || is.nan(pimo) == T){
      pimo <- 0
    } else {
      pimo <- round(pimo, 0)
      return(as.vector(pimo))
    }
  }
  if(spp == "PIPO"){
    if(pixx > 0){
      pipo <- pixx*(pipo/(pico + pije + pila + pimo + pipo))
      pipo <- round(pipo, 0)
    }
      if(is.na(pipo) == T || is.nan(pipo) == T){
        pipo <- 0
      } else {
        pipo <- round(pipo, 0)
        return(as.vector(pipo))
      }
  }
}


#######Updating OrganizeSdls() to include XX spp as actual spp and change to ActualSdls()########
  
  
  ActualSdls <- function(site){
  sdl <- All_sdls(site)
  
  ####Adding in actual spp for XX spp
  if("ABXX" %in% sdl$Species){
    a <- data.frame("Species" = rep("ABCO", round(FixSdlsXX(site, "ABCO"))))
    b <- data.frame("Species" = rep("ABMA", round(FixSdlsXX(site, "ABMA"))))
    ugh <- rbind(a, b)
    abxx <- cbind(sdl[sdl$Species == "ABXX", ], sample(ugh[,"Species"]))
    colnames(abxx) <- c("Site", "Remove", "Size Class", "Species")
    abxx <- abxx[, c("Site", "Species", "Size Class")]
    other <- sdl[sdl$Species != "ABXX", ]
    colnames(other) <- colnames(abxx)
    sdl <- rbind(abxx, other)
  } 
  
  if("PIXX" %in% sdl$Species){
    c <- data.frame("Species" = rep("PICO", round(FixSdlsXX(site, "PICO"))))
    d <- data.frame("Species" = rep("PIJE", round(FixSdlsXX(site, "PIJE"))))
    e <- data.frame("Species" = rep("PILA", round(FixSdlsXX(site, "PILA"))))
    f <- data.frame("Species" = rep("PIMO", round(FixSdlsXX(site, "PIMO"))))
    g <- data.frame("Species" = rep("PIPO", round(FixSdlsXX(site, "PIPO"))))
    ugh <- rbind(c, d, e, f, g)
    pixx <- cbind(sdl[sdl$Species == "PIXX", ], sample(ugh[,"Species"]))
    colnames(pixx) <- c("Site", "Remove", "Size Class", "Species")
    pixx <- pixx[, c("Site", "Species", "Size Class")]
    other <- sdl[sdl$Species != "PIXX", ]
    colnames(other) <- colnames(pixx)
    sdl <- rbind(pixx, other)
  }
    return(sdl)
}    
  
  

#######################Add Heights and coordinates####################
#Based on first year
##########Still showing XX spp. need to fix.##############
####################################
####################################
SdlMap <- function(site){
  map <- ActualSdls(site)
  
  sdl25 <- map[map$"Size Class" == 25, #subsetting by size class
               c("Site", "Species", "Size Class")] 
  sdl50 <- map[map$"Size Class" == 50, 
               c("Site", "Species", "Size Class")] 
  sdl75 <- map[map$"Size Class" == 75, 
               c("Site", "Species", "Size Class")] 
  #sdl100 <- map[map$"Size Class "== 100, 
  #              c("Site", "Species", "Size Class")] 
  #sdl137 <- map[map$"Size Class" == 137, 
  #              c("Site", "Species", "Size Class")]
  ht25 <- data.frame("Height" = round(runif(nrow(sdl25), #creating random heights
                                            min = .1,
                                            max = .24),
                                      2))
  ht50 <- data.frame("Height" = round(runif(nrow(sdl50),
                                            min = .25,
                                            max = .54),
                                      2))
  ht75 <- data.frame("Height" = round(runif(nrow(sdl75),
                                            min = .50,
                                            max = .74),
                                      2))
  #ht100 <- data.frame("Height" = round(runif(nrow(sdl100),
  #                                           min = .75,
  #                                           max = .99),
  #                                     2))
  #ht137 <- data.frame("Height" = round(runif(nrow(sdl137),
  #                                           min = 1.0,
  #                                           max = 1.34),
  #                                     2))
  sdl25 <- cbind(sdl25, ht25) #assigning created heights to correct seedlings 
  sdl50 <- cbind(sdl50, ht50)
  sdl75 <- cbind(sdl75, ht75)
  #sdl100 <- cbind(sdl100, ht100)
  #sdl137 <- cbind(sdl137, ht137)
  map <- rbind(sdl25, sdl50, sdl75)#, sdl100, sdl137) #binding rows on top of each other
  
  if(site == "SJ" || site == "SJM"){
    
    #SJ and SJM are 80 x 100
    sq1x <- data.frame("X" = round(runif(nrow(map), min = 0, max = 79.9), 2)) 
    sq1y <- data.frame("Y" = round(runif(nrow(map), min = 0, max = 99.9), 2))
    
    sq2x <- data.frame("X" = round(runif(nrow(map), min = 80, max = 159.9), 2))
    sq2y <- data.frame("Y" = round(runif(nrow(map), min = 100, max = 199.9), 2))
    
    sq3x <- data.frame("X" = round(runif(nrow(map), min = 160, max = 240), 2))
    sq3y <- data.frame("Y" = round(runif(nrow(map), min = 200, max = 300), 2))
    
  } else {
    #SP and SJP are 100 x 80
    sq1x <- data.frame("X" = round(runif(nrow(map), min = 0, max = 99.9), 2)) 
    sq1y <- data.frame("Y" = round(runif(nrow(map), min = 0, max = 79.9), 2))
    
    sq2x <- data.frame("X" = round(runif(nrow(map), min = 100, max = 199.9), 2))
    sq2y <- data.frame("Y" = round(runif(nrow(map), min =0, max = 159.9), 2))
    
    sq3x <- data.frame("X" = round(runif(nrow(map), min = 200, max = 300), 2))
    sq3y <- data.frame("Y" = round(runif(nrow(map), min = 160, max = 240), 2))
  }
  
  

  
  #Binding each x&y coordinate to seedling map
  sdl1.1 <- cbind(map, sq1x, sq1y); sdl2.1 <- cbind(map, sq2x, sq1y); sdl3.1 <- cbind(map, sq3x, sq1y);
  sdl1.2 <- cbind(map, sq1x, sq2y); sdl2.2 <- cbind(map, sq2x, sq2y); sdl3.2 <- cbind(map, sq3x, sq2y);
  sdl1.3 <- cbind(map, sq1x, sq3y); sdl2.3 <- cbind(map, sq2x, sq3y); sdl3.3 <- cbind(map, sq3x, sq3y);
  
  #Binding each of 9 plots on top of each other to create 3x3 grid
  map <- rbind(sdl1.1, sdl1.2, sdl1.3,
               sdl2.1, sdl2.2, sdl2.3,
               sdl3.1, sdl3.2, sdl3.3)
  
  diam <- data.frame("Diam" = sapply(0.02, rep, nrow(map))) #default diameter for seedlings
  type <- data.frame("Type" = replicate(nrow(map), "seedling")) #Add type
  paramg1 <- data.frame("ParamG1" = sapply(0, rep, nrow(map))) #Empty parameter for now - applies to heritability 
  map <- cbind(map, diam, type, paramg1)
  map <- map[, c("X", "Y", "Species", "Type", "Diam", "Height", "ParamG1")] #reorder
  rownames(map) <- c() #remove row names
  
  return(map)
}


#visual check

plot(SdlMap("SJ")$X, SdlMap("SJ")$Y)



###########Adult trees also have unknown species... need to clean data.
##However, no instances of unknown genera only. no need to program for that. 

###Function to identify how many XX species need to be replaced with actual spp
FixAdultXX <- function(site, spp){
  temp <- TreeMap(site)
  tbl <- table(temp[, "Species"])
  if(is.na(tbl["ABCO"]) == FALSE && tbl["ABCO"] > 0){
    abco <- tbl["ABCO"]
  } else {
    abco <- 0
  } 
  
  if(is.na(tbl["ABMA"]) == FALSE && tbl["ABMA"] > 0){
    abma <- tbl["ABMA"]
  } else {
    abma <- 0
  }
  
  if(is.na(tbl["ABXX"]) == FALSE && tbl["ABXX"] >0){
    abxx <- tbl["ABXX"]
  } else {
    abxx <- 0
  }
  
  if(is.na(tbl["PICO"]) == FALSE && tbl["PICO"] > 0){
    pico <- tbl["PICO"]
  } else {
    pico <- 0
  }
  
  if(is.na(tbl["PIJE"]) == FALSE && tbl["PIJE"] > 0){
    pije <- tbl["PIJE"]
  } else {
    pije <- 0
  }
  
  if(is.na(tbl["PILA"]) == FALSE && tbl["PILA"] > 0){
    pila <- tbl["PILA"]
  } else {
    pila <- 0
  }
  
  if(is.na(tbl["PIMO"]) == FALSE && tbl["PIMO"] > 0){
    pimo <- tbl["PIMO"]
  } else {
    pimo <- 0
  }
  
  if(is.na(tbl["PIPO"]) == FALSE && tbl["PIPO"] > 0){
    pipo <- tbl["PIPO"]
  } else {
    pipo <- 0
  }
  
  if(is.na(tbl["PIXX"]) == FALSE && tbl["PIXX"] > 0){
    pixx <- tbl["PIXX"]
  } else {
    pixx <- 0
  }
  
  
  if(spp == "ABCO"){
    if(abxx > 0){
      abco <- abxx*(abco/(abco + abma)) 
    } 
    if(is.na(abco) == T || is.nan(abco) == T){
      abco <- 0
    } else {
      abco <- round(abco, 0)
      return(as.vector(abco))
    }
  }
  
  if(spp == "ABMA"){
    if(abxx > 0){
      abma <- abxx*(abma/(abco + abma)) 
    }
    if(is.na(abma) == T || is.nan(abma) == T){
      abma <- 0
    } else {
      abma <- round(abma, 0)
      return(as.vector(abma))
    }
  }
  
  if(spp == "PICO"){
    if(pixx > 0){
      pico <- pixx*(pico/(pico + pije + pila + pimo + pipo))
    }
    if(is.na(pico) == T || is.nan(pico) == T){
      pico <- 0
    } else {
      pico <- round(pico, 0)
      return(as.vector(pico))
    }
  }
  if(spp == "PIJE"){
    if(pixx > 0){
      pije <- pixx*(pije/(pico + pije + pila + pimo + pipo))
    }
    if(is.na(pije) == T || is.nan(pije) == T){
      pije <- 0
    } else {
      pije <- round(pije, 0)
      return(as.vector(pije))
    }
  }
  if(spp == "PILA"){
    if(pixx > 0){
      pila <- pixx*(pila/(pico + pije + pila + pimo + pipo)) 
    }
    if(is.na(pila) == T || is.nan(pila) == T){
      pila <- 0
    } else {
      pila <- round(pila, 0)
      return(as.vector(pila))
    }
  }
  if(spp == "PIMO"){
    if(pixx > 0){
      pimo <- pixx*(pimo/(pico + pije + pila + pimo + pipo)) 
    }
    if(is.na(pimo) == T || is.nan(pimo) == T){
      pimo <- 0
    } else {
      pimo <- round(pimo, 0)
      return(as.vector(pimo))
    }
  }
  if(spp == "PIPO"){
    if(pixx > 0){
      pipo <- pixx*(pipo/(pico + pije + pila + pimo + pipo)) 
    }
    if(is.na(pipo) == T || is.nan(pipo) == T){
      pipo <- 0
    } else {
      pipo <- round(pipo, 0)
      return(as.vector(pipo))
    }
  }
}

############put function into table for easier viewing####################
table_replace <- function(site){
  tbl_rep <- data.frame("ABCO" = FixAdultXX(site, "ABCO"),
                        "ABMA" = FixAdultXX(site, "ABMA"),
                        "PICO" = FixAdultXX(site, "PICO"),
                        "PIJE" = FixAdultXX(site, "PIJE"),
                        "PILA" = FixAdultXX(site, "PILA"),
                        "PIMO" = FixAdultXX(site, "PIMO"),
                        "PIPO" = FixAdultXX(site, "PIPO"))
  return(tbl_rep)
}

###########Replacing unknown spp with proportionate species#########

adlt_replace <- function(site){
  treemap <- TreeMap(site)
  if(nrow(treemap[treemap$Species == "ABXX", ]) > 0){
  ab_map <- replace(treemap[treemap$Species == "ABXX", ], 
          3, 
          c(rep("ABCO", table_replace(site)["ABCO"]),
            rep("ABMA", table_replace(site)["ABMA"])))
  } else{
    ab_map <- c()
  }
  if(nrow(treemap[treemap$Species == "PIXX", ]) > 0){
  pi_map <- replace(treemap[treemap$Species == "PIXX", ], 
                    3, 
                    c(rep("PICO", table_replace(site)["PICO"]),
                      rep("PIJE", table_replace(site)["PIJE"]),
                      rep("PILA", table_replace(site)["PILA"]),
                      rep("PIMO", table_replace(site)["PIMO"]),
                      rep("PIPO", table_replace(site)["PIPO"])))
  } else {
    pi_map <- c()
  }
  total <- rbind(treemap[!grepl("XX", treemap[["Species"]]),],
                     ab_map, pi_map)
  return(total)
  
}

###BInding to make full map
Full_map <- function(site){
  map <- rbind(adlt_replace(site), SdlMap(site))
  ###Clean out any species that we aren't looking at
  for(i in c("Too hard to tell", "UMCA", "CEIN", "UNKN")){
    map <- map[!grepl(i, map$Species),]
  }
  
  
  return(map)
}






plot(Full_map("SJ")$X, Full_map("SJ")$Y) 


write.table(Full_map("SJ"), "SJ_map.txt", quote=F, sep="\t", row.names = F)
