##############1 hectar plot#####################
#reset
rm(list = ls())

# load the tree data (in this data set, newX and newY correspond to coordinates corected for the tilted plots)
load("tree.Rdata")

# load the seedlings data -- ** only seedlings > 10 size class
seedlings <- read.csv(("LgSdlData.csv"),sep=',')

# load the dates for DBH measures for each plot
years <- read.table(("treeyears.txt"), h=T, row.names = 1)

####


plots <- c('YOHOPIPO', 'BBBPIPO', 'CCRPIPO', 'CRCRPIPO', 'FFS7CONTROL','FFS6BURN', 'FFS5BURN', 'SURIP', 'SUABCO',
           'SUPILA', 'FRPIJE', 'FFS2BURN', 'LMCC', 'LOTHAR', 'LOGSEGI', 'UPTHAR', 'LOLOG', 'UPLOG', 'LOGPIJE',     
           'SFTRABMA', 'WTABMA', 'POFLABMA', 'PGABMA', 'EMSLOPE', 'EMRIDGE')


first_year <- 1999 ####Saving what we want to be the first year

first_years <- matrix(0, nrow = 1, ncol = length(plots))
colnames(first_years) <- plots

for(i in plots){  #######Loop to find first year for each plot ***Taking year closest to but less than 1999 if possible
  plt <- years[i,]
  if(max(na.omit(plt[plt <= first_year])) == -Inf){
   firstyr <- min(na.omit(plt[plt >= first_year]))
  }else{
    firstyr <- max(na.omit(plt[plt <= first_year]))
  }
  first_years[, i] <- firstyr 
}
###############################Adults###########################################

TreeMap <- function(plot){
  
  at_first_year <- which(years[plot, ] == as.numeric(first_years[,plot]), arr.ind = T)[, "col"]
  
  # select trees that are in the plot, at the first year
  map <- tree[tree$PLOT == plot & 
                !is.na(tree[,paste0("DBH",at_first_year)]),]
  
  # create the map with X, Y, species, type (here we have only adults), DBH and height (will be computed by SORTIE based on DBH, set to 0)
  map <- cbind(map[,c("newX","newY","SppCode")],
               "Adult",
               map[,paste0("DBH",at_first_year)],
               0, 0)
  colnames(map) <- c("X","Y","Species","Type","Diam","Height", "ParamG1")
  
  # same map around to avoid border problems (as ther is no torus conditions anymore)
  indx <- rep(c(rep(0,nrow(map)),rep(1,nrow(map)),rep(2,nrow(map))),3)
  indy <- c(rep(0,3*nrow(map)),rep(1,3*nrow(map)),rep(2,3*nrow(map)))
  map <- rbind(map,map,map,map,map,map,map,map,map)
  map$X <- round(map$X + indx * 100,3)
  map$Y <- round(map$Y + indy * 100,3)
  
  return(map)
}

#visual check: 
#plot(TreeMap("BBBPIPO")$X,TreeMap("BBBPIPO")$Y)

##############################Seedlings####################################
plot <- c("BBBPIPO", "CCRPIPO", "CRCRPIPO", "EMRIDGE", "EMSLOPE", "FFS2BURN", "FFS5BURN", 
           "FFS6BURN", "FFS7CONTROL", "FRPIJE", "LMCC", "LOGPIJE", "LOGSEGI", "LOLOG", 
           "PGABMA", "POFLABMA", "SFTRABMA", "SUABCO", "SUPILA", "SURIP", "UPLOG", "UPTHAR",
           "WTABMA", "YOHOPIPO")
species <- c("ABCO", "ABMA", "ABXX", "CADE", "PICO", "PIJE", "PIMO", "PIPO", "PIXX", "QUCH",
             "QUKE")

seedlings_clean <- seedlings[seedlings$SPPCODE %in% species, ]
seedlings_clean <- droplevels(seedlings_clean, except = species)


library("tidyverse")
library("tidyr")
#Need to:
#(Only use seedlings from 1st year - 1999)
#1. Multiply total seedlings x8 to fit in adult plot (16 subplots, only 2 were sampled for seedlings)
#2. Multiply new total x9 to create sortie plot - just as adults are. Sortie plot is a 3x3 grid.
#3. Give seedlings random x and y coordinates
#4. Give seedlings random height that fits within their size class
#5. Add diam column where all are 0.02 cm, and Param G1 column of 0s.

OrganizeSdls <- function(site){ ####Function to clean and view sdls as list (year 1999 only)
  sdl <- seedlings_clean[seedlings_clean$PLOT_NAME == site, 
                   c("PLOT_NAME", "SPPCODE", "SIZE_CLASS", "COUNT1999")] #subsetting by plot
  sdl <- uncount(data = sdl, weights = sdl$COUNT1999) #list of each occurance individually
  sdl <- sdl[sdl$COUNT1999 != 0, ]
  sdl <- sdl[ , c("PLOT_NAME", "SPPCODE", "SIZE_CLASS")]
  colnames(sdl) <- c("Plot", "Species", "Size Class")
  sdl <- sdl[sdl$"Size Class" > 2, ]
  return(sdl)
} 



viewSdlTbl <- matrix(0, length(plot), length(species)) ###View all sdls as table by plot
rownames(viewSdlTbl) <- plot
colnames(viewSdlTbl) <- species
for(i in plot){
  viewSdlTbl[i, ] <- table(OrganizeSdls(i)[, "Species"])
}



FixSdlsXX <- function(site, spp){ ### replaces unknown species with proportional known spp
  tbl <- table(OrganizeSdls(site)[, "Species"])
  if(is.na(tbl["ABCO"]) == FALSE){
    abco <- tbl["ABCO"]
  } else {
    abco <- 0
  } 
  
  if(is.na(tbl["ABMA"]) == FALSE){
    abma <- tbl["ABMA"]
  } else {
    abma <- 0
  }
  
  abxx <- tbl["ABXX"]
  if(is.na(tbl["PICO"]) == FALSE){
    pico <- tbl["PICO"]
  } else {
    pico <- 0
  }
  
  if(is.na(tbl["PIJE"]) == FALSE){
    pije <- tbl["PIJE"]
  } else {
    pije <- 0
  }
  
  if(is.na(tbl["PILA"]) == FALSE){
    pila <- tbl["PILA"]
  } else {
    pila <- 0
  }
  
  if(is.na(tbl["PIMO"]) == FALSE){
    pimo <- tbl["PIMO"]
  }
  
  if(is.na(tbl["PIPO"]) == FALSE ){
    pipo <- tbl["PIPO"]
  } else {
    pipo <- 0
  }
  
  pixx <- tbl["PIXX"]
  
  if(spp == "ABCO"){
    if(is.na(abco) == F)
      return(abxx*(abco/(abco + abma)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "ABMA"){
    if(is.na(abma) == F)
      return(abxx*(abma/(abco + abma)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "PICO"){
    if(is.na(pico) == F)
      
      return(pixx*(pico/(pico + pije + pila + pimo + pipo)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "PIJE"){
    if(is.na(pije) == F)
      return(pixx*(pije/(pico + pije + pila + pimo + pipo)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "PILA"){
    if(is.na(pila) == F)
      return(pixx*(pila/(pico + pije + pila + pimo + pipo)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "PIMO"){
    if(is.na(pimo) == F)
      return(pixx*(pimo/(pico + pije + pila + pimo + pipo)))
    else{
      return("Species not found in this plot")
    }
  }
  if(spp == "PIPO"){
    if(is.na(pipo) == F)
      return(pixx*(pipo/(pico + pije + pila + pimo + pipo)))
    else{
      return("Species not found in this plot")
    }
  }
}

ActualSdls <- function(site){
  sdl <- OrganizeSdls(site)
  
  
  ####Adding in actual spp for XX spp
  if("ABXX" %in% sdl$Species){
    a <- data.frame("Species" = rep("ABCO", round(FixSdlsXX(site, "ABCO"))))
    b <- data.frame("Species" = rep("ABMA", round(FixSdlsXX(site, "ABMA"))))
    ugh <- rbind(a, b)
    abxx <- cbind(sdl[sdl$Species == "ABXX", ], sample(ugh[,"Species"]))
    colnames(abxx) <- c("Plot", "Remove", "Size Class", "Species")
    abxx <- abxx[, c("Plot", "Species", "Size Class")]
    other <- sdl[sdl$Species != "ABXX", ]
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
    colnames(pixx) <- c("Plot", "Remove", "Size Class", "Species")
    pixx <- pixx[, c("Plot", "Species", "Size Class")]
    other <- sdl[sdl$Species != "PIXX", ]
    sdl <- rbind(pixx, other)
  } 
  return(sdl)
  
}


SizeClass1 <- 10
SizeClass2 <- c(25, 50)
SizeClass3 <- c(75, 100, 137)
SizeClasses <- c(10, 25, 50, 75, 100, 137)

##############Create map with nested function ActualSlds()############

##Add size class 10 to check if maps get too big or if ok####
####not yet functioning properly####

SdlMap <- function(site){
  sdl <- ActualSdls(site)
  
  sdl <- rbind(sdl, sdl, sdl, sdl, sdl, sdl, sdl, sdl) ### *8 to mimic actual size plot
  
 # sdl10 <- sdl[sdl$"Size Class" == 10, #subsetting by size class
 #             c("Plot", "Species", "Size Class")] ### too many with < 10 added
  sdl25 <- sdl[sdl$"Size Class" == 25, 
               c("Plot", "Species", "Size Class")] 
  sdl50 <- sdl[sdl$"Size Class" == 50, 
               c("Plot", "Species", "Size Class")] 
  sdl75 <- sdl[sdl$"Size Class" == 75, 
               c("Plot", "Species", "Size Class")] 
  sdl100 <- sdl[sdl$"Size Class" == 100, 
                c("Plot", "Species", "Size Class")] 
  sdl137 <- sdl[sdl$"Size Class" == 137, 
                c("Plot", "Species", "Size Class")] 
  
  #ht10 <- data.frame("Height" = round(runif(nrow(sdl10), #creating random heights
  #                                          min = .01,
  #                                          max = .09),
  #                                    2))
  ht25 <- data.frame("Height" = round(runif(nrow(sdl25), 
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
  ht100 <- data.frame("Height" = round(runif(nrow(sdl100),
                                            min = .75,
                                            max = .99),
                                      2))
  ht137 <- data.frame("Height" = round(runif(nrow(sdl137),
                                            min = 1.0,
                                            max = 1.34),
                                      2))
  #sdl10 <- cbind(sdl10, ht10)
  sdl25 <- cbind(sdl25, ht25) #assigning created heights to correct seedlings 
  sdl50 <- cbind(sdl50, ht50)
  sdl75 <- cbind(sdl75, ht75)
  sdl100 <- cbind(sdl100, ht100)
  sdl137 <- cbind(sdl137, ht137)
  sdl <- rbind(sdl25, sdl50, sdl75, sdl100, sdl137) #binding rows on top of each other
  

  #create x and y coordinates for all seedlings in 3x3 grid
  sq1x <- data.frame("X" = round(runif(nrow(sdl), min = 0, max = 99.9), 2)) 
  sq1y <- data.frame("Y" = round(runif(nrow(sdl), min = 0, max = 99.9), 2))
  
  sq2x <- data.frame("X" = round(runif(nrow(sdl), min = 100, max = 199.9), 2))
  sq2y <- data.frame("Y" = round(runif(nrow(sdl), min = 100, max = 199.9), 2))
  
  sq3x <- data.frame("X" = round(runif(nrow(sdl), min = 200, max = 300), 2))
  sq3y <- data.frame("Y" = round(runif(nrow(sdl), min = 200, max = 300), 2))
  
  #Binding each x&y coordinate to seedling map
  sdl1.1 <- cbind(sdl, sq1x, sq1y); sdl2.1 <- cbind(sdl, sq2x, sq1y); sdl3.1 <- cbind(sdl, sq3x, sq1y);
  sdl1.2 <- cbind(sdl, sq1x, sq2y); sdl2.2 <- cbind(sdl, sq2x, sq2y); sdl3.2 <- cbind(sdl, sq3x, sq2y);
  sdl1.3 <- cbind(sdl, sq1x, sq3y); sdl2.3 <- cbind(sdl, sq2x, sq3y); sdl3.3 <- cbind(sdl, sq3x, sq3y);
  
  #Binding each of 9 plots on top of each other to create 3x3 grid
  sdl <- rbind(sdl1.1, sdl1.2, sdl1.3,
               sdl2.1, sdl2.2, sdl2.3,
               sdl3.1, sdl3.2, sdl3.3)
  
  ####Adding correct columns and column names for sortie#########
  diam <- data.frame("Diam" = sapply(0.02, rep, nrow(sdl))) #default diameter for seedlings
  type <- data.frame("Type" = replicate(nrow(sdl), "seedling")) #Add type
  paramg1 <- data.frame("ParamG1" = sapply(0, rep, nrow(sdl))) #Empty parameter for now - applies to heritability 
  sdl <- cbind(sdl, diam, type, paramg1)
  sdl <- sdl[, c("X", "Y", "Species", "Type", "Diam", "Height", "ParamG1")] #reorder
  colnames(sdl) <- c("X", "Y" ,"Species", "Type", "Diam", "Height", "ParamG1") #rename
  rownames(sdl) <- c() #remove row names
  
  # ###Adding column for size class############
  #  sdl <- cbind(sdl, data.frame("SizeClass" = sapply(0, rep, nrow(sdl))))
  # if(nrow(sdl[sdl$Height < .1, ]) > 0){
  #   one <- sdl[sdl$Height < .1, ]
  #   one$SizeClass <- 1
  # }else{
  #   one <- c()
  # }
  # if(nrow(sdl[sdl$Height > .1 && sdl$Height < .5, ]) > 0){
  #   two <- sdl[sdl$Height > .1 && sdl$Height < .5, ]
  #   two$SizeClass <- 2
  # } else{
  #   two <- c()
  # }
  # if(nrow(sdl[sdl$Height > .5, ]) > 0){
  #   three <- sdl[sdl$Height > .5, ]
  #   three$SizeClass <- 3
  # }else{
  #   three <- c()
  # }
  # 
  # 
  # sdl <- rbind(one, two, three)
  
  return(sdl)

}
##fixing size classes to be correct






#visual check
plot(SdlMap("BBBPIPO")$X, SdlMap("BBBPIPO")$Y)


##############Making final map and saving####################
###To create map for certain plot just type in MapSave("") with the plot you want in "", 
### into console and press enter. This function should generate a usable map for sortie.


MapSave <- function(plot){
  map <- rbind(TreeMap(plot), if(sum(viewSdlTbl[plot, ]) == 0){
    c()
  }else {
    SdlMap(plot)
  })
  write.table(map, paste0(plot, "_map.txt"), quote=F, sep="\t", row.names = F)
}

for(i in plot){
  MapSave(i)
}



























