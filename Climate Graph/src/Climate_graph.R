rm(list = ls())

#############BBBPIPO#######################
BBCurrent <- read.table("BBBPIPO_Current.txt", sep = "\t")
BBCNRM <- read.table("BBBPIPO_CNRM.txt", sep = "\t")
BBCCSM <- read.table("BBBPIPO_CCSM.txt", sep = "\t")
BBMIROC <- read.table("BBBPIPO_MIROC.txt", sep = "\t")

Steps <- seq(2007, 2099, 1)
### All climates together
library(ggplot2)
library(gtable)
library(gridExtra)
library(cowplot)
library(scales)
library(grid)
library(ggpubr)

CCSM4 <- "#A6611A"
CNRM <- "black"
MIROC <- "#80CDC1"
CNTRL <- "#018571"

BBBPIPO.dt <- data.frame("Year" = Steps, "Current JanMin" = BBCurrent$January.Minimum, "Current ppt" = BBCurrent$Precipitation, "Current JulMax" = BBCurrent$July.Maximum.Temperature, 
                         "CCSM JanMin" = BBCCSM$January.Minimum, "CCSM ppt" = BBCCSM$Precipitation, "CCSM JulMax" = BBCCSM$July.Maximum.Temperature,
                         "MIROC JanMin" = BBMIROC$January.Minimum, "MIROC ppt" = BBMIROC$Precipitation, "MIROC JulMax" = BBMIROC$July.Maximum.Temperature,
                         "CNRM JanMin" = BBCNRM$January.Minimum, "CNRM ppt" = BBCNRM$Precipitation, "CNRM JulMax" = BBCNRM$July.Maximum.Temperature)

BB_JanMin.plot <- ggplot(BBBPIPO.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.JanMin, colour = "Current")) + 
  geom_line(aes(y = CCSM.JanMin, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JanMin, colour = "MIROC")) +
  geom_line(aes(y = CNRM.JanMin, colour = "CNRM")) + 
  ylab("Jan Min Temp") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))
 
BB_ppt.plot <- ggplot(BBBPIPO.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.ppt, colour = "Current")) + 
  geom_line(aes(y = CCSM.ppt, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.ppt, colour = "MIROC")) +
  geom_line(aes(y = CNRM.ppt, colour = "CNRM")) + 
  ylab("Precipitation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(0, 3100)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))

BB_JulMax.plot <- ggplot(BBBPIPO.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.JulMax, colour = "Current")) + 
  geom_line(aes(y = CCSM.JulMax, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JulMax, colour = "MIROC")) +
  geom_line(aes(y = CNRM.JulMax, colour = "CNRM")) + 
  ylab("Jul Max Temp") + ggtitle("BBBPIPO") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(17.5, 40)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))


BBBPIPO_graph <- arrangeGrob(BB_JulMax.plot, BB_ppt.plot, nrow = 1, ncol = 2) 


#ggsave("BBBPIPOClimateGraph.png", BBBPIPO_graph, height = 5, width = 5, scale = 2)
#ggsave("BBBJanMin.png", BB_JanMin.plot, height = 5, width = 5, scale = 2)
#ggsave("BBBPpt.png", BB_ppt.plot, height = 5, width = 5, scale = 2)
#ggsave("BBBJulMax.png", BB_JulMax.plot, height = 5, width = 5, scale = 2)

##############POFLABMA##################

POCurrent <- read.table("POFLABMACurrent.txt", sep = "\t")
POCNRM <- read.table("POFLABMA_CNRM.txt", sep = "\t")
POCCSM <- read.table("POFLABMA_CCSM.txt", sep = "\t")
POMIROC <- read.table("POFLABMA_MIROC.txt", sep = "\t")

### All climates together

POFLABMA.dt <- data.frame("Year" = Steps, "Current JanMin" = POCurrent$January.Minimum, "Current ppt" = POCurrent$Precipitation, "Current JulMax" = POCurrent$July.Maximum.Temperature, 
                         "CCSM JanMin" = POCCSM$January.Minimum, "CCSM ppt" = POCCSM$Precipitation, "CCSM JulMax" = POCCSM$July.Maximum.Temperature,
                         "MIROC JanMin" = POMIROC$January.Minimum, "MIROC ppt" = POMIROC$Precipitation, "MIROC JulMax" = POMIROC$July.Maximum.Temperature,
                         "CNRM JanMin" = POCNRM$January.Minimum, "CNRM ppt" = POCNRM$Precipitation, "CNRM JulMax" = POCNRM$July.Maximum.Temperature)

PO_JanMin.plot <- ggplot(POFLABMA.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.JanMin, colour = "Current")) + 
  geom_line(aes(y = CCSM.JanMin, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JanMin, colour = "MIROC")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))

PO_ppt.plot <- ggplot(POFLABMA.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.ppt, colour = "Current")) + 
  geom_line(aes(y = CCSM.ppt, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.ppt, colour = "MIROC")) +
  geom_line(aes(y = CNRM.ppt, colour = "CNRM")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(0, 3100)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))

PO_JulMax.plot <- ggplot(POFLABMA.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.JulMax, colour = "Current")) + 
  geom_line(aes(y = CCSM.JulMax, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JulMax, colour = "MIROC")) +
  geom_line(aes(y = CNRM.JulMax, colour = "CNRM")) + 
  ggtitle("POFLABMA") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(17.5, 40)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))

POFLABMA_graph <- arrangeGrob(PO_JulMax.plot, PO_ppt.plot, nrow = 1, ncol = 2)
#ggsave("POFLABMAClimateGraph.png", POFLABMA_graph, height = 5, width = 5, scale = 2)
#ggsave("POJanMin.png", PO_JanMin.plot, height = 2, width = 5, scale = 2)
#ggsave("POJulMax.png", PO_JulMax.plot, height = 2, width = 5, scale = 2)
#ggsave("POPpt.png", PO_ppt.plot, height = 2, width = 5, scale = 2)


#################UPLOG######################


UPCurrent <- read.table("UPLOGCurrent.txt", sep = "\t")
UPCNRM <- read.table("UPLOG_CNRM.txt", sep = "\t")
UPCCSM <- read.table("UPLOG_CCSM.txt", sep = "\t")
UPMIROC <- read.table("UPLOG_MIROC.txt", sep = "\t")

### All climates together

UPLOG.dt <- data.frame("Year" = Steps, "Current JanMin" = UPCurrent$January.Minimum, "Current ppt" = UPCurrent$Precipitation, "Current JulMax" = UPCurrent$July.Maximum.Temperature, 
                         "CCSM JanMin" = UPCCSM$January.Minimum, "CCSM ppt" = UPCCSM$Precipitation, "CCSM JulMax" = UPCCSM$July.Maximum.Temperature,
                         "MIROC JanMin" = UPMIROC$January.Minimum, "MIROC ppt" = UPMIROC$Precipitation, "MIROC JulMax" = UPMIROC$July.Maximum.Temperature,
                         "CNRM JanMin" = UPCNRM$January.Minimum, "CNRM ppt" = UPCNRM$Precipitation, "CNRM JulMax" = UPCNRM$July.Maximum.Temperature)

UP_JanMin.plot <- ggplot(UPLOG.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.JanMin, colour = "Current")) + 
  geom_line(aes(y = CCSM.JanMin, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JanMin, colour = "MIROC")) +
  geom_line(aes(y = CNRM.JanMin, colour = "CNRM")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))
  

UP_ppt.plot <- ggplot(UPLOG.dt, aes(x = Year, fill = "Climate Sequence")) + 
  geom_line(aes(y = Current.ppt, colour = "Current")) + 
  geom_line(aes(y = CCSM.ppt, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.ppt, colour = "MIROC")) +
  geom_line(aes(y = CNRM.ppt, colour = "CNRM")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(0, 3100)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))


UP_JulMax.plot <- ggplot(UPLOG.dt, aes(x = Year, fill = "Climate Sequence")) +
  geom_line(aes(y = Current.JulMax, colour = "Current")) + 
  geom_line(aes(y = CCSM.JulMax, colour = "CCSM")) + 
  geom_line(aes(y = MIROC.JulMax, colour = "MIROC")) +
  geom_line(aes(y = CNRM.JulMax, colour = "CNRM")) +
  ggtitle("UPLOG") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6),
                     limits = c(2000, 2099)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(17.5, 40)) + 
  scale_color_manual(name = "Sequence",
                     values = c(CCSM4, CNRM, MIROC, CNTRL))

UPLOG_graph <- arrangeGrob(UP_JulMax.plot, UP_ppt.plot, nrow = 1, ncol = 2)
#ggsave("UPLOGClimateGraph.png", UPLOG_graph, height = 5, width = 5, scale = 2)
#ggsave("UPJanMin.png", UP_JanMin.plot, height = 2, width = 5, scale = 2)
#ggsave("UPJulMax.png", UP_JulMax.plot, height = 2, width = 5, scale = 2)
#ggsave("UPPpt.png", UP_ppt.plot, height = 2, width = 5, scale = 2)

###########Graphs for Paper###############


allgraph <- ggarrange(BB_JulMax.plot, UP_JulMax.plot, PO_JulMax.plot,
                      BB_ppt.plot, UP_ppt.plot, PO_ppt.plot,
                      BB_JanMin.plot, UP_JanMin.plot, PO_JanMin.plot,
                      nrow = 3, ncol = 3, common.legend = T, legend = "bottom")

allgraph
ggsave("SforSgraph.png", height = 10, width = 13, dpi = 500, scale = 0.6, type = "cairo")
dev.off




