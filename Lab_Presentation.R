# ------------------------------------------
# Figures for Lab Group Presentation 5/8/20
# 04 May 2020
# EKJ
# ------------------------------------------
#

# Preliminaries -----------------------------------------------------------

# LOAD LIBRARIES
library(ggplot2)
require(ggplot2)
library(patchwork)
require(patchwork)
library(scales)
require(scales)

# LOAD DATA
data <- read.csv("StandData_LongIsland.csv")
print(data)

# GROUP DATA
# EJ, 2019
EJ <- data[data$DATA == "EJ",]

# MH, 2016
MH <- data[data$DATA == "MH",]

# Box Plots: STATUS and COVER ----------------------------------------------

box_plot_colors <- c("lightcoral", "palegreen2")
box_plot_colors2 <- c("lightcoral", "palegreen2", "lightcoral", "palegreen2")
show_col(box_plot_colors)

# Basal area

ba_status_cover <- ggplot(data = data, aes(x = COVER, y = BA_HA, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2) + 
               #outlier.shape = "square") +
  #geom_point(color = "grey30", 
  #position = position_jitter(width = 0.1, height = 0),
  #size = 2) +
  labs(title = "Basal area by stand type and infestation status",
       x = "Stand type",
       y = "Basal area per ha (m2/ha)",
       caption = "Data: EJ, 2019 and MH, 2016",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.y = element_text(vjust = 3))
#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(ba_status_cover)

# Trees per ha

tpha_status_cover <- ggplot(data = data, aes(x = COVER, y = TPHA, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2) + 
               #outlier.shape = "square") +
  #geom_point(color = "grey30", 
             #position = position_jitter(width = 0.1, height = 0),
             #size = 2) +
  labs(title = "Trees per ha by stand type and infestation status",
       x = "Stand type",
       y = "Trees per ha",
       caption = "Data: EJ, 2019 and MH, 2016",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 3))
#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(tpha_status_cover)

# QMD

QMD_status_cover <- ggplot(data = data, aes(x = COVER, y = QMD, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2) + 
               #outlier.shape = "square") +
  #geom_point(color = "grey30", 
             #position = position_jitter(width = 0.1, height = 0),
             #size = 2) +
  labs(title = "QMD by stand classification and infestation status",
       x = "Stand type",
       y = "QMD (cm)",
       caption = "Data: EJ, 2019 and MH, 2016",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 3))
#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(QMD_status_cover)

# relative density

RD_status_cover <- ggplot(data = EJ, aes(x = COVER, y = REL_DEN, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2) +
               #outlier.shape = "square") +
  #geom_point(color = "grey30", 
             #position = position_jitter(width = 0.1, height = 0),
             #size = 2) +
  labs(title = "Relative density by stand classification and infestation status",
       x = "Stand type",
       y = "Relative density",
       caption = "Data: EJ, 2019 and MH, 2016",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(vjust = 3))

#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(RD_status_cover)

###############################################################

# PATCH WORK

# FIGURE 1

patch1 <- ba_status_cover + tpha_status_cover + 
  plot_annotation(title = "Density of Long Island Stands",
                  subtitle = "Data: EJ, 2019 and MH, 2016",
                  theme = theme(plot.title = element_text(hjust = 0.5),
                                plot.subtitle = element_text(hjust = 0.5, vjust = -1)))

print(patch1)

###############################################################

# Diameter distributions --------------------------------------------------

# Tree diameters


data$diameterbins <- cut(DiameterDist$DBH, c(0, 5, 10, 15, 20, 25, 30,34), 
                                 labels = c("1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34"))

require(ggplot2)

#Colors for plot

cols<- c("OH" = "gray20", "RM" = "salmon", "SM" = "darkblue", "PB" = "yellow", 
         "HI" = "thistle1", "AB" = "darkorange", "WA" = "gray45", 
         "WP" = "purple", "BT" = "lightskyblue", "RO" = "red", "EH" = "green4")


#plot- TPA 
ggplot(DiameterDist, aes(x = diameterbins, y = TPA, fill = Species)) + #x is diameter class, y is trees per hectare (for me), fill is species column
  geom_bar(stat = "identity")+ #stat has to equal identity for this to work
  scale_fill_manual(values = cols)+ #I linked my own colors to each species for consistency, see below
  scale_x_discrete(limits = c("5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34"))+ #if I don't do this, then it will skip empty categories
  xlab("Diameter Class (DBH in inches)")+
  ylab("TPA (number of trees/acre)")+
  ggtitle("TPA Diameter Distribution")+
  theme_bw()

spord=c("RO","SM","AB","EH","WP","RM","BT","HI","OH","WA","PB")


#plot-BA
ggplot(DiameterDist, aes(x = diameterbins, y = Basal.Area, fill = factor(Species, levels=spord))) + #x is diameter class, y is trees per hectare (for me), fill is species column
  geom_bar(stat = "identity")+ #stat has to equal identity for this to work
  scale_fill_manual(values = cols, name="")+ #I linked my own colors to each species for consistency, see below
  scale_x_discrete(limits = c("1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34"))+ #if I don't do this, then it will skip empty categories
  xlab("Diameter Class (DBH in inches)")+
  ylab("Basal Area (ft^2/acre)")+
  ggtitle("Basal Area Diameter Distribution")+
  #scale_fill_discrete(name="legend")
  theme_bw()                 


