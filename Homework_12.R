# ------------------------------------------
# BIOL 381 HW 12: Advanced ggplotting
# 16 Apr 2020
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

# Infested
INF <- data[data$STATUS == "INF",]

# Uninfested
UN <- data[data$STATUS == "UN",]

# HWMX forest type
HWMX <- data[data$COVER == "HWMX",]

# HWMX forest type
HWMX <- data[data$COVER =="HWMX",]




# Summary data

# basal area / ha
mean_ba <- mean(data$BA_HA)
mean_ba_EJ <- mean(EJ$BA_HA)
mean_ba_MH <- mean(MH$BA_HA)

# trees / ha
mean_tpha <- mean(data$TPHA)
mean_tpha_EJ <- mean(EJ$TPHA)
mean_tpha_MH <- mean(MH$TPHA)

# proportion PIRI
mean_piri <- mean(data$PROP_PIRI)
mean_piri_EJ <- mean(EJ$PROP_PIRI)
mean_piri_MH <- mean(MH$PROP_PIRI)

# stand metrics
mean_sdi <- mean(EJ$SDI)
mean_rd <- mean(EJ$REL_DEN)

summary_data <- data.frame(mean_ba, mean_tpha, mean_piri)

###############################################################


# BOX PLOTS ---------------------------------------------------------------

# basal area per ha: all stands

ba_boxplot <- ggplot(data = data, aes(x = STATUS, y = BA_HA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Basal area per ha of infested and uninfested stands", 
       x = "Infestation status", 
       y = "Basal area per ha (m^2/ha)",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 3))
  
print(ba_boxplot) 

# trees per ha: all stands
tpha_boxplot <- ggplot(data = data, aes(x = STATUS, y = TPHA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Trees per ha of infested and uninfested stands",
       x = "Infestation status", 
       y = "Trees per ha",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 3))

print(tpha_boxplot) 


# proportion pitch HWMX: all stands
piri_boxplot <- ggplot(data = data, aes(x = STATUS, y = BA_HA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Proportion pitch HWMX of infested and uninfested stands",
       x = "Infestation status", 
       y = "Proportion pitch HWMX",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 3))

print(piri_boxplot)

# QMD: all data
QMD_boxplot <- ggplot(data = data, aes(x = STATUS, y = QMD)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "QMD of infested and uninfested stands",
       x = "Infestation status", 
       y = "Quadratic mean diameter (cm)",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 3))

print(QMD_boxplot)

# SDI: EJ, 2019
SDI_boxplot <- ggplot(data = EJ, aes(x = STATUS, y = SDI)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "SDI of infested and uninfested stands",
       x = "Infestation status", 
       y = "Stand density index",
       caption = "Data: EJ, 2019") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 2))

print(SDI_boxplot) 

# relative density: EJ, 2019
RD_boxplot <- ggplot(data = EJ, aes(x = STATUS, y = REL_DEN)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Relative density of infested and uninfested stands",
       x = "Infestation status", 
       y = "Relative density",
       caption = "Data: EJ, 2019") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 2))

print(RD_boxplot) 

###############################################################


# Patchwork ---------------------------------------------------------------


###############################################################

# SDI and RELATIVE DENSITY

# plots for patchwork -----------------------------------------------------

# SDI: EJ, 2019
SDI_boxplot_PW <- ggplot(data = EJ, aes(x = STATUS, y = SDI)) +
  geom_boxplot(outlier.colour = "red") +
  labs(#title = "SDI of infested and uninfested stands",
       x = "Infestation status", 
       y = "Stand density index",
       caption = "Data: EJ, 2019") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 2))

print(SDI_boxplot_PW) 

# relative density: EJ, 2019
RD_boxplot_PW <- ggplot(data = EJ, aes(x = STATUS, y = REL_DEN)) +
  geom_boxplot(outlier.colour = "red") +
  labs(#title = "Relative density of infested and uninfested stands",
       x = "Infestation status", 
       y = "Relative density",
       caption = "Data: EJ, 2019") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(vjust = 2))

print(RD_boxplot_PW)

# SDI + relative density --------------------------------------------------

SDI_RD_boxplots <- SDI_boxplot_PW + RD_boxplot_PW + 
  plot_annotation(title = "SDI and relative density of infested and uninfested stands", 
                  theme = theme(plot.title = element_text(hjust = 0.5)))

print(SDI_RD_boxplots)

###############################################################

# BASAL AREA, TPHA, PROPORTION HWMX, AND QMD

# plots for patchwork -----------------------------------------------------

# basal area per ha: all stands

ba_boxplot_PW <- ggplot(data = data, aes(x = STATUS, y = BA_HA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Basal area per ha", 
       x = "Infestation status", 
       y = "Basal area per ha (m^2/ha)",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(vjust = 3))

print(ba_boxplot_PW) 

# trees per ha: all stands
tpha_boxplot_PW <- ggplot(data = data, aes(x = STATUS, y = TPHA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Trees per ha",
       x = "Infestation status", 
       y = "Trees per ha",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
       axis.title.y = element_text(vjust = 3))

print(tpha_boxplot_PW) 


# proportion pitch HWMX: all stands
piri_boxplot_PW <- ggplot(data = data, aes(x = STATUS, y = BA_HA)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "Proportion pitch HWMX",
       x = "Infestation status", 
       y = "Proportion pitch HWMX",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(vjust = 3))

print(piri_boxplot_PW)

# QMD: all data
QMD_boxplot_PW <- ggplot(data = data, aes(x = STATUS, y = QMD)) +
  geom_boxplot(outlier.colour = "red") +
  labs(title = "QMD",
       x = "Infestation status", 
       y = "Quadratic mean diameter (cm)",
       caption = "Data: EJ, 2019 and MH, 2016") +
  scale_x_discrete(labels=c("INF" = "Infested", "UN" = "Uninfested")) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(vjust = 3))

print(QMD_boxplot_PW)

# basal area + tpha + proportion HWMX + QMD
(ba_boxplot_PW + tpha_boxplot_PW) / (QMD_boxplot_PW + piri_boxplot_PW)+
  plot_annotation(title = "Stand metrics of infested and uninfested stands", 
                  theme = theme(plot.title = element_text(size = 13, hjust = 0.5)))

###############################################################

# SCATTER PLOTS -----------------------------------------------------------

# Status ------------------------------------------------------------------

# basal area and TPHA

ba_tpha_scatterplot <- ggplot(data = data, mapping = aes(x = TPHA, y = BA_HA, color = STATUS)) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  labs(title = "Trees per ha and basal area for uninfested and infested stands",
       subtitle = "Data: EJ, 2019 and MH, 2016",
       x = "Trees per ha",
       y = "Basal area per ha",
       color = "Infestation status") +
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 3)) +
  scale_color_manual(labels = c("Infested", "Uninfested"), values = c("tomato2", "forestgreen"))

print(ba_tpha_scatterplot)

# QMD and TPHA

QMD_tpha_scatterplot <- ggplot(data = data, mapping = aes(x = TPHA, y = QMD, color = STATUS)) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  labs(title = "Trees per ha and QMD for uninfested and infested stands",
       subtitle = "Data: EJ, 2019 and MH, 2016",
       x = "Trees per ha",
       y = "Quadratic mean diameter (cm)",
       color = "Infestation status") +
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 3)) +
  scale_color_manual(labels = c("Infested", "Uninfested"), values = c("tomato2", "forestgreen"))

print(QMD_tpha_scatterplot)


# COVER -------------------------------------------------------------------

# Proportion HWMX and trees per ha
tpha_piri_scatterplot <- ggplot(data = data, mapping = aes(x = TPHA, y = PROP_PIRI, color = COVER)) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  labs(title = "Trees per ha and proportion pitch HWMX by stand classification",
       subtitle = "Data: EJ, 2019 and MH, 2016",
       x = "Trees per ha",
       y = "Proportion pitch HWMX",
       color = "Stand classification") +
  theme_bw(base_size = 10) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 3)) +
  scale_color_manual(labels = c("Oak-pine", "Pine"), values = c("mediumorchid", "seagreen"))

print(tpha_piri_scatterplot)

# QMD by tpha
tpha_QMD_scatterplot <- ggplot(data = data, mapping = aes(x = TPHA, y = QMD, color = COVER)) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  labs(title = "Trees per ha and QMD by stand classification",
       subtitle = "Data: EJ, 2019 and MH, 2016",
       x = "Trees per ha",
       y = "Quadratic mean diameter (cm)",
       color = "Stand classification") +
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 3)) +
  scale_color_manual(labels = c("Oak-Pine", "Pine"), values = c("mediumorchid", "seagreen"))

print(tpha_QMD_scatterplot)

# QMD by basal area
ba_QMD_scatterplot <- ggplot(data = data, mapping = aes(x = BA_HA, y = QMD, color = COVER)) +
  geom_point(size = 3) + 
  geom_smooth(method = "lm") +
  labs(title = "Basal area and QMD by stand classification",
       subtitle = "Data: EJ, 2019 and MH, 2016",
       x = "Basal area (m^2/ha",
       y = "Quadratic mean diameter (cm)",
       color = "Stand classification") +
  theme_bw(base_size = 12) + 
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12, vjust = 3)) +
  scale_color_manual(labels = c("Oak-Pine", "Pine"), values = c("mediumorchid", "seagreen"))

print(ba_QMD_scatterplot)

###############################################################

# Box Plots: STATUS and COVER ----------------------------------------------

box_plot_colors <- c("lightcoral", "palegreen2")
box_plot_colors2 <- c("lightcoral", "palegreen2", "lightcoral", "palegreen2")
show_col(box_plot_colors)

# Basal area

ba_status_cover <- ggplot(data = data, aes(x = COVER, y = BA_HA, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2, 
               outlier.shape = "square") +
  geom_point(color = "grey30", 
            position = position_jitter(width = 0.1, height = 0),
            size = 2) +
  labs(title = "Basal area by stand classification and infestation status",
       x = "Stand classification",
       y = "Basal area per ha (m2/ha)",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  #theme(axis.text.x = element_text(size = 12),
  #      axis.text.y = element_text(size = 12),
  #      axis.title.y = element_text(size = 12),
  #      axis.title.y = element_text(size = 12))


print(ba_status_cover)

# Trees per ha

tpha_status_cover <- ggplot(data = data, aes(x = COVER, y = TPHA, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2, 
               outlier.shape = "square") +
  geom_point(color = "grey30", 
             position = position_jitter(width = 0.1, height = 0),
             size = 2) +
  labs(title = "Trees per ha by stand classification and infestation status",
       x = "Stand classification",
       y = "Trees per ha",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(tpha_status_cover)

# QMD

QMD_status_cover <- ggplot(data = data, aes(x = COVER, y = QMD, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2, 
               outlier.shape = "square") +
  geom_point(color = "grey30", 
             position = position_jitter(width = 0.1, height = 0),
             size = 2) +
  labs(title = "Trees per ha by stand classification and infestation status",
       x = "Stand classification",
       y = "QMD",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(QMD_status_cover)

# relative density

RD_status_cover <- ggplot(data = EJ, aes(x = COVER, y = REL_DEN, by = STATUS, fill = STATUS)) +
  geom_boxplot(outlier.color =  "black", 
               outlier.size = 2, 
               outlier.shape = "square") +
  geom_point(color = "grey30", 
             position = position_jitter(width = 0.1, height = 0),
             size = 2) +
  labs(title = "Relative density by stand classification and infestation status",
       x = "Stand classification",
       y = "Relative density",
       fill = "Infestation status") +
  scale_x_discrete(labels=c("HWMX" = "Oak-pine", "PINE" = "Pine")) +
  scale_fill_manual(labels = c("Infested", "Uninfested"), values = box_plot_colors) +
  theme_bw(base_size = 13) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#theme(axis.text.x = element_text(size = 12),
#      axis.text.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12),
#      axis.title.y = element_text(size = 12))


print(RD_status_cover)



