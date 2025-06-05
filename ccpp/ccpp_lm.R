#------------------------------------------------------------------------
# Deskriptive Datenanalyse 
#
# Gruppe E
# FH Wedel
# Ökonometrie
#------------------------------------------------------------------------


library(readxl)
library(ggplot2)

#---------------------------------------------------------------
# Daten einlesen; Kennzahlen
#--------------------------------------------------------------- 

# Daten einlesen
Daten <- read_excel("ccpp_data.xlsx")

#Erster Überblick
View(Daten)
summary(Daten)

#---------------------------------------------------------------
# Plot Ursprungsdaten
#--------------------------------------------------------------- 
ggplot(Daten, aes(x = AT, y = PE)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(
    title = "Stromprouktion in Abhängigkeit von der Temperatur",
    x = "Temperatur",
    y = "Stromproduktion"
  ) +
  theme_minimal()

ggplot(Daten, aes(x = V, y = PE)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(
    title = "Stromproduktion in Abhängigkeit vom Abluftvakuum",
    x = "Abluftvakuum",
    y = "Stromproduktion"
  ) +
  theme_minimal()

ggplot(Daten, aes(x = AP, y = PE)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(
    title = "Stromproduktion in Abhängigkeit vom Umgebungsdruck",
    x = "Umgebungsdruck",
    y = "Stromproduktion"
  ) +
  theme_minimal()

ggplot(Daten, aes(x = RH, y = PE)) +
  geom_point(color = "darkblue", alpha = 0.4) +
  labs(
    title = "Stromproduktion in Abhängigkeit von der relativen Luftfeuchtigkeit",
    x = "Relative Luftfeuchtigkeit",
    y = "Stromproduktion"
  ) +
  theme_minimal()

#---------------------------------------------------------------
#Plot Histogramme
#--------------------------------------------------------------- 
library(scales) # Für deutsche Zahlenformate

Histo_Temperatur <- 
  ggplot(Daten, aes(x = AT )) +  
  geom_histogram(fill = "blue", color = "black") + 
  labs(title = "Abhängige Variable: \nTemperatur", 
       x = "Temperatur", 
       y = "Häufigkeit")+  
  scale_x_continuous(breaks = seq(0, max(Daten$AT), by = 50), labels = comma_format(big.mark = ".",decimal.mark = ","))+
  geom_vline(aes(xintercept = mean(AT)), 
             color = "red", 
             linetype=1,
             linewidth = 1)+
  annotate("text", x = mean(Daten$AT), y = 15, label = "Mittelwert", 
           angle = 90, vjust = 1.2, colour = "red", size = 3.5) +
  theme(plot.title = element_text(size = 10, face = "bold")) + # Titel; Schriftgröße: 14, Fettdruck
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))

Histo_Abluftvakuum <- 
  ggplot(Daten, aes(x = V )) +  
  geom_histogram(fill = "lightgreen", color = "black") + 
  labs(title = "unahängige Variable: \nAbluftvakuum", 
       x = "Abluftvakuum", 
       y = "Häufigkeit")+  
  scale_x_continuous(breaks = seq(0, max(Daten$V), by = 5), labels = comma_format(big.mark = ".",decimal.mark = ","))+
  geom_vline(aes(xintercept = mean(V)), 
             color = "darkred", 
             linetype=1,
             linewidth = 1)+
  annotate("text", x = mean(Daten$V), y = 8, label = "Mittelwert", 
           angle = 90, vjust = 1.2, colour = "darkred", size = 3.5) +
  theme(plot.title = element_text(size = 10, face = "bold")) + # Titel; Schriftgröße: 14, Fettdruck
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))

Histo_Umgebungsdruck <- 
  ggplot(Daten, aes(x = AP )) +  
  geom_histogram(fill = "lightgray", color = "black", binwidth=5) + 
  labs(title = "unahängige Variable: \nUmgebungsdruck", 
       x = "Umgebungsdruck", 
       y = "Häufigkeit")+  
  scale_x_continuous(breaks = seq(0, max(Daten$AP), by = 25), labels = comma_format(big.mark = ".",decimal.mark = ","))+
  geom_vline(aes(xintercept = mean(AP)), 
             color = "darkred", 
             linetype=1,
             linewidth = 1)+
  annotate("text", x = mean(Daten$AP), y = 8, label = "Mittelwert", 
           angle = 90, vjust = 1.2, colour = "darkred", size = 3.5) +
  theme(plot.title = element_text(size = 10, face = "bold")) + # Titel; Schriftgröße: 14, Fettdruck
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))

Histo_Luftfeuchtigkeit <- 
  ggplot(Daten, aes(x = RH )) +  
  geom_histogram(fill = "orange", color = "black", binwidth=5) + 
  labs(title = "unahängige Variable: \nLuftfeuchtigkeit", 
       x = "Leuftfeuchtigkeit", 
       y = "Häufigkeit")+  
  scale_x_continuous(breaks = seq(0, max(Daten$RH), by = 25), labels = comma_format(big.mark = ".",decimal.mark = ","))+
  geom_vline(aes(xintercept = mean(RH)), 
             color = "darkred", 
             linetype=1,
             linewidth = 1)+
  annotate("text", x = mean(Daten$RH), y = 8, label = "Mittelwert", 
           angle = 90, vjust = 1.2, colour = "darkred", size = 3.5) +
  theme(plot.title = element_text(size = 10, face = "bold")) + # Titel; Schriftgröße: 14, Fettdruck
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))

Histo_Stromproduktion <- 
  ggplot(Daten, aes(x = PE )) +  
  geom_histogram(fill = "orange", color = "black", binwidth=5) + 
  labs(title = "unahängige Variable: \nStromproduktion", 
       x = "Stromproduktion", 
       y = "Häufigkeit")+  
  scale_x_continuous(breaks = seq(0, max(Daten$PE), by = 25), labels = comma_format(big.mark = ".",decimal.mark = ","))+
  geom_vline(aes(xintercept = mean(PE)), 
             color = "darkred", 
             linetype=1,
             linewidth = 1)+
  annotate("text", x = mean(Daten$PE), y = 8, label = "Mittelwert", 
           angle = 90, vjust = 1.2, colour = "darkred", size = 3.5) +
  theme(plot.title = element_text(size = 10, face = "bold")) + # Titel; Schriftgröße: 14, Fettdruck
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8))

#4 Create panel out of histograms
library(multipanelfigure) # mehrere Bildchen in einem Raster anordnen

Histogramme <- multi_panel_figure(rows = 3,
                                  columns = 2,
                                  panel_label_type = "none")

# wo soll welches Bild gezeigt werden?
Histogramme %<>%
  fill_panel(Histo_Temperatur, row = 1, column = 1) %<>%
  fill_panel(Histo_Umgebungsdruck,           row = 1, column = 2) %<>%
  fill_panel(Histo_Luftfeuchtigkeit,        row = 2, column = 1) %<>%
  fill_panel(Histo_Abluftvakuum,        row = 2, column = 2) %<>%
  fill_panel(Histo_Stromproduktion,        row = 3, column = 1)
print(Histogramme)

#---------------------------------------------------------------
#Boxplots
#---------------------------------------------------------------
boxplot_Stromproduktion <-
  ggplot(Daten, aes(x = PE )) +  
  geom_boxplot(fill="green")+
  stat_boxplot(geom="errorbar")+
  labs(title="Stromproduktion")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

boxplot_Temperatur <-
  ggplot(Daten, aes(x = AT )) +  
  geom_boxplot(fill="lightblue")+
  stat_boxplot(geom="errorbar")+
  labs(title="Temperatur")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

boxplot_Abluftvakuum <-
  ggplot(Daten, aes(x = V )) +  
  geom_boxplot(fill="lightblue")+
  stat_boxplot(geom="errorbar")+
  labs(title="Abluftvakuum")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

boxplot_Umgebungsdruck <-
  ggplot(Daten, aes(x = AP )) +  
  geom_boxplot(fill="lightblue")+
  stat_boxplot(geom="errorbar")+
  labs(title="Umgebungsdruck")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

boxplot_Luftfeuchtigkeit <-
  ggplot(Daten, aes(x = RH )) +  
  geom_boxplot(fill="lightblue")+
  stat_boxplot(geom="errorbar")+
  labs(title="Luftfeuchtigkeit")+
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()


Boxplots <- multi_panel_figure(rows = 3,
                               columns = 2,
                               panel_label_type = "none")

# wo soll welches Bild gezeigt werden?
Boxplots %<>%
  fill_panel(boxplot_Stromproduktion, row = 1, column = 1) %<>%
  fill_panel(boxplot_Temperatur,  row = 1, column = 2) %<>%
  fill_panel(boxplot_Abluftvakuum, row = 2, column = 1) %<>%
  fill_panel(boxplot_Umgebungsdruck, row = 2, column = 2) %<>%
  fill_panel(boxplot_Luftfeuchtigkeit, row = 3, column = 1) 

Boxplots

#---------------------------------------------------------------
#Korrelationen
#------------------------------------------------------------
library(vcd) #für Heatmaps und mosaic
library(tidycomm) #Korrelationen
library(corrplot) # Korrelationen


#Nur die Daten extrahieren, über die Korrelationen gerechnet werden sollen
Daten_subset = subset(Daten, 
                      select = c(PE, AT, V, AP, RH))

#Schnell mal die Korrelationsmatrix anschauen
Daten_subset%>% 
  correlate() %>% 
  to_correlation_matrix()

# Visualisieren der Korrelationen von mehreren Variablen
Daten_subset %>% 
  correlate() %>%   
  visualize()

#Heatmap der Korrelatonen: via ggplot2

# 1. Berechnung der Korrelationsmatrix
cor_matrix <- cor(Daten_subset)

#Heatmap der Korrelatonen: via corrplot
corrplot(cor_matrix, 
         method = "square",
         col = colorRampPalette(c("darkblue", "white", "darkred"))(50),
         addCoef.col = "black",  # Korrelationskoeffizienten anzeigen
         cl.pos = "r",  
         cl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,  
         title = "Heatmap der Korrelationen", 
         tl.cex = 0.8,  # Textgröße für Achsenbeschriftungen
         number.cex = 1  # Textgröße für Korrelationen
)












