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
