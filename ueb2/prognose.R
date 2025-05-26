#------------------------------------------------------------------------
# Prognoseintrevall vs. Konfidenzintervall

# FH Wedel
# Ökonometrie
#------------------------------------------------------------------------

# Indivdiduellen Arbeitspfad wählen
# setwd("C:/Users/fbo/_FH Wedel/Vorlesungen/Master - Ökonometrie/Beispiel_Übungen/Ü1 - Bivariates Lineares Modell")


library(ggplot2)
library(readxl)

# Daten einlesen
Daten <- read_excel("Earnings_and_Height.xlsx")

# Oder für earnings ~ education  
Daten$x <- Daten$educ
Daten$y <- Daten$earnings

# Für earnings ~ age
Daten$x <- Daten$age
Daten$y <- Daten$earnings

# Oder für earnings ~ height  
Daten$x <- Daten$height
Daten$y <- Daten$earnings

ggplot(Daten, aes(x = x, y = y)) +
  geom_point(color = "darkblue", size=3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  theme_minimal()

# Lineares Modell schätzen
Modell <- lm(y ~ x, data = Daten)

# Neue x-Werte zum Vorhersagen (außerhalb des Bereichs)
new_data <- data.frame(x = seq(min(Daten$x) - 2, max(Daten$x) + 2, length.out = 200))

# Vorhersagen
pred_conf <- predict(Modell, newdata = new_data, interval = "confidence")
pred_pred <- predict(Modell, newdata = new_data, interval = "prediction")

# Zusammenfügen
plot_data <- data.frame(
  x = new_data$x,
  fit = pred_conf[, "fit"],
  lwr_conf = pred_conf[, "lwr"],
  upr_conf = pred_conf[, "upr"],
  lwr_pred = pred_pred[, "lwr"],
  upr_pred = pred_pred[, "upr"]
)

ggplot() +
  geom_point(data = Daten, aes(x = x, y = y), color = "darkblue", size = 2) +
  geom_line(data = plot_data, aes(x = x, y = fit), color = "darkred", size = 1.2) +
  geom_ribbon(data = plot_data, aes(x = x, ymin = lwr_pred, ymax = upr_pred), fill = "yellow", alpha = 0.2) +
  geom_ribbon(data = plot_data, aes(x = x, ymin = lwr_conf, ymax = upr_conf), fill = "lightblue", alpha = 0.3) +  labs(
    title = "Konfidenz- vs Prognoseintervall",
    subtitle = "Blau: Konfidenzintervall (Mittelwert) / Grau: Prognoseintervall (Einzelwert)",
    x = "x",
    y = "y"
  ) +
  theme_minimal()