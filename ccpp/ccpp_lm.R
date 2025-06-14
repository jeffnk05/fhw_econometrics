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

#------------------------------------------------------------------------
# Schätzung des Modells mit Funktion lm()
#------------------------------------------------------------------------

Modell= lm(PE ~ AT + V + AP + RH, data = Daten)

sm = summary(Modell)
print(sm)

#------------------------------------------------------------------------
# Bessere Darstellungen der Summary
#------------------------------------------------------------------------
library(gt)

# Koefﬁzienten-Tabelle vorbereiten
coef_df <- as.data.frame(sm$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df$Interpretation <- ifelse(coef_df$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")

# Spaltenreihenfolge anpassen
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]

# gt-Tabelle erzeugen
gt(coef_df) %>%
  tab_header(title = "Regressionskoeffizienten") %>%
  cols_label(
    Variable = "Variable",
    Estimate = "Schätzwert",
    `Std. Error` = "Standardfehler",
    `t value` = "t-Wert",
    `Pr(>|t|)` = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`),
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )


# Einzelwerte berechnen
r2 <- sm$r.squared
adj_r2 <- sm$adj.r.squared
sigma <- sm$sigma
df_model <- sm$df[1]
df_resid <- sm$df[2]
fstat <- sm$fstatistic
f_p <- 1 - pf(fstat["value"], df1 = fstat["numdf"], df2 = fstat["dendf"])

# DataFrame für Zusammenfassung
info_df <- data.frame(
  Kennzahl = c("R²", "Adjustiertes R²", "F-Statistik", "p-Wert (F)", "df Modell", "df Residuen", "sigma"),
  Wert = c(round(r2, 4), round(adj_r2, 4), round(fstat["value"], 4), round(f_p, 4), df_model, df_resid, round(sigma, 4))
)

# gt-Tabelle
gt(info_df) %>%
  tab_header(title = "Modellkennzahlen") %>%
  fmt_number(columns = "Wert", decimals = 2) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#------------------------------------------------------------------------
# Graphische Darstellungen
#------------------------------------------------------------------------

#Modellauswahl mit Regsubsets
library(leaps) 
RegDaten = data.frame(Stromproduktion    = Daten$PE, 
                      Temperatur     = Daten$AT, 
                      Abluftvakuum = Daten$V,
                      Umgebungsdruck = Daten$AP,
                      Luftfeuchtigkeit = Daten$RH)

best_subset = regsubsets(
  Stromproduktion ~ .,
  data =  RegDaten,
  nvmax = 4,
  nbest = 4
)

summary(best_subset)

plot(best_subset, scale="adjr2", 
     main="Adjusted R^2")

#------------------------------------------------------------------------
# Schätzung des Modells mit Funktion lm() mit weniger Koeffizienten
#------------------------------------------------------------------------

Modell= lm(PE ~ AT + RH, data = Daten)

sm = summary(Modell)
print(sm)

#------------------------------------------------------------------------
# Bessere Darstellungen der Summary
#------------------------------------------------------------------------
library(gt)

# Koefﬁzienten-Tabelle vorbereiten
coef_df <- as.data.frame(sm$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df$Interpretation <- ifelse(coef_df$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")

# Spaltenreihenfolge anpassen
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]

# gt-Tabelle erzeugen
gt(coef_df) %>%
  tab_header(title = "Regressionskoeffizienten") %>%
  cols_label(
    Variable = "Variable",
    Estimate = "Schätzwert",
    `Std. Error` = "Standardfehler",
    `t value` = "t-Wert",
    `Pr(>|t|)` = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`),
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

# Einzelwerte berechnen
r2 <- sm$r.squared
adj_r2 <- sm$adj.r.squared
sigma <- sm$sigma
df_model <- sm$df[1]
df_resid <- sm$df[2]
fstat <- sm$fstatistic
f_p <- 1 - pf(fstat["value"], df1 = fstat["numdf"], df2 = fstat["dendf"])

# DataFrame für Zusammenfassung
info_df <- data.frame(
  Kennzahl = c("R²", "Adjustiertes R²", "F-Statistik", "p-Wert (F)", "df Modell", "df Residuen", "sigma"),
  Wert = c(round(r2, 4), round(adj_r2, 4), round(fstat["value"], 4), round(f_p, 4), df_model, df_resid, round(sigma, 4))
)

# gt-Tabelle
gt(info_df) %>%
  tab_header(title = "Modellkennzahlen") %>%
  fmt_number(columns = "Wert", decimals = 2) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#------------------------------------------------------------------------
# test der Annahmen
#------------------------------------------------------------------------


Daten$Stromproduktion_hat = fitted(Modell)
Daten$u_hat = residuals(Modell)

# Q-Q-Plot
ggplot(Daten, aes(sample = u_hat)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "darkred", linewidth = 1) +
  labs(title = "Q-Q-Plot der Residuen", x = "Theoretische Quantile", y = "Empirische Quantile") +
  theme_minimal()

# QQ-Plot
# Vergleich der Verteilung der Residuen des Modells mit einer theoretischen Normalverteilung.
# Ziel: überprüfen, ob die Residuen annähernd normalverteilt sind
# x-Achse: Theoretische Quantile einer Normalverteilung
# y-Achse: Beobachtete Quantile der standardisierten Residuen aus deinem Modell
# Ziel?
# Die Punkte sollten entlang der diagonalen Linie verlaufen,das spricht für Normalverteilung der Residuen.

#----------------------------------------------------------------------------------
# Multicollinearity
#---------------------------------------------------------------------------------
library(car)

vif_values <- vif(Modell)

vif_data <- data.frame(
  Variable = names(vif_values),
  VIF = as.numeric(vif_values)
)
ggplot(vif_data, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "orange", alpha = 0.7) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 1, y = 4.7, label = "good", hjust = 0, vjust = 1, size = 3) +
  annotate("text", x = 1, y = 9.7, label = "acceptable", hjust = 0, vjust = 1, size = 3) +
  labs(title = "Variance Inflation Factors (Multicollinearity)",
       y = "VIF", x = "Regressor") +
  theme_minimal()

ggplot(data = results, aes(x = .fitted, y = .resid, size = .cooksd)) +
  geom_hline(yintercept = 0, colour = "firebrick3") +
  geom_point(alpha = .5) +
  geom_text(aes(label = rownames(results))) +
  scale_size_area("Cook's distance")

#------------------------------------------------------------------------
# Heteroskedastizität diagnostizieren
#------------------------------------------------------------------------
#Goldfeld-Quandt Test
gqtest(lm(Modell), point=0.5, fraction=0, alternative=c("greater", "two.sided", "less"), data=list())
#p-value 0.1173 > 0,05 also Heteroskedastizität 

# Breusch-Pagan-Test
library(lmtest)
bptest(Modell)  
#p-value 0.0005041 < 0,05 also Heteroskedastizität 

# =============================================================================
# Modell gewichten
# =============================================================================

# Vorbereitung: Fitted Values und Residuen berechnen
fitted_vals <- fitted(Modell)
residuals_ols <- residuals(Modell)

# OPTION 1: Gewichtung basierend auf fitted values
# Annahme: Var(u) = σ² * fitted²
weights_fitted <- 1 / (fitted_vals^2)
Modell_WLS <- lm(PE ~ RH + AT, data = Daten, weights = weights_fitted)


# =============================================================================
# MODELLE VERGLEICHEN
# =============================================================================

# Funktion zur Bewertung der Modelle
evaluate_model <- function(model, name) {
  # Residuenanalyse
  res <- residuals(model)
  fitted <- fitted(model)
  
  # R² 
  r_squared <- summary(model)$r.squared
  
  # AIC (für Modellvergleich)
  aic_val <- AIC(model)
  
  # Test auf verbleibende Heteroskedastizität
  bp_test <- tryCatch({
    bp_result <- bptest(model)
    bp_result$p.value
  }, error = function(e) NA)
  
  # Zusammenfassung
  return(data.frame(
    Modell = name,
    R_squared = round(r_squared, 4),
    AIC = round(aic_val, 2),
    BP_pvalue = round(bp_test, 4),
    Heterosked_behoben = ifelse(is.na(bp_test), "Fehler", 
                                ifelse(bp_test > 0.05, "JA", "NEIN"))
  ))
}

# =============================================================================
# SCHRITT 4: OLS vs WLS VERGLEICHEN
# =============================================================================

# Einfacher Vergleich: nur OLS vs WLS1

# OLS Ergebnisse
print(summary(Modell))


# WLS Ergebnisse  
cat("WLS-MODELL (Gewichtung: 1/fitted²):\n")
print(summary(Modell_WLS))

# Test auf verbleibende Heteroskedastizität

# OLS
bp_ols <- bptest(Modell)
cat("OLS - Breusch-Pagan Test:\n")
cat("p-value =", round(bp_ols$p.value, 6))
if(bp_ols$p.value < 0.05) {
  cat(" → Heteroskedastizität vorhanden\n")
} else {
  cat(" → Keine Heteroskedastizität\n") 
}

# WLS
bp_wls <- bptest(Modell_WLS)
cat("\nWLS1 - Breusch-Pagan Test:\n") 
cat("p-value =", round(bp_wls$p.value, 6))
if(bp_wls$p.value < 0.05) {
  cat(" → Heteroskedastizität NICHT behoben\n")
} else {
  cat(" → Heteroskedastizität erfolgreich behoben!\n")
}

# =============================================================================
# 1. KOEFFIZIENTEN-VERGLEICH MIT GT
# =============================================================================

cat("=== KOEFFIZIENTEN-VERGLEICH ===\n")

# OLS Koeffizienten-Tabelle
sm_ols <- summary(Modell)
coef_df_ols <- as.data.frame(sm_ols$coefficients)
coef_df_ols$Variable <- rownames(coef_df_ols)
coef_df_ols$Interpretation <- ifelse(coef_df_ols$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")
coef_df_ols$Modell <- "OLS"

# WLS Koeffizienten-Tabelle  
sm_wls <- summary(Modell_WLS)
coef_df_wls <- as.data.frame(sm_wls$coefficients)
coef_df_wls$Variable <- rownames(coef_df_wls)
coef_df_wls$Interpretation <- ifelse(coef_df_wls$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")
coef_df_wls$Modell <- "WLS (1/fitted²)"

# Kombinierte Tabelle
coef_combined <- rbind(
  coef_df_ols[, c("Modell", "Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")],
  coef_df_wls[, c("Modell", "Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]
)

# GT-Tabelle für Koeffizienten
gt_coef <- gt(coef_combined) %>%
  tab_header(title = "Regressionskoeffizienten Vergleich: OLS vs WLS") %>%
  cols_label(
    Modell = "Modell",
    Variable = "Variable", 
    Estimate = "Schätzwert",
    `Std. Error` = "Standardfehler",
    `t value` = "t-Wert",
    `Pr(>|t|)` = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)"
  ) %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(rows = Modell == "OLS")
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"), 
    locations = cells_body(rows = Modell == "WLS (1/fitted²)")
  )

print(gt_coef)

# =============================================================================
# 2. MODELLKENNZAHLEN-VERGLEICH MIT GT
# =============================================================================

cat("\n=== MODELLKENNZAHLEN-VERGLEICH ===\n")

# OLS Kennzahlen
r2_ols <- sm_ols$r.squared
adj_r2_ols <- sm_ols$adj.r.squared
sigma_ols <- sm_ols$sigma
df_model_ols <- sm_ols$df[1]
df_resid_ols <- sm_ols$df[2]
fstat_ols <- sm_ols$fstatistic
f_p_ols <- 1 - pf(fstat_ols["value"], df1 = fstat_ols["numdf"], df2 = fstat_ols["dendf"])
aic_ols <- AIC(Modell)
bic_ols <- BIC(Modell)

# WLS Kennzahlen
r2_wls <- sm_wls$r.squared
adj_r2_wls <- sm_wls$adj.r.squared  
sigma_wls <- sm_wls$sigma
df_model_wls <- sm_wls$df[1]
df_resid_wls <- sm_wls$df[2]
fstat_wls <- sm_wls$fstatistic
f_p_wls <- 1 - pf(fstat_wls["value"], df1 = fstat_wls["numdf"], df2 = fstat_wls["dendf"])
aic_wls <- AIC(Modell_WLS)
bic_wls <- BIC(Modell_WLS)

# Heteroskedastizität-Tests
bp_ols <- bptest(Modell)
bp_wls <- bptest(Modell_WLS)

# DataFrame für Modellkennzahlen
info_df <- data.frame(
  Kennzahl = c("R²", "Adjustiertes R²", "F-Statistik", "p-Wert (F)", 
               "AIC", "BIC", "sigma", "df Modell", "df Residuen",
               "BP-Test p-Wert", "Heteroskedastizität"),
  OLS = c(
    round(r2_ols, 4), round(adj_r2_ols, 4), round(fstat_ols["value"], 4), 
    round(f_p_ols, 6), round(aic_ols, 2), round(bic_ols, 2), 
    round(sigma_ols, 4), df_model_ols, df_resid_ols,
    round(bp_ols$p.value, 6), ifelse(bp_ols$p.value < 0.05, "VORHANDEN", "NICHT vorhanden")
  ),
  WLS = c(
    round(r2_wls, 4), round(adj_r2_wls, 4), round(fstat_wls["value"], 4),
    round(f_p_wls, 6), round(aic_wls, 2), round(bic_wls, 2),
    round(sigma_wls, 4), df_model_wls, df_resid_wls, 
    round(bp_wls$p.value, 6), ifelse(bp_wls$p.value < 0.05, "VORHANDEN", "BEHOBEN!")
  ),
  Unterschied = c(
    round(r2_wls - r2_ols, 4), round(adj_r2_wls - adj_r2_ols, 4),
    round(fstat_wls["value"] - fstat_ols["value"], 4), 
    round(f_p_wls - f_p_ols, 6), round(aic_wls - aic_ols, 2), 
    round(bic_wls - bic_ols, 2), round(sigma_wls - sigma_ols, 4),
    df_model_wls - df_model_ols, df_resid_wls - df_resid_ols,
    round(bp_wls$p.value - bp_ols$p.value, 6), "OLS→WLS")
)

# GT-Tabelle für Kennzahlen
gt_info <- gt(info_df) %>%
  tab_header(title = "Modellkennzahlen Vergleich: OLS vs WLS") %>%
  cols_label(
    Kennzahl = "Kennzahl",
    OLS = "OLS", 
    WLS = "WLS (1/fitted²)",
    Unterschied = "Unterschied (WLS - OLS)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_fill(color = "lightcoral"),
    locations = cells_body(rows = Kennzahl == "Heteroskedastizität", columns = "OLS")
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(rows = Kennzahl == "Heteroskedastizität", columns = "WLS")
  )

print(gt_info)