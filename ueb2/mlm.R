#------------------------------------------------------------------------
# Multivariates Lineares Modell
# Absatz = beta_0 + beta_1 * Preis + beta_2*Marketingausgaben +u
# y = 10 -1,5*Preis +1,0*Marketingausgaben + u

# Preis: gleichverteilt (5; 25)
# Marketingsausgaben: gleichverteilt (0;100)
# u: NV(0; 12)

# Prof. Dr. Franziska Bönte
# FH Wedel
# Ökonometrie
#------------------------------------------------------------------------

# Indivdiduellen Arbeitspfad wählen
setwd("C:/Users/fbo/_FH Wedel/Vorlesungen/Master - Ökonometrie/Beispiel_Übungen/Ü2 - Multivariates Lineares Modell")


library(readxl)
library(ggplot2)

# Daten einlesen
Daten <- read_excel("Earnings_and_Height.xlsx")

#------------------------------------------------------------------------
# Manuelle Berechnung der Regressionsparameter
#------------------------------------------------------------------------
# Design-Matrix X (mit Intercept)
X <- cbind(1, Daten$age, Daten$educ, Daten$height)
colnames(X) <- c("Intercept", "age", "educ", "height")

# Zielvariable
y <- Daten$earnings

# "echte" Residuen - nicht vorhanden
# u <- Daten$u

# Manuelle Berechnung der Koeffizienten (OLS-Formel)
XtX_inv <- solve(t(X) %*% X)
beta_hat <- XtX_inv %*% t(X) %*% y

# Vektor der geschätzten Koeffizienten
colnames(beta_hat) <- "Schaetzer"
rownames(beta_hat) <- c("beta_0", "beta_1", "beta_2", "beta_3")
print(beta_hat)


#------------------------------------------------------------------------
# Manuelle Berechnung der Prognose, Modellgüte (R^2)
#------------------------------------------------------------------------

# Vorhersagen
y_hat <- X %*% beta_hat
Daten$Absatzmenge_hat <- y_hat

# Residuen
u_hat <- y - y_hat
Daten$u_hat <-u_hat

# Quadratsummen
SST <- sum((y - mean(y))^2)             # Total Sum of Squares
SSE <- sum(u_hat^2)                     # Residual Sum of Squares
SSR <- sum((y_hat - mean(y))^2)         # Explained Sum of Squares

# R² und adj. R²
n <- nrow(X)
k <- ncol(X) - 1  # Anzahl erklärender Variablen

R2 <- SSR / SST
R2_adj <- 1 - (SSE / (n - k - 1)) / (SST / (n - 1))

cat("R²:", round(R2, 4), "\n")
cat("Adjustiertes R²:", round(R2_adj, 4), "\n")


#------------------------------------------------------------------------
# Manuelle Berechnung der t-Tests auf Signifikanz der Parameter
#------------------------------------------------------------------------

# Schätzung der Fehlervarianz
sigma_u_hat_sq <- SSE / (n - k - 1)
sigma_u_hat <- sqrt(sigma_u_hat_sq)

# Varianz-Kovarianz-Matrix der Koeffizienten
var_beta_hat <- sigma_u_hat_sq * XtX_inv
se_beta_hat <- sqrt(diag(var_beta_hat))

# t-Werte
t_wert <- beta_hat[,1] / se_beta_hat

# p-Werte
p_wert <- 2 * pt(-abs(t_wert), df = n - k - 1)

# Ausgabe
ergebnisse <- data.frame(
  Schätzer = round(beta_hat[,1], 4),
  SE = round(se_beta_hat, 4),
  t = round(t_wert, 4),
  p = round(p_wert, 4)
)
print(ergebnisse)


#------------------------------------------------------------------------
# Darstellung der Ergebnisse als Tabelle
#------------------------------------------------------------------------

# Lade das gt-Paket
library(gt)

# Neue Spalte: Interpretation der p-Werte
ergebnisse$Interpretation <- ifelse(ergebnisse$p <= 0.05, "signifikant", "nicht signifikant")

# Anzeige mit gt
library(gt)
gt(ergebnisse, rowname_col = "rowname") %>%
  tab_header(
    title = "Manuelle Regressionsanalyse mit t-Test"
  ) %>%
  fmt_number(
    columns = c(Schätzer, SE, t, p),
    decimals = 2
  ) %>%
  cols_label(
    Schätzer = "Schätzwert",
    SE = "Standardfehler",
    t = "t-Wert",
    p = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

#------------------------------------------------------------------------
# Manuelle Berechnung des F-Tests auf Signifikanz des GesamtModells
#------------------------------------------------------------------------

# F-Statistik
f_stat <- ((SST - SSE) / k) / (SSE / (n - k - 1))
f_crit <- qf(0.95, df1 = k, df2 = n - k - 1)
p_f <- 1 - pf(f_stat, df1 = k, df2 = n - k - 1)

# Interpretation
interpretation_f <- ifelse(p_f <= 0.05, 
                           "Modell ist insgesamt signifikant", 
                           "Modell ist nicht signifikant")



# Ausgabe
cat("F-Statistik:", round(f_stat, 4), "\n")
cat("Kritischer Wert (5%-Niveau):", round(f_crit, 4), "\n")
cat("p-Wert:", round(p_f, 4), "\n")
cat("Interpretation:", interpretation_f, "\n")
cat("Standardbaweichung der Residuen:", sigma_u_hat, "\n")


#Ausgabe der Testergebnisse als Tabelle

# Daten vorbereiten
f_df <- data.frame(
  F_Statistik = round(f_stat, 4),
  df1 = k,
  df2 = n - k - 1,
  p_Wert = round(p_f, 4),
  Interpretation = interpretation_f,
  Sigma_u = sigma_u_hat
)


#Tabelle
gt(f_df) %>%
  tab_header(
    title = "F-Test für Gesamtsignifikanz des Modells"
  ) %>%
  cols_label(
    F_Statistik = "F-Statistik",
    df1 = "df1 (Zähler)",
    df2 = "df2 (Nenner)",
    p_Wert = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)",
    Sigma_u = "Sigma" 
  ) %>%
  fmt_number(columns = c(F_Statistik, p_Wert, Sigma_u),
             decimals = 2
  ) %>%
  fmt_number(columns = c(df1, df2),
             decimals = 0
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )




#------------------------------------------------------------------------
# Schätzung des Modells mit Funktion lm()
#------------------------------------------------------------------------

Modell= lm(y ~ age + educ + height, data = Daten)

sm = summary(Modell)
print(sm)

#------------------------------------------------------------------------
# Bessere Darstellungen der Summary
#------------------------------------------------------------------------

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


#Summary als schöne Latex-Tabelle
#2 Alternativen: xtable und kable

#Summary mit xtable
library(xtable)

# Regressionskoeffizienten vorbereiten
coef_df <- as.data.frame(sm$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df$Interpretation <- ifelse(coef_df$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")
coef_df <- coef_df[c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]

# Als xtable und anzeigen
xtable(coef_df, 
       caption = "Regressionskoeffizienten", 
       digits = c(0, 0, 2, 2, 2, 2, 0))

#Summary mit kable
library(knitr)
library(kableExtra)

# Erzeuge kable mit kableExtra
coef_df %>%
  kbl(booktabs = TRUE, format = "latex", digits = 2, caption = "Regressionskoeffizienten") %>%
  kable_styling(latex_options = c("striped", "hold_position", "scale_down"))



#------------------------------------------------------------------------
# Daten und Prognoses in Excel abspeichern
#------------------------------------------------------------------------


#Spalten auswählen und ggf. umbenennen
Exportdaten <- Daten[, c("earnings", "age", "educ", "height", "Absatzmenge_hat",  "u_hat")]

# Excel-Datei speichern
library(writexl)
write_xlsx(Exportdaten, path = "Multivariates_Lineares_Modell_mit_Prognosen.xlsx")


#------------------------------------------------------------------------
# Graphische Darstellungen
#------------------------------------------------------------------------

# Q-Q-Plot
ggplot(Daten, aes(sample = u_hat)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "darkred", linewidth = 1) +
  labs(title = "Q-Q-Plot der Residuen", x = "Theoretische Quantile", y = "Empirische Quantile") +
  theme_minimal()



# Graphik der Regression in Abhängigkeit der Bildung (educ)
# Mittelwerte von age und height berechnen
age_mittel <- mean(Daten$age)
height_mittel <- mean(Daten$height)

# Neue Datenreihe mit age und height auf konstantem Wert
plotdaten_educ <- Daten
plotdaten_educ$age <- age_mittel
plotdaten_educ$height <- height_mittel

# Prognose mit konstantem age und height
plotdaten_educ$y_hat <- predict(Modell, newdata = plotdaten_educ)

# Plot mit ggplot2
ggplot(Daten, aes(x = educ, y = earnings)) +
  geom_point(alpha = 0.6, color = "darkblue") +
  geom_line(data = plotdaten_educ, aes(x = educ, y = y_hat), color = "red", linewidth = 1.2) +
  labs(title = "Regression: Einkommen ~ Bildung (Alter & Größe konstant)",
       y = "Einkommen (earnings)", x = "Bildung (educ)") +
  theme_minimal()

# Graphik der Regression in Abhängigkeit des Alters (age)

# Mittelwerte von educ und height
educ_mittel <- mean(Daten$educ)
height_mittel <- mean(Daten$height)

# Neue Daten mit konstantem educ und height
plotdaten_age <- Daten
plotdaten_age$educ <- educ_mittel
plotdaten_age$height <- height_mittel
plotdaten_age$y_hat <- predict(Modell, newdata = plotdaten_age)

# Plot: Age → x-Achse, y_hat → y-Achse
ggplot() +
  geom_point(data = Daten, aes(x = age, y = earnings), alpha = 0.6, color = "darkblue") +
  geom_line(data = plotdaten_age, aes(x = age, y = y_hat), color = "red", linewidth = 1.2) +
  labs(title = "Einfluss von Alter auf Einkommen (Bildung & Größe konstant)",
       x = "Alter (age)", y = "Einkommen (earnings)") +
  theme_minimal()

# Graphik der Regression in Abhängigkeit der Körpergröße (height)
# Mittelwerte von educ und age
educ_mittel <- mean(Daten$educ)
age_mittel <- mean(Daten$age)

# Neue Daten mit konstantem educ und age
plotdaten_height <- Daten
plotdaten_height$educ <- educ_mittel
plotdaten_height$age <- age_mittel
plotdaten_height$y_hat <- predict(Modell, newdata = plotdaten_height)

# Plot: Height → x-Achse, y_hat → y-Achse
ggplot() +
  geom_point(data = Daten, aes(x = height, y = earnings), alpha = 0.6, color = "darkblue") +
  geom_line(data = plotdaten_height, aes(x = height, y = y_hat), color = "red", linewidth = 1.2) +
  labs(title = "Einfluss von Körpergröße auf Einkommen (Bildung & Alter konstant)",
       x = "Körpergröße (height)", y = "Einkommen (earnings)") +
  theme_minimal()

# Dreidimensionale Darstellung der Regression (educ vs age, height konstant)
library(plotly)

# Sequenzen für die zwei Hauptvariablen erstellen
educ_seq <- seq(min(Daten$educ), max(Daten$educ), length.out = 30)
age_seq <- seq(min(Daten$age), max(Daten$age), length.out = 30)

# Matrix für die Regressionsebene (height auf Mittelwert fixiert)
height_mittel <- mean(Daten$height)
z_matrix <- outer(age_seq, educ_seq, function(a, e) {
  beta_hat[1] + beta_hat[2] * a + beta_hat[3] * e + beta_hat[4] * height_mittel
})

# 3D Plot
plot_ly(Daten, x = ~age, y = ~educ, z = ~earnings, type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = 'darkblue')) %>%
  add_surface(x = age_seq, y = educ_seq, z = z_matrix,
              opacity = 0.7, showscale = FALSE,
              colorscale = list(c(0,1), c("lightblue", "red"))) %>%
  layout(scene = list(
    xaxis = list(title = "Alter (age)"),
    yaxis = list(title = "Bildung (educ)"),
    zaxis = list(title = "Einkommen (earnings)")
  ),
  title = "3D Regression: Einkommen ~ Alter + Bildung (Größe konstant)")

# Alternative: 3D Plot mit educ vs height (age konstant)
age_mittel <- mean(Daten$age)
z_matrix2 <- outer(height_seq <- seq(min(Daten$height), max(Daten$height), length.out = 30), 
                   educ_seq, function(h, e) {
                     beta_hat[1] + beta_hat[2] * age_mittel + beta_hat[3] * e + beta_hat[4] * h
                   })

plot_ly(Daten, x = ~height, y = ~educ, z = ~earnings, type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = 'darkgreen')) %>%
  add_surface(x = height_seq, y = educ_seq, z = z_matrix2,
              opacity = 0.7, showscale = FALSE,
              colorscale = list(c(0,1), c("lightgreen", "orange"))) %>%
  layout(scene = list(
    xaxis = list(title = "Körpergröße (height)"),
    yaxis = list(title = "Bildung (educ)"),
    zaxis = list(title = "Einkommen (earnings)")
  ),
  title = "3D Regression: Einkommen ~ Größe + Bildung (Alter konstant)")

#Korrelation der 4 Variablen
# Nur die Variablen extrahieren, die für die Korrelationsberechnung benötigt werden
korrelationsdaten <- Daten[, c("age", "educ", "height", "earnings")]

# Korrelationsmatrix berechnen
corr_matrix <- cor(korrelationsdaten)

# Bildchen
library(ggcorrplot)
ggcorrplot(corr_matrix,
           method = "square",     # auch: "circle" möglich
           type = "full",        
           lab = TRUE,            # Korrelationswert anzeigen
           lab_size = 6,
           colors = c("red", "white", "blue"),
           title = "Korrelation zwischen Alter, Bildung, Größe und Einkommen",
           ggtheme = theme_minimal() +
             theme(plot.title = element_text(hjust = 0.5))  # zentrieren
)


#Modellauswahl mit Regsubsets
library(leaps) 
RegDaten = data.frame(Einkommen = Daten$earnings, 
                      age       = Daten$age, 
                      Bildung   = Daten$educ,
                      Größe     = Daten$height)

best_subset = regsubsets(
  Einkommen ~ .,
  data =  RegDaten,
  nvmax = 2,
  nbest = 2
)

summary(best_subset)

plot(best_subset, scale="adjr2", 
     main="Adjusted R^2")




