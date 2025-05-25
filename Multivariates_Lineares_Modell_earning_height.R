# ---------------------------------------------------------------
# Multivariates Lineares Modell: earnings = f(height_cm, age)
# ---------------------------------------------------------------

# Arbeitsverzeichnis anpassen
setwd("C:/Users/timot/Desktop/Uni/FHWEDEL/2.Semester/econometrics/Beispiel Code/Uebung_2")

# Pakete laden
library(readxl)
library(ggplot2)
library(gt)
library(xtable)
library(knitr)
library(kableExtra)
library(writexl)

# Daten einlesen
Daten <- read_excel("Earnings_and_Height_Dataset.xlsx")

mean_earnings <- mean(Daten$earnings, na.rm = TRUE)
median_earnings <- median(Daten$earnings, na.rm = TRUE)

Daten$height_cm <- Daten$height * 2.54  # cm-Berechnung vor Statistik
mean_height <- mean(Daten$height_cm, na.rm = TRUE)
median_height <- median(Daten$height_cm, na.rm = TRUE)

mean_age <- mean(Daten$age, na.rm = TRUE)
median_age <- median(Daten$age, na.rm = TRUE)

# Größe in cm berechnen
Daten$height_cm <- Daten$height * 2.54

# ---------------------------------------------------------------
# Manuelle Regression: earnings ~ height_cm + age
# ---------------------------------------------------------------

#Regressionsdaten aufbauen: Designmatrix X (mit Intercept) und Zielvariable y.
X <- cbind(1, Daten$height_cm, Daten$age)
colnames(X) <- c("Intercept", "height_cm", "age")
y <- Daten$earnings

#Regressionskoeffizienten berechnen (OLS): Berechnet Schätzer: Intercept, Effekt von height_cm, Effekt von age.
XtX_inv <- solve(t(X) %*% X)
beta_hat <- XtX_inv %*% t(X) %*% y
colnames(beta_hat) <- "Schaetzer"
rownames(beta_hat) <- colnames(X)

# Vorhersagen & Residuen
y_hat <- X %*% beta_hat
Daten$earnings_hat <- y_hat
u_hat <- y - y_hat
Daten$u_hat <- u_hat

# Quadratsummen
SST <- sum((y - mean(y))^2)
SSE <- sum(u_hat^2)
SSR <- sum((y_hat - mean(y))^2)

n <- nrow(X)
k <- ncol(X) - 1

R2 <- SSR / SST
R2_adj <- 1 - (SSE / (n - k - 1)) / (SST / (n - 1))

cat("R²:", round(R2, 4), "\n")
cat("Adjustiertes R²:", round(R2_adj, 4), "\n")

# Standardfehler, t-Werte, p-Werte
sigma_u_hat_sq <- SSE / (n - k - 1)
sigma_u_hat <- sqrt(sigma_u_hat_sq)
var_beta_hat <- sigma_u_hat_sq * XtX_inv
se_beta_hat <- sqrt(diag(var_beta_hat))
t_wert <- beta_hat[, 1] / se_beta_hat
p_wert <- 2 * pt(-abs(t_wert), df = n - k - 1)

# Tabelle
ergebnisse <- data.frame(
  Schätzer = round(beta_hat[,1], 4),
  SE = round(se_beta_hat, 4),
  t = round(t_wert, 4),
  p = round(p_wert, 4)
)
ergebnisse$Interpretation <- ifelse(ergebnisse$p <= 0.05, "signifikant", "nicht signifikant")

gt(ergebnisse, rowname_col = "rowname") %>%
  tab_header(title = "Manuelle Regression: earnings ~ height_cm + age") %>%
  fmt_number(columns = c(Schätzer, SE, t, p), decimals = 3) %>%
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

# ---------------------------------------------------------------
# F-Test
# ---------------------------------------------------------------

f_stat <- ((SST - SSE) / k) / (SSE / (n - k - 1))
f_crit <- qf(0.95, df1 = k, df2 = n - k - 1)
p_f <- 1 - pf(f_stat, df1 = k, df2 = n - k - 1)

cat("F-Statistik:", round(f_stat, 4), "\n")
cat("Kritischer Wert:", round(f_crit, 4), "\n")
cat("p-Wert:", round(p_f, 4), "\n")
cat("Interpretation:", ifelse(p_f <= 0.05, "Modell ist signifikant", "Modell ist nicht signifikant"), "\n")
cat("Standardabweichung der Residuen:", sigma_u_hat, "\n")

# Ausgabe als Tabelle
f_df <- data.frame(
  F_Statistik = round(f_stat, 4),
  df1 = k,
  df2 = n - k - 1,
  p_Wert = round(p_f, 4),
  Interpretation = ifelse(p_f <= 0.05, "signifikant", "nicht signifikant"),
  Sigma_u = sigma_u_hat
)

gt(f_df) %>%
  tab_header(title = "F-Test für Gesamtsignifikanz") %>%
  cols_label(
    F_Statistik = "F-Statistik",
    df1 = "df1 (Zähler)",
    df2 = "df2 (Nenner)",
    p_Wert = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)",
    Sigma_u = "σ"
  ) %>%
  fmt_number(columns = c(F_Statistik, p_Wert, Sigma_u), decimals = 2) %>%
  fmt_number(columns = c(df1, df2), decimals = 0)

# ---------------------------------------------------------------
# lm()-Vergleichsmodell
# ---------------------------------------------------------------

Modell <- lm(earnings ~ height_cm + age, data = Daten)
sm <- summary(Modell)

coef_df <- as.data.frame(sm$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df$Interpretation <- ifelse(coef_df$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]

gt(coef_df) %>%
  tab_header(title = "lm()-Modell: earnings ~ height_cm + age") %>%
  cols_label(
    Variable = "Variable",
    Estimate = "Schätzwert",
    `Std. Error` = "Standardfehler",
    `t value` = "t-Wert",
    `Pr(>|t|)` = "p-Wert",
    Interpretation = "Interpretation (α = 0,05)"
  ) %>%
  fmt_number(columns = c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`), decimals = 3)

# ---------------------------------------------------------------
# Visualisierung: Partial Plot height_cm (bei konstantem Alter)
# ---------------------------------------------------------------

height_seq <- seq(min(Daten$height_cm), max(Daten$height_cm), length.out = 100)
age_mean   <- mean(Daten$age)

newdata_height <- data.frame(
  height_cm = height_seq,
  age = age_mean
)

newdata_height$earnings_hat <- predict(Modell, newdata = newdata_height)

ggplot() +
  geom_point(data = Daten, aes(x = height_cm, y = earnings), alpha = 0.4, color = "darkgray") +
  geom_line(data = newdata_height, aes(x = height_cm, y = earnings_hat), color = "red", linewidth = 1.2) +
  labs(title = "Einfluss der Körpergröße auf Einkommen (bei konstantem Alter)",
       x = "Körpergröße (cm)", y = "Einkommen") +
  theme_minimal()

# ---------------------------------------------------------------
# 3D-Visualisierung mit plotly
# ---------------------------------------------------------------

library(plotly)

# Gitter aus height_cm und age-Werten
height_vals <- seq(min(Daten$height_cm), max(Daten$height_cm), length.out = 30)
age_vals    <- seq(min(Daten$age), max(Daten$age), length.out = 30)

# Datenraster für Vorhersage
grid <- expand.grid(height_cm = height_vals, age = age_vals)
grid$earnings_hat <- predict(Modell, newdata = grid)

# 3D Plot
plot_ly() %>%
  add_markers(data = Daten,
              x = ~height_cm, y = ~age, z = ~earnings,
              marker = list(color = 'blue', size = 2),
              name = "Datenpunkte") %>%
  add_surface(x = height_vals, y = age_vals,
              z = matrix(grid$earnings_hat, nrow = 30, byrow = TRUE),
              opacity = 0.7, showscale = FALSE,
              colorscale = list(c(0,1), c("lightblue", "red")),
              name = "Regressionsfläche") %>%
  layout(scene = list(
    xaxis = list(title = "Körpergröße (cm)"),
    yaxis = list(title = "Alter"),
    zaxis = list(title = "Einkommen")
  ),
  title = "3D-Regression: Einkommen ~ Körpergröße + Alter")
