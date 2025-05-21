# ---------------------------------------------------------------
# Arbeitsverzeichnis setzen (Pfad ggf. anpassen)
# ---------------------------------------------------------------
setwd("C:/Users/timot/Desktop/Uni/FHWEDEL/2.Semester/econometrics/Beispiel Code/Uebung_2")

# ---------------------------------------------------------------
# Pakete laden
# ---------------------------------------------------------------
library(readxl)
library(ggplot2)
library(gt)
library(xtable)
library(knitr)
library(kableExtra)
library(writexl)

# ---------------------------------------------------------------
# Daten einlesen
# ---------------------------------------------------------------
Daten <- read_excel("Earnings_and_Height_Dataset.xlsx")

# ---------------------------------------------------------------
# Größe von Inch in Zentimeter umrechnen
# ---------------------------------------------------------------
Daten$height_cm <- Daten$height * 2.54

# ---------------------------------------------------------------
# Manuelle Berechnung der Regression: earnings ~ height_cm
# ---------------------------------------------------------------
X <- cbind(1, Daten$height_cm)
colnames(X) <- c("Intercept", "height_cm")
y <- Daten$earnings

XtX_inv <- solve(t(X) %*% X)
beta_hat <- XtX_inv %*% t(X) %*% y

colnames(beta_hat) <- "Schaetzer"
rownames(beta_hat) <- c("beta_0", "beta_1")
print(beta_hat)

# Prognose & Residuen
y_hat <- X %*% beta_hat
u_hat <- y - y_hat

# Prognosefehler berechnen
mae <- mean(abs(u_hat))  # Mean Absolute Error
mse <- mean(u_hat^2)     # Mean Squared Error

# Modellgüte (R² & adj. R²)
SST <- sum((y - mean(y))^2)
SSE <- sum(u_hat^2)
SSR <- sum((y_hat - mean(y))^2)
n <- nrow(X)
k <- ncol(X) - 1

R2 <- SSR / SST
R2_adj <- 1 - (SSE / (n - k - 1)) / (SST / (n - 1))

cat("R²:", round(R2, 4), "\n")
cat("Adjustiertes R²:", round(R2_adj, 4), "\n")

# ---------------------------------------------------------------
# t-Tests auf Regressionskoeffizienten
# ---------------------------------------------------------------
sigma_u_hat_sq <- SSE / (n - k - 1)
se_beta_hat <- sqrt(diag(sigma_u_hat_sq * XtX_inv))
t_wert <- beta_hat[,1] / se_beta_hat
p_wert <- 2 * pt(-abs(t_wert), df = n - k - 1)

ergebnisse <- data.frame(
  Schätzer = round(beta_hat[,1], 4),
  SE = round(se_beta_hat, 4),
  t = round(t_wert, 4),
  p = round(p_wert, 4),
  row.names = c("beta_0", "beta_1")
)

ergebnisse$Interpretation <- ifelse(ergebnisse$p <= 0.05, "signifikant", "nicht signifikant")

# gt-Tabelle
gt(ergebnisse, rowname_col = "rowname") %>%
  tab_header(title = "Manuelle Regression: earnings ~ height_cm") %>%
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
# lm()-Modell (Vergleich)
# ---------------------------------------------------------------
Modell_cm <- lm(earnings ~ height_cm, data = Daten)
summary(Modell_cm)

# Schöne Darstellung
coef_df <- as.data.frame(summary(Modell_cm)$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df$Interpretation <- ifelse(coef_df$`Pr(>|t|)` <= 0.05, "signifikant", "nicht signifikant")
coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Interpretation")]

gt(coef_df) %>%
  tab_header(title = "lm()-Regression: earnings ~ height_cm") %>%
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
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

# ---------------------------------------------------------------
# Visualisierung
# ---------------------------------------------------------------
ggplot(Daten, aes(x = height_cm, y = earnings)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 200) +
  geom_smooth(method = "lm", color = "red", linewidth = 1.2) +
  labs(title = "Zusammenhang: Einkommen vs. Körpergröße (cm)",
       x = "Körpergröße (cm)", y = "Einkommen") +
  theme_minimal()
