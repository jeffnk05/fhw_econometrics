#------------------------------------------------------------------------
# Multivariates Lineares Modell
# earnings = beta_0 + beta_1 * age + beta_2 * educ + beta_3 * height
# y = -49740.7267 + 333.2802*age + 3945.6465*educ + 441.5084*height + u

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
# Schätzung des Modells mit Funktion lm()
#------------------------------------------------------------------------

Modell= lm(Zufriedenheit ~ Kaffeekonsum + Meetings + Teekonsum, data = Daten)

sm = summary(Modell)
print(sm)

Daten$Zufriedenheit_hat = fitted(Modell)
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

#------------------------------------------------------------------------
# Graphische Darstellungen
#------------------------------------------------------------------------

# 3D-Bildchen der Schätzung
library(plotly)

x_seq <- seq(min(Daten$Kaffeekonsum), max(Daten$Kaffeekonsum), length.out = 30)
y_seq <- seq(min(Daten$Meetings), max(Daten$Meetings), length.out = 30)
gitter <- expand.grid(Meetings = y_seq, Kaffeekonsum = x_seq)
gitter$Zufriedenheit_hat <- predict(Modell, newdata = gitter)
z_matrix <- matrix(gitter$Zufriedenheit_hat, nrow = length(y_seq), ncol = length(x_seq))

plot_ly(Daten, x = ~Kaffeekonsum, y = ~Meetings, z = ~Zufriedenheit,
        type = "scatter3d", mode = "markers",
        marker = list(size = 3, color = 'darkblue')) %>%
  add_surface(x = ~x_seq, y = ~y_seq, z = ~z_matrix,
              opacity = 0.7, showscale = FALSE,
              colorscale = list(c(0, 1), c("lightblue", "red"))) %>%
  layout(scene = list(
    xaxis = list(title = "Kaffeekonsum"),
    yaxis = list(title = "Meetings"),
    zaxis = list(title = "Zufriedenheit")
  ))


#------------------------------------------------------------------------------------------------------------
#Test auf Annahmen des Linearen Modells
#------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------
# Graphische Übersichten
#-----------------------------------------------------------

library(ggfortify)
autoplot(Modell, which = 1:4, ncol = 2)

library(broom)
results = augment(lm(Modell), Daten)
results
par(mfrow=c(2,2))
plot(Modell)
par(mfrow=c(1,1))

# Residuals vs. Fitted
# plottet die u_hat gegen die y_hat
# Punkte mit Nummern: auffallende Residuen (meist AUsreißer), Nr = Id des Datensatzes
# rote Linie: LOESS-Kurve/Glättungskurve (Locally Estimated Scatterplot Smoothing): 
# visuelle Hilfe, um zu erkennen, ob ein systematischer Zusammenhang zwischen den u_hat und y_hat besteht.
# Interpretation:
# - Flach nahe Null: Residuen gleichmäßig,gute Modellanpassung
# - Trend (z.B. U-/Trichterform): Hinweis auf Heteroskedastizität oder Nichtlinearität
# - Kurve steigt/fällt: Modell ist möglicherweise falsch spezifiziert (z.B. lineare Form ungeeignet)
# hier: Linie ist leicht gewellt, aber kein klarer systematischer Trend.
#   Das spricht nicht zwingend gegen Homoskedastizität, sollte aber untersucht werden

# Scale-Location (Spread-Location)
# (bzw. „sqrt(Standardized Residuals) vs Fitted“)
# Y-Achse: Wurzel der standardisierten Residuen
# X-Achse: Vorhergesagte Werte
# Ziel: Eine horizontale Wolke, Linie ist flach: Streuung ist überall gleich
# Warnsignale für Heteroskedastizität:
# - Steigende oder fallende rote Linie:	Hinweis auf Heteroskedastizität
# - Trichterform nach oben oder unten:	Residuen streuen stärker bei hohen/niedrigen 
# - Ausreißerpunkte stark entfernt:	Einzelbeobachtungen mit hoher Varianz

# Residuals vs. Leverage
# x-Achse: Leverage – ein Maß dafür, wie ungewöhnlich ein Punkt in Bezug auf die unabhängigen Variablen ist
# y-Achse: Standardisierte Residuen – zeigen, wie stark ein Punkt vom Modell abweicht
# Rote Linie: LOESS-Glättung wie bei den anderen Plots
# Gestrichelte Linien: Linien gleicher Cook's Distance – Maß für den Gesamteinfluss eines Punkts
# Ziel:erkennen, ob einzelne Beobachtungen das Modell unverhältnismäßig stark beeinflussen (hoher Residualfehler UND hoher Leverage).

# Cook's Distance Plot
# misst, wie stark eine einzelne Beobachtung die geschätzten Regressionskoeffizienten beeinflusst, 
# wenn sie entfernt wird.
# Idee:Was passiert mit dem Modell, wenn ich Beobachtung i rausnehme?
# Wenn sich die Schätzung stark ändert, hat Beobachtung i hat großen Einfluss.
# Berechnung aus:
# - Residualwert einer Beobachtung (also wie „abseits“ sie liegt) und
# - Hebelwert (engl. leverage), also wie ungewöhnlich ihre Position in der Regressorenmenge ist.
# Faustregel für Auffälligkeit:
# D > 4/n: Beobachtung ist potenziell einflussreich
# D > 1: In vielen Fällen problematisch (aber selten bei kleinen Stichproben)
# hier: n=20 Schwellenwert 4/20=0,2
# Was tun bei auffälliger Cook's Distance?
#   Beobachtung prüfen (Tippfehler? Extremwerte?)
#   Ggf. Modell mit und ohne diese Beobachtungen vergleichen
#   Alternativen prüfen: robuste Regression, Sensitivitätsanalyse

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
#
results %>%
  arrange(.cooksd) %>%
  mutate(row_id = 1:n()) %>%
  select(row_id, .fitted, .resid) %>%
  top_n(5)

ggplot(data = results, aes(x = .fitted, y = .resid, size = .cooksd)) +
  geom_hline(yintercept = 0, colour = "firebrick3") +
  geom_point(alpha = .5) +
  geom_text(aes(label = rownames(results))) +
  scale_size_area("Cook's distance")


#------------------------------------------------------------------------
# Modell korrigieren
#------------------------------------------------------------------------
#Modellauswahl mit Regsubsets
library(leaps) 
RegDaten = data.frame(Zufriedenheit    = Daten$Zufriedenheit, 
                      Kaffee     = Daten$Kaffeekonsum, 
                      Tee = Daten$Teekonsum,
                      Meetings = Daten$Meetings
)

best_subset = regsubsets(
  Zufriedenheit ~ .,
  data =  RegDaten,
  nvmax = 3,
  nbest = 3
)

summary(best_subset)

plot(best_subset, scale="adjr2", 
     main="Adjusted R^2")



Modell= lm(Zufriedenheit ~ Kaffeekonsum + Meetings, data = Daten)

sm = summary(Modell)
print(sm)


#------------------------------------------------------------------------
# Heteroskedastizität diagnostizieren
#------------------------------------------------------------------------

# 1. Residuen vs. Kaffeekonsum
ggplot(Daten, aes(x = Kaffeekonsum, y = u_hat)) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(title = "Residuen vs. Kaffeekonsum", y = "Residuen", x = "Kaffeekonsum") +
  theme_minimal()

# 2. Residuen vs. Meetings
ggplot(Daten, aes(x = Meetings, y = u_hat)) +
  geom_point(color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(title = "Residuen vs. Meetings", y = "Residuen", x = "Meetings") +
  theme_minimal()


# Oder via Tests
library(lmtest)
bptest(Modell)  # Breusch-Pagan-Test

#Goldfeld-Quandt Test
gqtest(lm(Modell), point=0.5, fraction=0, alternative=c("greater", "two.sided", "less"), data=list())
#Result equals 1 so completely homoscedasticity





#---------------------------------------------------------------------------------
#Normal Distribution of Residuals
#---------------------------------------------------------------------------------

ggplot(data = results, aes(.std.resid)) +
  geom_histogram(aes(y = ..density..)) +
  stat_function(fun = dnorm, lwd = 1, col = 'red') +
  ggtitle("Standard-Normal Distribution")

ggplot(data = results, aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline(colour = "red") +
  ggtitle("Q-Q Plot")

#Test for Normal Distribution

#Kolmogorov-Smirnof Test
ks.test(rstandard(lm(Modell)),"pnorm", 0,1,alternative="two.sided")
#Result: p-value > 0.05
#        --> conclude that the distribution of residuals is not significantly different than normal distribution


install.packages("tseries")  
library(tseries)
#Jarque-Bera Test
jarque.bera.test(rstandard(lm(Modell)))
#Result: P-Value > 0.05, 
#        --> distribution of residuals is not significantly different than normal distribution. 


#---------------------------------------------------------------------------------------
# Independence of Residuals: E(X'beta)=0
#--------------------------------------------------------------------------------------

# blue line: linearity of parameters lm(...)
# red line: robust smoothing

#Graph Kaffeekonsum, Zufriedenheit
par(mfrow=c(2,1))
ggplot(results, aes(Kaffeekonsum, Zufriedenheit)) + # aes(x, y) 
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "loess", se = F, color = "red") 

#Graph Meetings, Zufriedenheit
ggplot(results, aes(Meetings, Zufriedenheit)) + # aes(x, y)
  geom_jitter(alpha = .1) +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "loess", se = F, color = "red") 
par(mfrow=c(1,1))


#---------------------------------------------------------------------------------
# Autocorrelation
#---------------------------------------------------------------------------------

#Durbin-Watson Test
#autocorrelation of disturbances 1. order: u_t = delta * u_(t-1) + e_t 
#H0=rho(1)=0: no Autocorrelation
#Result: high Autocorrelation 1. order

dwtest(lm(Modell), alternative = c("greater","two.sided", "less"), iterations=15, tol=1e-10, data =list())
# Hier: keine Autokorrelation erster Ordnung, da p-value >0,05


#---------------------------------------------------------------------------------
#Testing for structural changes
#---------------------------------------------------------------------------------

library(strucchange)

par(mfrow=c(1,1))
ocus = efp(Modell, type="OLS-CUSUM", data=Daten)
me = efp(Modell, type="ME", data=Daten)
bound.ocus = boundary(ocus, alpha=0.05)
plot(ocus)

#H0: There are no structural changes
sctest(ocus)

ocus2 = efp(Modell, type="OLS-MOSUM", data=Daten)
me2 = efp(Modell, type="ME", data=Daten)
bound.ocus2 = boundary(ocus2, alpha=0.05)
plot(ocus2)

sctest(ocus2)

#Result: keine Strukturbrüche

#Berechnet F-Statistiken für jeden möglichen Zeitpunkt als Strukturbruchpunkt (nach dem Verfahren von Andrews, 1993).

fs = Fstats(Modell, data = Daten)
plot(fs, alpha = 0.05)
sctest(fs, type="expF")
#H0 gets rejected if mean of F statistics gets too high or too low. Here: It is very    high (equals 1).
# x_achsen-Werte: relative Position des Wertes in der Zeitreihe: 
# bei 20 Beobachtungen heisst x= 0,5, dass es sich um den 10. Beobachtungswert handelt

