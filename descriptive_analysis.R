library(tidyverse)
library(scales)
library(tidyr)

df <- read.csv("./spotify_artist_data.csv")

View(df)

dim(df)

# Check datatypes of dataframe
str(df)

# Convert Datatypes to numeric 
# Remove commas
df$Lead.Streams <- as.numeric(gsub(",", "", df$Lead.Streams))
df$Feats <- as.numeric(gsub(",", "", df$Feats))
df[c("Tracks", "One.Billion", "X100.Million")] <- lapply(df[c("Tracks", "One.Billion", "X100.Million")], as.numeric )

# Check null values
sum(is.na(df$Lead.Streams))
sum(is.na(df$Tracks))

# Remove rows with NA values
newdf <- df[df$Artist.Name != "Artist", ]
head(df[is.na(df$Tracks), ])
newdf <- df[!is.na(df$Tracks), ]

# Base statistics
summary(newdf)

# Each name should only occur ones
table(newdf$Artist.Name)

# Calculate means
mean(newdf$Lead.Streams)
mean(newdf$Feats)
mean(newdf$Tracks)
mean(newdf$One.Billion)
mean(newdf$X100.Million)

# Histograms 
ggplot(newdf, aes(x = Lead.Streams)) + geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  geom_vline(aes(xintercept = median(Lead.Streams)),
             color = "red", linetype = "dashed", linewidth = 1)
+
  labs(
    title = "Distribution of Streams",
    x = "Streams (log10 scale)",
    y = "Count"
  )

ggplot(newdf, aes(x = Feats)) + geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10(
    labels = label_number(scale_cut = cut_short_scale())
  ) + 
  geom_vline(aes(xintercept = median(Feats)),
             color = "red", linetype = "dashed", linewidth = 1)
  +
  labs(
    title = "Distribution of Feats",
    x = "Feats (log10 scale)",
    y = "Count"
  )

ggplot(newdf, aes(x = One.Billion)) + geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10(
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  geom_vline(aes(xintercept = median(One.Billion)),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of one billions streams",
    x = "X Billion Streams (log10 scale)",
    y = "Count"
  )

ggplot(newdf, aes(x = X100.Million)) + geom_histogram(bins = 30, fill = "blue") +
  scale_x_log10(
    breaks = c(1, 5, 10, 50, 100),
    labels = comma_format()
  ) +
  geom_vline(aes(xintercept = median(X100.Million, na.rm = TRUE)),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Distribution of X100 million streams",
    x = "X100 million streams (log10 scale)",
    y = "Count"
  )

long_df <- pivot_longer(newdf, cols = c(Lead.Streams, Feats, Tracks, One.Billion, X100.Million), 
                      names_to = "Metric", 
                      values_to = "Value")

# Boxplots
ggplot(long_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  scale_y_log10(labels = label_comma(big.mark = ".", decimal.mark = ",")) +  # Deutsches Format
  labs(
    title = "Verteilung der Spotify Artist Metriken",
    x = "",
    y = "Wert (log-Skala)"
  ) +
  scale_x_discrete(labels = c(
    "Lead.Streams" = "Lead Streams (in Millionen)",
    "Feats" = "Anzahl Features",
    "Tracks" = "Anzahl Tracks",
    "One.Billion" = "Streams in Milliarden",
    "X100.Million" = "Streams in 100 Millionen"
  )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# ggplot(data = data[1: 10,]) + geom_line(mapping = aes(x = Artist.Name, y = Lead.Streams))

#---------------------------------------------------------------
# Korrelationen - Spotify Artist Data
#---------------------------------------------------------------
library(vcd)        # Für Heatmaps und Mosaikplots
library(tidycomm)   # Für einfache Korrelationsberechnungen
library(corrplot)   # Für die Visualisierung der Korrelationen

# Nur relevante numerische Variablen extrahieren
korrelation_subset <- subset(newdf, select = c(Lead.Streams, Feats, Tracks, One.Billion, X100.Million))

# Erste Korrelationsmatrix ausgeben
korrelation_subset %>%
  correlate() %>%
  to_correlation_matrix()

# Korrelationen visualisieren
korrelation_subset %>%
  correlate() %>%
  visualize()

# Klassische Korrelationsmatrix erstellen
cor_matrix <- cor(korrelation_subset, use = "complete.obs")

# Grafikränder anpassen
par(mar = c(1, 1, 5, 1))  # unten, links, oben, rechts

# Heatmap erstellen
corrplot(cor_matrix,
         method = "square",
         col = colorRampPalette(c("darkblue", "white", "darkred"))(21),
         addCoef.col = "black",
         cl.pos = "r",
         cl.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,      
         tl.cex = 1,      
         number.cex = 1)
