# 1 Zeile, 2 Spalten
par(mfrow = c(1, 2), oma = c(0,0,3,0))

h <- hist(
  guess$Q6.1.2_1,
  breaks = seq(0.5, 7.5, by = 1),   # sorgt für saubere Balken für 1,2,3,...7
  col = "lightblue",
  border = "black",
  xlab = "Skala von 1-7 (1 = question, 7 = obey)",
  ylab = "Anzahl Personen",
  main = ""
)

# Prozente berechnen
perc <- round(h$counts / sum(h$counts) * 100,1)

text(
  x = h$mids,
  y = h$counts,
  labels = paste0(perc, "%"),
  pos = 1
)

boxplot(
  guess$Q6.1.2_1,
  vertical = TRUE,               # horizontale Box besser neben Histogramm
  col = "lightgreen",
  #main = "Boxplot",
  xlab = "Skala von 1-7 (1 = question, 7 = obey)"
)

mtext("Q6.1.2_1 - In my society, followers are expected to", 
      outer = TRUE,   # für die gesamte Plotfläche
      cex = 1.5,      # Schriftgröße
      font = 2)       # Fettschrift

