mean(guess$Q2.1_1, na.rm = TRUE)

median(guess$Q2.1_1, na.rm = TRUE)

get_mode <- function(x) {
  x <- na.omit(x)                 # fehlende Werte ignorieren
  uniq_vals <- unique(x)          # eindeutige Werte finden
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

get_mode(guess$Q2.1_1)

var(guess$Q2.1_1, na.rm = TRUE)

sd(guess$Q2.1_1, na.rm = TRUE)


#BERECHNUNG VON ZWEITER VARIABEL

#na.rm = TRUE ->entfernt fehlende werde, NA remove
str(guess$Q6.1.2_1) #str findet heraus, was die Variabel ist 
guess$Q6.1.2_1 <- as.numeric(guess$Q6.1.2_1) #Werte von Charakteren in Zahlen wandeln


median(guess$Q6.1.2_1, na.rm = TRUE)
mean(guess$Q6.1.2_1, na.rm = TRUE)
var(guess$Q6.1.2_1, na.rm = TRUE)
sd(guess$Q6.1.2_1, na.rm = TRUE)


modus <- names(sort(table(guess$Q6.1.2_1), decreasing = TRUE))[1]
modus
#MODUS ERKLÄRUNG:
table(guess$Q6.1.2_1) #zählt Häufigkeiten (auflistung aller Werte und die Häufigkeit darunter)
#sort(..., decreasing=TRUE) -> sortiert nach grösster Häufigkeit
#names(...)[1] -> liefert die Kategorie mit der höchsten Häufigkeit
