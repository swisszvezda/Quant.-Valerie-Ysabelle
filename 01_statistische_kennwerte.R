#ÜBUNGEN

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


#START PROJEKT

#ERSTER SCHRITT FÜR ALLE VARIABLEN -> ÜBERSICHT + SKALA BILDEN

#Gründungsabsicht, Variable 1
guess$Q4.1.1_2 <- as.numeric(guess$Q4.1.1_2) #Variablenwerte in Zahlen umwandeln
summary(guess$Q4.1.1_2, na.rm = TRUE) #Überblick Werte der Variable // na.rm = TRUE = fehlende Werte werden ignoriert

#Erwartete Reaktion des persönlichen Umfelds, Variable 2
#Als erstes: Alle drei Variablen gleichzeitig in Zahlen umwandeln
guess[, c("Q6.2_1", "Q6.2_2", "Q6.2_3")] <-
  lapply(guess[, c("Q6.2_1", "Q6.2_2", "Q6.2_3")], as.numeric)
#Als zweites: Skala bilden (Mittelwert der drei Items) -> neue Variable "umfeldreaktion"
#"rowMeans" = Mittelwert pro Person
guess$umfeldreaktion <- rowMeans(
  guess[, c("Q6.2_1", "Q6.2_2", "Q6.2_3")],
  na.rm = TRUE
)
#Als drittes: Überblick Werte der neuen Variable "umfeldreaktion"
summary(guess$umfeldreaktion)

#Wahrnehmung gesellschaftlicher Strukturen, Variable 3
#Als erstes: Alle drei Variablen gleichzeitig in Zahlen umwandeln
guess[, c("Q6.1.1_1", "Q6.1.2_1", "Q6.1.3_1")] <-
  lapply(guess[, c("Q6.1.1_1", "Q6.1.2_1", "Q6.1.3_1")], as.numeric)
#Als zweites: Skala bilden (Mittelwert der drei Items) -> neue Variable "umfeldreaktion"
#"rowMeans" = Mittelwert pro Person
guess$gesellschaft <- rowMeans(
  guess[, c("Q6.1.1_1", "Q6.1.2_1", "Q6.1.3_1")],
  na.rm = TRUE
)
#Als drittes: Überblick Werte der neuen Variable "umfeldreaktion"
summary(guess$gesellschaft)

#ZWEITER SCHRITT FÜR ALLE DREI VARIABLEN -> BERECHNUNG MITTELWERT + STANDARDABWEICHUNG
mean(guess$Q4.1.1_2, na.rm = TRUE)
sd(guess$Q4.1.1_2, na.rm = TRUE)

mean(guess$gesellschaft, na.rm = TRUE)
sd(guess$gesellschaft, na.rm = TRUE)

mean(guess$umfeldreaktion, na.rm = TRUE)
sd(guess$umfeldreaktion, na.rm = TRUE)

#DRITTER SCHRITT: Häufigkeit + Prozentangaben berechnen Variable "Geschlecht"
#Zuerst Neue Variable mit Labels erstellen
guess$geschlecht <- factor(
  guess$Q7.2,
  levels = c(0, 1),
  labels = c("Mann", "Frau")
)
table(guess$geschlecht) #Häufigkeit anzeigen
prop.table(table(guess$geschlecht)) * 100  #Prozentangaben berechnen

