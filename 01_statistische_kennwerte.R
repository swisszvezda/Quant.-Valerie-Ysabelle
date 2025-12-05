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
