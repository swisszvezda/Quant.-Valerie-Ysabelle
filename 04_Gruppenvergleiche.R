#ÜBUNGEN
library(openxlsx)
guess <- read.xlsx("guess.xlsx", sheet = 1)

guess_clean <- subset(guess, Q7.2 %in% c(0, 1))

t.test(Q2.1_1 ~ Q7.2, data = guess_clean)

t.test(Q2.1_2 ~ Q7.2, data = guess_clean)

wilcox.test(Q2.1_1 ~ Q7.2, data = guess_clean)

#ÜBUNGEN WILCOXON
#Wilcoxon Gründungsintention
wilcox.test(Q4.1.1_2 ~ Q7.2, data = gender)
#stat Kennwerte wilcoxon
aggregate(
  Q4.1.1_2 ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    Median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

#Wilcoxon Q6.2_1
wilcox.test(Q6.1.1_1~ Q7.2, data = gender)
#Wilcoxon Q6.2_2
wilcox.test(Q6.1.2_1~ Q7.2, data = gender)
#Wilcoxon Q6.2_3
wilcox.test(Q6.1.3_1~ Q7.2, data = gender)
#Wilcoxon
wilcox.test(gesellschaft ~ Q7.2, data = gender)
#stat Kennwerte Wilcoxon
aggregate(
  gesellschaft ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    Median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)

#Wilcoxon Q6.2_1
wilcox.test(Q6.2_1~ Q7.2, data = gender)
#Wilcoxon Q6.2_2
wilcox.test(Q6.2_2~ Q7.2, data = gender)
#Wilcoxon Q6.2_3
wilcox.test(Q6.2_3~ Q7.2, data = gender)
#Wilcoxon umfeldreaktion
wilcox.test(umfeldreaktion ~ Q7.2, data = gender)
#stat Kennwerte
aggregate(
  umfeldreaktion ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    Median = median(x, na.rm = TRUE),
    IQR = IQR(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)




#Subset bilden von gender & labels geben
gender <- subset(guess, Q7.2 %in% c(0, 1))

gender$Q7.2 <- factor(
  gender$Q7.2,
  levels = c(0, 1),
  labels = c("male", "female")
)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#START PROJEKT


#INTENTION ZUM GRÜNDEN
#Q4.1.1_2 numerisch machen
is.numeric(guess$Q4.1.1_2)
gender$Q4.1.1_2 <- as.numeric(gender$Q4.1.1_2)
#t-test
t.test(Q4.1.1_2 ~ Q7.2, data = gender)
#stat Kennwerte t-test
aggregate(
  Q4.1.1_2 ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    M = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    N = sum(!is.na(x))
  )
)


#GESELLSCHAFTLICHE STRUKTUREN
#variablen numerisch machen
gender$Q6.1.1_1 <- as.numeric(as.character(gender$Q6.1.1_1))
gender$Q6.1.2_1 <- as.numeric(as.character(gender$Q6.1.2_1))
gender$Q6.1.3_1 <- as.numeric(as.character(gender$Q6.1.3_1))
is.numeric(gender$Q6.1.1_1)
is.numeric(gender$Q6.1.2_1)
is.numeric(gender$Q6.1.3_1)

#Index für gesellschaftliche Strukturen bilden
gender$gesellschaft <- rowMeans(
  gender[, c("Q6.1.1_1", "Q6.1.2_1", "Q6.1.3_1")],
  na.rm = TRUE
)


#t.test Q6.1.1_1
t.test(Q6.1.1_1~ Q7.2, data = gender)
#t.test Q6.1.2_1
t.test(Q6.1.2_1~ Q7.2, data = gender)
#t.test Q6.1.3_1
t.test(Q6.1.3_1~ Q7.2, data = gender)

#t.test gesellschaft
t.test(gesellschaft~ Q7.2, data = gender)
#stat Kennwerte
aggregate(
  gesellschaft ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    M  = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    N  = sum(!is.na(x))
  )
)



#REAKTION DES PERSÖNLICHEN UMFELDES
#variablen numerisch machen
gender$Q6.2_1 <- as.numeric(as.character(gender$Q6.2_1))
gender$Q6.2_2 <- as.numeric(as.character(gender$Q6.2_2))
gender$Q6.2_3 <- as.numeric(as.character(gender$Q6.2_3))
is.numeric(gender$Q6.2_1)
is.numeric(gender$Q6.2_2)
is.numeric(gender$Q6.2_3)

#t.test Q6.2_1
t.test(Q6.2_1~ Q7.2, data = gender)
#t.test Q6.2_2
t.test(Q6.2_2~ Q7.2, data = gender)
#t.test Q6.2_3
t.test(Q6.2_3~ Q7.2, data = gender)

#t.test umfeldreaktion
t.test(umfeldreaktion ~ Q7.2, data = gender)
#stat Kennwerte
aggregate(
  umfeldreaktion ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    M  = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    N  = sum(!is.na(x))
  )
)


