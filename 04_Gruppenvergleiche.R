library(openxlsx)
guess <- read.xlsx("guess.xlsx", sheet = 1)

guess_clean <- subset(guess, Q7.2 %in% c(0, 1))
t.test(Q2.1_1 ~ Q7.2, data = guess_clean)

t.test(Q2.1_2 ~ Q7.2, data = guess_clean)

wilcox.test(Q2.1_1 ~ Q7.2, data = guess_clean)




#Subset bilden von gender & labels geben
gender <- subset(guess, Q7.2 %in% c(0, 1))

gender$Q7.2 <- factor(
  gender$Q7.2,
  levels = c(0, 1),
  labels = c("male", "female")
)



#INTENTION ZUM GRÜNDEN
#Q4.1.1_2 numerisch machen
is.numeric(guess$Q4.1.1_2)
guess$Q4.1.1_2 <- as.numeric(guess$Q4.1.1_2)
#t-test
t.test(Q4.1.1_2 ~ Q7.2, data = gender)
#stat Kennwerte
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
gender$soc_structure_index <- rowMeans(
  gender[, c("Q6.1.1_1", "Q6.1.2_1", "Q6.1.3_1")],
  na.rm = TRUE
)

#t.test Q6.1.1_1
t.test(Q6.1.1_1~ Q7.2, data = gender)
#t.test Q6.1.2_1
t.test(Q6.1.2_1~ Q7.2, data = gender)
#t.test Q6.1.3_1
t.test(Q6.1.3_1~ Q7.2, data = gender)

#t.test soc_structure_index
t.test(soc_structure_index~ Q7.2, data = gender)
#stat Kennwerte
aggregate(
  soc_structure_index ~ Q7.2,
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

#Index für Reaktion pers. Umfeld bilden
gender$environment_index <- rowMeans(
  gender[, c("Q6.2_1", "Q6.2_2", "Q6.2_3")],
  na.rm = TRUE
)

#t.test Q6.2_1
t.test(Q6.2_1~ Q7.2, data = gender)
#t.test Q6.2_2
t.test(Q6.2_2~ Q7.2, data = gender)
#t.test Q6.2_3
t.test(Q6.2_3~ Q7.2, data = gender)

#t.test environment_index
t.test(environment_index ~ Q7.2, data = gender)
#stat Kennwerte
aggregate(
  environment_index ~ Q7.2,
  data = gender,
  FUN = function(x) c(
    M  = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    N  = sum(!is.na(x))
  )
)

