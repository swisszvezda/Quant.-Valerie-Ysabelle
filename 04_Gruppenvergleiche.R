library(openxlsx)
guess <- read.xlsx("guess.xlsx", sheet = 1)

guess_clean <- subset(guess, Q7.2 %in% c(0, 1))
t.test(Q2.1_1 ~ Q7.2, data = guess_clean)

t.test(Q2.1_2 ~ Q7.2, data = guess_clean)

wilcox.test(Q2.1_1 ~ Q7.2, data = guess_clean)
