library(haven)
library(tidyr)
library(tidyverse)
#library(plyr)
setwd("C:/Users/elefi/OneDrive/Desktop/5FS/statistisches praktikum")
setwd("C:/Users/Eleftheria/OneDrive/Desktop/5FS/statistisches praktikum")
data <- read_sav("Daten_RadAktiv_final_Kunde.sav")

data$f0201new <- replace(data$f0201, data$f0201 == 1, 1) %>%
  replace( data$f0201 == 2, 1) %>%
  replace(data$f0201 == 3, 1) %>%
  replace(data$f0201 == 4, 0) %>%
  replace(data$f0201 == 5, 0)



# Fahrradfahren ja /nein -> jas zusammengef?gt
data$f01new <- replace(data$f01, data$f01 == 2, 1)
data$f01new <- replace(data$f01, data$f01 == 1, 1)
data$f01new <- replace(data$f01, data$f01 == 3, 2)
data$f01new <- as.factor(data$f01new) 
data$f01new <- plyr::mapvalues(data$f01new, from = c(1, 2), to = c("Ja", "Nein"))

#ÖPNV Ja oder nein
data$f03new <- replace(data$f03, data$f03 == 1, 1) %>%
  replace( data$f03 == 2, 1) %>%
  replace(data$f03 == 3, 1) %>%
  replace(data$f03 == 4, 1) %>%
  replace(data$f03 == 5, 1) %>%
  replace(data$f03 == 6, 0)
# frage 2 umkodieren
data$f0202new <- replace(data$f0202, data$f0202 == 5, 1) %>%
  replace(data$f0202 == 4, 2) %>%
  replace(data$f0202 == 2, 4) %>%
  replace(data$f0202 == 1, 5)

data$f0203new <- replace(data$f0203, data$f0203 == 5, 1) %>%
  replace(data$f0203 == 4, 2) %>%
  replace(data$f0203 == 2, 4) %>%
  replace(data$f0203 == 1, 5)

data$f0204new <- replace(data$f0202, data$f0202 == 5, 1) %>%
  replace(data$f0203 == 4, 2) %>%
  replace(data$f0203 == 2, 4) %>%
  replace(data$f0203 == 1, 5)
#dummykodierung von frage 4
f04_newdata <- filter(data, f04 != 88 & f04 != 99)
results_f4 <- fastDummies::dummy_cols(as.factor(f04_newdata$f04), remove_first_dummy = T)
results_f4$y <-  f04_newdata$f0201new
#results_f4 <- cbind(results_f4, f04_newdata$f0201new)
#dummykodierung von frage 6
f04_newdata <- filter(f04_newdata, f06 != 88 & f06 != 99)
results_f6 <- fastDummies::dummy_cols(as.factor(f04_newdata$f06), remove_first_dummy = T)
results_f6$y <-  f04_newdata$f0201new

#schulabschluss
results_f15 <- fastDummies::dummy_cols(as.factor(data$f15), remove_first_dummy = T, ignore_na = TRUE)
results_f15$y <-  data$f0201new
#großstadt 
results_f14 <- fastDummies::dummy_cols(as.factor(data$f14), remove_first_dummy = T)
results_f14$y <-  data$f0201new
#schichten
results_f22 <- fastDummies::dummy_cols(as.factor(data$f22), remove_first_dummy = T, ignore_na = TRUE)
results_f22$y <-  data$f0201new

#frage 11
data$f11_new <- replace(data$f11, data$f11 == 2, 0)
# zusammenfügen von dummy-variablen
dummy_data <- cbind(results_f14, results_f15, results_f1603, results_f22)
dummy_data$y <- data$f0201new 

model_dummy <- glm(y ~ . - .data, family = binomial, dummy_data) 




data$f0201new <- replace(data$f0201, data$f0201 == 1, 1) %>%
  replace( data$f0201 == 2, 1) %>%
  replace(data$f0201 == 3, 1) %>%
  replace(data$f0201 == 4, 0) %>%
  replace(data$f0201 == 5, 0)

#ÖPNV Ja oder nein
data$f03new <- replace(data$f03, data$f03 == 1, 1) %>%
  replace( data$f03 == 2, 1) %>%
  replace(data$f03 == 3, 1) %>%
  replace(data$f03 == 4, 1) %>%
  replace(data$f03 == 5, 1) %>%
  replace(data$f03 == 6, 0)

# frage 2 umkodieren
data$f0202new <- replace(data$f0202, data$f0202 == 5, 1) %>%
  replace(data$f0202 == 4, 2) %>%
  replace(data$f0202 == 2, 4) %>%
  replace(data$f0202 == 1, 5)

data$f0203new <- replace(data$f0203, data$f0203 == 5, 1) %>%
  replace(data$f0203 == 4, 2) %>%
  replace(data$f0203 == 2, 4) %>%
  replace(data$f0203 == 1, 5)

data$f0204new <- replace(data$f0202, data$f0202 == 5, 1) %>%
  replace(data$f0203 == 4, 2) %>%
  replace(data$f0203 == 2, 4) %>%
  replace(data$f0203 == 1, 5)

data$f11_new <- replace(data$f11, data$f11 == 2, 0)

data_reg <- data
write.csv(data_reg, "data_reg.csv")
