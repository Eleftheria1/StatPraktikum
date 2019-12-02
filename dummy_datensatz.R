library(haven)
library(dplyr)
library(data.table)

# datensatz mit dem ich arbeite
setwd("C:/Users/Lisa/Documents/LMU/5. Semester/Statistisches Praktikum")
data <- read_sav("Daten_RadAktiv_final_Kunde.sav")

# datensatz mit allen anfangsvariablen, einige werden zum neuen datensatz hinzugefügt
daten <- read_sav("Daten_RadAktiv_final_Kunde.sav")

# alle unnützliche variablen löschen
data <- data[, -c(1,2,5,6,7,8,9,10,11,13,16,18:48,54:56,58,60,62,63,70:90,92,94,96:100,103:142)]

# alle von uns neu definierte Variablen hinzufügen
data$f01new <- replace(daten$f01, daten$f01 == 2, 1) %>%
  replace(daten$f01 == 1, 1) %>%
  replace(daten$f01 == 3, 2)

data$f0201new <- replace(daten$f0201, daten$f0201 == 1, 1) %>%
  replace(daten$f0201 == 2, 1) %>%
  replace(daten$f0201 == 3, 1) %>%
  replace(daten$f0201 == 4, 0) %>%
  replace(daten$f0201 == 5, 0)

data$f0202new <- replace(daten$f0202, daten$f0202 == 5, 1) %>%
  replace(daten$f0202 == 4, 2) %>%
  replace(daten$f0202 == 2, 4) %>%
  replace(daten$f0202 == 1, 5)

data$f0203new <- replace(daten$f0203, daten$f0203 == 5, 1) %>%
  replace(daten$f0203 == 4, 2) %>%
  replace(daten$f0203 == 2, 4) %>%
  replace(daten$f0203 == 1, 5)

data$f0204new <- replace(daten$f0202, daten$f0202 == 5, 1) %>%
  replace(daten$f0203 == 4, 2) %>%
  replace(daten$f0203 == 2, 4) %>%
  replace(daten$f0203 == 1, 5)

data$f03new <- replace(daten$f03, daten$f03 == 1, 1) %>%
  replace(daten$f03 == 2, 1) %>%
  replace(daten$f03 == 3, 1) %>%
  replace(daten$f03 == 4, 1) %>%
  replace(daten$f03 == 5, 1) %>%
  replace(daten$f03 == 6, 0)

# alle variablen (außer "sonstiges") als factor
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)

# f14a? was ist das

# alle variablen ohne frage 8 als dummy

results <- fastDummies::dummy_cols(data, remove_first_dummy = T, ignore_na = T)
results$y <- data$f0201new

results$f04_88 <- NULL
results$f04_99 <- NULL
results$f06_88 <- NULL
results$f06_99 <- NULL

# datensatz für frage 8 als dummy

data_f8 <- dplyr::select(daten, starts_with("f08"))
data_f8[sapply(data_f8, is.numeric)] <- lapply(data_f8[sapply(data_f8, is.numeric)], as.factor)

results_f8 <- fastDummies::dummy_cols(data_f8, remove_first_dummy = T, ignore_na = T)
results_f8$y <- data$f0201new
write.csv(results_f8, "dummy_f8.csv")

dummy_f8_NA <- na.omit(results_f8)
dummy_f8_NA <- dummy_f8_NA[, -c(1:28)]

write.csv(dummy_f8_NA, "dummy_f8_NA.csv")

# alle dummy variablen zusammen mit anfangsvariablen, kompletter datensatz

com_data <- cbind(results, results_f8)
com_data$f1601 <- daten$f1601 # wie viele personen im hh
com_data$f1602 <- daten$f1602
com_data$f20 <- daten$f20 # alter
com_data$f0102_88 <- NULL
com_data$f04 <- gsub(x = com_data$f04, pattern = "88", replacement = "")
com_data$f04 <- gsub(x = com_data$f04, pattern = "99", replacement = "")
com_data$f06 <- gsub(x = com_data$f06, pattern = "88", replacement = "")
com_data$f06 <- gsub(x = com_data$f06, pattern = "99", replacement = "")

write.csv(com_data, "com_data.csv")

# f19a? brauchen wir es

# nur dummy-variablen plus alter, mig, hh personen, ohne allen anfangsvariablen
dummy_data <- com_data[,-c(1:31,126:153)]

write.csv(dummy_data, "dummy_data.csv")

