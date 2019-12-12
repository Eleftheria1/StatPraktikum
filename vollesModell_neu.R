library("arm")
setwd("C:/Users/Lisa/Documents/LMU/5. Semester/Statistisches Praktikum")
data <- read.csv("data.csv")
daten <- read_sav("Daten_RadAktiv_final_Kunde.sav")
data$X <- NULL
data$f01new <- NULL # damit funktioniert Regression nicht

data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], as.factor)

data$f1601 <- daten$f1601 # wie viele personen im hh
data$f1602 <- daten$f1602
data$f20 <- daten$f20 # alter

data_NA <- na.omit(data)

data$f0204new <- NULL # damit funktionieren marginale Effekte nicht
data_NA$f0204new <- NULL

mod_full <- glm(f0201new ~ . , family = binomial, data = data)
summary(mod_full)
AIC(mod_full)

margin_full <- logitmfx(formula = f0201new ~ ., atmean = TRUE, data = data)
margin_full

# kommt das gleiche raus
mod_full_NA <- glm(f0201new ~ . , family = binomial, data = data_NA)
summary(mod_full_NA)
AIC(mod_full_NA)

# hier auch
anova(mod_full, mod_full_NA)
