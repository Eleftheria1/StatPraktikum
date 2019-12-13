library("boot")
library("corrplot")
library("caret")
library(e1071)
library(mfx)
library("dplyr")
library(effects)

setwd("C:/Users/Lisa/Documents/LMU/5. Semester/Statistisches Praktikum/Analysen")

data <- read.csv("data.csv")
daten <- read_sav("Daten_RadAktiv_final_Kunde.sav")
data$X <- NULL
data$f01new <- NULL

data[sapply(data, is.integer)] <- lapply(data[sapply(data, is.integer)], as.factor)

data$f1601 <- daten$f1601 # wie viele personen im hh
data$f1602 <- daten$f1602
data$f20 <- daten$f20 # alter

data_NA <- na.omit(data)

#####################
### frage 2
modell_f2 <- glm(f0201new ~ f0202new + f0203new, family = binomial, data = data)
summary(modell_f2)
exp(modell_f2$coefficients)
plot(allEffects(modell_f2))

margin_f2 <- logitmfx(modell_f2, atmean = TRUE, data = data)
margin_f2

### frage 3
modell_f3 <- glm(f0201new ~ f03new, family = binomial, data = data)
summary(modell_f3)
exp(modell_f3$coefficients)
plot(allEffects(modell_f3))

margin_f3 <- logitmfx(formula = f0201new ~ f03new, atmean = TRUE, data = data)
margin_f3

### frage 4
modell_f4 <- glm(f0201new ~ f04, family=binomial, data=data)
summary(modell_f4)
exp(modell_f4$coefficients)
plot(allEffects(modell_f4))

margin_f4 <- logitmfx(formula = f0201new ~ f04, atmean = TRUE, data = data)
margin_f4

### frage 5
modell_f5 <- glm(f0201new ~ f05, family=binomial, data=data)
summary(modell_f5)
exp(modell_f5$coefficients)
plot(allEffects(modell_f5))

margin_f5 <- logitmfx(formula = f0201new ~ f05, atmean = TRUE, data = data)
margin_f5

### frage 4 + frage 5, interaktionen
modell_f4_f5 <- glm(f0201new ~ f04*f05, family=binomial, data=data, maxit=260)
summary(modell_f4_f5)
exp(modell_f4_f5$coefficients)
plot(allEffects(modell_f4_f5))

### frage 6
modell_f6 <- glm(f0201new ~ f06,  family = binomial, data = data)
summary(modell_f6)
exp(modell_f6$coefficients)
plot(allEffects(modell_f6))

margin_f6 <- logitmfx(formula = f0201new ~ f06, atmean = TRUE, data = data)
margin_f6

### frage 7
modell_f7 <- glm(f0201new ~ f07,  family = binomial, data = data)
summary(modell_f7)
exp(modell_f7$coefficients)
plot(allEffects(modell_f7))

margin_f7 <- logitmfx(formula = f0201new ~ f07, atmean = TRUE, data = data)
margin_f7

### frage 6 + frage 7, interaktionen
modell_f6_f7 <- glm(f0201new ~ f06*f07,  family = binomial, data = data)
summary(modell_f6_f7)
exp(modell_f6_f7$coefficients)
plot(allEffects(modell_f6_f7))

margin_f6_f7 <- logitmfx(formula = f0201new ~ f06*f07, atmean = TRUE, data = data)
margin_f6_f7

### frage 8
modell_f8 <- glm(f0201new ~ f0801+f0802+f0803+f0804+f0805+f0806+f0807+f0808+   
                 f0809+f0810+f0811+f0812+f0813+f0814+f0815+   
                 f0816+f0817+f0818+f0819+f0820+f0821+f0822+f0823+
                 f0824+f0825+f0826+f0827+f0828,  family = binomial, data = data)
summary(modell_f8)
exp(modell_f8$coefficients)
plot(allEffects(modell_f8))

margin_f8 <- logitmfx(modell_f8, atmean = TRUE, data = data)
margin_f8

### frage 9
modell_f9 <- glm(f0201new ~ f0901 + f0902 + f0903,  family = binomial, data = data)
summary(modell_f9)
exp(modell_f9$coefficients)
plot(allEffects(modell_f9))

margin_f9 <- logitmfx(modell_f9, atmean = TRUE, data = data)
margin_f9

### frage 10
modell_f10 <- glm(f0201new ~ f1001 + f1002 + f1003 + f1004,  family = binomial, data = data)
summary(modell_f10)
exp(modell_f10$coefficients)
plot(allEffects(modell_f10))

margin_f10 <- logitmfx(modell_f10, atmean = TRUE, data = data)
margin_f10

### frage 11
modell_f11 <- glm(f0201new ~ f11,  family = binomial, data = data)
summary(modell_f11)
exp(modell_f11$coefficients)
plot(allEffects(modell_f11))

margin_f11 <- logitmfx(modell_f11, atmean = TRUE, data = data)
margin_f11

### frage 12, geschlecht, Ref.kategorie Männer (kodiert mit 1)
modell_f12 <- glm(f0201new ~ f12,  family = binomial, data = data)
summary(modell_f12)
exp(modell_f12$coefficients)
plot(allEffects(modell_f12))

margin_f12 <- logitmfx(modell_f12, atmean = TRUE, data = data)
margin_f12

### frage 14
modell_f14 <- glm(f0201new ~ f14,  family = binomial, data = data)
summary(modell_f14)
exp(modell_f14$coefficients)
plot(allEffects(modell_f14))

margin_f14 <- logitmfx(modell_f14, atmean = TRUE, data = data)
margin_f14

### frage 15
modell_f15 <- glm(f0201new ~ f15,  family = binomial, data = data)
summary(modell_f15)
exp(modell_f15$coefficients)
plot(allEffects(modell_f15))

margin_f15 <- logitmfx(modell_f15, atmean = TRUE, data = data)
margin_f15

### frage 16
modell_f16 <- glm(f0201new ~ f1601 + f1602 + f1603,  family = binomial, data = data)
summary(modell_f16)
exp(modell_f16$coefficients)
plot(allEffects(modell_f16))

margin_f10 <- logitmfx(modell_f10, atmean = TRUE, data = data)
margin_f10

### frage 16, 2
modell_f16_2 <- glm(f0201new ~ f1601 + f1602 + f1603 +I(f1601^2)+I(f1602^2),  family = binomial, data = data)
summary(modell_f16_2)
exp(modell_f16_2$coefficients)
plot(allEffects(modell_f16_2))

margin_f10 <- logitmfx(modell_f10, atmean = TRUE, data = data)
margin_f10

### Mig, 1 ja, 2 nein
modell_Mig <- glm(f0201new ~ Mig,  family = binomial, data = data)
summary(modell_Mig)
exp(modell_Mig$coefficients)
plot(allEffects(modell_Mig))

margin_Mig <- logitmfx(modell_Mig, atmean = TRUE, data = data)
margin_Mig

### Alter
modell_alter <- glm(f0201new ~ f20,  family = binomial, data = data)
summary(modell_alter)
exp(modell_alter$coefficients)
plot(allEffects(modell_alter))

margin_alter <- logitmfx(modell_alter, atmean = TRUE, data = data)
margin_alter

### Alter in ^2
modell_alter_2 <- glm(f0201new ~  f20+I(f20^2),  family = binomial, data = data)
summary(modell_alter_2)
exp(modell_alter_2$coefficients)
plot(allEffects(modell_alter_2))

margin_alter <- logitmfx(modell_alter, atmean = TRUE, data = data)
margin_alter

anova(modell_alter, modell_alter_2)

### Schicht
modell_schicht <- glm(f0201new ~ f22,  family = binomial, data = data)
summary(modell_schicht)
exp(modell_schicht$coefficients)
plot(allEffects(modell_schicht))

margin_schicht <- logitmfx(modell_schicht, atmean = TRUE, data = data)
margin_schicht

### Autonutzung abhängig von Schicht
data$auto <- replace(daten$f0202, daten$f0202 == 1, 1) %>%
  replace(daten$f0202 == 2, 1) %>%
  replace(daten$f0202 == 3, 1) %>%
  replace(daten$f0202 == 4, 0) %>%
  replace(daten$f0202 == 5, 0)

modell_schicht_auto <- glm(auto ~ f22,  family = binomial, data = data)
summary(modell_schicht_auto)
exp(modell_schicht_auto$coefficients)
plot(allEffects(modell_schicht_auto))

### Interaktion von Abschluss und Schicht
modell_abs_sch <- glm(f0201new ~ f22*f15,  family = binomial, data = data)
summary(modell_abs_sch)
exp(modell_abs_sch$coefficients)
plot(allEffects(modell_schicht))

margin_schicht <- logitmfx(modell_schicht, atmean = TRUE, data = data)
margin_schicht

### Interaktion von Alter und Geschlecht
mod_alter_g <- glm(f0201new ~ f20*f12, family = binomial, data = data)
summary(mod_alter_g)
AIC(mod_alter_g)
