library("boot")
library("corrplot")
library("caret")
library(e1071)
#library(fastDummies)
library(mfx)
library("dplyr")

setwd("C:/Users/elefi/OneDrive/Desktop/5FS/statistisches praktikum")
setwd("C:/Users/Eleftheria/OneDrive/Desktop/5FS/statistisches praktikum")
data <- read_sav("Daten_RadAktiv_final_Kunde.sav")
data_reg <- read.csv("data_reg.csv")
frage8_data <- read.csv("frage8_data.csv")
dummy_f8_NA <- read.csv("dummy_f8_NA.csv")



modell_frage3 <- glm(f0201new ~ f03new, family = binomial, data = data_reg)
summary(modell_frage3)


#####################
# frage 2
modell_frage2 <- glm(f0201new ~ f0202 + f0203 + f0204, family = binomial, data = data_reg)
summary(modell_frage2)
exp(modell_frage2$coefficients)

####################
# GLM mit neuen variablen
modell_frage2new <- glm(f0201new ~ f0202new + f0203new + f0204new, family = binomial, data = data_reg) 
summary(modell_frage2new)
exp(modell_frage2new$coefficients)
########################################
modell10 <- glm(f0201new ~ f1001 + f1002 + f1003 + f1004, family = binomial, data = data_reg)
summary(modell10)
exp(modell10$coefficients)

## marginale effekte
logitmfx(f0201new ~ f1001 + f1002 + f1003 + f1004, data = data_reg )


#### frage 11

modell11 <- glm(f0201new ~ f11_new, family = binomial, data = data_reg)
summary(modell11)
exp(modell11$coefficients)

## geschlecht
modell12 <- glm(f0201new ~ f12, family = binomial, data = data_reg)
summary(modell12)
exp(modell12$coefficients)
logitmfx(f0201new ~ f12, data_reg)

## alter
modell_alter <- glm(f0201new ~ f20, family = binomial, data = data_reg)
summary(modell_alter)
exp(modell_alter$coefficients)

modell_alter_geschlecht <- glm(f0201new ~ f20 + f12, family = binomial, data = data_reg)
summary(modell_alter_geschlecht)
exp(modell_alter_geschlecht$coefficients)

#### frage 16

# data$f1603new <- replace(data$f1603, data$f1603 == 2, 0)
modell_f16 <- glm(f0201new ~ f1601 + f1602 + f1603,  family = binomial, data = data_reg)
summary(modell_f16)
#migrationshintergund
modell_mig <- glm(f0201new ~ Mig, family = binomial, data = data_reg)
summary(modell_mig)
exp(modell_mig$coefficients)

modell1 <- glm(f0201new ~ f0202, family = binomial, data = data_reg)
summary(modell1)
exp(modell1$coefficients)
cor(data_reg$f0201new, data_reg$f0202)


modell_frage8 <- glm(f0201new ~ ., family = binomial, data = frage8_data)
summary(modell_frage8)
#modell frage 8 aber kodiert
model_f8 <- glm(y ~ . -X , family = binomial, data = dummy_f8_NA)
summary(model_f8)
####################################

modell_f4 <- glm(y ~ f04_2+f04_3+f04_4+f04_5+f04_6+f04_7+f04_8+f04_9+f04_10+f04_11+f04_12,  family = binomial, data = data_dummy)
summary(modell_f4)
exp(modell_f4$coefficients)
########################################

modell_f6 <- glm(y ~ f06_2+f06_3+f06_4+f06_5+f06_6+f06_7+f06_8+f06_9+f06_10+f06_11,  family = binomial, data = data_dummy)
summary(modell_f6)
exp(modell_f6$coefficients)

#############
# dummykodierung großstadt
modell_f14 <- glm(y ~ f14_2+f14_3+f14_4,  family = binomial, data = data_dummy)
summary(modell_f14)
exp(modell_f14$coefficients)

########
#schulabschluss
modell_f15 <- glm(y ~ f15_1+f15_2+f15_3+f15_4+f15_5+f15_6,  family = binomial, data = data_dummy)
summary(modell_f15)
exp(modell_f15$coefficients)

# schichten 
modell_f22 <- glm(y ~ f22_2+f22_3+f22_4+f22_5, family = binomial, data = data_dummy)
summary(modell_f22)
exp(modell_f22$coefficients)




