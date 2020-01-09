library("MASS")
library("glmnet")
library("mboost")
library("leaps")
#test mit frage 8 kodiert

# test mit frage 8
backward_AIC <- stepAIC(mod_full, direction = "backward", trace = F)
backward_AIC$anova #The backward elimination procedure eliminated variables
                   # f0803, f0809, f0827...
                   # kept variables: Final Model:
                   #f0201new ~ f0101 + f05 + f07 + f0801 + f0803 + f0807 + f0808 + 
                  #f0810 + f0811 + f0812 + f0813 + f0814 + f0815 + f0818 + f0819 + 
                  #f0821 + f0823 + f0824 + f0825 + f0827 + f0902 + f0903 + f1003 + 
                  # f11 + f12 + f1603 + f180201 + f180202 + Mig + f0202new + 
                  # f0203new + f20
                  # stepAIC removes the Multicollinearity if it exists, from the model 
mod_finalneu <- glm(f0201new ~ f0101 + f05 + f07 + f0801 + f0803 + f0807 + f0808 + 
                      f0810 + f0811 + f0812 + f0813 + f0814 + f0815 + f0818 + f0819 + 
                      f0821 + f0823 + f0824 + f0825 + f0827 + f0902 + f0903 + f1003 + 
                      f11 + f12 + f1603 + f180201 + f180202 + Mig + f0202new + 
                      f0203new + f20, family = binomial, data = data)
summary(mod_finalneu)

stepwise_AIC <- stepAIC(modell_frage8, direction = "both", trace = F)
stepwise_AIC1 <- stepAIC(mod_full, direction = "backward", trace = T)

stepwise_AIC$anova

finalmodel <- glm(y ~ f0101_2 + f0102_4 + f0102_6 + f0102_7 + f04_2 + f04_7 + f04_10 + 
                     f05_2 + f05_3 + f05_4 + f05_5 + f05_6 + f06_2 + f06_3 + f06_4 + 
                     f06_6 + f06_8 + f06_9 + f07_2 + f07_3 + f07_4 + f07_5 + f1001_1 + 
                     f1003_1 + f11_2 + f12_2 + f15_2 + f1603_2 + f1801_2 + f180202_2 + 
                     f22_2 + f22_3 + f22_5 + f0202new_3 + f0202new_4 + f0203new_2 + 
                     f0203new_3 + f0203new_4 + f0203new_5 + f0204new_3 + f0204new_4 + 
                     f0801_3 + f0801_4 + f0802_2 + f0802_3 + f0803_3 + f0804_3 + 
                     f0804_4 + f0805_2 + f0805_3 + f0805_4 + f0806_2 + f0806_3 + 
                     f0807_3 + f0807_4 + f0808_3 + f0808_4 + f0809_2 + f0810_2 + 
                     f0810_3 + f0810_4 + f0811_3 + f0811_4 + f0812_2 + f0812_3 + 
                     f0812_4 + f0813_2 + f0813_3 + f0813_4 + f0814_3 + f0814_4 + 
                     f0815_2 + f0815_3 + f0815_4 + f0816_4 + f0817_4 + f0818_2 + 
                     f0819_3 + f0819_4 + f0820_3 + f0821_3 + f0821_4 + f0822_2 + 
                     f0822_3 + f0823_3 + f0823_4 + f0824_2 + f0824_4 + f0825_2 + 
                     f0825_4 + f0826_3 + f0827_2 + f0827_3 + f0827_4 + f0828_2 + 
                     f1602 + f20, family = binomial, data = dummy_data
)
summary(finalmodel) #bei trace = True kommt dieses modell als letztes mit geringstem aic
# 1670.986 im vergleich volles modell: 1784.484 
#macht aber nicht zu ende wegen fehlermeldung
#Fehler in stepAIC(mod_full, direction = "backward", trace = T) : 
## Anzahl der benutzten Zeilen hat sich geändert: Fehlende Werte entfernen?
#was bedeutet fehlermeldung? wegen NAs !!!

#deswegen versuch ohne NAs
stepwise_AIC2 <- stepAIC(mod_full_NA, direction = "backward", trace = T)
stepwise_AIC2$anova
finalmodel_Na <- glm(y ~ f0101_2 + f0102_4 + f0102_6 + f0102_7 + f04_2 + f04_7 + f04_10 + 
                       f05_2 + f05_3 + f05_4 + f05_5 + f05_6 + f06_4 + f07_2 + f07_3 + 
                       f07_4 + f07_5 + f1003_1 + f11_2 + f12_2 + f15_2 + f1603_2 + 
                       f1801_2 + f180202_2 + f0202new_3 + f0202new_4 + f0203new_2 + 
                       f0203new_3 + f0203new_4 + f0203new_5 + f0204new_3 + f0801_3 + 
                       f0801_4 + f0802_2 + f0802_3 + f0803_3 + f0804_3 + f0804_4 + 
                       f0805_2 + f0805_3 + f0805_4 + f0808_3 + f0808_4 + f0809_2 + 
                       f0810_2 + f0810_3 + f0810_4 + f0811_4 + f0812_3 + f0812_4 + 
                       f0813_2 + f0813_3 + f0813_4 + f0814_3 + f0814_4 + f0815_2 + 
                       f0815_3 + f0815_4 + f0816_4 + f0817_4 + f0818_2 + f0819_3 + 
                       f0819_4 + f0821_3 + f0821_4 + f0822_2 + f0823_3 + f0823_4 + 
                       f0825_4 + f0826_3 + f20,
                     family = binomial, data = data_NA)
summary(finalmodel_Na) # 72 variables used from 162
AIC(finalmodel_Na) #1646.9

#neuer datensatz mit ausgewählten variablen
data_final <- dplyr::select(data_NA, f0101_2 , f0102_4 , f0102_6 ,f0102_7 ,f04_2 , f04_7 , f04_10 , 
                       f05_2 , f05_3 , f05_4 ,f05_5 ,f05_6 , f06_4 , f07_2 , f07_3 ,
                       f07_4 , f07_5 , f1003_1 , f11_2 ,f12_2 , f15_2 , f1603_2 ,
                       f1801_2 , f180202_2 , f0202new_3 , f0202new_4 , f0203new_2 , 
                       f0203new_3 , f0203new_4 , f0203new_5 , f0204new_3 , f0801_3 , 
                       f0801_4 , f0802_2 , f0802_3 , f0803_3 , f0804_3 , f0804_4 , 
                       f0805_2 , f0805_3 , f0805_4 , f0808_3 , f0808_4 , f0809_2 , 
                       f0810_2 , f0810_3 , f0810_4 , f0811_4 , f0812_3 , f0812_4 , 
                       f0813_2 , f0813_3 , f0813_4 , f0814_3 , f0814_4 , f0815_2 , 
                       f0815_3 , f0815_4 , f0816_4 , f0817_4 , f0818_2 , f0819_3 , 
                       f0819_4 , f0821_3 , f0821_4 , f0822_2 , f0823_3 , f0823_4 , 
                       f0825_4 , f0826_3 , f20 , y)

stepwise_fin <- stepAIC(finalmodel_Na, direction = "both", trace = F)
stepwise_fin$anova # shows that direction forward and both give same results




fit1 <- glm(f0201new ~ ., data = frage8_data, family = binomial)
fit2 <- glm(f0201new ~ 1, data= frage8_data, family = binomial)
forward_AIC <- stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
forward_AIC$anova
#take final model
finmod_AIC <- glm(f0201new ~ f0814 + f0819 + f0812 + f0807 + f0811 + f0815 + f0801 + 
                    f0808 + f0810 + f0813 + f0821 + f0817 + f0825 + f0823 + f0820 + 
                    f0805 + f0816, family = binomial, data = frage8_data)
summary(finmod_AIC) # AIC = 3245.4



backward_BIC <- stepAIC(modell_frage8, direction = "both", trace = F, k = log(nrow(frage8_data)))
backward_BIC$anova
# take final model
finalmod_BIC <- glm(f0201new ~ f0801 + f0807 + f0808 + f0810 + f0811 + f0812 + f0813 + 
                      f0814 + f0815 + f0817 + f0819 + f0821, family = binomial, data = frage8_data)
summary(finalmod_BIC) # AIC = 3252.9
summary(modell_frage8) # AIC = 3261.4 without feature selection



####################
#Lasso
#benutze datensatz von aicsep final modell
modf <- glmnet(model.matrix(~ . - y, data = data_final), data_final$y, family = "binomial")
plot(modf,xvar="lambda",label=T)
library(ISLR)
ergf<-cv.glmnet(model.matrix(~ . - y ,data=data_final),data_final$y,family="binomial")
abline(v=log(ergf$lambda.1se))
modff <- glmnet(model.matrix(~ . - y ,data=data_final),data_final$y,family="binomial",lambda=ergf$lambda.1se)
coefficients(modff)
which(coefficients(modff) != 0) 
modfff <- glm(y ~ f0101_2 + f0102_4 + f0102_6 + f0102_7 + f04_2 + f04_7 + f04_10 + 
      f05_2 + f05_3 + f05_4  + f05_6 + f06_4 + f07_2 + f07_3 + 
      f07_4 + f07_5 + f1003_1 + f11_2 + f12_2 + f15_2 + f1603_2 + 
      f1801_2 + f180202_2 + f0202new_3 + f0202new_4 + f0203new_2 + 
      f0203new_3 + f0203new_4 + f0204new_3 + f0801_3 + 
      f0801_4 + f0802_2 + f0802_3 + f0803_3 + f0804_3  + 
     f0805_3 + f0808_3 + f0808_4 + f0809_2 + 
       f0810_3 + f0810_4 + f0811_4 + f0812_3 + f0812_4 + 
      f0813_2 + f0813_3 + f0813_4 + f0814_3 + f0814_4 + f0815_2 + 
      f0815_3 + f0815_4 + f0816_4 + f0817_4 + f0818_2 + f0819_3 + 
      f0819_4 + f0821_3 + f0821_4 + f0822_2 + f0823_3 + f0823_4 + 
      f0825_4 + f0826_3 + f20,
    family = binomial, data = data_NA)
summary(modfff) # 1663.2


mod2<- glmnet(model.matrix(~ . - f0201new ,data=data),data$f0201new,family="binomial")
plot(mod2,xvar="lambda",label=T)
library(ISLR)
erg2<-cv.glmnet(model.matrix(~ . - f0201new ,data=data),data$f0201new,family="binomial")
abline(v=log(erg2$lambda.1se))
mod3<- glmnet(model.matrix(~ . - f0201new ,data=data),data$f0201new,family="binomial",lambda=erg2$lambda.1se)
coefficients(mod3)
which(coefficients(mod3) != 0) # wie viele variablen wurden gewählt ?
                               # 1 variables selected 
                              # by running mod3 -> getting DF = 51 because of p+1

model_las <- glm(y~ f0101_2 + f0102_6 + f04_2 + f04_4 + f04_10 +
                   f05_2 + f05_3 + f05_4 + f05_6 + f06_9 + f07_3+
                   f07_4 + f07_6 + f11_2 + f12_2 + f1603_2 +
                   f180202_2 + f0202new_4 + f0203new_3 + f0203new_4+
                   f0801_3 + f0801_4 + f0802_4 + f0805_3 +
                   f0807_2 + f0807_4 + f0808_3 + f0808_4 +
                   f0810_3 + f0810_4 + f0811_4 + f0812_3 + f0812_4 +
                   f0813_2 + f0813_4 + f0814_2 + f0814_4 + f0815_4 +
                   f0817_2 + f0817_4 + f0818_2 + f0819_4 + f0820_3 +
                   f0821_3 +f0821_4 + f0823_3 + f0823_4 + f0825_4 + 
                   f0826_3 + f20, family = binomial, data = data_NA)

summary(model_las) # AIC 1713.8


tLL <- mod3$nulldev - deviance(mod3)
k <- mod3$df
n <- mod3$nobs

AICs <- -tLL + 2*k + 2*k*(k+1)/(n-k-1)
AICs #-3867.239

BIC <- log(n)*k - tLL
BIC
###########################
#### Boosting 

mod4<-glmboost(f0201new ~ . -f0201new ,data=data,family="Binomial"(type="glm"),control = boost_control(mstop = 3000))
x11()
old_par <- par(mar = c(5,5,5,7.5))
plot(mod4)
summary(mod4)
mod4


# take final model
model_boost <- glm(f0201new ~ f0801 + f0802 + f0804 + f0805 + f0807 + f0808 + f0810
                   + f0811 + f0812 + f0813 + f0814 + f0815 + f0817 + f0819 + f0821 
                   + f0823 + f0825, family = binomial, data = frage8_data)
summary(model_boost) # AIC = 3249.2


model_fullboost <- glm(y ~  f0101_2   +   f0102_6   +   f0102_7     +   f04_2  +     f04_4    +    f04_5 +
                         f04_6    +   f04_10   +     f05_3     +   f05_4   +     f05_5    +    f05_6    +    f06_3 +
                         f06_9    +    f07_2    +    f07_3     +   f07_4   +     f07_6    +    f11_2     +   f12_2+
                         f15_6    +  f1603_2   + f180202_2 +  f0202new_4  + f0203new_3  + f0203new_4  +    f0801_3 +
                         f0801_4    +  f0802_3    +  f0802_4    +  f0803_2  +    f0803_3   +   f0805_3   +   f0807_2+
                         f0807_3    +  f0807_4    +  f0808_3    +  f0808_4    +  f0809_2    +  f0810_3  +    f0810_4+
                         f0811_4    +  f0812_3    +  f0812_4   +   f0813_2   +   f0813_3   +   f0813_4   +   f0814_2+
                         f0814_3   +   f0814_4   +   f0815_4   +   f0817_2   +   f0817_4  +    f0818_2   +   f0819_2+
                         f0819_4   +   f0820_3    +  f0821_3    +  f0821_4   +   f0823_3    +  f0823_4    +  f0825_4+
                         f0826_2  +    f0826_3    +      f20 ,
                       family = binomial, data = dummy_data)
summary(model_fullboost)
AIC(model_fullboost)

cvm4 <- cvrisk(mod4)
x11()
old_par <- par(mar = c(5,5,5,7.5))
plot(cvm4)
cvm4
#### 