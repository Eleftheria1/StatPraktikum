library("MASS")
library("glmnet")
library("mboost")
#test mit frage 8 kodiert
backward_AIC <- stepAIC(model_f8, direction = "backward", trace = F)
backward_AIC$anova
mod_lisa <- glm(y ~ f0801_3 + f0801_4 + f0802_4 + f0803_2 + f0803_4 + f0804_3 + 
  f0805_3 + f0806_3 + f0807_2 + f0807_3 + f0807_4 + f0808_3 + 
  f0808_4 + f0809_2 + f0809_4 + f0810_2 + f0810_3 + f0810_4 + 
  f0811_2 + f0811_3 + f0811_4 + f0812_2 + f0812_3 + f0812_4 + 
  f0813_2 + f0813_3 + f0813_4 + f0814_3 + f0814_4 + f0815_2 + 
  f0815_3 + f0815_4 + f0816_2 + f0817_4 + f0818_2 + f0819_3 + 
  f0819_4 + f0820_4 + f0821_3 + f0821_4 + f0823_3 + f0823_4 + 
  f0824_2 + f0825_3 + f0825_4 + f0826_3, family = binomial, data = dummy_f8_NA)
summary(mod_lisa)


# test mit frage 8
frage8_data <- read.csv("frage8_data.csv")
modell_frage8 <- glm(f0201new ~ ., family = binomial, data = frage8_data)
AIC(modell_frage8)
backward_AIC <- stepAIC(modell_frage8, direction = "backward", trace = T)
backward_AIC$anova #The backward elimination procedure eliminated variables
                   # f0803, f0809, f0827...
                   # kept variables: Final Model:
                   #f0201new ~ f0801 + f0805 + f0807 + f0808 + f0810 + f0811 
                   #  + f0812 + f0813 + f0814 + f0815 + f0816 + f0817 + f0819 + f0820 + f0821 + 
                   # f0823 + f0825
                  # stepAIC removes the Multicollinearity if it exists, from the model 
mod_8 <- glm(f0201new ~ f0801 + f0805 + f0807 + f0808 + f0810 + f0811 + 
               f0812 + f0813 + f0814 + f0815 + f0816 + f0817 + f0819 + f0820 + 
               f0821 + f0823 + f0825, family = binomial, data = frage8_data)
summary(mod_8)




stepwise_AIC <- stepAIC(modell_frage8, direction = "both", trace = F)
stepwise_AIC$anova

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
mod2<- glmnet(model.matrix(~ . - f0201new ,data=frage8_data),frage8_data$f0201new,family="binomial")
plot(mod2,xvar="lambda",label=T)
library(ISLR)
erg2<-cv.glmnet(model.matrix(~ . - f0201new ,data=frage8_data),frage8_data$f0201new,family="binomial")
abline(v=log(erg2$lambda.1se))
mod3<- glmnet(model.matrix(~ . - f0201new ,data=frage8_data),frage8_data$f0201new,family="binomial",lambda=erg2$lambda.1se)

coefficients(mod3)

###########################
#### Boosting 

mod4<-glmboost(f0201new~ . -f0201new ,data=frage8_data,family="Binomial"(type="glm"),control = boost_control(mstop = 1000))
plot(mod4)
summary(mod4)
# take final model
model_boost <- glm(f0201new ~ f0801 + f0802 + f0804 + f0805 + f0807 + f0808 + f0810
                   + f0811 + f0812 + f0813 + f0814 + f0815 + f0817 + f0819 + f0821 
                   + f0823 + f0825, family = binomial, data = frage8_data)
summary(model_boost) # AIC = 3249.2


cvm4 <- cvrisk(mod4)
plot(cvm4)
cvm4
#### 