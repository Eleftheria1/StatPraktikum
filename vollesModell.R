library("arm")
setwd("C:/Users/Eleftheria/OneDrive/Desktop/5FS/statistisches praktikum/Daten")
dummy_data <- read.csv("dummy_data.csv")
dummy_data <- dummy_data[, -c(56: 71)]
dummy_data$f0201new_1 <- NULL
dummy_data$X <- NULL
dummy_data$f0204new_5 <- NULL
dummy_data$f01new_2 <- NULL
#dummy_data[is.na(dummy_data)] <- 0
data_NA <- na.omit(dummy_data)

mod_full <- glm(y ~ . , family = binomial, data = dummy_data) 
summary(mod_full)
AIC(mod_full)

mod_full_NA <- glm(y ~ . , family = binomial, data = data_NA) 
summary(mod_full_NA)
AIC(mod_full_NA)

mod_full1 <- bayesglm(y ~ . , family = binomial, data = dummy_data)
summary(mod_full1)
