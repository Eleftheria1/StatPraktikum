library(haven)
library(dplyr)
library(klaR)

setwd("C:/Users/Lisa/Documents/StatPraktikum")
data <- read.csv("data_bearbeitung.csv")
data_alt <- read.csv("data.csv")

# Faktoranalyse im Datensatz speichern
data$umwelt <- round(1/4*(data$f0820+data$f0821+data$f0822+data$f0822))
data$infra <- round(1/3*(data$f0801+data$f0824+data$f0825))
data$image <- round(1/2*(data$f0816+data$f0818))
data$zweck <- round(1/4*(data$f0806+data$f0810+data$f0812+data$f0813))
data$umfeld <- round(0.5*(data$f0814 - data$f0815))
data$infra2 <- round(1/3*(data$f0826 - data$f0827 + data$f0828))
data$anstr <- round(1/3*(data$f0805 + data$f0807 + data$f0809))

data <- dplyr::select(data, -c(f0820, f0821,f0822,f0822, f0801, f0824, f0825, f0816,
                            f0818, f0806, f0810, f0812, f0813, f0814, f0815,
                            f0826, f0827, f0828, f0805, f0807, f0809))

data_NA <- na.omit(data)
data[is.na(data)] <- 35                                                                #NA ersetzen
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)     #als Faktor speichern
data$X <- NULL                                                                          #x l?schen
data$f20 <- data_alt$f20                                                                #f20 ersetzen (Alter)
data$f1601 <- data_alt$f1601                                                            #f1601 ersetzen (Anzahl Personen)
data$f1602 <- data_alt$f1602                                                            #f1602 ersetzen (Anzahl Kinder)
data[is.na(data)] <- 35  #Bei den neuen Variablen NA ersetzen

#data$f20 <- replace(data$f20, data$f20 %in% c(10:30), 1) %>%
#replace(data$f20 %in% c(31:50), 2) %>%
#replace(data$f20 %in% c(51:90), 3)



# data transformation
data_cluster <- data_NA %>%
  filter(f0201new == "0") %>% #fahrradfahrer raus
  dplyr::select(-f0201new)

library(klaR)
cluster.results <- kmodes(data_cluster, 3, iter.max = 10, weighted = FALSE)
cluster.results

data_cluster2 <- cbind(data_cluster, cluster.results$cluster)
