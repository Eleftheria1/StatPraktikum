library(haven)


setwd("D:/Christina/Uni/5.Semester/stat. Praktikum")
data <- read.csv("data_bearbeitung.csv")
data_alt <- read_sav("Daten_RadAktiv_final_Kunde.sav")

data[is.na(data)] <- 35                                                                 #NA ersetzen
data[sapply(data, is.numeric)] <- lapply(data[sapply(data, is.numeric)], as.factor)     #als Faktor speichern
data$X <- NULL                                                                          #x löschen
data$f20 <- data_alt$f20                                                                #f20 ersetzen (Alter)
data$f1601 <- data_alt$f1601                                                            #f1601 ersetzen (Anzahl Personen)
data$f1602 <- data_alt$f1602                                                            #f1602 ersetzen (Anzahl Kinder)
data[is.na(data)] <- 35                                                                 #Bei den neuen Variablen NA ersetzen

