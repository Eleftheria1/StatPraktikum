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
data_grafik$f01new <- replace(data$f01, data$f01 == 2, 1)
data_grafik$f01new <- replace(data$f01, data$f01 == 1, 1)
data_grafik$f01new <- replace(data$f01, data$f01 == 3, 2)
data_grafik$f01new <- as.factor(data_grafik$f01new)
data_grafik$f01new <- plyr::mapvalues(data_grafik$f01new, from = c(1, 2), to = c("Ja", "Nein"))

#Extra Datensatz data_grafik f?r Grafiken
data_grafik <- data.frame(data$f01new, data$f0201,  data$f0202, data$f11, data$f14, data$f15, data$f0201new, data$f03, data$f04, data$f05, data$f06, data$f07, data$f12, data$f20, data$Mig )
data_ja_grafik <- data_ja

data_grafik$f0201 <- as.factor(data_grafik$data.f0201)
data_grafik$f0201 <-  plyr::mapvalues(data_grafik$f0201, from = c(1, 2, 3, 4, 5), to = c("(fast) t?glich", "1-3 Mal pro Woche", "1-3 Mal pro Monat", "seltener als 1 Mal im Monat", "(fast) nie"))
data_ja_grafik$f0201 <- as.factor(data_ja_grafik$f0201)
data_ja_grafik$f0201 <-  plyr::mapvalues(data_ja_grafik$f0201, from = c(1, 2, 3, 4, 5), to = c("(fast) t?glich", "1-3 Mal pro Woche", "1-3 Mal pro Monat", "seltener als 1 Mal im Monat", "(fast) nie"))
data_grafik$f0202 <- as.factor(data_grafik$data.f0202)
data_grafik$f0202 <-  plyr::mapvalues(data_grafik$f0202, from = c(1, 2, 3, 4, 5), to = c("(fast) t?glich", "1-3 Mal pro Woche", "1-3 Mal pro Monat", "seltener als 1 Mal im Monat", "(fast) nie"))
data_grafik$f11 <-plyr::mapvalues(data_grafik$data.f11, from = c(1, 2), to = c("Ja", "Nein")) 
data_grafik$f14 <-plyr::mapvalues(data_grafik$data.f14, from = c(1, 2, 3, 4), to = c("Gro?stadt", "Stadt", "Vorort einer Gro?stadt", "Dorf"))  
data_grafik$f0201new <- as.factor(data_grafik$data.f0201new)  
data_grafik$f0201new <-plyr::mapvalues(data_grafik$f0201new, from = c(1, 0), to = c("Ja", "Nein"))  
data_grafik$f15 <-plyr::mapvalues(data_grafik$data.f15, from = c(0, 1, 2, 3, 4, 5, 6), to = c("noch in schulischer Ausbildung", "kein Schulabschluss", "Volksschul-/Hauptschul-/ Mittelschulabschluss", "Realschulabschluss/Mittlere Reife", "Abschluss der Polytechnischen Oberschule (DDR)", "Fach-/Abitur", "abgeschlossenes Studium"))  
data_grafik$f03 <-plyr::mapvalues(data_grafik$data.f03, from = c(1, 2, 3, 4, 5, 6), to = c("Jahreskarte", "Monatskarte", "Wochenkarte", "Studentenkarte", "andere", "keine"))
data_grafik$f04 <-plyr::mapvalues(data_grafik$data.f04, from = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 88, 99), to = c("Ver?nderung Infrastruktur", "(beinahe) Radunfall", "Teilnahme an Radtraining", "Radfahren gelernt", "Rad verf?gbar", "kein Rad verf?gbar", "E-Bike verf?gbar", "kein E-Bike verf?gbar", "?PNV Dauerkarte verf?gbar", "keine ?PNV Dauerkarte verf?gbar", "Auto Nutzung m?glich", "keine Auto Nutzung m?glich", "andere Ver?nderung", "keine Ver?nderung")) 
data_grafik$f05 <- plyr::mapvalues(data_grafik$data.f05, from = c(1, 2, 3 ,4, 5, 6), to = c("weder davor noch danach", "angefangen Rad zu fahren", "danach h?ufiger", "danach gleich oft", "danach seltener", "danach gar nicht mehr"))  
data_grafik$f06 <-plyr::mapvalues(data_grafik$data.f06, from = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 88, 99), to = c("Umzug", "Beginn Studium/Ausbildung", "Jobwechsel", "Renteneintritt", "Arbeitslos", "Ende Partnerschaft", "Geburt 1. Kind", "Kind ausgezogen","Geburt 1. Enkel", "Krankheit", "Krankheit Partner", "anderes", "keins")) 
data_grafik$f07 <- plyr::mapvalues(data_grafik$data.f07, from = c(1, 2, 3 ,4, 5, 6), to = c("weder davor noch danach", "angefangen Rad zu fahren", "danach h?ufiger", "danach gleich oft", "danach seltener", "danach gar nicht mehr"))  
data_grafik$f12 <-plyr::mapvalues(data_grafik$data.f12, from = c(1, 2), to = c("m?nnlich", "weiblich"))
data_grafik$Mig <- plyr::mapvalues(data_grafik$data.Mig, from = c(1, 2), to = c("Ja", "Nein"))

#speichern der datensätze data_ja_grafik und data_grafik
write.csv(data_ja_grafik, "data_ja_grafik.csv")
write.csv(data_grafik, "data_grafik.csv")
#wer hat ein rad
prop.table(count(data$f01new)[,2])
data %>%
  group_by(f01new) %>%
  summarise(abs = n(), rel = abs/length(data$f01new)) %>%
  ggplot(aes(x = f01new,y = rel, fill = f01new)) + geom_bar(stat = "identity", show.legend = F) +
  xlab("Fahrradfahrer Ja/Nein") +
  ylab("Relative H?ufigkeit")


# wie oft f?hrt die person rad
ggplot(data_grafik, aes(data_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)), fill = "blue")+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Fahrrad")

# wie oft fahren die leute die ein rad haben

ggplot(data_ja_grafik, aes(data_ja_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)), fill = "blue")+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Fahrrad, die ein Fahrrad besitzt")

# wer hat ein ebike
data_ja$f0101 <- plyr::mapvalues(data_ja$f0101, from = c(1, 2), to = c("Ja", "Nein"))

data_ja %>%
  group_by(f0101) %>%
  summarise(abs = n(), rel = abs/length(data_ja$f0101)) %>%
  ggplot(aes(x = f0101,y = rel, fill = f0101 )) + geom_bar(stat = "identity", show.legend = F) +
    xlab("Ebike Ja/Nein") +
    ylab("Relative H?ufigkeit") 
  

# autonutzung
ggplot(data_grafik, aes(data_grafik$f0202,y = ..prop.., group = data_grafik$data.f01new, fill = data_grafik$data.f01new)) + 
  geom_bar( position = "dodge")+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Auto, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")

b <- table(data_grafik$f0202, data_grafik$f0201)
# auf y achse das fahrrad (zweites in table)
mosaicplot(b,  
           main="Mosaikplot f?r die Fahrrad und Autonutzung",
           xlab="Autonutzung",
           ylab="Fahrradnutzung")

# wer ?berlegt sich ein fahrrad zu kaufen
data$f11 <- as.factor(data$f11)
ggplot(data_grafik, aes(data_grafik$f11,y = ..prop.., group = data_grafik$data.f01new, fill = data_grafik$data.f01new)) + 
  geom_bar( position = "dodge")+
  xlab("Fahrrad kaufen") +
  ylab("Anzahl Personen")+
  ggtitle("Wer ?berlegt ein Fahrrad zu kaufen, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")



# gro?stadt
data$f14 <- as.factor(data$f14)
ggplot(data_grafik, aes(data_grafik$f14,y = ..prop.., group = data_grafik$data.f01new, fill = data_grafik$data.f01new)) + 
  geom_bar( position = "dodge")+
  xlab("Ort") +
  ylab("Anzahl Personen")+
  ggtitle("Wo leben die Personen, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")


 a <- table(data_grafik$f14, data_grafik$f0201)
mosaicplot(a,
           main="Mosaikplot f?r die Fahrradnutzung innerhalb unterschiedlicher Orte",
           xlab="Ort",
           ylab="Fahrradnutzung")

# schulabschluss
c <- table(data_grafik$f15, data_grafik$f0201)
mosaicplot(c,
           main="Mosaikplot f?r die Fahrradnutzung unterschieden am Schulabschluss",
           xlab="Schulabschluss",
           ylab="Fahrradnutzung")

mosaicplot(table(data_grafik$f15, data_grafik$f0201new),
           main="Mosaikplot f?r die Fahrradfahrer unterschieden am Schulabschluss",
           xlab="Schulabschluss",
           ylab="Fahrradfahrer")

# ?PNV
mosaicplot(table(data_grafik$f03, data_grafik$f0201new),
           main="Mosaikplot f?r die Fahrradfahrer unterschieden an der Nutzung des ?PNV",
           xlab="Dauerkarte",
           ylab="Fahrradfahrer")

# ereignis
mosaicplot(table(data_grafik$f04, data_grafik$f05),
           main="Mosaikplot f?r die Auswirkungen der Fahrradnutzung von dem letzten Ereignis",
           xlab="Ereignis",
           ylab="Fahrradnutzung")
mosaicplot(table(data_grafik$f06, data_grafik$f07),
           main="Mosaikplot f?r die Auswirkungen der Fahrradnutzung von dem letzten Ereignis",
           xlab="Ereignis",
           ylab="Fahrradnutzung")

# geschlecht
data$f12 <- as.factor(data$f12)
data$f0201new <- as.factor(data$f0201new)
ggplot(data_grafik, aes(data_grafik$f0201new,y = ..prop.., group =data_grafik$f12, fill = data_grafik$f12)) + 
  geom_bar( position = "dodge")+
  xlab("Fahrradfahrer") +
  ylab("Anzahl Personen")+
  ggtitle("Wer ist Fahrradfahrer, unterschieden durch Geschlecht")+
  scale_fill_discrete(name = "Geschlecht")

# alter spinogramm
plot(f0201new ~ data.f20, data = data_grafik,
     xlab = "Alter", ylab = "Fahrradfahrer", main = "Fahrradnutzung bedingt durch das Alter")

#migrationshintergrund
data$Mig <- as.factor(data$Mig)
ggplot(data_grafik, aes(data_grafik$Mig,y = ..prop.., group =data_grafik$f0201new, fill = data_grafik$f0201new)) + 
  geom_bar( position = "dodge")+
  xlab("Migrationshintergrund") +
  ylab("Anzahl Personen")+
  ggtitle("Wer ist Fahrradfahrer, unterschieden am Migrationshintergrund")+
  scale_fill_discrete(name = "Fahrradfahrer")



