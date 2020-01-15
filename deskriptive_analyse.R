library(haven)
library(tidyr)
library(tidyverse)
library(ggmosaic)
library(corrplot)


setwd("C:/Users/elefi/OneDrive/Desktop/5FS/statistisches praktikum")
setwd("C:/Users/Eleftheria/OneDrive/Desktop/5FS/statistisches praktikum")
data_ja_grafik <- read.csv("data_ja_grafik.csv")
data_grafik <- read.csv("data_grafik.csv")
data_dummy <- read.csv("dummy_data.csv")
frage8_data <- read.csv("frage8_data.csv")

#wer hat ein rad
data_grafik %>%
  group_by(f01new) %>%
  summarise(abs = n(), rel = abs/length(data_grafik$f01new)) %>%
  ggplot(aes(x = f01new,y = rel, fill = f01new)) + geom_bar(stat = "identity", show.legend = F) +
  xlab("Fahrradbesitzer") +
  ylab("Anteil Personen") + ggtitle("Wer besitzt ein Fahrrad")+theme_minimal()


# wie oft f?hrt die person rad
ggplot(data_grafik, aes(data_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)))+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Fahrrad")

# wie oft fahren die leute die ein rad haben
data_ja_grafik$f0201 <- ordered(data_ja_grafik$f0201, levels=c("(fast) nie","seltener als 1 Mal im Monat","1-3 Mal pro Monat", "1-3 Mal pro Woche","(fast) täglich"))
ggplot(data_ja_grafik, aes(data_ja_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)), fill = "cadetblue")+
  xlab("Häufigkeit") +
  ylab("Anteil Personen")+
  ggtitle("Wie oft fährt eine Person Fahrrad, die ein Fahrrad besitzt")+
  theme_minimal()

# wer hat ein ebike

data_ja_grafik %>%
  group_by(f0101) %>%
  summarise(abs = n(), rel = abs/length(data_ja_grafik$f0101)) %>%
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


ggplot(data_grafik) + geom_mosaic(aes(product(f0201, f0202), fill = f0201), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrrad und Autonutzung") +
  ylab("Fahrradnutzung") +
  xlab("Autonutzung")

# wer ?berlegt sich ein fahrrad zu kaufen
ggplot(data_grafik, aes(data_grafik$f11,y = ..prop.., group = data_grafik$f01new, fill = data_grafik$f01new)) + 
  geom_bar( position = "dodge")+
  xlab("Fahrrad kaufen") +
  ylab("Anteil Personen")+
  ggtitle("Wer ?berlegt ein Fahrrad zu kaufen, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")



# gro?stadt
ggplot(data_grafik, aes(data_grafik$f14,y = ..prop.., group = data_grafik$f01new, fill = data_grafik$f01new)) + 
  geom_bar( position = "dodge")+
  xlab("Ort") +
  ylab("Anteil Personen")+
  ggtitle("Wo leben die Personen, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")




ggplot(data_grafik) + geom_mosaic(aes(product(f0201, f14), fill = f0201), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrradnutzung innerhalb unterschiedlicher Orte") +
  ylab("Fahrradnutzung") +
  xlab("Ort")

# schulabschluss
ggplot(data_grafik) + geom_mosaic(aes(product(f0201, f15), fill = f0201), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrradnutzung unterschieden am Schulabschluss") +
  ylab("Fahrradnutzung") +
  xlab("Schulabschluss")

data_grafik$f15 <- as.factor(data_grafik$f15)
data_grafik$f0201new <- as.factor(data_grafik$f0201new)
data_grafik$f15 <- ordered(data_grafik$f15, levels=c("noch in schulischer Ausbildung", "kein Schulabschluss", "Volksschul-/Hauptschul-/ Mittelschulabschluss", "Realschulabschluss/Mittlere Reife", "Abschluss der Polytechnischen Oberschule (DDR)", "Fach-/Abitur", "abgeschlossenes Studium"))
data_grafik$f0201new <- ordered(data_grafik$f0201new, levels=c("Ja","Nein"))

ggplot(data_grafik) + geom_mosaic(aes(product(f15,f0201new), fill = f15), show.legend = F) +
  ggtitle("Fahrradnutzung unteschieden am Schulabschluss") +
  ylab("Schulabschluss") +
  xlab("Fahrradnutzung")+ theme_minimal()


# ?PNV
ggplot(data_grafik) + geom_mosaic(aes(product(f03,f0201new), fill = f03), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrradfahrer unterschieden an der Nutzung des ?PNV") +
  ylab("Dauerkarte") +
  xlab("Fahrradnutzung")


# geschlecht
ggplot(data_grafik,aes(data_grafik$f0201new,y = ..prop.., group =data_grafik$f12,fill = data_grafik$f12)) + 
  geom_bar(position = "dodge")+
  xlab("Fahrradfahrer") +
  ylab("Anteil Personen")+
  ggtitle("Wer ist Fahrradfahrer, unterschieden durch Geschlecht")+
  scale_fill_discrete(name = "Geschlecht") + theme_minimal()

# alter spinogramm
spineplot(f0201new ~ data.f20, data = data_grafik, col = c("aquamarine1", "cadetblue"),
     xlab = "Alter", ylab = "Fahrradfahrer", main = "Fahrradnutzung bedingt durch das Alter")

#migrationshintergrund
ggplot(data_grafik, aes(data_grafik$Mig,y = ..prop.., group =data_grafik$f0201new, fill = data_grafik$f0201new)) + 
  geom_bar( position = "dodge")+
  xlab("Migrationshintergrund") +
  ylab("Anteil Personen")+
  ggtitle("Wer ist Fahrradfahrer, unterschieden am Migrationshintergrund")+
  scale_fill_discrete(name = "Fahrradfahrer")

#frage 8 korrelation
corrplot(cor(frage8_data), type = "upper", method = "square")

