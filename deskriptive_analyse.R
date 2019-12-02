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
  xlab("Fahrradfahrer Ja/Nein") +
  ylab("Relative H?ufigkeit")


# wie oft f?hrt die person rad
ggplot(data_grafik, aes(data_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)))+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Fahrrad")

# wie oft fahren die leute die ein rad haben

ggplot(data_ja_grafik, aes(data_ja_grafik$f0201)) + geom_bar(aes(y=..count../sum(..count..)), fill = "blue")+
  xlab("H?ufigkeit") +
  ylab("Anzahl Personen")+
  ggtitle("Wie oft f?hrt eine Person Fahrrad, die ein Fahrrad besitzt")

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
ggplot(data_grafik, aes(data_grafik$f11,y = ..prop.., group = data_grafik$data.f01new, fill = data_grafik$data.f01new)) + 
  geom_bar( position = "dodge")+
  xlab("Fahrrad kaufen") +
  ylab("Anteil Personen")+
  ggtitle("Wer ?berlegt ein Fahrrad zu kaufen, unterschied zwischen Fahrradbesitzern")+
  scale_fill_discrete(name = "Fahrradbesitzer")



# gro?stadt
ggplot(data_grafik, aes(data_grafik$f14,y = ..prop.., group = data_grafik$data.f01new, fill = data_grafik$data.f01new)) + 
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

ggplot(data_grafik) + geom_mosaic(aes(product(f15,f0201new), fill = f15), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrradnutzung unterschieden am Schulabschluss") +
  ylab("Schulabschluss") +
  xlab("Fahrradnutzung")


# ?PNV
ggplot(data_grafik) + geom_mosaic(aes(product(f03,f0201new), fill = f03), show.legend = F) +
  ggtitle("Mosaikplot f?r die Fahrradfahrer unterschieden an der Nutzung des ?PNV") +
  ylab("Dauerkarte") +
  xlab("Fahrradnutzung")


# geschlecht
ggplot(data_grafik, aes(data_grafik$f0201new,y = ..prop.., group =data_grafik$f12, fill = data_grafik$f12)) + 
  geom_bar( position = "dodge")+
  xlab("Fahrradfahrer") +
  ylab("Anteil Personen")+
  ggtitle("Wer ist Fahrradfahrer, unterschieden durch Geschlecht")+
  scale_fill_discrete(name = "Geschlecht")

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

