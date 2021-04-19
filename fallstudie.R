# R Commander laden, um Daten einzulesen
# install.packages("Rcmdr")
library(dplyr)
library(ggplot2)
library(Rcmdr)
# Datensatz einlesen
Dataset <- 
  readXL("C:/Users/hariskos/Documents/R/4-SK-04-WIM19/verbrauchsuebersicht_ba_steglitz-zehlendorf_2017-2018-2019_final.xlsx",
         rownames = FALSE, 
         header = TRUE, 
         na = "", 
         sheet = "immo", 
         stringsAsFactors = TRUE)

# Datensatz erkunden
str(Dataset)
glimpse(Dataset)
# Wie viele Beobachtungen hat der Datensatz? 1116
# Wie viele Variablen hat der Datensatz? 5
# Wie viele Variablen sind als Factor kodiert? 3
# Wie viele Variablen sind numerisch? 2
# Welche Variablen sind numerisch? Jahr und Verbrauch
View(Dataset)

# Numerische Statistiken
summary(Dataset)
# Wieviele Beobachtungen werden als Schulen klassifiziert? 366
Dataset %>% 
  filter(Jahr == 2017,
         Zweck == "Schule") %>% 
  summarize(Anzahl = n())

# Datensatz gruppieren und Gesamtverbrauch nach Zweck, Energie und Jahr bestimmen
Datensatz_gruppiert <- Dataset %>% 
  group_by(Zweck, Energie, Jahr) %>% 
  summarize(Verbrauch = sum(Verbrauch,
                            na.rm = TRUE)) %>% 
  print(n = Inf)

# Stromdaten
Daten_Strom <- Datensatz_gruppiert %>% 
  filter(Energie == "Strom")

# Wärmedaten
Daten_Waerme <-  Datensatz_gruppiert %>% 
  filter(Energie == "Waerme")

# Visualisierung der Stromdaten
ggplot(data = Daten_Strom) +
  geom_line(mapping = aes(x = Jahr,
                          y = Verbrauch)) +
  facet_wrap(facets = ~Zweck,
             scales = "free_y")

# Visualisierung der Wärmedaten
ggplot(data = Daten_Waerme) +
  geom_line(mapping = aes(x = Jahr,
                          y = Verbrauch)) +
  facet_wrap(facets = ~Zweck,
             scales = "free_y")

# Boxplot Strom
Daten_Boxplot_Strom <- Dataset %>% 
  filter(Energie == "Strom")

ggplot(data = Daten_Boxplot_Strom) +
  geom_boxplot(mapping = aes(x = Zweck,
                             y = Verbrauch)) +
  facet_wrap(facets = ~Jahr)

# Boxplot Wärme
Daten_Boxplot_Strom <- Dataset %>% 
  filter(Energie == "Waerme")

ggplot(data = Daten_Boxplot_Strom) +
  geom_boxplot(mapping = aes(x = Zweck,
                             y = Verbrauch)) +
  facet_wrap(facets = ~Jahr)

# Streudiagramm
a <- Dataset %>% filter(Energie == "Strom")
b <- Dataset %>% filter(Energie == "Waerme")

Strom <- a$Verbrauch 
Waerme <- b$Verbrauch

Daten_Streudiagramm <- cbind(a[, c(1, 2, 4)], Strom, Waerme)

ggplot(data = Daten_Streudiagramm) +
  geom_point(mapping = aes(x = Strom,
                           y = Waerme,
                           color = Zweck)) +
  facet_wrap(facets = ~Jahr) +
  scale_x_log10() +
  scale_y_log10()

