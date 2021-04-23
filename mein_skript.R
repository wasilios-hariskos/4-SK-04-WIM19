# Pakete installieren ----

# Pakete müssen am Anfang einmal installiert werden. Dazu kommentieren Sie diese aus (# entfernen)
# Nach der Installation können Sie die Zeilen wieder kommentieren (# hinzufügen)
# install.packages("vctrs") #ggf. beim Mac vorher laufen lassen
# install.packages("gapminder")
# install.packages("dplyr")
# install.packages("ggplot2")

# Pakete laden ----
library(gapminder) # Datensatz
library(dplyr) # data plyr / Datenzange / Pipe Operator %>% ist in diesem Paket
library(ggplot2)

# Den Datensatz erkunden ----
gapminder # Ausgabe
help(gapminder) # Hilfe
View(gapminder) # Excel-ähnliche Ausgabe
str(gapminder) # Base-R Überblick
glimpse(gapminder) # Tidyverse Überblick

# filter(): Den Datensatz filtern ----
## 1 Kriterium
gapminder %>% 
  filter(country == "Germany") # Erste Bedingung

## 2 Kriterien
gapminder %>%
  filter(continent == "Europe", # Erste Bedingung (das Komma ist ein UND)
         year == 2002) # Zweite Bedingung

# arrange(): Den Datensatz sortieren ----
# Beispiel 1
gapminder %>%
  filter(year == 1997, 
         continent == "Europe") %>% 
  arrange(desc(lifeExp)) %>% # sortieren, Reihenfolge umkehren mit desc() oder -lifeExp
  print(n = Inf) # geben 20 zeilen aus, mit Inf (= Infinity) alle Zeilen

#Bespiel 2
countries <- c("Germany", # Vektor mit Ländern
               "Sweden", 
               "Portugal",
               "Austria")

gapminder %>%
  filter(year == 1982, 
         country %in% countries) %>% # Land in Länder
  arrange(desc(lifeExp)) #  von hoch zu niedrig sortiert


# mutate(): Eine neue Variable im Datensatz erstellen ----
gapminder <- gapminder %>% # Zuweisung durch <- , dadurch wird neue Spalte popMM gespeichert
  mutate(popMM = pop / 1000000)

gapminder
# select(): Spalten auswählen----
gapminder %>% 
  filter(country == "Germany") %>% 
  select(year, lifeExp, gdpPercap)
# Verkettung von Funktionen mit dem Pipe Operator %>% ----
gdp_1997 <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  filter(year == 1997) %>%
  arrange(desc(gdp))

print(gdp_1997) # Ausgabe in der Konsole

# summarize(): Numerische Zusammenfassungen ----
gdp_1997 %>%
  summarize(meanLifeExp = mean(lifeExp), # Mittelwert
            medianIncome = median(gdpPercap), # Median
            SumIncome = sum(gdp), # Summe
            MinIncome = min(gdp), # Minimum
            MaxIncome = max(gdp)) # Maximum

# group_by(): Gruppierte numerische Zusammenfassungen ----
## Eine Gruppe
gdp_1997 %>%
  group_by(continent) %>% 
  summarize(meanLifeExp = mean(lifeExp), # Mittelwert
            medianIncome = median(gdpPercap), # Median
            SumIncome = sum(gdp), # Summe
            MinIncome = min(gdp), # Minimum
            MaxIncome = max(gdp)) # Maximum
## Zwei Gruppen
by_continent_year <- gapminder %>%
  mutate(gdp = gdpPercap * pop) %>%
  group_by(continent, year) %>% 
  summarize(meanLifeExp = mean(lifeExp), # Mittelwert
            medianIncome = median(gdpPercap), # Median
            SumIncome = sum(gdp), # Summe
            MinIncome = min(gdp), # Minimum
            MaxIncome = max(gdp),
            totalPopB = sum(pop/1e9)) %>%  # Maximum
  print(n = Inf)

# Daten visualisieren mit ggplot2 ----
# Streudiagramm geom_point() ----
# Beispiel 1
ggplot(data = by_continent_year) + # Datensatz spezifizieren
  geom_point(mapping = aes(x = year, # Was soll auf die x-Achse?
                           y = meanLifeExp, # Was soll auf die y-Achse?
                           color = continent, # Farbe als dritte Dimension
                           size = totalPopB)) # Populationsgröße als vierte Dimension
# Beispiel 2
ggplot(data = gapminder) +
  geom_point(mapping = aes(x = gdpPercap,
                           y = lifeExp,
                           color = continent,
                           size = pop)) +
  scale_x_log10() +
  facet_wrap(facets = ~ year,
             nrow = 3)

# Schritt für Schritt Anleitung
gapminder_1997 <- gapminder %>%
  filter(year == 1997)

glimpse(gapminder_1997)
## Ohne Facetten (nur Jahr 1997)
ggplot(data = gapminder_1997) + 
  geom_point(mapping = aes(x = gdpPercap, 
                           y = lifeExp,
                           color = continent,
                           size = popMM)) +
  scale_x_log10()
# Mit Facetten (alle Jahre)
ggplot(data = gapminder) + 
  geom_point(mapping = aes(x = gdpPercap, 
                           y = lifeExp,
                           color = continent,
                           size = popMM)) +
  scale_x_log10() +
  facet_wrap(facets = ~year)




# Liniendiagramm geom_line() ----
## Beispiel 1
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(meanGdpPercap =  mean(gdpPercap))

ggplot(data = by_year) +
  geom_line(mapping = aes(x = year, y = meanGdpPercap)) +
  expand_limits(y = 0)

## Beispiel 2
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(meanGdpPercap =  mean(gdpPercap))

ggplot(data = by_year_continent) +
  geom_line(mapping = aes(x = year, 
                          y = meanGdpPercap,
                          color = continent)) +
  expand_limits(y = 0)

## Beispiel 3
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap =  median(gdpPercap))

ggplot(data = by_year_continent) +
  geom_line(mapping = aes(x = year, 
                          y = medianGdpPercap,
                          color = continent)) +
  expand_limits(y = 0)

# Balkendiagramm geom_col() ----
## Beispiel 1
europe1997 <- gapminder %>%
  filter(year == 1997,
         continent == "Europe",
         pop > 50e6)

ggplot(data = europe1997) +
  geom_col(mapping = aes(x = country,
                         y = lifeExp))

## Beispiel 2
europe1997 <- gapminder %>%
  filter(year == 1997,
         pop > 50 * 10^6)

europe1997

ggplot(data = europe1997) +
  geom_col(mapping = aes(y = country, 
                         x = lifeExp,
                         fill = continent))

# Histogramm geom_histogram() ----
ggplot(data = gapminder_1997) +
  geom_histogram(mapping = aes(x = lifeExp,
                               fill = continent),
                 binwidth = 5)
# Boxplot geom_boxplot() ----
ggplot(data = gapminder_1997) +
  geom_boxplot(mapping = aes(x = continent,
                             y = lifeExp,
                             color = continent))

# Challenges: Grafische Benutzeroberfläche R Commander----
## 1: Installieren und laden des R Commanders
# install.packages("Rcmdr") # nach der Installation mit # auskommentieren
# Wenn der R Commander versehentlich geschlossen wird, dann Session neustarten und laden
library(Rcmdr)
## 2: Laden der Datendatei "gapminderData1997.csv" (siehe Moodle)
Dataset <- 
  read.table(file = "C:/Users/hariskos/Documents/R/4-SK-04-WIM19/gapminderData1997.csv",
             header = TRUE, 
             stringsAsFactors = TRUE, 
             sep = ",", 
             na.strings = "NA", 
             dec = ".", 
             strip.white = TRUE)
## 3: Erstellen einer deskriptiven Statistik der Datenmatrix "Dataset"
summary(Dataset)
## 4: Berechnung des Länderdurchschnitts der Lebenserwartung für jeden Kontinent
### Deskriptive Statistiken >> Tabelle mit Statistiken
Tapply(formula = lifeExp ~ continent, 
       fun = mean, 
       na.action = na.omit, 
       data = Dataset) # mean by groups
# Tidyverse Methode
Dataset %>% 
  group_by(continent) %>% 
  summarize(mean(lifeExp))
## 5: Berechnung der Korrelation von Lebenserwartung und Einkommen
### Exkurs: Grafische Veranschaulichung
ggplot(data = Dataset,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)
### Berechnung mit R Commander
### Deskriptive Statistiken >> Korrelationsmatrix
cor(Dataset[,c("gdpPercap","lifeExp")], use = "complete")

### Exkurs: Perfekte Korrelation
### Exkurs: Grafische Veranschaulichung einer Korrelation von 1
ggplot(data = Dataset,
       mapping = aes(x = gdpPercap,
                     y = gdpPercap)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)

### Exkurs: Grafische Veranschaulichung einer Korrelation von 1
ggplot(data = Dataset,
       mapping = aes(x = lifeExp,
                     y = pop)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE)

## 6: Ein Histogramm erstellen (Häufigkeitsverteilung)
### Lebenserwartung
with(Dataset, Hist(lifeExp, scale="frequency", breaks="Sturges", 
                   col="darkgray"))
### Populationsgröße
with(Dataset, Hist(pop, scale="frequency", breaks="Sturges", 
                   col="darkgray"))
### Einkommen
with(Dataset, Hist(gdpPercap, scale="frequency", breaks="Sturges", 
                   col="darkgray"))
### Verteilung des Einkommens gruppiert nach Kontinenten
with(Dataset, Hist(gdpPercap, groups=continent, scale="frequency", 
                   breaks="Sturges", col="darkgray"))
### Tidyverse-Methode (wird nachgeliefert)
ggplot(data = Dataset) +
  geom_histogram(mapping = aes(x = gdpPercap),
                 binwidth = 5) +
  facet_wrap(facets = ~continent)

## 7: Ein Streudiagramm erstellen
### Einkommen versus Lebenserwartung
scatterplot(lifeExp~gdpPercap, regLine=FALSE, smooth=FALSE, boxplots=FALSE, 
            data=Dataset)

### Einkommen versus Lebenserwartung für jeden Kontinenten
scatterplot(lifeExp~gdpPercap | continent, 
            regLine=FALSE, smooth=FALSE, boxplots=FALSE, 
            by.groups=TRUE, data=Dataset)

## 8: Ein Balkendiagramm erstellen
with(Dataset, Barplot(continent, xlab="continent", 
                      ylab="Frequency", label.bars=TRUE))

## 9: Einen Boxplot erstellen
Boxplot(lifeExp~continent, data=Dataset, 
        id=list(method="y"))

# Vertiefung: Funktionen ----
## f(x, y, z), wobei f die Funktion ist sind x, y, z sind Inputs oder Argumente
## mean(x, trim = 0, na.rm = FALSE)
## Was macht der Pipe Operator x %>% f(y, z) <-> f(x, y, z)
### Hilfefunktion nutzen
help(mean)
help(NA)
### Mittelwert
noten <- c(2, 5, 1, 1, NA) # notenvektor, letzte Note ist nicht bekannt
notendurchschnitt <- mean(x = noten, na.rm = TRUE)
notendurchschnitt
### Median vs Mittelwert
noten2 <- c(1, 1, 1, 1, 1, 1, 5)

median(x = noten2)
mean(x = noten2)
### Mittelwertsfunktion selber schreiben
meine_noten <- c(2, 5, 1, 1, 4)
meine_noten
#### summe bilden
help(sum)
summe <- sum(meine_noten)
summe
#### anzahl der vektorelemente bestimmen
help(length)
anzahl <- length(meine_noten)
anzahl
#### mittelwert berechnen
summe / anzahl

#### funktion schreiben
##### Input
meine_noten <- c(2, 5, 1, 1, 4)
##### Funktionselemente (EVA Prinzip)
mymean <- function(x) { # x EINGABE
  summe <- sum(x) # VERARBEITUNG
  anzahl <- length(x)
  mittelwert <- summe / anzahl
  return(mittelwert) # AUSGABE
}
View(mymean)

# Eigene Funktion ausprobieren
noten <- c(1, 2, 3, 4, NA, NA)
mean(noten, na.rm = TRUE) # standardfunktion
mymean(noten) # eigene funktion

# Zweite Mittelwertsfunktion, die mit fehlenden Werten NA umgehen kann
mymean2 <- function(x) { # x EINGABE
  summe <- sum(x, na.rm = TRUE) # VERARBEITUNG
  anzahl <- length(x) - sum(is.na(x))
  mittelwert <- summe / anzahl
  return(mittelwert) # AUSGABE
}

mymean2(noten) # kann mit NA umgehen
mymean(noten)  # kann nicht mit NA umgehen








