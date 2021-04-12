# Pakete installieren ----

# Pakete müssen am Anfang einmal installiert werden. Dazu kommentieren Sie diese aus (# entfernen)
# Nach der Installation können Sie die Zeilen wieder kommentieren (# hinzufügen)
# install.packages("vctrs") #ggf. beim Mac vorher laufen lassen
# install.packages("gapminder")
# install.packages("dplyr")
# install.packages("ggplot2")

# Pakete laden ----
library(gapminder) # Datensatz
library(dplyr) # data plyr / Datenzange
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
## 1: Installieren und laden des R Commanders----
install.packages("Rcmdr") # nach installation mit # auskommentieren
library(Rcmdr)
## 2: Laden der Datendatei "gapminderData1997.csv" (siehe Moodle)----
## 3: Auswählen der aktiven Datenmatrix "gapminder_1997"----
## 4: Erstellen einer deskriptiven Statistik der Datenmatrix
## 5: Berechnung des Länderdurchschnitts der Lebenserwartung für jeden Kontinenten
## 6: Berechnung der Korrelation von Lebenserwartung und Einkommen
## 7: Ein Histogramm erstellen
## 8: Ein Streudiagramm erstellen
## 9: Ein Balkendiagramm erstellen
## 10: Einen Boxplot erstellen
