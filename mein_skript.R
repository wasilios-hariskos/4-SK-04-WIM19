# Pakete installieren ----

# install.packages("vctrs") #ggf. beim Mac vorher laufen lassen
# install.packages("gapminder")
# install.packages("dplyr")

# Pakete laden ----
library(gapminder) # Datensatz
library(dplyr) # data plyr / Datenzange

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
  print(n = Inf) # geben 20 zeilen aus, mit Inf alle Zeilen
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
# Verkettung von Funktionen ----
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
