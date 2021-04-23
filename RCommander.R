# R Oeffnen und in die Console library(Rcmdr) eingeben,
# um den R Commander zu oeffnen

# DATENSATZ gapminder----
#1 Paket laden
library(gapminder, pos=16)
#2 Datenmatrix aus geladenem Paket auslesen
data(gapminder, package="gapminder")
#3 Hilfe zur aktiven Datenmatrix aufrufen
help("gapminder")
#4 Datensatz ausgeben
gapminder
#5 Numerische Zusammenfassung
summary(gapminder)

# DATENSATZ gapminder1997----
#6 Beobachtungen aus den Jahr 1997 filtern
gapminder1997 <- subset(gapminder, subset=year == 1997)
#7 Datensatz ausgeben
gapminder1997
tail(gapminder1997)
#8 Numerische Zusammenfassung
summary(gapminder1997)
#9 Histogramm erstellen
with(gapminder1997, Hist(lifeExp, groups=continent, scale="frequency", breaks="Sturges", 
  col="darkgray"))
#10 Streudiagramm erstellen
scatterplot(lifeExp~gdpPercap | continent, regLine=FALSE, smooth=FALSE, boxplots=FALSE, 
  by.groups=TRUE, data=gapminder1997)
#11 Streudiagramm mit logarithmierter x-Achse erstellen
scatterplot(lifeExp~gdpPercap | continent, log="x", regLine=FALSE, smooth=FALSE, 
  boxplots=FALSE, by.groups=TRUE, data=gapminder1997)
#12 Streudiagramm mit logarithmierter x-Achse erstellen + Regressionsgeraden
scatterplot(lifeExp~gdpPercap | continent, log="x", regLine=TRUE, smooth=FALSE, 
  boxplots=FALSE, by.groups=TRUE, data=gapminder1997)
#13 Boxplot erstellen
Boxplot(lifeExp~continent, data=gapminder1997, id=list(method="y"))
#14 Beobachtungen aus dem Jahr 1997 nach den Kontinenten gruppieren und den Median berechnen
gapminder1997ByContinent<- aggregate(lifeExp ~ continent, data=gapminder1997, FUN=median)
#15 Datensatz ausgeben
gapminder1997ByContinent
#16 Numerische Zusammenfassung
summary(gapminder1997ByContinent)
# Datensatz gapminderGermany----
#17 Beobachtungen aus Deutschland filtern8
gapminderGermany <- subset(gapminder, subset=country == "Germany")
#18 Datensatz ausgeben
gapminderGermany
#19 Numerische Zusammenfassung
summary(gapminderGermany)
#20 Liniendiagram erstellen
with(gapminderGermany, lineplot(year, lifeExp))

