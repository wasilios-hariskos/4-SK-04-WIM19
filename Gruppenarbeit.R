# Einfache Rechenoperationen----
(3 + 5) / 3

1 + 1

a <- 5
a

b <- 5 + 7 * 2
b

c = 12 - 2
c

# R-Objekte/Datenstrukturen----

# Numerischen Vektor a erzeugen 
a <- c(2, 4, 5, 3, 18, 20154, 3.5)
# Objekt a anzeigen 
a

# Character-Vektor erzeugen b
b <- c('Philipp', "Eva", "Felix")
b

# Numerischer Vektor erzeugen Vektor
Vektor <- c(45.2, 44.8, 46.3)
Vektor

# Vektorwerte verdoppeln
Vektor * 2

# Objekt Vektor aufrufen
Vektor

# Laenge von Vektor a
length(a)

# Typ des Vektors a und b
str(a)
str(b)

# Neuer Vektor a
a <- c(2, 4, 5, 1, 9, 7)
a
str(a)

# Erzeugung Matrix (3x2) auf Basis von Vektor 
matrix(data = a, 
       nrow = 3, 
       ncol = 2)
A <- matrix(data = a, 
            nrow = 3, 
            ncol = 2)
A

# Neuer Vektor b
b <- c(4, 7, 8, 2, 1)
# Erzeugung Matrix mit Vektorlaenge < Elementeanzahl
B <- matrix(data = b, 
            nrow = 3, 
            ncol = 2)
B

# Uebungsaufgabe 1: Erzeugung Matrix B (2x3) aus den Zahlen 1 bis 6----
# B <- matrix(data = ?,
#             nrow = ?,
#             ncol = ?)
# B
# Erzeugung eines Array----
a_dim3 <- array(dim = c(3, 5, 3))
a_dim3
# Erzeugung einer Liste----
Liste <- list(a, b, A, B)
Liste

# Erzeugung eines Datensatzes
# Vektor Namen mit Namen der Familie Simpson nach Alter
Namen <- c("Homer", "Marge", "Bart", "Lisa", "Maggie")
Namen

# Vektor mit Altersangaben
Alter <- c(36, 34, 10, 8, 1)
Alter

# Datensatz Daten mit den Vektoren Namen und Alter
Daten <- data.frame(Namen, Alter)
Daten
str(Daten)


# Erzeugung eines Data Frames aus Vektoren----
x <- c("A", "B", "C")
y <- c(33, 22, 11)
z <- c(0, 0, 1)
z <- as.logical(z)
z
# z <- c(FALSE, FALSE, TRUE)
# Alternativ:  z <- c("m", "m", "w")

dat <- data.frame(x, y, z)
dat

# Datenindizierung----

# Spalte mit Namen x des Datensatzes dat anzeigen 
dat$x

# 3. Element von a
a[3]
a

# Matrix M aus Vektor a erzeugen
M <- matrix(data = a, 
            nrow = 3, 
            ncol = 2)
M

# Indizieren 2. Element der 1. Spalte
M[2, 1]

# Werte der 2. Zeile
M[2, ]

# Werte der 1. Spalte
M[ ,1]

# Indizierung aus Liste
Liste[[3]][1, 1]
A

# Indizierung aus Datensatz "Simpsons"-Daten
Daten[ , "Alter"]
# Alternative 1:  
Daten[ , 2]  
# Alternative 2:
Daten$Alter 


# Alle Faelle mit Erwachsenen im Datensatz Daten
Daten[Daten$Alter > 17, ]
# alternativ
Daten[Alter > 17, ]
# noch ohne Zuweisung
# Datenmanipulation eine Vektors----
# Datenmanipulation eines Vektors

# Vektor erstellen
Vektor <- c(10:15)
Vektor

# 3. Element des Vektors soll durch 33 ersetzt werden
Vektor[3] <- 33
Vektor

# 2. bis 4. Element soll durch "xx"
Vektor[2:4] <- "xx"
Vektor

# Datentypzuweisung von numeric auf character
str(Vektor)

# Komplexere Rechenoperationen----
exp(sqrt(1))
sqrt(exp(1))   

1 + 2
1 - 2
4 * 5
4 / 5
4^2
exp(1)
log(1)
log(exp(0))
x <- 25
y <- x + 1
x * y
