#ANOVA
#Analiza wariancji


###############################################
#1 anova 1 (automaty) 
###############################################

#czy różne warunki ciśnieniowe mają wpływ na średnią produkcję
#pewnego wyrobu 
#Analizę wariancji poprzedź testem równości wariancji, np. z
#wykorzystaniem funkcji bartlett.test.


pressure=read.csv("Anova_cisnienie.csv", sep=";")
alpha = 0.05


#Zanim przeprowadzimy analize wariancji, musimy sprawdzić czy wariancja każdej
#z tych grup jest równa sobie (bartlett.test) (Test czy wariancje są jednorodne)

# H0: sigsq_n = sigsq_ś = sigsq_s = sigsq_bs 

# Musimy ułożyć dane w jeden długi wektor

wyniki = c(pressure$Niskie, pressure$Srednie, pressure$Silne, pressure$BardzoSilne)

lab=names(pressure)
n1=length(pressure$Niskie)
n2=length(pressure$Srednie)
n3=length(pressure$Silne)
n4=length(pressure$BardzoSilne)

obiekty = c(rep(lab[1], n1),
            rep(lab[2], n2),
            rep(lab[3], n3),
            rep(lab[4], n4))
obiekty = c(rep(lab, each=10))
data.frame(wyniki, obiekty)
bartlett.test(wyniki, obiekty) # alpha = 0.05 < 0.5009 = p.value -> nie odrzucamy H0
# Na poziomie istotności 5% dane nie potwierdzają, że wariancje poziomów ciśnienia 
# nie są jednorodne

###################################################

# Możemy przeprowadzić analizę wariancji
# H0: mi_n = mi_ś = mi_s = mi_bs H1: ~H0
# H0: średnia produkcja balonów jest równa dla każdego poziomu napompowania

# Tablica ANOVA

# Źródła        Sumy        Stopnie       Średnie      F    ||   Obszar
#Zmienności  Kwadratów      Swobody      Kwadraty           ||    Krytyczny

# Obiekty     28.275           3           9.42       2.27  ||  (F_3,36,1-alpha ; INF)

#   Błąd      149.69          36          4.158                 R = (2.287,  INF)

# Całkowita   177.975         39



anova(lm(wyniki ~ obiekty)) #alpha = 0.05 < 0.09735 p.value -> nie odrzucamy H0

###############################################
#3 anova 3 (automaty)  bledy pomiarow rozkłady normalne o takiej samej wariancji
###############################################

#Każdym z trzech mikrometrów zmierzono kilkukrotnie ten sam przedmiot i uzyskano wyniki:
#Mikrometr I: 4,5; 4,7; 4,8; 4,7
#Mikrometr II: 4,7; 4,8; 4,5; 4,7; 4,4; 4,8
#Mikrometr III: 4,8; 4,9; 4,8; 4,9; 4,8

#Zakładając, że błędy pomiarów mają rozkłady normalne o takiej samej wariancji,
#na poziomie istotności
#0,05 zweryfikuj hipotezę, że wybór mikrometru ma wpływ na uzyskane wyniki.

mikrometry=read.csv("Anova_mikrometr.csv", sep=";", dec=",")

m1=mikrometry$mikrometrI
m2=mikrometry$mikrometrII
m3=mikrometry$mikrometrIII

n1=length(na.omit(m1))
n2=length(na.omit(m2))
n3=length(na.omit(m3))

lab = names(mikrometry)

wyniki = c(na.omit(m1), na.omit(m2), na.omit(m3))

obiekty=c(rep(lab[1], n1), rep(lab[2], n2), rep(lab[3], n3))

data.frame(obiekty, wyniki)

anova(lm(wyniki~obiekty)) #alpha = 0.05 < 0.068 (Pr(>F))-> nie odrzucamy H0

# Na poziomie istotności 5% danie nie potwierdzają, że wybór mikrometru ma wpływ
# na pomiar


###############################################
#4 anova 4 24 sportowcow, 4 grupy (niepalacy, lekko, srednio, duzo)
###############################################

cig_player = read.csv("Anova_sportowcy.csv", sep=';')

wyniki = c(cig_player$Niepalacy,
           cig_player$Lekkopalacy, 
           cig_player$Sredniopalacy, 
           cig_player$Duzopalacy)

lab = names(cig_player)

obiekty=c(rep(lab, each=length(cig_player$Niepalacy)))
data.frame(wyniki, obiekty)

#(a) Zakładając  rozkład normalny, na poziomie istotności
#0,01, sprawdź czy palenie papierosów może wpływać na rytm zatokowy serca.
bartlett.test(wyniki, obiekty) # alpha=0.01 < 0.8517 -> nie odrzucamy H0

anova(lm(wyniki~obiekty)) # alpha = 0,01 > 0.004 -> odrzucamy H0
# Na poziomie istotności 1% dane potwierdzają, że zatokowy rytm serca graczy różni się
# względem intensywności ich palenia

# Na poziomie istotności 1% dane potwierdzają, że intensywność palenia ma wpływ
# na średni rytm zatokowy serca sportowców

########################################################
#b
#wyznaczenia grup jednorodnych porównywanych średnich obiektowych


# Sprawdzamy czy istnieją w tym teście grupy jednorodne

TukeyHSD(aov(wyniki~obiekty), ordered=T)
# diff - różnica średnich
# lwr - lower bound obszaru krytycznego
# upr - upper bound obszaru krytycznego
# p adj - wartość p.value

# Jeśli 0 należy do przedziału ufności oznacza, że grupa jest jednorodna
# Tam gdzie małe p.value to nie jest jednorodne

plot(TukeyHSD(aov(wyniki~obiekty), ordered=T))
#Grupy jednorodne: L-D, D-S, D-N, S-N => N-S-D, L-D
#Jednorodne
#(NS
#ND    -> tworzy się cykl N D -> D S -> S N
#DS)
#LD