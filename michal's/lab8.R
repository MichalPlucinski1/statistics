#ANOVA

#1.

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

# Możemy przeprowadzić analizę wariancji

# H0: mi_n = mi_ś = mi_s = mi_bs H1: ~H0
# H0: średnia produkcja balonów jest równa dla każdego poziomu napompowania

# Tablica ANOVA

# Źródła        Sumy        Stopnie       Średnie      F    ||   Obszar
#Zmienności  Kwadratów      Swobody      Kwadraty           ||    Krytyczny

# Obiekty     28.275           3           9.42       2.27  ||  (F_3,36,1-alpha ; INF)

#   Błąd      149.69          36          4.158                 R = (2.287,  INF)

# Całkowita   177.975         39

# Suma wszystkich obserwacji dla obiektów w pierwszej grupie
T1 = sum(pressure$Niskie)
T2 = sum(pressure$Srednie)
T3 = sum(pressure$Silne)
T4 = sum(pressure$BardzoSilne)

Tt = T1 + T2 + T3 + T4

Nn = n1 + n2 + n3 + n4
Nn = length(wyniki)

# Całkowita Suma Kwadratów
SSG = sum(wyniki^2) - Tt^2/Nn

# Suma Kwadratów Obiektów
SST = T1^2/n1 + T2^2/n2 + T3^2/n3 + T4^2/n4 - Tt^2/Nn

# Suma Kwadratów Błędów
SSE = SSG - SST

# Stopnie Swobody (zależą od liczby obserwacji)

# Całkowita
Df_c = Nn-1

#Obiektów 
#Są 4 obiekty 
4 - 1 

# Błędów
Df_c - 3

# Średnie kwadraty

#Obiektowe
MST = SST/3

#Błędów
MSE = SSE/36

# Statystyka F
F_test = MST/MSE

# Obszar krytyczny
qf(1-alpha, 3, 36)

# F_test nie należy do obszaru krytycznego R -> Nie odrzucamy H0

# Na poziomie istotności 5% dane nie potwierdzają, że zastosowane ciśnienie 
# ma wpływ na wielkość produkcji

# R alternatywa 

anova(lm(wyniki ~ obiekty)) #alpha = 0.05 < 0.09735 p.value -> nie odrzucamy H0


#3.

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

anova(lm(wyniki~obiekty)) #alpha = 0.05 < 0.068 -> nie odrzucamy H0

# Na poziomie istotności 5% danie nie potwierdzają, że wybór mikrometru ma wpływ
# na pomiar

#4.

cig_player = read.csv("Anova_sportowcy.csv", sep=';')

wyniki = c(cig_player$Niepalacy,
           cig_player$Lekkopalacy, 
           cig_player$Sredniopalacy, 
           cig_player$Duzopalacy)

lab = names(cig_player)

obiekty=c(rep(lab, each=length(cig_player$Niepalacy)))
data.frame(wyniki, obiekty)

bartlett.test(wyniki, obiekty) # alpha=0.01 < 0.8517 -> nie odrzucamy H0

anova(lm(wyniki~obiekty)) # alpha = 0,01 > 0.004 -> odrzucamy H0

# Na poziomie istotności 1% dane potwierdzają, że zatokowy rytm serca graczy różni się
# względem intensywności ich palenia

# Na poziomie istotności 1% dane potwierdzają, że intensywność palenia ma wpływ
# na średni rytm zatokowy serca sportowców

# Sprawdzamy czy istnieją w tym teście grupy jednorodne

TukeyHSD(aov(wyniki~obiekty), ordered=T)
# diff - różnica średnich
# lwr - lower bound obszaru krytycznego
# upr - upper bound obszaru krytycznego
# p adj - wartość p.value

# Jeśli 0 należy do przedziału ufności oznacza, że grupa jest jednorodna
# Tam gdzie małe p.value to nie jest jednorodne

plot(TukeyHSD(aov(wyniki~obiekty), ordered=T))

#Jednorodne
#(NS
#ND    -> tworzy się cykl N D -> D S -> S N
#DS)
#LD