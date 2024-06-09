
dane=read.csv("dane_hip.csv", sep=";", dec = ",")

###############################################
#1 hipotezy na podstawie średniej (automaty)
###############################################

#(1)
# Hipotezy (na podstawie średniej)
# H0: mi <= 4 [m/s]   H1: mi > 4 [m/s]

# (2)
# W ~ N(mi, sig), mi i sig nieznane (3)
wind = na.omit(dane$wiatr)

mi0 = 4 # to co jest w hipotezie
# (3)
p1=t.test(wind, mu=mi0, alternative="greater")$p.value

# (4)
# alpha = 0.05  >   (p1) p-value=0.01705  -> odrzucamy hipotezę H0


#(5) Interpretacja
# Na poziomie istotności 5% dane potwierdzają, że średnia wartość wiatru w badanej 
# okolicy przekraczają 4 m/s.


###############################################
#2 hipotezy na podstawie średniej 2 (automaty)
###############################################


#wartość wynosi co najmniej 3,5
#przypuszczenie: znacznie mniejszy niż 3,5.
#zmienna opisująca wartości współczynnika COP jest zmienną losową o rozkładzie
#normalnym 
#poziom istotności α=0,01
#czy wątpliwości nabywcy są słuszne. 

heat = na.omit(dane$pompa)
#(1)
# H0:  mi >= 3.5    H1:  mi < 3.5
#(2)  H ~ N(mi, sif), sig nieznane (3) przypadek
#(3)
mi0=3.5
alpha=0.01
p2=t.test(heat, mu=mi0, alternative = "less")$p.value
#(4)
# alpha = 0.01  <  p2 = 0.152 -> nie odrzucamy H0
#(5)
# Na poziomie istotności 1% dane nie potwierdzają, że pompa 
# cieplna ma współczynnik efektywności mniejszy niż 3.5



###############################################
#3 hipoteza pytająca  średnią 3 (automaty) z odchyleniem standardowym
###############################################


#rozkład wyników pomiarów jest normalny
#odchyleniem standardowym 5 m
#Dokonano 5 niezależnych
#poziom istotności α=0,05
#zweryfikuj hipotezę, że średnia głębokość morza w tym rejonie
#jest różna od 870 m. 


#(1)
#H0:  mi = 870  H1: mi != 870

#(2) G ~ N(mi, sig), znana sigma (1) finish yourself
morze=na.omit(dane$morze)
library(BSDA)
p3 = z.test(morze, sigma.x = 5, mu = 870, alternative="two.sided")
#Na poziomie istotności 0.05 średnia głębokość morza jest rowna 870 (brak podstaw do
#odrzucenia h0, ktora mowi, ze jest rowna)
#p-value -0.6547
#alfa=0.05 < p-value ->nie mamy podstaw do odrzucenia
#na poziomie istotności alfa =0.05 
#dane nie potwierdzają hipotezy badacza


###############################################
#4 hipotezy 4 (automaty) wszystkie mają być G
###############################################


#nominalnej grubości 0,04 mm
#poziom istotności α=0,02
#blaszki są grubsze niż nominalna grubość


blaszki = na.omit(dane$blaszki)
# h0 mu <= 0.04 h1 mu > 0.04
alpha=0.02
zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = 0.04, alternative="greater")
#alpha 0.02, p-value 0.054
#alpha<p-value -> nie odrzucamy hipotezy jednak, ona źle wpisala alfe
#na poziomie istotnosci 0.02 dane nie potwierdzają hipotezy że produkowane przez ten automat blaszki są
#grubsze niż nominalna grubość

###############################################
#5 hipotezy 5 wariancja mniejsza niż 
###############################################

#wykonano 10 oznacze
#rozkład zawartości jest normalny
#poziom istotności α = 0,05

mleko = na.omit(dane$mleko)
alfa =0.05

#a)
#Czy obserwacje przeczą hipotezie, że średnia zawartość tłuszczu w mleku wynosi 1,7 %?
#H0  sig^2 >=0.02 H1 sig^2<0.02

chi2=(n-1)*var(mleko)/(0.02)
#H0:  mi = 1.7    H1: mi != 1.7
qchisq(alfa,n-1)

#b)
#Czy można twierdzić, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0,02 (%)^2.
#(1)
#H0:  sig^2 >= 0.02   H1:  sig^2 < 0.02 
# używając testów
library(TeachingDemos)
ssq0=0.02

sigma.test(mleko, sigmasq=ssq0, alternative="less")
# alpha=0,05  <   p.value=0.1835 -> nie odrzucamy H0

#Na poziomie istotności 5% danie nie potwierdzają, że wariancja zawartości mleka
# jest mniejsz niż 0,02
#################################################################################################
#duże liczby #
#################################################################################################

###############################################
#8 hipotezy 8 (automaty) 
###############################################

#losowo z 2500 osób 1600 chce głosować
alfa = 0.05
#czy proba przeczy, ze 60% ogółu zamierza wziąć udział?
N=2500
T=1600
PHAT =T/N
P0=0.6

#1  H0:P=60% H1:P!=60%
Z=(PHAT-P0)/sqrt(P0*(1-P0)/N)
Z

qnorm(1-alfa/2)
#R: (-inf ,-1.959964) suma (1.959964,inf)

#na poziomie istotności alfa dane potwierdzają hipoteze że
#proporcja osób chcących wziąc udział w wyborach jest różna od 60%

binom.test(T,N,p=P0,alternative="two.sided")
#odrzucamy h0



###############################################
#9 hipotezy 9 (automaty) 
###############################################


#2% jaj jest złej jakości

#Wylosowano 1200
#16 okazało się złej jakości
alfa = 0.05
N=1200
T = 16
PHAT =T/N
P0 = 0.02
#1  H0:P >= 60 H1: P < 0.02

binom.test(T,N,p=P0,alternative="less")

# brak podstaw do odrzucenia h0, frekcja jest mniejsza.

###############################################
#10 hipotezy 10 (automaty) 
###############################################

# 1100 doroslych
# 1000 odp, że nie przeczyt. ksiażki
alpha = 0.05
#czy opinia, że > 90 nie przeczytało książki

# h0: p <= 90  h1: p > 90 

N = 1100
T = 1000
p0 = 0.9

binom.test(T,N,p=P0,alternative="greater")
#alpha < p.value -> nie odrzucamy H0
# nie czaje



 



