dane=read.csv('dane_est.csv', sep=";")

#1. 
# D ~ N(mi, sig)
# czytanie danych (Produkcja diamentów daną metodą)
diam=na.omit(dane$diamenty)
#a)
# Populacja - diamenty produkowane daną metodą (Wszystkie)
# Próba - wybrana losowo grupa diamentów wyprodukowanych daną metodą (Wybranych np. 12 )
# Zmienna losowa - waga diamentu (liczona w karatach)

# Średnia całej populacji (mi)

# Wariancja całej populacji (sigma^2)

# Odchylenie standardowe całej populacji (sigma)

#b) 
# Średnią z próby - ocena (xbar)
xbar=mean(diam)

# Wariancja z próby ( s^2 )

ssq=var(diam)
# Odchylenie standardowe z próby ( s )

s=sd(diam)
s=sqrt(ssq)
# Używanie twierdzenie czebeszewa daje nam o wiele za duży przedział 
(xbar+3*s) - (xbar-3*s)

#c)
#Obliczamy przedział ufności
# 1-alpha = 0,95 - poziom ufności
alpha = 0.05
# nie znamy prawdziwych wartośc średnich i odchylenia dla całej populacji
# (1) odpada
#mihat = mi
#sighat = sig
# (2) odpada, nie mamy dużej populacji
# (3) nieznane oba mi i sig, stosujemy
n=length(na.omit(diam))

# Błąd estymacji
estErr = qt(1-alpha/2, n-1) * (s / sqrt(n))
L = xbar - estErr
U = xbar + estErr
# L zaokrąglamy zawsze w dół!
# U zaokrąglamy zawsze w góre!
# Nasz przedział to (0.498, 0.57)
# Z ufnością 95% przedział od 0.49 karata do 0.57 karata pokrywa nieznaną 
# średnią wagę WSZYSTKICH ediamnetów produkowanych badaną metodą

# II (lekko możliwe niepoprawna wersja)
# W 95% średnia próby będzie się znajdować w przedziale ufności

# Pytanie na egzaminie 
# Jaka jest różnica między 
# Czy jak sformułuje następującą odpowiedź przedział 
# L do U 95% ufności, pokrywa próbkową średnią

# Z ufnością 95 % średnia pokrywa średnią próbkową ŹLE!
# Z ufnością 95% średnia pokrywa średnią populacyjną DOBRZE !

#Używanie pakietu do obliczanie przedziału ufności
ttt = t.test(diam) # conf.level = 0.95 by default
# Przedział ufności
ttt$conf.int

#d)
ttt2 = t.test(diam, conf.level = 0.98)
ttt2$conf.int

#e)
# Przedział ufności dla wariancju (slajd)

L = ((n-1)*ssq)/(qchisq(1-alpha/2,n-1))
U = ((n-1)*ssq)/(qchisq(alpha/2,n-1))

# Z ufnością 95% (1-alhpa) przedział od 0.0015 karata^2 (wariancja) 
# do 0.0089 karata^2 pokrywa nieznaną wariancję wagi WSZYSTKICH diamentów 
# produkowanych badaną metodą

# Aby uzyskać dobrze interpretowalny przedział można spierwiastkować 
# i przekształcić na odchylenie standardowe

sqrt(L)
sqrt(U)

# Z ufnością 95% przedział od 0.039 karata do 0.095 karata pokrywa nieznane 
# odchylenie standardowe wagi WSZYSTKICH produkowanych diamentów badaną metodą.

# Można łatwiej za pomocą wbudowanej funkcji
sigTest = sigma.test(diam, conf.level = 1 -alpha )
sqrt(sigTest$conf.int)


#2. 
mlk = na.omit(dane$mleko)
#a)
# Populacja - wszystkie matki karmiące piersią
# Próbka -  wybrana losowa grupa matek karmiących piersią
# Zmienna losowa - liczba cząsteczek PCB na milion w mleku matek

#b)
# Średnia punktowa wszystkich danych

# śrenio 6 matek miało zanieczyszczone mleko piersiowe przez PCB
mean(mlk)

#c)
# Wariancja oraz odchylenie standardowe wszystkich danych

var(mlk)

# Średnio ilość matek z zanieczyszczonym mlekiem odchodzi od średniej o 5 
sd(mlk)

#d)
# Przedział ufności dla mi 

# Z ufnością 95% przedział od 3.42 matek do 8.18 matek pokrywa 
# nieznaną prawdziwą średnią populacyjną.  
ms = t.test(mlk)
ms$conf.int

#e)
# Przedział ufności dla sigma

# Z ufnością 95% przedział od 14.95 mam do 55,15 mam pokrywa 
# nieznaną prawdziwą wariancję populacyjną.
mv = sigma.test(mlk)
mv$conf.int

#3.
cigs = na.omit(dane$papierosy)
n=length(cigs)  
nic_sig = 0.7

#a 
xbar = mean(cigs)
alpha=0.05

estErr = qnorm(1-alpha/2)*(nic_sig/sqrt(n))

L=xbar - estErr
U=xbar + estErr
L
U

z.test(cigs, sigma.x=nic_sig)
#b) (Pewniak na kolokwium!!!!)
#Długośc przedziału teraz
U - L
# równoważne
2 * estErr
# 2 * E <= 0.3
# 2 * qnorm(1-alpha/2)*(nic_sif/sqrt(n)) <= 0.3
# nic_sig/sqrt(n) = 0.3 / (2*qnorm(1-alpha/2))
# 2*nic_sig*qnorm(1-alpha/2)/0.3 = sqrt(n)

n = (2*nic_sig*qnorm(1-alpha/2)/0.3)^2

# Potrzeba conajmniej 84 elementy 


#4) (do it yourself)
# Ma rozkład normalny

wod=na.omit(dane$wodorosty)
n=length(wod)

#a)
xbar=mean(wod)
ssq=var(wod)

#b)
alpha=0.1
estErr = qt(1-alpha/2, n-1)*(sqrt(ssq)/sqrt(n))
L=xbar-estErr
U=xbar+estErr

t.test(wod, conf.level = 1-alpha)$conf.int

#c)
L=((n-1)*ssq/qchisq(1-alpha/2, n-1))
U=((n-1)*ssq/qchisq(alpha/2, n-1))

#5) (z.test)

#6)
n=1200
xbar=4.7
s=2.2
alpha=0.05

phT = zsum.test(xbar, s, n, conf.level = 1-alpha)
phT$conf.int

#Z ufnością 95% przedział od 4.57 minut do 4.83 minut pokrywa prawdziwą 
# średnią długości rozmowy telefonicznej w godzinach popołudniowych.

# Dla wariancji 
L=(n-1)*s^2/qchisq(1-alpha/2, n-1)
U=(n-1)*s^2/qchisq(alpha/2, n-1)

# Dla odchylenia 
sqrt(L)
sqrt(U)

# Z ufnością 95% od 2.11 minut do 2.3 minut pokrywa prawdziwe
# odchylenie standardowe rozmów telefonicznych w godzinach południowych

#7)
n = 365
mi = 102
var = 81
s = sqrt(var)
alpha = 0.02

zsum.test(mi, s, n, conf.level=1-alpha)
# Z ufnością 98% od 101.9 hl do 103.01 hl pokrywa prawdziewe 
# średnie zużycie wody w fabryce 

#b) Nie i chuj. 122 jest daleko za przedziałem ufności 

#10.
# Zmienna losowa - czy puszka jest dopełniona czy nie ?
# Prawdopodobieństwo sukcesu - proporcja populacyjna

n = 100
# sukcesy (niedopełnione puszki) 
t = 4

# Oszacowanie punktowe
phat = t/n

#Przedział ufności dla proporcji 
alpha=0.05

estErr = qnorm(1-alpha/2)*sqrt(phat*(1-phat)/n)
L=phat - estErr
U=phat + estErr

# z ufnością 95% od 0.15% do 7.8% pokrywa się z prawdziwą proprocją populacyjną.
U - L
b = binom.test(t, n)
b$conf.int

b$conf.int[2] - b$conf.int[1]

# Przedział jest dłuższy niż ze wzroru (czyli gorzej)

b = prop.test(t, n)
b$conf.int

b$conf.int[2] - b$conf.int[1]

# A teraz to już wogle

#11. 

# Przedział ufności dla proporcji z dużym n
n=120
t=24
phat = t/n
alpha=0.1

# Testy
#(1)
binTest = binom.test(t, n, conf.level = 1-alpha)
#(2)
propTest = prop.test(t, n, conf.level = 1-alpha)
#(3)
estErr = qnorm(1-alpha/2)*sqrt((phat*(1-phat))/n)
L=phat-estErr
U=phat+estErr

#Zasięgi
#(1) #Największy przedział (ledwo)
binTest$conf.int[2] - binTest$conf.int[1]
#(2)
propTest$conf.int[2] - propTest$conf.int[1]
#(3) # Najmniejszy przedział 
U-L

# Przyjmuje środkowy przedział dla sprawiedliwości (nie optymistyczny (3) nie pesymistyczny (1))

# z ufnością 90% prawdziwy odsetek monterów nie przestrzegających zasad BHP znajduje się
# pomiędzy 14,3%, a 27%
