install.packages("nortest")
library(nortest)
###################################################################
#     1  chi2 1      
###################################################################

points = read.csv("normalnosc_punkty.csv", sep = ";")
ozon = read.csv("normalnosc_ozon.csv", sep=";")

# 1 test
#38% było zatrudnionych w innej
#32% samozatrudnionych, 
#23% było freelancerami lub konsultantami,
#a 7% założyło własne firmy

# 2 test 
# 300 emerytowanych menedżerów
#122 pracowało dla innej firmy, 
#85 prowadziło działalność na własny rachunek,
#76 pracowało jako freelancer
#lub doradzało, a 17 założyło własne firmy.

#Czy przy istotności 10% dane potwierdzają, że odsetki
#poszczególnych zatrudnionych w hrabstwie Allegheny różnią się od ich
#odpowiedników w skali całego kraju?

obs=c(122, 85, 76, 17)
n = sum(obs)
teor=c(0.38*n, 0.32*n, 0.23*n, 0.07*n)


alpha = 0.1

# Hipoteza
# Każda częstość występowania jest równa tej teoretycznej
# H0: rozkład częstości pracujących emerytów 
#     ze względu na typ pracy jest zgodny z oczekiwaniem
# H1: co najmniej jedna ta częstość jest inna

# Trzeba uważać na precyzje floating point 
chisq.test(obs, p = teor/n) #p.val = 0.3485 df = 3 (ilość kategorii - 1)

# alpha = 0.1 < p.value => nie odrzucamy H0

#Interpretacja
# Na poziomie istotności 10% dane nie potwierdzając, że społeczność 
# emerycka różni się od oczekiwanej prognozy 

###################################################################
#     2  chi2 2      
###################################################################


#74% to wypadki, 16% to zabójstwa, a 10% to samobójstw

# 68 wypadków śmiertelnych, 
#27 zabójstw i 
#5 samobójstw. 
#istotności 10% dane potwierdzają, 
#że odsetki poszczególnych zgonów różnią się od przedstawionych w artykule?


obs=c(68, 27, 5)
n = sum(obs)
teor=c(0.74 * n, 0.16 * n, 0.1 * n)


alpha = 0.1
chisq.test(obs, p = teor/n) #p.val = 0.3485 df = 3 (ilość kategorii - 1)
# 0.005 < alpha -> odrzucamy h0
#Interpretacja
# Na poziomie istotności 10% dane potwierdzają, że odsetki poszczególnych
# zgonów rożni się od przedstawionych w artykule

###################################################################
#     4  chi2 4     pogrupowane w przedziały, czy stężenie ma rozkład normalny, duzo testow
###################################################################

# stezenie ozonu
# 78 kolejnych dni

# Z pliku

file=read.csv("normalnosc_ozon.csv",sep=";",dec=",")
ozon=file$ozon
br=c(0,2,4,6,8,10,12)
k=length(br)-1
series=cut(ozon,br)
y=table(series) # tabela grup, tworzenie grup


m=mean(ozon)
s=sd(ozon)
n=length(ozon)

hist(ozon,br,freq=F)
curve(dnorm(x,m,s),xlim=c(br[1],br[k+1]),col=3, add=TRUE)


observedf=c();
for (i in 1:dim(y)){
  observedf=c(observedf,y[[i]])
}
observedf


normprobabilities=c();
for (i in 1:length(br)-1) {
  normprobabilities=c(normprobabilities,pnorm(br[i+1],m,s)-pnorm(br[i],m,s))
}
normprobabilities[1]=pnorm(br[2],m,s)
normprobabilities[length(br)-1]=1-pnorm(br[length(br)-1],m,s)

normalfreq=normprobabilities*n
normalfreq

#Zauważmy, że w dwóch ostatnich klasach mamy mniej niż 5 obserwacji.
#Dlatego musimy połączyć te klasy.

normalfreq[k-1]=normalfreq[k-1]+normalfreq[k]
normalfreq=normalfreq[-c(k)]

observedf[k-1]=observedf[k-1]+observedf[k]
observedf=observedf[-c(k)]

normalfreq
observedf


k=k-1

expectedp=normalfreq/sum(normalfreq)
expectedp

chisq.test(observedf,p=expectedp)


# Jaki test powinniśmy użyć do oceny normalności
# Porównanie wyników:

# Pearson p.val tak naprawdę jest między p.val1, a p.val2
pearson.test(ozon, n.classes = 6, adjust=F) #p.val1 = 0.3708
pearson.test(ozon, n.classes = 6, adjust=T) #p.val2 = 0.1457
# tak czy siak p.val > alpha = 0.05
# Nie odrzucamy H0


# Kołmogorowa-lillieforsa - dobry tylko dla małych próbów n < 50

# Cramer von Mises
cvm.test(ozon) # p.val=0.332
# Nie odrzucamy H0

# Anderson-Darling
ad.test(ozon) #p.val = 0.275
# Nie odrzucamy H0

# Shapiro-Wilk
shapiro.test(ozon) #p.val = 0.1098
# Ciągle nie odrzucamy 

#Żaden z testów nie odrzuca hipotezy od normalności rozkłady stężenie ozonu 

###################################################################
#     6  chi2 6     czy oceny mają rozkład normalny, duzo testow, 15*4 ocen < 50.
###################################################################


punkty = points$punkty
cvm.test(punkty) # p.val < 0.01
ad.test(punkty)  # p.val < 0.01
shapiro.test(punkty) #p.val < 0.01
# odrzucamy H0, wszystkie mniejsze
br=seq(min(punkty), max(punkty), length=6)
hist(punkty, br, freq=F)


###################################################################
#     8  chi2 8     duzo danych (100)
###################################################################

# 100 pasazerow
# 3 linie, odp tak i nie
#czy zgubienie zależy od linii lotniczej?
tak=c(10,7,4)
nie=c(90,93,96)
#istotności 0,05

#H0: nie ma zależności miedzy proporcja 
#    zgubionych bagazy, a linia lotnicza

TK = data.frame(tak, nie) # Tablica Kontyngencji

chisq.test(TK) #p.val = 0.251
# alpha < p.val nie odrzucamy H0

#Na poziomie istotności 5% dane nie potwierdzają, że odsetek pasażerów
# którzy zgubili bagaż zależy od linii lotniczych



###################################################################
#     9  chi2 9     bardzo duzo danych (ok. 1000)
###################################################################
#3 odp wzgledem wieku

za =c(96, 96, 90, 36)
przeciw = c(201, 189, 195, 234)
nw = c(3,15,15,30)

tk = data.frame(za, przeciw, nw)

chisq.test(tk) # p.val = 0
#p-val < alpha odrzucamy h0
#na poziomie istotności alpha dane potwierdzają, że opinia o instalowaniu anted jest zależna od wieku

