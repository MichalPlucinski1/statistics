points = read.csv("normalnosc_punkty.csv", sep = ";")
ozon = read.csv("normalnosc_ozon.csv", sep=";")

#1. 
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

#2.
obs=c(68, 27, 5)
n=sum(obs)
teor=c(0.74*n, 0.16*n, 0.10*n)
prop=c(0.74, 0.16, 0.10)

chisq.test(obs, p=prop)# p.val = 0.005121

#alpha = 0.1 > p.val -> odrzucamy H0

#Interpretacja
# Na poziomie istotności 10% dane potwierdzają, że odsetki poszczególnych
# zgonów rożni się od przedstawionych w artykule

#4.

# Z pliku
####################################################
file=read.csv("normalnosc_ozon.csv",sep=";",dec=",")
ozon=file$ozon
br=c(0,2,4,6,8,10,12)
k=length(br)-1
series=cut(ozon,br)
y=table(series)

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

#Zauważmy, że w dwóch ostatnich klasach mamy mniej niż 5 obserwacji. Dlatego musimy połączyć te klasy.

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
############################################################################

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
# nad rondem kapoeira w Poznaniu 

#6.

punkty = points$punkty
cvm.test(punkty) # p.val małe
ad.test(punkty)  # p.val małe
shapiro.test(punkty) #p.val małe
# odrzucamy H0
br=seq(min(punkty), max(punkty), length=6)
hist(punkty, br, freq=F)

#8.
tak=c(10,7,4)
nie=c(90,93,96)

#H0: nie ma zależności miedzy proporcja 
#    zgubionych bagazy, a linia lotnicza

TK = data.frame(tak, nie) # Tablica Kontyngencji

chisq.test(TK) #p.val = 0.251
# alpha < p.val nie odrzucamy H0

#Na poziomie istotności 5% dane nie potwierdzają, że odsetek pasażerów
# którzy zgubili bagaż zależy od linii lotniczych

#9.

za =c(96, 96, 90, 36)
przeciw = c(201, 189, 195, 234)
nw = c(3,15,15,30)

tk = data.frame(za, przeciw, nw)

chisq.test(tk) # p.val = 0
# odrzucamy H0

# Na pozimie istotności alpha dane potwierdzają, że coś tam
