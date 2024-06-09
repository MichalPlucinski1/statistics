# Wykład 8 - Analiza wariancyjna (ANOVA)

# Przykład z górczniczymi pacmanami

dane = read.csv("anova_bacteria1.csv", sep=";")

# Pytanie: Jakiej narodowości kupić pacmany (balans cena, jakość)

# H0:  mi1=mi2=mi3    H1: ~H0 
class(dane) #data.frame

b1=na.omit(dane$b1)
b2=na.omit(dane$b2)
b3=na.omit(dane$b3)

t1=sum(b1)
t2=sum(b2)
t3=sum(b3)

t = t1 + t2 + t3

n1 = length(b1)
n2 = length(b2)
n3 = length(b3)

n=n1+n2+n3

k = 3

sumAllSq=sum(b1^2) + sum(b2^2) + sum(b3^2)

ssg = sumAllSq-t^2/n # G

sst=t1^2/n1+t2^2/n2+t3^2/n3-t^2/n # T

sse = ssg - sst # E

MST = sst / (k-1)

MSE = sse / (n-k)

F_stat = MST/MSE

# Tabela ANOVA

# źródło    |    SS     \  Degrees of  |  MS  |  F   |   QF
#zmienności |           \   Freedom    |      |      |

# T         |  323.33   \     2        161.69   4.048 > 3.402
     
# E         |  958.62   \     24       39.94

# G         |  1282     \     26

alpha=0.05

QF = qf(1-alpha, k-1, n-k)


# Odrzucamy hipoteze zerową F > QF


# Test Bartletta

# Dane muszą być w data.frame
class(dane)

obiekty=rep(names(dane))
obiekty=c(rep("b1", n1), rep("b2", n2), rep("b3", n3))
wyniki=c(b1,b2,b3) # długi wektor wartości 

data.frame(obiekty, wyniki)

# Test jednorodności wariancji !
# H0: sig1sq=sig2sq=sig3sq 
bartlett.test(wyniki, obiekty);

#alpha = 0.05 < 0.77 = p.value -> nie odrzucamy H0
# możemy przeprowadzić analizę wariancji

#H0: mi1=mi2=mi3 H1: ~H0
model=lm(wyniki~obiekty)

anova(model) # Zamiast qf ma p.value 

# alpha = 0.05 > 0.03 = p.value -> odrzucamy H0

# New test
#
# Chcemy sprawdzić, które odmiany Pacmanów są podobne 
#

# Przedziały ufności dla mi_i - mi_j


plot(TukeyHSD(aov(wyniki~obiekty), ordered=T, conf.level=0.95))

# p adj jest < alpha tylko dla b2-b3

# dla wykresu patrzymy czy przedział każdego porównania ma w sobie 0 
# jeśli ma to są one jednorodne (podobne do siebie)

# 1 i 3 są podobne

# 2 i 1 są podobne

# 2 z 3 nie są podobne 
     

###
### Układ blokowy
###

dane2=read.csv("anova_bacteria2.csv", sep=";")

lab=names(dane2)

b1=dane2$bac1
b2=dane2$bac1
b3=dane2$bac2

wyniki = c(b1, b2, b3)
length(wyniki)
obiekty=c(rep(lab[2], length(b1)), rep(lab[3], length(b2)), rep(lab[4], length(b3)))
length(obiekty)
bloki = c(rep(dane2$X, 3))
length(bloki)
data.frame(obiekty, wyniki, bloki)

bartlett.test(wyniki, obiekty)
bartlett.test(wyniki, bloki)

# H0: tau1 = tau2 = tau3
anova(lm(wyniki ~ obiekty + bloki))
# alpha = 0.05 > -2.6*10^-6 = p.value -> odrzucamy H0

# uwzględniając efekty lokalizacji na poziomie istotności 5% dane potwierdzają, 
# że odmiana bakterii ma wpływ na średni urobek.

# dla testu
anova(lm(wyniki~obiekty))

# Przykład dwukierunkowa Anova

dane3=ToothGrowth

len=dane3$len
dose=dane3$dose
supp=dane3$supp

#Czy możemy przeprwodzić analize wariancji
bartlett.test(len, supp) #0.233 > 0.05
bartlett.test(len, dose) #0.717 > 0.05
#Mozna

anova(lm(len~dose + supp)) # bez efektu interakcji

# Na poziomie istotności 5% dane potweirdzają, że zarówno metoda podawania jak 
# i dawka witaminy C mają wpływ na wielkość odonblastów u świnek morskich

anova(lm(len~dose + supp + supp*dose)) #Bierze pod uwagę efeky interakcji

#Back to Bacterie 
# Inconsistend row duplicates

wyniki=c(35,	31,	33,	29,	35,	41,	53,	41,	40,	31,	41,	33,	37,	32,	34,	43,	45,	43,	41,	27,	27,	25,	24,	26,	29,	37,	39,	33,	36,	22)
length(wyniki)
obiekty=c("bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac1",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac2",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3",	"bac3")
length(obiekty)
bloki=c("s1",	"s1",	"s1",	"s2",	"s2",	"s2",	"s3",	"s3",	"s4",	"s4",	"s1",	"s1",	"s2",	"s2",	"s2",	"s3",	"s3",	"s3",	"s4",	"s4",	"s1",	"s1",	"s2",	"s2",	"s3",	"s3",	"s3",	"s4",	"s4",	"s4")
length(bloki)

razem=data.frame(wyniki,obiekty,bloki)
razem

bartlett.test(wyniki, obiekty) #0.8277 > 0.05
bartlett.test(wyniki, bloki) #0.8891 > 0.05

anova(lm(wyniki~obiekty + bloki))

# W każdym bloku musi przynajmniej raz być zmierzony każdy z obiektów

# Detmer Advise
# Pokazuje Wszysktie zależności na temat jednorodności
TukeyHSD(aov(wyniki~obiekty + bloki), ordered=T)
