# Przykład 1


typ1=c(2830, 2840, 2800, 2880,2820)
typ2=c(2790, 2720, 2770, 2780, 2760)

alpha = 0.01
#Przypadek (1) - Normalność i równe wariancje

#a)
n1=length(typ1)
n2=length(typ2)
xbar=mean(typ1)
ybar=mean(typ2)
s1sq=var(typ1)
s2sq=var(typ2)
df=n1+n2-2 # Degrees of freedom

Ssq_p = ((n1-1)*s1sq+(n2-1)*s2sq)/df
(880 + 730)/2 # Równe Ssq_p

kwantyl=qt(1-alpha/2,df)

estErr=kwantyl*sqrt(Ssq_p*(1/n1+1/n2))
L=xbar-ybar-estErr
U=xbar-ybar+estErr

# Z ufnością 99% przedział od 9.78 h do 130.22 pokrywa nieznaną różnicę średnich
# czasów diałania dwóch typów żarówek energooszczędnych

rrr = t.test(typ1, typ2, var.equal = T, conf.level = 1-alpha)
rrr$conf.int

#b)
# Przedział wychodzi dodatni, więc widać, że mi1 jest większa
# Lecz czy to jest znaczącą różnica?

#H0:      mi1  <=  mu2           H1:      mi1 > mi2

mi0 = 0

# Statystyka testowa
t = (xbar - ybar-mi0)/(sqrt(Ssq_p*(1/n1+1/n2))) #3.9

# Obszar krytyczny
qt(1-alpha,df) # do nieskonczonosci

#R  = (2.89, INF)
# Chcemy mieć jak najszerszy obszar krytyczny !

#t należy do R -> odrzucamy H0

# Na poziomie istotności 1% dane potwierdzają, że żywotność żarówek pierwszego typu 
# jest dłuższa niż żarówek drugiego typu.

rrrr = t.test(typ1, typ2, var.equal = T, mu=0, alternative = "greater")
rrrr$p.value
# alpha = 0.01  >  p.value = 0.0022 -> odrzucamy H0

# porównywanie wariancji

# Równa się zawsze jest w H0 !!!!!!
# Sprawdzamy jednorodność wariancji 
# H0: sigma1sq = sigma2sq    H1: sigma1sq != sigma2sq

#Statystyka f
f = s1sq/s2sq # 1.205

# Obszar krytyczny
lewy_kwantyl = qf(alpha/2, n1-1, n2-1)
prawy_kwantyl = qf(1-alpha/2, n1-1, n2-1)
# R = (-INF, 0.044) u (23.15, INF)

# Statystyka f = 1.205 nie należy do obszaru krytycznego 
# Zatem nie odrzucamy H0

#Na poziomie istotności 1% dane nie potwierdzają, że wariancję są różne

library(PairedData)
var.test(typ1, typ2)$p.value
# alpha = 0.01  <  p.value = 0.86 -> nie odrzucamy H0

# Przykład 2

#tradycyjna
tt=63
nt=100
#nowa
tn=107
nn=150

#manualnie
alpha=0.05
z=qnorm(1-alpha/2)

phatn=tn/nn
phatt=tt/nt

#nowa - tradycyjna (przedział ufności?)
estErr = z*sqrt(phatn*(1-phatn)/nn + phatt*(1-phatt)/nt)
L=phatn-phatt - estErr
U=phatn-phatt + estErr

U-L

# Z ufnością 95% przedział od -0,03 do 0.21 pokrywa nieznaną 
# różnicę proporcji osób, które zdały egzamin będąc uczonym metodą
# AW a T.

pp=prop.test(c(tt,tn), c(nt, nn), conf.level = 1-alpha)
pp$conf.int[2]-pp$conf.int[1]


#b)

#   H0:  pt >= pn     H1:    pt < pn

#manualnie

#Statystyka testowa 
phat = (tn+tt)/(nn+nt)
# KOlejność ma znaczenie ! Musi być tak jak w hipotezie 
z_t = (phatt-phatn)/sqrt(phat*(1-phat)*(1/nn+1/nt))

#Obszar krytyczny (-INF, -z)
z = -qnorm(1-alpha) 
# R (-inf, -1.64)

# z=-1.38 nie należy do przedziału R -> brak pdostaw do odrzuczenia H0

# Na poziomie istotności 5% dane nie potweirdzającą hipotezy zerowej 
# nowa metda jest skuteczniejsza.

# KOlejność ma znaczenie ! Musi być tak jak w hipotezie 
prop.test(c(tt, tn), c(nt, nn), alternative="less")
#alpha = 0.05  <  p.value = 0.1065  -> Nie odrzucamy H0

#Z istotnością 5% nie możemy stwierdzić, ze nowa metoda jest lepsza



# Przykład 3

przed = c(70,80,72,76,76,76,72,78,82,64,74, 92, 74, 68, 84)
po = c(68,72,62,70,58,66,68,52,64,72,74,60,74,72,74)

d=przed-po
alpha=0.05

# Estymacja (przedział ufności)
# Normalność, nie znamy sigma (t.test)

tttt = t.test(d)
tttt$conf.int

# Z ufnością 95% przedział od 2.72 mmHG do 14.88 mmHG  pokrywa nieznaną średnią 
# różnicę ciśnienia krwi przed i po podaniu leku,


# Hipotezy

# H0: miprzed <= mipo       H1: miprzed > mipo
# H0: miprzed - mipo <= 0   H1: miprzed - mipo > 0
# H0: miD <= 0              H1: miD > 0

t.test(d, mu=0, alternative = "greater")

# alpha = 0.05  >   p-value = 0.003875 -> mamy podstawy do odrzucenia hipotezy zerowej

#Na poziomie istotności 5% dane potwierdzają skuteczność leku 