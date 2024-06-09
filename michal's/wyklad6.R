#Testowanie hipotez 
# Przykład 2

opony = c(53.417, 51, 48.583, 53, 49, 51, 52, 50)
n=length(opony)
xbar=mean(opony)
s=sd(opony) # sigma obliczona
alpha=0.05

#(1)
#H0: mi <= 50  | H1: mi > 50
# rozkład jest normalny -> przypadek (1, 3)
# prawdziwa Sigma nie jest znana -> przypadek 3

#(2) przypadek 3
mi0=50 # wartość w hipotezie

# Statystyka testowa
t = (xbar - mi0)/(s/sqrt(n)) #1.607

# (3)
#obszar krytyczny dla tych hipotez to
qt(1-alpha, n-1) #1.894
# nasz przedział to R=(1.894, INF)


# (4)
# t nie należy do R !!!
# brak podstaw do odrzucenia H0 !

# (5)
# Interpretacja
# Na poziome istotności 5% dane nie potwierdzają, że 
# średnia żywotność opon przekracza 50 tyś mil.

# Przy zmianie alpha, jedyne co się zmienia to poziom krytyczny 
# Może to wpłynąć na interpretacje hipotezy

alpha = 0.1
qt(1-alpha, n-1) #1.414
# R= (1.414, INF)
# t = 1.607, należy do R -> odrzucamy H0
# Na poziomie istotności 10% dane potwierdzają, że 
# średnia żywotność opon przekracza 50 tyś mil

t.test(opony, mu=mi0, alternative="greater") #ważne ! (p-value)

# df - degrees of freedom (stopnie swobody) (n-1)
#----

# alpha = 0.05 p-value = 0.07 -> krytyczny poziom istotności 
#                                w którym zmieniamy interpretacje

#----


# Testowanie hipotez sig^2

#Przykład 3

czas=c(10, 10, 15, 12, 9, 8, 4, 10, 3, 4, 6, 5, 7, 8, 13)
n=length(czas)
ssq=var(czas)
alpha=0.05

#(1)
# H0: sigmasq >= 25   H1: sigmasq < 25

#(2)
sigmasq0=25 #Oblicznona
chisq = (n-1)*ssq / sigmasq0 #6.917

#(3)
qchisq(alpha,n-1) #6.57
# R = (0, 6.57)
# (4) chsq nie należy do R -> brak podstaw do odrzucenia H0

# (5) Interpretacja
# Na poziomie istotności 5% dnae nie potrwierdzają przypuszczenia
# że zróżnicowanie czasu naprawy urządzeń jest krótsze niż 25 h^2

sigma.test(czas, sigmasq=sigmasq0, alternative="less")
# alpha = 0.05    <   p-value = 0.06 -> brak podstaw do odrzucenia H0



# Testowanie hipotez dla populacji p
# Przykład 4

n=200
t=111
phat=t/n
alpha=0.05
p0=0.5

# (1)
#H0:   p = 0.5    #H1:    p != 0.5

# (2)
# Tylko jedna możliwość statystyki testowej
z=(phat-p0)/sqrt(p0*(1-p0)/n)

# (3)

qnorm(1-alpha/2) #1.959
# R = (-inf, -1.96) lub (1.96, inf)

# (4)
# z nie należy do R
# brak podstaw do odrzucenia H0

# (5)
# Na poziomie ufności 5% dane nie potwierdzają, że po 10
# latach proporcja inżynierów pozostających w swojej profesji
# jest różna od 50% 

# Daje zawsze najszerszy przedział ufności (gorszy wynik)
binom.test(t, n, p=0.5, alternative = "two.sided", conf.level = 1-alpha)

# alpha = 0.05  <  p-value =0.137  -> brak podstaw do odrzucenia H0

prop.test(t, n, p=0.5, alternative="two.sided", conf.level = 1-alpha)

# Takie same wartości (ale mogą się różnić lekko w innych przypadkach)
# alpha = 0.05  <  p-value =0.137  -> brak podstaw do odrzucenia H0
