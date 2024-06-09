#Lab 6

dane=read.csv("dane_hip.csv", sep=";")

#1.

# Ręcznie

# (1)
# Hipotezy (na podstawie średniej)
# H0: mi <= 4 [m/s]   H1: mi > 4 [m/s]

# (2)
# W ~ N(mi, sig), mi i sig nieznane (3) [chyba, nie sprawdzałem]

#(3)
# Statystyka testowa
wind = na.omit(dane$wiatr)
n=length(wind)
alpha = 0.05
xbar = mean(wind)
s = sd(wind)
mi0 = 4 # to co jest w hipotezie

t = (xbar-mi0)/(s/sqrt(n)) #2.4

#(4) 
# Wyznaczanie obszaru krytycznego 
# Obszart krtytczny (t_n-1,1-alpha, INF)
kwant = qt(1-alpha,n-1)

# R = (1.796, INF)
# 2.4 należy do obszaru krytycznego -> odrzucamy hipotezę H0

#(5) Interpretacja

# Na poziomie istotności 5% dane potwierdzają, że średnia wartość wiatru w badanej 
# okolicy przekraczają 4 m/s.

# Za pomocą testów

#(1)
# Hipotezy (na podstawie średniej)
# H0: mi <= 4 [m/s]   H1: mi > 4 [m/s]

# (2)
# W ~ N(mi, sig), mi i sig nieznane (3)

# (3)
p1=t.test(wind, mu=mi0, alternative="greater")$p.value

# (4)
# alpha = 0.05  >   (p1) p-value=0.01705  -> odrzucamy hipotezę H0


#(5) Interpretacja
# Na poziomie istotności 5% dane potwierdzają, że średnia wartość wiatru w badanej 
# okolicy przekraczają 4 m/s.


#2. 

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

#3. 

#(1)
#H0:  mi = 870  H1: mi != 870

#(2) G ~ N(mi, sig), znana sigma (1) finish yourself

#4.

#H0:  mi  <= 0.04  H1:   mi > 0.04

#(2) Nie jest normalny rozkład, próba duża (2)
# finish again

#5. 

milk = na.omit(dane$mleko)
n=length(milk)
#a)
#(1)
#H0:  mi = 1.7    H1: mi != 1.7

#(2) Przypadek trzeci

#finish 

#b)

#(1)
#H0:  sig^2 >= 0.02   H1:  sig^2 < 0.02 

#(2) przypadek trzeci

#Ręcznie
ssq0=0.02
s=var(milk)
alpha=0.05
chisq=((n-1)*s)/ssq0

# Obszar krytyczny 
# R (0, qchisq(aplha, n-1))

qchisq(alpha, n-1) #3.32

# chisq nie należy do R -> nie odrzucamy H0

# używając testów
library(TeachingDemos)
sigma.test(milk, sigmasq=ssq0, alternative="less")

# alpha=0,05  <   p.value=0.1835 -> nie odrzucamy H0

#Na poziomie istotności 5% danie nie potwierdzają, że wariancja zawartości mleka
# jest mniejsz niż 0,02

#10.

n=1100
t=1000
alpha=0.05

#H0: p <= 0.9    H1: p > 0.9

phat=1000/1100
pZ=0.9

#ręcznie 

z=(phat - pZ)/(sqrt(pZ*(1-pZ)/n)) # 1.005

# R = (1.644, INF) 
# t nie należy do R -> nie odrzucamy H0

# na poziomie istotności 0.05 dane nie potwierdzają, że 
# 90% polaków nie czyta kśiążek

binom.test(t, n, p=pZ, alternative="greater")

#alpha < p.value -> nie odrzucamy H0