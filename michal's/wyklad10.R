# Testy zgodności
# Przykład 1

# Zgodnie z tekstem to częstość powinna być taka sama dla każdego smaku 
# jeśli nie mają preferencji.
# Czy dane to potwierdzają ? Zgodnie z tekstem każdy smak powininen mieć 20 sprzedaży

observ = c(32, 28, 16, 14, 10)
oczek = rep(20, 5)

diff = observ - oczek

stat_test = sum((diff^2)/oczek) #18

k = length(observ) #ile mamy kategorii

alpha = 0.05

chi_q = qchisq(1-alpha, k-1) #9.49

# R = (9.49, INF), 18 należy do R  -> odrzucamy H0 

# Na poziomie istotności 5% dane potwierdzają, że klienci 
# mają preferencje dotyczące smaku soku

# p = prawdopodobieństwo częstości !!!
chisq.test(observ, p=oczek/sum(oczek)) 

# alpha = 0.05 > 0.0012 = p.val -> odrzucamy H0

# Test Normalności rozkładu

data = read.csv("normality_data.csv", sep=";")
t = data$t
u = data$u
min(t)
max(t)

br=seq(83,164,length=7)

hist(t, breaks=br, freq=F)

tbar = mean(t)
sdt = sd(t)
curve(dnorm(x, tbar, sdt), 80, 170, add=T)

obs=table(cut(t, breaks=br))
obs/sum(obs)

normpropabilities=c()
for(i in 1:length(br)-1){
  normpropabilities[i]=pnorm(br[i+1], tbar, sdt) - pnorm(br[i], tbar, sdt) 
}  
normpropabilities # nie sumuje się do jeden !

# Trzeba dodać pole idące do nieskończoności z obu stron
normpropabilities[1]=normpropabilities[1]+pnorm(br[1], tbar, sdt)
normpropabilities[1]=pnorm(br[2], tbar, sdt)
normpropabilities[6]=1-pnorm(br[6], tbar, sdt)

sum(normpropabilities)

# Teraz robimy test zgodności

chisq.test(obs, p=normpropabilities) 
# Dostajemy uwagę bo liczba oczekiwana ostatnie klasy normProb < 5
# Łączymy dwie ostatnie klasy łączymy w jedną 
normpropabilities[5] = normpropabilities[5] + normpropabilities[6]
normpropabilities=normpropabilities[-6]

obs[5] = obs[5] + obs[6]
obs = obs[-6]

chisq.test(obs, p=normpropabilities) #No warnings. p.val=0.3369

# alpha = 0.05 < 0.3369 = p.val -> nie odrzucamy H0

# H0 : zmienna ma rozkład normalny H1: zmienna nie ma rozkłady normalnego

# Na poziomie istotności 5% dane nie potwierdzają, że 
# zmienna t różni się istotnie od rozkładu normalnego

# Robienie takiego testu mocno zależy od wyboru szeregu rozdzielczego !!!
# Nie obiektywny sposób, mocno może się różnić wynikowe p.value

# Wpływ różnych testów na badania

pearson.test(t, n.classes = 7, adjust=T) # p.val = 0.03145
# Prawdziwe p.val coś pomiędzy
pearson.test(t, n.classes = 7, adjust=F) # p.val = 0.1016

# najsłabszy
lillie.test(t) #0.01708

#Najmocniejszy test
shapiro.test(t) #0.1259


# Test niezależności
# Przykład 3

piel = c(100,80,20)
lek = c(50,120,30)

TK = data.frame(piel, lek) # tablica kontengencji

chisq.test(TK) #p.val prawie 0 -> odrzucamy H0
