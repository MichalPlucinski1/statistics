dane = read.csv("MatProd.csv", sep = ";")

# Produkcja ma zależeć od tego ile materiału zużywa

plot(dane$materialy, dane$produkcja)
# Widać pewne podobieństwo do funckji liniowej dodatniej

cov(dane$materialy, dane$produkcja) #27.8

# Kowariancja jest różna od 0, zatem istnieje związek 
# między ilością zużytych materiałów, a wartością produkcji

# Kowariancja jest dodatnia, a więc związek jest rosnący
# Im więcej materiałów zużyjemy tym większa będzie wielkość produkcji

cor(dane$materialy, dane$produkcja) #0.967

# Istnieje bardzo silna zależność liniowa między
# ilością materiałów, a wartością produkcji

b1hat = cov(dane$materialy, dane$produkcja)/var(dane$materialy) #0.2222
b0hat = mean(dane$produkcja) - b1hat * mean(dane$materialy) #2.3562

# oszacowane Równanie prostej regresji
# yhat = 2.36 + 0.22x

curve(b0hat + b1hat*x, 0, 30, add=T)

regLin = lm(dane$produkcja ~ dane$materialy)
# Intercepts -> b0
abline(regLin)

(b0hat + 23*b1hat)*1000

x = dane$materialy
y = dane$produkcja

prosta = lm(y~x)
predict(prosta, data.frame(x=23))
predict(prosta, data.frame(x=c(23, 14)))

#Współczynnik determinacji
(cor(x, y))^2

# Weryfikacja hipotezy istotności regresji

# H0: b1=0    H1:  b1 != 0

anova(prosta)

# Alpha = 0.01 > 0=p.value -> odrzucamy H0

# Na poziomie istotności 0.01 dane potwierdzają, że regresja jest istotna

summary(prosta)
# Idzie to przez t.test
# Multiple R-squared: współczynnik determinacji
