

###################################################################
#  1 regresja   wykres punktowy przedstawiający zależność wielkości produkcji od ilości zużytego 
###################################################################

#wyznaczyć relację miedzy końcową
#wielkością produkcji środków chemicznych Y (w kg) 
#w zależności od ilości zużytego surowca X (w litrach)

#a)punktowy przedstawiający zależność wielkości produkcji od ilości zużytego 

chem = read.csv("Reg_chemikalia.csv", sep=";")
x = chem$surowiec
y = chem$produkt

#a)
plot(y~x) # y~x = y zależy od x 

###################################################################

#b
#Wyznacz i zinterpretuj kowariancję próbkową między ilością zużytego surowca a wielkością produkcji.


n = length(x)
cov(x, y) #138.4889 
1/(n-1)*(sum(x*y)-length(x)*(mean(x)*mean(y))) #138.4889


# Kowariancja nie jest zerowa, a więc istnieje pewna 
# liniowa zależność między ilością produktu a wykorzystaną ilością surowcem
###################################################################

# c
#Wyznacz i zinterpretuj współczynnik korelacji.

cor(x, y) #0.895
cov(x, y)/(sd(x)*sd(y)) #0.895

# Istnieje bardzo silna dodatnia zależność liniowa między 
# ilością produktu, a wykorzystaną ilośćią surowca 
###################################################################

#d
#Wyznacz ocenę prostej regresji między wielkością produkcji a ilością zużytego surowca.

#LSE
b1hat = cov(x, y)/var(x) #3.619
b0hat = mean(y) - b1hat*mean(x) #22.404

prosta = lm(y~x)
prosta$coefficients #Intercept = b0hat, x = b1hat
b0hat = prosta$coefficients[1]
b1hat = prosta$coefficients[2]

# y =  22.404 + 3.619*x 

###################################################################

#e) 
#Dodaj do wykresu punktowego prostą regresji.
curve(b0hat + b1hat*x, 0, 30, add=T)
abline(prosta)
###################################################################

#f)
#W jaki sposób zmieni się wielkość produkcji, jeśli ilość surowca wzrośnie o 1 litr? 
# Wzrośnie o współczynnik b1, jako, że x jest wyznaczony w litrach 
# i nie trzeba zmieniać jednostek


###################################################################

#g)
#Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 20 litrów surowca?
b0hat + b1hat*20 #94.78571 kg
predict(prosta, data.frame(x=20))
###################################################################

#h)
#Jaka będzie wielkość produkcji, jeśli zużyjemy do produkcji 15 litrów surowca?
b0hat + b1hat*15 #76.69 kg
predict(prosta, data.frame(x=15))

predict(prosta, data.frame(x=c(15,20)))


###################################################################

#i)
#Oceń dopasowanie prostej regresji do danych.
#Współczynnik determinacji
Rsq = cov(x, y)^2/(var(x)*var(y)) #80.1 %
Rsq = cor(x,y)^2

# Zmienność wielkość produkcji w 80% wyjaśniona 
# jest przez zmienność ilości surowca 205 to zmienność losowa

# Przy 80% wyjaśnionej zmienności można odpuścić znajdowanie 
# bardziej skomplikowanych modeli

###################################################################

#j
#Zweryfikuj test o istotności regresji. 
#Przyjmij poziom istotności 5%. Zinterpretuj wynik

alpha = 0.05

# H0: b1 = 0 H1: b1 != 0

anova(lm(y~x)) #p.val = 0.0004617 

# alpha = 0.05 > p.val -> odrzucamy H0

# Na poziomie istotności 5% dane potwierdzają, że regresja liniowa jest istotna





