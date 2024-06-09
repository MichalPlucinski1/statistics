# Regresja !

#1. 

chem = read.csv("Reg_chemikalia.csv", sep=";")
x = chem$surowiec
y = chem$produkt

#a)
plot(y~x) # y~x = y zależy od x 

#b)
n = length(x)
cov(x, y) #138.4889 
1/(n-1)*(sum(x*y)-length(x)*(mean(x)*mean(y))) #138.4889

# Kowariancja nie jest zerowa, a więc istnieje pewna 
# liniowa zależność między ilością produktu a wykorzystaną ilością surowcem

#c)
cor(x, y) #0.895
cov(x, y)/(sd(x)*sd(y)) #0.895

# Istnieje bardzo silna dodatnia zależność liniowa między 
# ilością produktu, a wykorzystaną ilośćią surowca 

#d)

#LSE
b1hat = cov(x, y)/var(x) #3.619
b0hat = mean(y) - b1*mean(x) #22.404

prosta = lm(y~x)
prosta$coefficients #Intercept = b0hat, x = b1hat
b0hat = prosta$coefficients[1]
b1hat = prosta$coefficients[2]

# y =  22.404 + 3.619*x 

#e)
curve(b0hat + b1hat*x, 0, 30, add=T)
abline(prosta)

#f)
# Wzrośnie o współczynnik b1, jako, że x jest wyznaczony w litrach 
# i nie trzeba zmieniać jednostek

#g)
b0hat + b1hat*20 #94.78571 kg
predict(prosta, data.frame(x=20))
#h)
b0hat + b1hat*15 #76.69 kg
predict(prosta, data.frame(x=15))

predict(prosta, data.frame(x=c(15,20)))

#i)
#Współczynnik determinacji
Rsq = cov(x, y)^2/(var(x)*var(y)) #80.1 %
Rsq = cor(x,y)^2

# Zmienność wielkość produkcji w 80% wyjaśniona 
# jest przez zmienność ilości surowca 205 to zmienność losowa

# Przy 80% wyjaśnionej zmienności można odpuścić znajdowanie 
# bardziej skomplikowanych modeli

#j

alpha = 0.05

# H0: b1 = 0 H1: b1 != 0

anova(lm(y~x)) #p.val = 0.0004617 

# alpha = 0.05 > p.val -> odrzucamy H0

# Na poziomie istotności 5% dane potwierdzają, że regresja liniowa jest istotna
  
