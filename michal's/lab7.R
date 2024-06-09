dane = read.csv("DwiePopulacje.csv", sep=";")

#1. 

alpha = 0.02
wood1 = na.omit(dane$cel1)
wood2 = na.omit(dane$cel2)

#a) # Jednorodność wariancji i normalność rozkładu (1) przypadek

#H0:   mi1 = mi2  H1: mi1 != mi2 
#H0:   mi1-mi2=0  H1: mi1-mi2!=0 


t.test(wood1, wood2, var.equal = TRUE, mu=0, alternative = "two.sided")


# alpha = 0.02 < 0.1352 = p.value -> nie odrzucamy H0

# Na poziomie istotności 2% dane nie potwierdzają, że średnia zawartość celulozy
# w drewnie danego typu z dwóch regionów A i B róźni się istotnie

#b)

#H0: sig1sq = sig2sq  H1: sig1sq != sig2sq
library(PairedData)

var.test(wood1, wood2, alternative = "two.sided")

# alpha = 0.02 < 0.3225 = p.value -> nie odrzucamu H0

# Na poziomie istotności 2% dane nie potwierdzają, że wariancje są niejdednorodne
# a, więc słusznie było przyjąć ich jednorodność

#c)
 
t.test(wood1, wood2, var.equal = T , conf.level = 1-alpha)

# Z ufnością 98% przedział od -13.5 do 3.15 pokrywa prawdziwą 
# różnicę średnich zawartości celulozy w drewnie pochodzącego 
# z dwóch reginów Polski 

# R = (-13.52, 3.143), 0 znajduje się w obszarze krytycznym

# Decyzja jest jednoznaczna jak z podpunktu a) 


#2. Normalność rozkładów bez założenia równości wariancji (2) przypadek 

alpha = 0.1
b_trad = na.omit(dane$tradycyjna)
b_new = na.omit(dane$nowa)

sum(b_trad)/length(b_trad)
#a)

#H0: mi_t <= mi_n   H1: mi_t > mi_n

#Najpierw trzeba zrobić test o jednorodniści wariancji

#H0: sig1sq = sig2sq   H1: sig1sq != sig2sq

var.test(b_trad, b_new, alternative = "two.sided")

# alpha = 0.1 < 0.3613 = p.val -> nie odrzucamy H0

# na poziomie istotności 10% dane nie potwierdzają, że wariancje nie są jednorodne

t.test(b_trad, b_new, var.equal = T, alternative="greater", mu=0)

# alpha = 0.1 < 0.6139 -> nie odrzucamy H0

# na poziomie istotności 10% dane nie potwierdzają, że tradycyjna metoda budowa  
# jest średnio szybsza od tradycyjnej


#3. 

alpha = 0.1

banks1 = na.omit(dane$publiczny)
banks2 = na.omit(dane$prywatny)

#H0: mi1 >= mi2    H1: mi1 < mi2 

#Badamy jednorodność wariancji 


#H0: sig1sq = sig2sq   H1: sig1sq != sig2sq

var.test(banks1, banks2, alternative = "two.sided")

# alpha = 0.1 > 0.086 = p.value -> odrzucamy H0

# Na poziomie istotności 10% dane potwierdzają, że wartości wariancji 
# nie są jednorodne


t.test(banks1, banks2, var.equal = F, alternative="less", mu=0)

# alpha = 0.1 > 0.023 -> odrzucamy H0

# na poziomie istotności 10% dane potwierdzają, że publicznie źródła
#  udzielają średnio wyższych kredytów niż prywatne


#4. 

alpha = 0.05
player1 = na.omit(dane$zawodnik1)
player2 = na.omit(dane$zawodnik2)

#H0: sig1sq >= sig1sq   H1: sig1sq < sig2sq


var.test(player1, player2, alternative = "less")

# alpha = 0.05 < 0.21 = p.value -> nie odrzucamy H0

# Na poziomie istotności 