dane = read.csv("DwiePopulacje.csv", sep=";")

#1. 

###############################################
#1 2pop 1 Jednorodność wariancji i normalność rozkładu (1) przypadek
###############################################



#Dla regionu I poddano analizie 8 próbek drewna
# dla regionu II przebadano 21 próbek drewna

#Przyjmując
#normalność rozkładu zawartości celulozy w drewnie i poziom istotności 0,02


alpha = 0.02
wood1 = na.omit(dane$cel1)
wood2 = na.omit(dane$cel2)

###############################################
#a) #verify hipotezę, że przeciętna zawartość celulozy dla regionu I różni się istotnie od przeciętnej
#zawartości celulozy dla regionu II.

#Przyjmij jednorodność wariancji populacji i normalność rozkładu
#badanej cechy;


#H0:   mi1 = mi2  H1: mi1 != mi2 
#H0:   mi1-mi2=0  H1: mi1-mi2!=0 


t.test(wood1, wood2, var.equal = TRUE, mu=0, alternative = "two.sided")


# alpha = 0.02 < 0.1352 = p.value -> nie odrzucamy H0

# Na poziomie istotności 2% dane nie potwierdzają, że średnia zawartość celulozy
# w drewnie danego typu z dwóch regionów A i B róźni się istotnie


###############################################
#b)
#sprawdź, czy założenie o równości wariancji było słuszne
#H0: sig1sq = sig2sq  H1: sig1sq != sig2sq
library(PairedData)

var.test(wood1, wood2, alternative = "two.sided")

# alpha = 0.02 < 0.3225 = p.value -> nie odrzucamu H0

# Na poziomie istotności 2% dane nie potwierdzają, że wariancje są niejdednorodne
# a, więc słusznie było przyjąć ich jednorodność



###############################################
#2 2pop 2   Normalność rozkładów bez założenia równości wariancji (2) przypadek
###############################################

#założono, że czas budowy metodą tradycyjną jest dłuższy niż czas
#budowy nową

# 10 obiektow stara
# 11 nowa

#rozkład czasu metodą tradycyjną jak i nową jest normalny, 
#zweryfikuj hipotezę, że średni czas budowy  tradycyjną jest dłuższy od średniego czasu nową. 

alpha = 0.1
b_trad = na.omit(dane$tradycyjna)
b_new = na.omit(dane$nowa)



sum(b_trad)/length(b_trad)
#a) wariancja rowne

#H0: mi_t <= mi_n   H1: mi_t > mi_n

#Najpierw trzeba zrobić test o jednorodniści wariancji

#H0: sig1sq = sig2sq   H1: sig1sq != sig2sq

var.test(b_trad, b_new, alternative = "two.sided")

# alpha = 0.1 < 0.3613 = p.val -> nie odrzucamy H0

# na poziomie istotności 10% dane nie potwierdzają, że wariancje nie są jednorodne
################ 
#b) srednia
t.test(b_trad, b_new, var.equal = T, alternative="greater", mu=0)

# alpha = 0.1 < 0.6139 -> nie odrzucamy H0

# na poziomie istotności 10% dane nie potwierdzają, że tradycyjna metoda budowa  
# jest średnio szybsza od tradycyjnej



###############################################
#3 2pop 3 Normalność rozkładów bez założenia równości wariancji (2) przypadek
###############################################


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



###############################################
#4 2pop 4 Normalność rozkładów bez założenia równości wariancji (2) przypadek
###############################################
#porownanie regullarnosci wynikow

alpha = 0.05
player1 = na.omit(dane$zawodnik1)
player2 = na.omit(dane$zawodnik2)

#Na poziomie istotności 0,05 zweryfikuj hipotezę 
#o większej regularności wyników pierwszego zawodnika. 

#H0: sig1sq >= sig1sq   H1: sig1sq < sig2sq


var.test(player1, player2, alternative = "less")

# alpha = 0.05 < 0.21 = p.value -> nie odrzucamy H0

# Na poziomie istotności 
















