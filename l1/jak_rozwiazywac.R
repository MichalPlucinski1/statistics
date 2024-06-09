#------------------------------test średniej (mniejsza lub większa) dla 1 MAŁEJ populacji 

#(1) sformułowanie hipotezy
#H0: mu <=  XXX (liczba) H1: mu > XXX (liczba)

#(2)obliczenie statystyki testowej - Rozkład normalny, sigma (odch. std.) nieznane => statystyka T (w5. s. 18 (3))
t.test(wiatr, mu = 4, alternative="greater") #greater, bo w H1 jest ">"

#(3) p-value = JEDEN_Z_WYNIKÓW_TTEST
#(4) podjecie decyzji
# alfa=0.05 > p-value = 0.01705 => odrzucamy H0

#(5) Interpretacja 
#na poziomie istotności alfa=0.05 dane potwierdzają hipotezę, że średnia ..............
#   np.  => Sensowna jest budowa elektrowni wiatrowej w okolicach Darłowa


#---------------------test średniej (różna) dla 1 MAŁEJ populacji

#(1) sformułowanie hipotezy
#H0: mu = XXX   H1: mu != XXX

SD= #podane w zadaniu
ALFA = #podane w zadaniu

#(2)obliczenie statystyki testowej - Rozkład normalny, sigma (odch. std.) znane => statystyka z (w5. s. 16 (1))
z.test(POPULACJA, sigma.x = SD, mu = XXX, alternative="two.sided")

#(3) p-value=wynik z.test
#(4) podjecie decyzji
# alfa=0.05 < p-value =  => brak podstaw do odrzucenia H0

#(5) Interpretacja 
#na poziomie istotności alfa=0.05 dane nie potwierdzają hipotezy, że średnia ...


#--------------------- test średniej (mniejsza lub większa) dla 1 DUŻEJ populacji

#(1) sformułowanie hipotezy
#grubsze niż
#H0: mu <= XXX  H1: mu > XXX

#(2)obliczenie statystyki testowej - dowolny rozkład, duża próba => statystyka zsum (w5. s. 17 (2))
zsum.test(mean(POPULACJA), sd(POPULACJA), length(POPULACJA), mu = XXX, alternative="greater")

#(3) p-value= WYNIK ZSUM.TEST
#(4) podjecie decyzji
# alfa=0.02 < p-value  => brak podstaw do odrzucenia H0

#(5) Interpretacja 
#na poziomie istotności alfa=0.02 dane nie potwierdzają hipotezy, że produkowane ....



#--------------------    test WARIANCJI dla 1 populacji

#(1) sformułowanie hipotezy
#H0: sig^2 >= XXX   H1: sig^2 < XXX

#(2) obliczenie statystyki testowej - hipoteza 0 sigma^2 => statystyka sigma.test (w5. s. 28)
sigma.test(POPULACJA, sigmasq = XXX , alternative="less")

#(3) p-value= WYNIK_SIGMA.TEST
#(4) podjecie decyzji
# alfa=0.05 < p-value  => brak podstaw do odrzucenia H0

#(5) Interpretacja
#na poziomie istotnosci alfa=0.05 dane nie potwierdzaja hipotezy, że wariancja ...............



#--------------------- hipoteza % z dużej populacji (póla testowa) (PROPORCJA)
#H0: p = XXX%   H1: p != XXX%

n=#ROZM_PÓLI_TESTOWEJ
T=#%_ODP_W_PÓLI
phat=#%_W_PÓLI / ROZM_PÓLI
p0=XXX

#(2)  obliczenie statystyki testowej - hipoteza o p => statystyka binom.test/prop.test (w5. s. 32)
binom.test (T , n, p = XXX, alternative="two.sided") 
#lub mozna prop.test(T , n, p = p0, alternative="two.sided") - wyjdzie mniej dokładny wynik

#(3) p-value= WYNIK_BINOM.TEST (bliskie zeru)
#(4) podjecie decyzji
# alfa=0.05 > p-value  => odrzucamy H0

#(5) Interpretacja
#na poziomie istotności alfa=0.05 dane potwierdzają hipotezę, że proporcja ...


#### LAB 6 - DWIE POPULACJE

#---------------- PORÓWNANIE ŚREDNIEJ 2 POPULACJI

#(1) sformułowanie hipotezy
#H0: mu1-mu2=XXX  H1:mu1-mu2!=XXX (mu0=XXX)
#H0: mu1=mu2  H1:mu1!=mu2

#(2) obliczenie statystyki testowej (w6, s. 5)
x_bar1=mean(POPULACJA_1)
x_bar2=mean(POPULACJA_2)
t.test(POPULACJA_1, POPULACJA_2, var.equal = TRUE, mu = XXX, alternative = "two.sided")

#(3)
#p-value = WYNIK_T.TEST

#(4) podjecie decyzji
# alfa=0.02 < p-value  => brak podstaw do odrzucenia H0

#(5) Interpretacja 
#Na poziomie istotności alfa=0.02 dane nie potwierdzają hipotezy, że ...........


#----------------- PORÓWNANIE WARIANCJI W POPULACJI

#(1) sformułowanie hipotezy
#H0: sig^2_1-sig^2=XXX  H1: sig^2_1-sig^2!=XXX

#(2) obliczenie statystyki testowej (w6, s. 17)
x_bar1=mean(POPULACJA_1)
x_bar2=mean(POPULACJA_2)
var.test(POPULACJA_1, POPULACJA_2, alternative = "two.sided")

#(3) wyznaczenie obszaru krytycznego
#p-value = WYNIK_VAR.TEST

#(4) podjecie decyzji
# alfa=0.02 < p-value => brak podstaw do odrzucenia H0

#(5) Interpretacja 
#Na poziomie istotności alfa=0.02 dane nie potwierdzają hipotezy o różności wariancji, zatem możemy założyć, że wariancje są jednorodne.


#------------- OBLICZANIE PRZEDZIAŁU UFNOŚCI ŚREDNIEJ
# obliczenie statystyki testowej (w6, s. 4)
x_bar1=mean(POPULACJA_1)
x_bar2=mean(POPULACJA_2)
t.test(POPULACJA_1, POPULACJA_2, var.equal = TRUE, conf.level = 1-alfa)
#na poziomie ufnosci 98% przedzial (-13.52; 3.15) pokrywa nieznana prawdziwą różnicę średnich zawartości celulozy w dwóch regionach

#Interpretacja
# Ponieważ przedział (WYNIK_T.TEST_DOLNY; WYNIK_T.TEST_GÓRNY) pokrywa wartość 0, zatem nie mamy podstaw do odrzucenia H0


#-----       PORÓWNANIE PROPORCJI 2 POPULACJI
n1 =1200
p1=0.78
n2=2000
p2=0.8
prop.test(c(n1*p1,n2*p2),c(n1,n2),conf.level = 0.9)
#na poziomie ufnosci 0.90 przedział -4.53%, 0.53% pokrywa nieznana prawdziwa roznice proporcji polakow i amerykanow zadowolonych z pracy


prop.test(c(n1*p1,n2*p2),c(n1,n2),alternative = "less")



#---------- ANALIZA WARIANCJI
obiekty = rep(names(dane),each = length(dane$Niskie))
wyniki = c(na.omit(dane$Niskie),na.omit(dane$Srednie), na.omit(dane$Silne), na.omit(dane$BardzoSilne))

cisnienieTest = data.frame(obiekty, wyniki)

srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty),mean)

#H0: sig_1^2 = sig_2^2 = sig_3^2= sig_4^2 : H1= -H0

bartlett.test(wyniki~obiekty,cisnienieTest)
alfa = 0.05
#alfa =0.05 < p-value = 0.5009
#na poziomie istotnosci 0.05 brak podstawy do odrzucenia h0
#brak podstaw do odrzucenia h0, stwierdzamy zatem że wariancje są jednorodne i można przeprowadzić anove

anova(lm(wyniki~obiekty))

#1 sposób: F=2.2665<F_t = 2.866266 -> nie mamy podstawy do odrzucenia H0
qf(1-alfa, 3,36)

#2 sposób: alfa = 0.05 < p.val =0.09735 -> nie mamy podstawy do odrzucenia H0

#Na poziomie istotności 5% nie mamy podstaw do odrzucenia H0
# Stwierdzamy zatem, że ciśnienie nie ma istotnego wpływu na wielkość produkcji


#----------------- GRUPY JEDNORODNE I ANALIZA WARIANCJI

obiekty = rep(names(dane), c(length(na.omit(dane$I)), length(na.omit(dane$II)), length(na.omit(dane$III)), length(na.omit(dane$IV))))
wyniki = c(na.omit(dane$I), na.omit(dane$II), na.omit(dane$III), na.omit(dane$IV))
chomikiTest = data.frame(obiekty, wyniki)

bartlett.test(wyniki~obiekty,chomikiTest)

#srednie probkowe
srednie = sapply(split(chomikiTest$wyniki, chomikiTest$obiekty),mean)

#sprawdzamy jednosc wariancji 
#zakladamy jednosc wariancji

# H0: mu1 = mu2= mu3 = mu4 H1:-H0

anova(lm(wyniki~obiekty))
#alfa = 0.05 > p val 0.02398, odrzucamy H0
#Stwierdzamy zatem, ze poziom imbreadu ma istotny wpływ na 
#mase gruczolu tarczycowego u chomików

#sprawdzamy, które metody czyli poziomy inbreadu mogą być uzane za podobne
#podobne = nie rozniace sie istotnie miedzy soba

TukeyHSD(aov(wyniki~obiekty))

#grupy jednorodne II-1I, III-I, III-II -> (I,II,III)
# III- II, IV -II, IV -III -> (II,III,IV)


#--------  KONWERSJA I REGRESJA

surowiec_x=chemikalia$surowiec
produkt_y=chemikalia$produkt

#(a)
plot(surowiec_x, produkt_y)

#(b)
cov(surowiec_x, produkt_y) #=138.4889
#Interpretacja
#Kowariancja jest różna od 0, zatem istnieje liniowa zależność między ilością zużytego surowca a końcową wielkością produkcji środków chemicznych
#Ponieważ kowariancja jest dodatnia, zatem wraz ze wzrostem ilości zużytego surowca wzrasta końcowa wielkość produkcji środków chemicznych

#(c) (aby sprawdzić siłę zależności należy wyznaczyć korelację:)
cor(surowiec_x, produkt_y) #r_XY=0.8953468
#wyznacz na podstawie w8 s.4
#współczynnik korelacji r=|0.895|>0.8, zatem istnieje bardzo silny związek liniowy między ilością zużytego surowca a końcową wielkością produkcji środków chemicznych

#(d)
#można zrobić na 2 sposoby
#sposób 1
#utworzenie prostej 
prosta=lm(produkt_y~surowiec_x)
prosta
#y= b0+b1*x
#y=(Intercept) + surowiec_x*x
#y=22.41 + 3.619*x - równanie prostej regresji liniowej między wielkością produkcji a ilością zużytego surowca.

#(e)
plot(surowiec_x, produkt_y);abline(prosta)

#(f)
#Jeśli ilość surowca wzrośnie o 1 litr to końcowa wielkość produkcji środków chemicznych wzrośnie o 3,62kg (interpretajca współczynnika regresji liniowej b1 (zad 1 pkt d))

#(g,h)
#w8 s.25
predict(prosta, data.frame(surowiec_x=c(20, 15))) #=g 94.78571 h 76.69048
# jeśli zużyjemy do produkcji 20 litrów surowca to koncowa ilosc produkcji wyniesie 94.79kg
#a jesli zużyjemy do produkcji 15 litrów surowca to końcowa ilosc produkcji wyniesie 76.69kg

#(i)
summary(prosta) #odczyt Multiple R-squared:  0.8016
#współczynnik determinacji R-squared=0.8016*100%=80.16%
#Prosta regresji liniowej jest dobrze dopasowana do danych (na zaj. zawsze >80 - dobrze dopasowane, <50 - słabo dopasowane, else średnio dopasowane)
#nie ma ścisłych zasad jak w w8 s.4 
#końcowa wielkość produkcji środków chemicznych jest wyjaśniona w około 80% przez ilość zużytego surowca

#(j)
#H0: b1=0 (regresja liniowa nie jest istotna)
#H1: b1!=0 (regresja liniowa jest istotna)

anova(prosta) #F-value=32.332
#sposob 1 - wnioskowanie ze statystyki F
alfa=0.05
n=length(surowiec_x)
qf(1-alfa, 1, n-2) #=5.317655
#F-value=32.332 > 5.317655=F_t => odrzucamy H0

#sposob 2 - porownanie alfy z p-value (Pr(>F) z anovy)
#alfa=0.05 > 0.0004617=p-value => odrzucamy H0

#Na poziomie istotności alfa=0.05 dane potwierdzają hipotezę, że regresja liniowa jest istotna


#--- lab ostatnie

#--- porównywanie badania do populacji

observedf= c(122,85,76,17) #czestosc zaobserwowanych
expectedp = c(0.38,0.32,0.23,0.07) #oczekiwane prawdopodobienstwo
chisq.test(observedf,p= expectedp)
#alfa = 0.1 
#pi = 0.3485
#alfa < pi
#brak podstaw do odrzucenia hipotezy h0
#na poziomie istotności 10% dane nie potwierdzaja hipotezy, że rozkład czestotliwosci emerytów którzy wrócili do pracy w hrabstwie Allegheny NIE odpowiada ogólnemu rozkładowii podanemu przez stowarzyszenie


#------ badanie czy coś MA rozkład normalny

#h0: stezenie ozonu ma rozkład normalny
pearson.test(dane$ozon, adjust = FALSE)$p.value
#alfa = 0.05 <p.value =.2689 - > brak podstaw do odrzucenia h0
pearson.test(dane$ozon, adjust = TRUE)$p.value
#alfa = 0.05 <p.value =.1459 - > brak podstaw do odrzucenia h0
lillie.test(dane$ozon)
#alfa = 0.05 <p.value =.2774 - > brak podstaw do odrzucenia h0
shapiro.test(dane$ozon)
#alfa = 0.05 <p.value =.1097 - > brak podstaw do odrzucenia h0

#na poziomie istotności 5% dane nie potwierdzaja hipotezy, że stęzenie azotu nie ma rozkładu normalnego 
