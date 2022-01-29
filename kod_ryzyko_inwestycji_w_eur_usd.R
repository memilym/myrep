
#IMPORTOWANIE DANYCH Z EXCELA
install.packages("xlsx")
library(readxl)
euro<-read_excel("~/Desktop/Studia/Teoriaryzyka/EUR.xlsx")
usd<- read_excel("~/Desktop/Studia/Teoriaryzyka/USD.xlsx")

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#STOPY STRAT DLA EURO
wo_euro<-euro$`eur/pln_o`
wz_euro<-euro$`eur/pln_z`
#wyliczenie stopy strat
euro_stopa_strat<-round(((wo_euro-wz_euro)/wo_euro)*100,2)
euro_stopa_strat
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#STOPY STRAT DLA USD
wo_usd<-usd$`usd/pln_o`
wz_usd<-usd$`usd/pln_z`
#wyliczenie stopy strat
usd_stopa_strat<-round(((wo_usd-wz_usd)/wo_usd)*100,2)
usd_stopa_strat
#-------------------------------------------------------------------------------
#HISTOGRAMY STOP STRAT
hist(euro_stopa_strat, xlab = "Stopy strat", ylab="Częstość występowania",
     col = rainbow(9, alpha=0.3), main="")
hist(usd_stopa_strat, xlab = "Stopy strat", ylab="Częstość występowania",
     col = rainbow(9, alpha=0.3), main="")
#-------------------------------------------------------------------------------
#DOBOR ROZKLADU ORAZ PODSTAWOWE CHARAKTERYSTYKI LICZBOWE
library(fitdistrplus)
#-------------------------------------------------------------------------------
#EURO
euro1<-(euro_stopa_strat-min(euro_stopa_strat)+0.01)
euro1

#Funkcja, ktora jest w stanie wyestymowac charakterystyki liczbowe
descdist(euro_stopa_strat, discrete = FALSE, boot=1000)

#Dobor rokladow
fw<-fitdist(euro1,"weibull")
fg<-fitdist(euro1,"gamma")
fln<-fitdist(euro1,"lnorm")
fn<-fitdist(euro1,"norm")

plot.legenda<-c("Weibull","lognormal","gamma","norm")

denscomp(list(fw,fln,fg,fn),legendtext=plot.legenda)

qqcomp(list(fw,fln,fg,fn),legendtext=plot.legenda)

cdfcomp(list(fw,fln,fg,fn),legendtext=plot.legenda)

summary(fw)
summary(fg)
summary(fn)
#-------------------------------------------------------------------------------
#USD
usd1<-(usd_stopa_strat-min(usd_stopa_strat)+0.01)
usd1

#Funkcja, ktora jest w stanie wyestymowac charakterystyki liczbowe
descdist(usd_stopa_strat, discrete = FALSE, boot=1000)

#Dobor rokladow
mw<-fitdist(usd1,"weibull")
mg<-fitdist(usd1,"gamma")
mln<-fitdist(usd1,"lnorm")
mn<-fitdist(usd1,"norm")
plot.legenda<-c("Weibull","lognormal","gamma","norm")
denscomp(list(mw,mln,mg,mn),legendtext=plot.legenda)

qqcomp(list(mw,mln,mg,mn),legendtext=plot.legenda)

cdfcomp(list(mw,mln,mg,mn),legendtext=plot.legenda)

summary(mw)
summary(mg)
summary(mn)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#TEST MARDIA

laczny<-data.frame(euro_stopa_strat, usd_stopa_strat)

install.packages("MVN")
install.packages("VIM")
library("MVN")

testmardia<-mvn(laczny, mvnTest="mardia")
testmardia
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#WZORY ANALITYCZNE DLA INWESTYCJI POJEDYNCZEJ

###VaR
#EURO
mu1=mean(euro_stopa_strat)
sigma1=sd(euro_stopa_strat)

VaR1=mu1+sigma1*qnorm(0.95, mean=0, sd=1)
VaR1

#USD
mu2=mean(usd_stopa_strat)
sigma2=sd(usd_stopa_strat)

VaR2=mu2+sigma2*qnorm(0.95, mean=0, sd=1)
VaR2

###ES
#EURO
ES1=mu1+sigma1*(dnorm(qnorm(0.95,mean=0,sd=1))/0.05)
ES1

#USD
ES2=mu2+sigma2*(dnorm(qnorm(0.95,mean=0,sd=1))/0.05)
ES2
#-------------------------------------------------------------------------------
#KOPULY
#-------------------------------------------------------------------------------
#WYKRESY OGRANICZENIA FRECHET
install.packages("plot3D")
library("plot3D")

x <- seq( 0, 1, length= 20)
y <- seq( 0, 1, length= 20)
m_function <- function(x,y) min(x,y)

z <- outer(x, y, Vectorize(m_function))
persp(x, y, z,theta = -10, phi = 30)

w_function <- function(x,y) max(x+y-1,0)
z2 <- outer(x, y, Vectorize(w_function))
persp(x, y, z2,theta = -10, phi = 30)

i= function(x,y)max(x*y)
z3<-outer(x,y,Vectorize(i))
persp(x,y,z3,theta=-10,phi=30,shade = 0.4)

#PORTFEL ZWYDERSYFIKOWANY- GENEROWANIE DANYCH Z ROZKLADU LACZNEGO
install.packages("copula")
library("copula")

laczny_m<-as.matrix(laczny)
dane_wyg<-pobs(laczny_m)

fit1<-fitCopula(claytonCopula(),dane_wyg,method='ml')
fit1
fit2<-fitCopula(frankCopula(),dane_wyg,method='ml')
fit2
fit3<-fitCopula(gumbelCopula(),dane_wyg,method='ml')
fit3
fit4<-fitCopula(tCopula(),dane_wyg,method='ml')
fit4
#df-Stopnie swobody
#Rho-wspolczynnik korelacji

#przypisywanie zmiennym kopul z odpowiednimi parametrami
clayton<-claytonCopula(param=2.467,dim=2)
frank<-frankCopula(param=9.44,dim=2)
gumbel<-gumbelCopula(param=2.626,dim=2)
t<-tCopula(param=0.8601,df=186,dim=2)

#wykresy kopul
persp(clayton,pCopula,main="Dystrybuanta",xlab="u",ylab="v",
      zlab="",theta=-35,shade=0.4,col="green")

persp(frank,pCopula,main="Dystrybuanta",xlab="u",ylab="v",
      zlab="",theta=-35,shade=0.4,col="cadetblue1")

persp(gumbel,pCopula,main="Dystrybuanta",xlab="u",ylab="v",
      zlab="",theta=-35,shade=0.4,col="darkmagenta")

persp(t,pCopula,main="Dystrybuanta",xlab="u",ylab="v",
      zlab="",theta=-35,shade=0.4,col="pink")

#generowanie danych z rozkladu lacznego

set.seed(997)
N<-1000
probki_kop<-rCopula(N,t)
dane<-cbind(qnorm(probki_kop[,1],mean=-0.04568966,sd=2.923059),
            qnorm(probki_kop[,2],mean=-0.1382759,sd=1.586701))


b=seq(0,1,0.1)
wynik=sapply(b,function(x)x*dane[,1]+(1-x)*dane[,2])
#VaR dla portfela zdywersyfikowanego
VaR=apply(wynik,2,function(x)quantile(x,0.95))
VaR

#ES dla portfela zdywersyfikowanego
dl=length(wynik[,2])
wynik_sort=apply(wynik,2,function(x)sort(x))
ES=apply(wynik_sort,2,function(x)mean(tail(x,0.05*dl)))
ES
#-------------------------------------------------------------------------------
#METODA WZOROW ANALITYCZNYCH DLA PORTFELA
#-------------------------------------------------------------------------------
sigma=cov(laczny[,2:1])

mu=apply(laczny,2,mean)
mu[1]
mu[2]

VaR_dwu <- function(a){
  b=1-a
  mu_V=mu[1]*a+mu[2]*b
  sigma_V=sqrt(sigma[1,1]*a*a+2*sigma[1,2]*a*b+sigma[2,2]*b*b)
  Value_at_Risk=mu_V+sigma_V*qnorm(0.95,mean=0,sd=1);
  return(Value_at_Risk)
}
VaR=sapply(b,VaR_dwu)
VaR

ES_dwu<-function(a){
  b=1-a
  mu_E=mu[1]*a+mu[2]*b
  sigma_E=sqrt(sigma[1,1]*a*a+2*sigma[1,2]*a*b+sigma[2,2]*b*b)
  ES=mu_E+sigma_E*(dnorm(qnorm(0.95,mean=0,sd=1))/0.05)
  return(ES)
}
ES=sapply(b,ES_dwu)
ES

