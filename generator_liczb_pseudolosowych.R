#Implementacja generatora liczb pseudolosowych z zadanych rozkladow

#Generator liczb pseudolosowych pochodzacych z rozkladu jednostajnego [0,1].
lcg <- function(m, a, c, seed, n = 10) {
  u <- numeric(n) #pusty wektor o dlugosci n
  x <- numeric(n + 1) #pusty wektor o dlugosci n+1
  x[1] <- seed #przypisujemy ziarno do pierwszego elementu wektora x
  for (i in c(1:n))
    #w petli definiujemy kongruencje liniowa
  {
    x[i + 1] <- (a * x[i] + c) %% m
    u[i] <- x[i + 1] / m
  }
  return(u)
}

install.packages("moments")
library(moments)
########################################################################
#ROZKLAD WYKLADNICZY Z WBUDOWANYM ROZKLADEM JEDNOSTAJNYM
gen_wyk <- function(n, lambda) {
  X = -1 / lambda * log(runif(n, 0, 1))
  hist(X, prob = TRUE, ylim = c(0, lambda - lambda / 4))
  lines(density(X), col = "purple")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(1 / lambda, log(2) /
                               lambda, "2", "6",
                             1 / lambda ^ 2),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
gen_wyk(10000, 0.5)
gen_wyk(10000, 2)
gen_wyk(10000, 3)
########################################################################
#ROZKLAD WYKLADNICZY Z GENERATOREM LCG

gen_wyk2 <- function(n, lambda) {
  X = -1 / lambda * log(lcg(214013, 2531011, 135, 7, n))
  hist(X, prob = TRUE)
  lines(density(X), col = "blue")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(1 / lambda, log(2) /
                               lambda, "2", "6",
                             1 / lambda ^ 2),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
gen_wyk2(10000, 0.5)
gen_wyk2(10000, 2)
gen_wyk2(10000, 3)
########################################################################
#ROZKLAD WYKLADNICZY WBUDOWANY
wbud_wykl <- function(n, lambda) {
  X = rexp(n, lambda)
  hist(X, prob = TRUE)
  lines(density(X), col = "pink")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(1 / lambda, log(2) /
                               lambda, "2", "6",
                             1 / lambda ^ 2),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
wbud_wykl(10000, 0.5)
wbud_wykl(10000, 2)
wbud_wykl(10000, 3)
#---------------------------------------------------------------------------------
#ROZKLAD WEIBULLA
gen_weib <- function(n, k, lambda) {
  X = lambda * (-log(1 - runif(n, 0, 1))) ^ (1 / k)
  hist(X, prob = TRUE)
  lines(density(X), col = "purple")
  miary_stat <- data.frame(
    "Nazwa" = c("Wartosc oczekiwana",
                "Mediana", "Wariancja"),
    "Wyniki teoretyczne" = c(
      lambda * gamma(1 + 1 / k),
      lambda * log(2) ^ (1 / k),
      lambda ^ 2 * (gamma(1 +
                            2 / k) - gamma(1 + 1 / k) ^ 2)
    ),
    "Wyniki rzeczywiste" = c(mean(X), median(X), var(X))
  )
  print(miary_stat)
  return(X)
}
gen_weib(10000, 0.5, 0.5)
gen_weib(10000, 3, 0.5)
gen_weib(10000, 5, 0.5)
gen_weib(10000, 5, 2)
gen_weib(10000, 5, 5)
###############################################################################################
#ROZKLAD WEIBULLA WBUDOWANY
wbud_weib <- function(n, k, lambda) {
  X = rweibull(n, k, lambda)
  hist(X, prob = TRUE, ylim = c(0, 5))
  lines(density(X), col = "pink")
  miary_stat <- data.frame(
    "Nazwa" = c("Wartosc oczekiwana",
                "Mediana", "Wariancja"),
    "Wyniki teoretyczne" = c(
      lambda * gamma(1 + 1 / k),
      lambda * log(2) ^ (1 / k),
      lambda ^ 2 * (gamma(1 +
                            2 / k) - gamma(1 + 1 / k) ^ 2)
    ),
    "Wyniki rzeczywiste" = c(mean(X), median(X), var(X))
  )
  print(miary_stat)
  return(X)
}
wbud_weib(10000, 0.5, 0.5)
wbud_weib(10000, 3, 0.5)
wbud_weib(10000, 5, 0.5)
wbud_weib(10000, 5, 2)
wbud_weib(10000, 5, 5)
#--------------------------------------------------------------------------------------
#ROZKLAD ARCUSASINUSA
gen_arcsin <- function(n) {
  X = 1 / 2 - 1 / 2 * cos(pi * runif(n, 0, 1))
  hist(X, prob = TRUE)
  lines(density(X), col = "purple")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(1 / 2, 1 / 2, 0, -3 /
                               2,
                             1 / 8),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
gen_arcsin(100)
gen_arcsin(10000)
gen_arcsin(8000000)
#########################################################################################
#ROZKLAD ARCUSASINUSA WBUDOWANY
install.packages("devtools")
devtools::install_github("ppernot/rgumlib")
library("rgumlib")

wbud_arcsin <- function(n) {
  X = rarcsine(n)
  hist(X, prob = TRUE)
  lines(density(X), col = "pink")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(1 / 2, 1 / 2, 0, -3 /
                               2,
                             1 / 8),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
wbud_arcsin(100)
wbud_arcsin(10000)
wbud_arcsin(8000000)
#---------------------------------------------------------------------------------------
#ROZKLAD NORMALNY (0,1)- METODA BOXA-MULLERA
BoxMuller <- function(N) {
  N0 <- ceiling(N / 2)
  U1 <- runif(N0, 0, 1)
  U2 <- runif(N0, 0, 1)
  R <- sqrt((-2) * log(U1))
  theta <- 2 * pi * U2
  Y1 <- R * cos(theta)
  Y2 <- R * sin(theta)
  z <- c(Y1, Y2)
  
  hist(z, prob = TRUE)
  lines(density(z), col = "purple")
  print(shapiro.test(z[1:5000]))
  print(ks.test(z, "pnorm"))
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(0, 0, 0, 0,
                             1),
    "Wyniki rzeczywiste" = c(mean(z), median(z), skewness(z),
                             kurtosis(z), var(z))
  )
  print(miary_stat)
  return(z)
}
BoxMuller(10)
BoxMuller(1000)
BoxMuller(100000)

#############################################################
#ROZKLAD NORMALNY METODA PODWOJNEGO WYKLADNICZEGO
doub_exp <- function(n) {
  X <- rep(0, n)
  for (i in c(1:n)) {
    U1 <- runif(1)
    U2 <- runif(1)
    U3 <- runif(1)
    X[i] <- -log(U1)
    while (U2 > exp(-0.5 * (X[i] - 1) ^ 2)) {
      U1 <- runif(1)
      U2 <- runif(1)
      U3 <- runif(1)
      X[i] <- -log(U1)
    }
    if (U3 <= 0.5) {
      X[i] <- -X[i]
    }
  }
  
  hist(X, prob = TRUE)
  lines(density(X), col = "purple")
  print(shapiro.test(X[1:5000]))
  print(ks.test(X, "pnorm"))
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(0, 0, 0, 0,
                             1),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
doub_exp(10)
doub_exp(1000)
doub_exp(100000)
#####################################################################################
#ROZKLAD NORMALNY WBUDOWANY
wbud_norm <- function(n) {
  X <- rnorm(n, 0, 1)
  hist(X, prob = TRUE)
  lines(density(X), col = "pink")
  print(shapiro.test(X[1:5000]))
  print(ks.test(X, "pnorm"))
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(0, 0, 0, 0,
                             1),
    "Wyniki rzeczywiste" = c(mean(X), median(X), skewness(X),
                             kurtosis(X), var(X))
  )
  print(miary_stat)
  return(X)
}
wbud_norm(10)
wbud_norm(1000)
wbud_norm(100000)
#---------------------------------------------------------------------------------
#ROZKLAD POISSONA
gen_poiss <- function(lambda) {
  X <- exp(-lambda)
  k <- 0
  p <- 1
  while (p > X) {
    k <- k + 1
    U <- runif(1)
    p <- p * U
  }
  return(k - 1)
}
gen_poiss_n <- function(n, lambda) {
  a <- rep(0, n)
  for (i in c(1:n)) {
    a[i] <- gen_poiss(lambda)
  }
  hist(a, prob = TRUE)
  lines(density(a), col = "purple")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(
      lambda,
      ceiling(lambda + 1 / 3 - 0.02 / lambda),
      lambda ^ (-1 / 2),
      lambda ^ (-1),
      lambda
    ),
    "Wyniki rzeczywiste" = c(mean(a), median(a), skewness(a),
                             kurtosis(a), var(a))
  )
  print(miary_stat)
  return(a)
}

gen_poiss_n(100, 2.5)
gen_poiss_n(100, 5)
gen_poiss_n(100, 10)
##############################################################################
#ROZKLAD POISSONA WBUDOWANY
wbud_poiss <- function(n, lambda) {
  a <- rpois(n, lambda)
  hist(a, prob = TRUE)
  lines(density(a), col = "pink")
  miary_stat <- data.frame(
    "Nazwa" = c(
      "Wartosc oczekiwana",
      "Mediana",
      "Skosnosc",
      "Kurtoza",
      "Wariancja"
    ),
    "Wyniki teoretyczne" = c(
      lambda,
      ceiling(lambda + 1 / 3 - 0.02 / lambda),
      lambda ^ (-1 / 2),
      lambda ^ (-1),
      lambda
    ),
    "Wyniki rzeczywiste" = c(mean(a), median(a), skewness(a),
                             kurtosis(a), var(a))
  )
  print(miary_stat)
  return(a)
}
wbud_poiss(100, 2.5)
wbud_poiss(100, 5)
wbud_poiss(100, 10)
###################################################################################
#PROBLEM
n = 1000000

x1 <- gen_weib(n, 8, 3)
x2 <- lcg(214013, 2531011, 135, 7, n)
x3 <- gen_wyk2(n, 0.7)
x4 <- BoxMuller(n)
x5 <- gen_wyk2(n, 1)
x6 <- gen_weib(n, 7, 4.5)


z = c() #czas zycia ukladu, czyli czas bez awarii srodka komunikacji
for (i in 1:n) {
  z[i] <-
    max(min(x1[i], x2[i]),
        min(x1[i], x3[i]),
        min(x2[i], x4[i], x5[i]),
        min(x3[i], x4[i], x5[i]),
        min(x4[i], x6[i]))
}
mean(z)
var(z)
hist(z, col = rainbow(11, alpha = 0.3))
plot(ecdf(z), col = "darkmagenta")
