#Modelowanie wielkości roszczeń ubezpieczeniowych rozkładem Gamma

#GKM1
GKM1 <- function(n, a, beta = 1) {
  if (a > 1)
  {
    a_hat <- a - 1
    b <- (a - 1 / (6 * a)) / a_hat
    m <- 2 / a_hat
    d <- m + 2
    Z <- c()
    count <- 0
    for (i in 1:n) {
      repeat {
        #do something
        count = count + 1
        U1 <- runif(1)
        U2 <- runif(1)
        V <- b * U2 / U1
        
        #exit if the condition is met
        if ((m * U1 - d + V + 1 / V <= 0) ||
            (m * log(U1) - log(V) + V - 1 <= 0))
          break
      }
      Z <- append(Z, a_hat * V * beta)
    }
    print(paste("Wymagana ilość iteracji:", count))
    return(Z)
  }
  else{
    print('Parametr a jest mniejszy bądź równy 1.')
  }
}

#Ahrens-Dieter
AD <- function(n, a, beta) {
  if (a <= 1) {
    b <- (a + exp(1)) / exp(1)
    Z1 <- c()
    count <- 0
    
    for (i in 1:n) {
      repeat {
        count = count + 1
        U1 <- runif(1)
        U2 <- runif(1)
        Y <- b * U1
        
        if (Y <= 1) {
          Z <- Y ^ (1 / a)
          if (U2 < exp(-Z))
            break
        }
        else{
          Z <- -log((b - Y) / a)
          if (U2 <= Z ^ (a - 1))
            break
        }
      }
      Z1 <- append(Z1, Z * beta)
    }
    print(paste("Wymagana ilość iteracji:", count))
    return(Z1)
  }
  else{
    print("Parametr kształtu a jest większy od 1.")
  }
}
#porownanie graficzne
results <- function(n, a, beta = 1) {
  if (a > 1) {
    Z <- GKM1(n, a, beta)
  }
  else if (0 < a & a <= 1) {
    Z <- AD(n, a, beta)
  }
  hist(Z, main = paste("beta =" , beta), prob = TRUE)
  lines(density(Z), col = "pink", lwd = 3)
  lines(density(rgamma(n, a, scale = beta)), col = "blue", lwd = 0.5)
}
#Porównanie działania
#GKM1 z gen.wbudowanym
#n=10, a=2
results(10, 2, 0.5)
results(10, 2, 1)
results(10, 2, 5)
#n=10, a=4
results(10, 4, 0.5)
results(10, 4, 1)
results(10, 4, 5)
#n=10, a=10
results(10, 10, 0.5)
results(10, 10, 1)
results(10, 10, 5)
#n=1000, a=2
results(1000, 2, 0.5)
results(1000, 2, 1)
results(1000, 2, 5)
#n=1000, a=4
results(1000, 4, 0.5)
results(1000, 4, 1)
results(1000, 4, 5)
#n=1000, a=10
results(1000, 10, 0.5)
results(1000, 10, 1)
results(1000, 10, 5)
#n=10000, a=2
results(10000, 2, 0.5)
results(10000, 2, 1)
results(10000, 2, 5)
#n=10000, a=4
results(10000, 4, 0.5)
results(10000, 4, 1)
results(10000, 4, 5)
#n=10000, a=10
results(10000, 10, 0.5)
results(10000, 10, 1)
results(10000, 10, 5)


#AD z gen. wbudowanym
#n=10, a=0.3
results(10, 0.3, 0.5)
results(10, 0.3, 1)
results(10, 0.3, 5)
#n=10, a=0.6
results(10, 0.6, 0.5)
results(10, 0.6, 1)
results(10, 0.6, 5)
#n=10, a=1
results(10, 1, 0.5)
results(10, 1, 1)
results(10, 1, 5)
#n=1000, a=0.3
results(1000, 0.3, 0.5)
results(1000, 0.3, 1)
results(1000, 0.3, 5)
#n=1000, a=0.6
results(1000, 0.6, 0.5)
results(1000, 0.6, 1)
results(1000, 0.6, 5)
#n=1000, a=1
results(1000, 1, 0.5)
results(1000, 1, 1)
results(1000, 1, 5)
#n=10000, a=0.3
results(10000, 0.3, 0.5)
results(10000, 0.3, 1)
results(10000, 0.3, 5)
#n=10000, a=0.6
results(10000, 0.6, 0.5)
results(10000, 0.6, 1)
results(10000, 0.6, 5)
#n=10000, a=1
results(10000, 1, 0.5)
results(10000, 1, 1)
results(10000, 1, 5)


#Porównanie czasu działania
system.time(GKM1(10000, 10, 1))
system.time(GKM1(10000, 4, 1))
system.time(GKM1(10000, 2, 1))

system.time(AD(10000, 0.3, 1))
system.time(AD(10000, 0.6, 1))
system.time(AD(10000, 1, 1))

#Modelowanie procesu rozszczeń ubezpieczeniowych.
GKM1_upr <- function(n, a, beta = 1) {
    if (a > 1)
    {
      a_hat <- a - 1
      b <- (a - 1 / (6 * a)) / a_hat
      m <- 2 / a_hat
      d <- m + 2
      Z <- c()
      for (i in 1:n) {
        repeat {
          #do something
          U1 <- runif(1)
          U2 <- runif(1)
          V <- b * U2 / U1
          
          #exit if the condition is met
          if ((m * U1 - d + V + 1 / V <= 0) ||
              (m * log(U1) - log(V) + V - 1 <= 0))
            break
        }
        Z <- append(Z, a_hat * V * beta)
      }
      return(Z)
    }
    else{
      print('Parametr a jest mniejszy bądź równy 1.')
    }
}

lambda=10
tMax=3
a=3
beta=2
K=180

set.seed(1234)

ilosc_roszcz <- rpois(n=1000, lambda=lambda*tMax)
wielkosc <- numeric(1000)
for (i in 1:1000){
  wielkosc[i] <- sum(GKM1_upr(ilosc_roszcz[i], a, beta))
  
}
wielkosc

czy_wieksze <- ifelse(wielkosc>K, 1, 0)
prawdop <- sum(czy_wieksze==1)/10
print(paste("Prawdopodobieństwo, że suma roszczeń przekroczy", K, 
            "jest równe", prawdop, "%"))

hist(wielkosc, main="", col="darkmagenta")
