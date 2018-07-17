#Menghitung integral dengan simulasi monte carlo
#Menentukan prior pada proses Bayesian
#Menentukan posterior dari proses Bayesian dengan prior tertentu
#Membuat algoritma MCMC dengan proses Gibbs Sampler
#Menggunakan winbugs
#Tampilan respon (hasil)
#Pencarian Optimal berdasarkan tampilan respon (hasil)

require(MASS)
require(foreign)
library('mcmc')
library('R2WinBUGS')

mdata<-read.spss("C:/Users/Yosral12/Desktop/datasimulasi.sav", to.data.frame=TRUE)
head(mdata)
str(mdata)

#Nomor 1
#a
mean(mdata$X)
mean(mdata$Y)
var(mdata$X)
var(mdata$Y)
X=mdata$X
Y=mdata$Y
corr(mdata)
(corr(mdata))^2
hist(mdata$X, probability=TRUE)
lines(density(mdata$X),col=2,lwd=2)
hist(mdata$Y, probability=TRUE)
lines(density(mdata$Y),col=2,lwd=2)
plot(mdata$X, mdata$Y)
mmodel<-lm(mdata$Y ~ mdata$X, x = mdata$X, y=mdata$Y)
summary(mmodel)

#Nomor 4

#Bagian (B)
#bootstrap
my=rnorm(n = 1000,mean = 0,sd = 1)
mx=rnorm(n = 1000,mean = 1,sd = 1)
Yboot<-c()
Xboot<-c()

kj.bootstrap.mean.simple <- function(Yvar, nB=500, verbose=T) {
  bootdf <- list()
  for (i in 1:nB) {
    bootdf[[i]] <- sample(Yvar, replace=TRUE)
  }
  
  Yboot<-bootdf
  
  real_mean<- mean(Yvar)
  real_stdev<- sqrt(var(Yvar))
  bootdf_mean<- mean(sapply(bootdf, mean))
  bootdf_stdev<-mean(sapply(bootdf, function(x){sqrt(var(x))}))
  ciupboot<-bootdf_mean+qnorm(0.975)*bootdf_mean/bootdf_stdev
  cilowboot<-bootdf_mean-qnorm(0.975)*bootdf_mean/bootdf_stdev
  
  
  # Interpretasi7
  if(verbose) { 
    cat("\n Resampling Bootstrap ( nB =", nB, ")", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (bootstrap) :", bootdf_mean,
        "\n | standar deviasi (populasi)  :", real_stdev, 
        "\n | standar error (bootstrap) :", bootdf_stdev,
        "\n | CI Upper (bootstrap) :", ciupboot,
        "\n | CI Lower (bootstrap) :", cilowboot
    )
    hist(sapply(bootdf, mean))
  }
  invisible(list(bootdf, bootdf_mean, real_mean, real_stdev, bootdf_stdev))
}
#jacknife
kj.jackknife.mean.simple <- function(Yvar, verbose=T) {
  jackdf <- list()
  jack <- numeric(length(Yvar)-1)
  for (i in 1:length (Yvar)){
    for (j in 1:length(Yvar)){
      if(j < i){ 
        jack[j] <- Yvar[j]
      }  else if(j > i) { 
        jack[j-1] <- Yvar[j]
      }
    }
    jackdf[[i]] <- jack
  }
  real_mean<- mean(Yvar)
  real_stdev<- sqrt(var(Yvar))
  jackdf_mean<- mean(sapply(jackdf, mean))
  jackdf_mean<- mean(sapply(jackdf, mean))
  jackdf_stdev<-mean(sapply(jackdf, function(x){sqrt(var(x))}))
  ciupjack<-jackdf_mean+qnorm(0.975)*jackdf_mean/jackdf_stdev
  cilowjack<-jackdf_mean-qnorm(0.975)*jackdf_mean/jackdf_stdev
  # Interpretasi
  if(verbose) { 
    cat(" Resampling Jackknife", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (jackknife) :", jackdf_mean,
        "\n | standar deviasi (populasi)  :", real_stdev, 
        "\n | standar error (bootstrap) :", jackdf_stdev,
        "\n | CI Upper (jackknife) :", ciupjack,
        "\n | CI Lower (jackknife) :", cilowjack
    )
    hist(sapply(jackdf, mean))
  }
  invisible(list(jackdf, jackdf_mean, real_mean))
}

jacky <- function(Yvar, verbose=T) {
  jackdf <- list()
  jack <- numeric(length(Yvar)-1)
  for (i in 1:length (Yvar)){
    for (j in 1:length(Yvar)){
      if(j < i){ 
        jack[j] <- Yvar[j]
      }  else if(j > i) { 
        jack[j-1] <- Yvar[j]
      }
    }
    jackdf[[i]] <- jack
  }
  return(as.vector(jackdf))
}

bootih<- function(Yvar, nB=500, verbose=T) {
  bootdf <- list()
  for (i in 1:nB) {
    bootdf[[i]] <- sample(Yvar, replace=TRUE)
  }
  return(as.vector(bootdf))
}

mmyjack<-as.vector(jacky(my))
mmyboot<-as.vector(bootih(my))
mmxjack<-as.vector(jacky(mx))
mmxboot<-as.vector(bootih(mx))

corr(mmyjack,mmyboot)
corr(mmxjack,mmxboot)

#bagian (C)

A<-c(70,67,59,78)
B<-c(80,90,78,93)
BA=cbind(A,B)

obsSTat <- t.test(B,A, data = BA, conf.level = 0.95)$statistic
N <- 20
## Number of Permutation
statPerm <- numeric(N)
for (i in 1: N) {
Ai <- sample(A,replace=F)
statPerm[i] <- t.test(B,Ai)$statistic
}

hist(statPerm)
## Two sided p-values
pval <- (1+sum(abs(statPerm) > abs(obsSTat)))/(N+1)
## Exact P-values
pval
## Asymtotic p-values
t.test(B,A)

#nomor 5


#Misalkan ada dua data, x dan y, masing2 berdist. chi-square 
# berderajat bebas 1 dan 5

#Two Sample T-Test

ynew<-rchisq(20,1)
xnew<-rchisq(20,5)

t.test(xnew,ynew, paired=FALSE, conf.level = 0.95)


#regresi linear sederhana
lm(ynew~xnew)
plot(lm(y~x))


