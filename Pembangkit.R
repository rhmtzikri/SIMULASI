randomUnif<-function(n,min,max){
  for (i in 1:n){
    unif<-runif(n,min,max)
  }
  return(unif)
}
randomUnif(50,20,30)
randombanget<-function(n,seed,a,c,m){
  U<-matrix()
  for (i in 1:n){
    x<-(a*seed+c)%%m
    seed<-x
    U[i]<-x/m
  }
  plot(seq(0,m,m/(n-1)),dunif(U,0,m),xlab ="x",ylab="f(x)")
  return(U)
}
randombanget(100,4,50,3,70)

bangkitNormal<- function(data1,data2){
  z1<-sqrt(-2*log(data1))*cos(2*pi*data2)
  z2<-sqrt(-2*log(data1))*sin(2*pi*data2)
  return(c(z1,z2))
}

randomNormalStandar<- function(n){
  u1<-randomUnif(n)
  u2<-randomUnif(n)
  z1<-sqrt(-2*log(u1))*cos(2*pi*u2)
  z2<-sqrt(-2*log(u1))*sin(2*pi*u2)
  Z<-c(z1,z2)
  return(Z)
}

randomNormal<-function(n,miu,sigma1){
  Z<-randomNormalStandar(n)
  Z<-miu+sigma1*Z
  return(Z)
}

randomNormal(n = 50,miu = 50,sigma1 = 5)

randomChisq<- function(n,df){
  chisqdf<-0
  if(missing(df)){
    Z<-randomNormalStandar(n)
    chisqdf<-Z^2
  } else {
    for (i in 1:df ){
      Z<-randomNormalStandar(n)
      chisq<-Z^2
      chisqdf<-chisqdf+chisq
    }
  }
  return(chisqdf)
}
randomChisq(10,1)

randomexp<- function(n,teta){
  u<-randomUnif(n)
  exp<--1/teta*log(u)
  return(exp)
}


inversexp<-function(n,beta){
  u<-randomUnif(n)
  exp<--beta*log(1-u)
  return(exp)
}

randomgamma<-function(n,m,beta){
  sigmau<-0
  for (i in 1:m){
    u<-randomUnif(n)
    sigmau<-sigmau+log(u)
  }
  gamma<-1/beta*sigmau
  return(gamma)
}

inverseGamma <- function(n,m,beta,tipe){
  jumlahU<-0
  productU<-1
  for (i in 1:m){
    u<-runif(n)
    jumlahU<-jumlahU+log(1-u)
    productU<-productU*log(1-u)
  }
  gammaSum <- -1/beta*jumlahU
  gammaProd<- 1/beta*productU
  if(missing(tipe)){
    return(cbind(gammaSum,gammaProd))
  } else if(tipe=="jumlah"){
    return(gammaSum)
  } else if(tipe=="product"){
    return(gammaProd)
  } 
}


invGamma<-inverseGamma(50,4,4)
gam<-randomgamma(50,4,4)
plot(density(gam))
plot(density(invGamma[,2]))


randomt<-function(n,df){
  Z<-randomNormalStandar(n)
  chisq<-randomChisq(n,df)
  t<-Z/sqrt(chisq/df)
}

randomF<-function(n,df1,df2){
  chisq1<-randomChisq(n/2,df1)
  chisq2<-randomChisq(n/2,df2)
  F<-chisq1/chisq2
  return(F)
}
randomF(10,1,2)
sampling <- function(data,size,n){
  sampel<-matrix(ncol = size,nrow=n)
  namess<-c()
  tabel<-matrix(ncol = size, nrow=6)
  sr<-matrix(nrow = 1,ncol = size)
  warna<-rainbow(size)
  plot(density(data),ylim = c(0,max(density(data)$y)+0.1))
  for (i in 1:size){
    sampel[,i]<-rbind(sample(data,n))
    tabel[,i]<- c(min(sampel[,i]),mean(sampel[,i]), max(sampel[,i]),var(sampel[,i]),sd(sampel[,i]),mad(sampel[,i]))
    
    jumlah<-0
    for(j in 1:n){
      selisih<-abs(sampel[j,i]-mean(sampel[,i]))
      jumlah<-jumlah+selisih
    }
    sr[,i]<-1/length(sampel[,i])*jumlah
    
    namess<-c(namess,paste("sampel[",i,"]",sep = ""))
    lines(density(sampel[,i]),col=warna[i])
  }
  
  
  colnames(tabel)<-namess
  tabelPop<- matrix(c(min(data),mean(data),max(data),var(data),sd(data),mad(data)))
  row.names(tabelPop)<-c("min","mean","max","var","std Deviasi","simp. Rata2")
  
  
  #tabel<-rbind(tabel,sr)
  row.names(tabel)<-c("min","mean","max","var","standar dev","simp. Rata2")
  cat("Deskriptif populasi\n")
  print(tabelPop,col.names=FALSE)
  cat("\t\t\t\t\tDeskriptif sampel\n")
  print(tabel)
  return(sampel)
}
sampling(runif(100),20,20)
##bangkitan sendiri
populasi <- randomUnif(1000)
sampel <- sampling(data = populasi, size = 5, n = 100)

##bangkitan package
populasi1 <-runif(1000)
sampel1<-sampling(data = populasi1,size = 5,n = 100)

##bangkitan sendiri
normalpop<-randomNormalStandar(500)
sampelnormal<-sampling(normalpop,size = 5,n = 100)

##bangkitan package
normalpop1<-rnorm(1000)
sampelnormal1<-sampling(normalpop,size = 5,n=100)

##bangkitan sendiri
normal<-randomNormal(500,10,4)
samplenormal<-sampling(data = normal,size = 10,n=100)

##bangkitan package
normal1<-rnorm(1000,10,4)
samplenormal1<-sampling(data = normal1, size = 10, n = 100)

##bangkitan sendiri
popchisq<-randomChisq(n=20)
sampelchisq<-sampling(data = popchisq,size = 3,n=10)

##bangkitan package
popchisq1<-rchisq(20,1)
samplechisq1<-sampling(data = popchisq1,size =3 ,n =10)

par(mfrow=c(1,2))
plot(density(popchisq),main = "plot density chisq bangkitan")
plot(density(popchisq1),main = "plot density chisq package")

##bangkitan sendiri
popchisqdf<-randomChisq(200,3)
sampelchisqdf<-sampling(data = popchisqdf,size = 3,n=10)

##bangkitan package
popchisqdf1<-rchisq(400,3)
samplechisqdf1<-sampling(data = popchisqdf1,size =3 ,n =10)

##bangkitan sendiri
popexp<-randomexp(500,2)
sampelexp<-sampling(data = popexp, size = 10, n = 100)

##bangkitan package
popexp1<-rexp(n = 1000,rate = 2)
sampelexp1<-sampling(data = popexp1, size = 10, n = 100)

##bangkitan sendiri
popt<-randomt(100,4)
samplet<-sampling(data = popt , size = 3, n = 50)

##bangkitan package
popt1<-rt(n = 200,4)
sampelt1<-sampling(data = popt1 , size = 3, n = 50)

##bangkitan sendiri
popF<-randomF(500,5,4)
sampleF<-sampling(data = popF , size = 3, n = 50)

##bangkitan package
popF1<-rf(n = 1000,2,3)
sampelF1<-sampling(data = popF1 , size = 10, n = 100)

##exponensial
exp1<-randomexp(1000,5)
sampleexp1<-sampling(exp1,10,100)

exp2<-inversexp(1000,5)
sampelexp2<-sampling(exp1,10,100)

data1<-randombanget(5,seed = 4,a = 50,c = 3,m = 70)
data2<-randombanget(5,seed = 4,a = 50,c = 3,m = 70)

data3<-bangkitNormal(data1,data2)
plot(density(data3))


randomIntegral<-function(n,min,max){
  for (i in 1:n){
    unif<-runif(n,min,max)
  }
  gbar<-sum(unif)/n
  I<-(max-min)*gbar
  return(I)
}
randomIntegral(1000,10,25)
randomIntegral(1000,0,1)

