##SCRIPT SIMDAT

##======================== MARKOV CHAIN MONTE CARLO (MCMC) ================================##

install.packages("mcmc")
library(mcmc)
data(logit)
logit


out <- glm(y ~ x1 + x2 + x3 + x4, data = logit,family = binomial(), x = TRUE)

x <- out$x
y <- out$y
lupost <- function(beta, x, y) {
  eta <- as.numeric(x %*% beta)
  logp <- ifelse(eta < 0, eta - log1p(exp(eta)), - log1p(exp(- eta)))
  logq <- ifelse(eta < 0, - log1p(exp(eta)), - eta - log1p(exp(- eta)))
  logl <- sum(logp[y == 1]) + sum(logq[y == 0])
  return(logl - sum(beta^2) / 8)
}

set.seed(42) # to get reproducible results
beta.init <- as.numeric(coefficients(out))
out <- metrop(lupost, beta.init, 1e3, x = x, y = y)
names(out)

out$accept


out <- metrop(out, nbatch = 1e4, x = x, y = y)
out$accept

out$time

out <- metrop(out, nbatch = 1e2, blen = 100,outfun = function(z, ...) c(z, z^2), x = x, y = y)
out$accept
out$time
out$batch


apply(out$batch, 2, mean)

foo <- apply(out$batch, 2, mean)
mu <- foo[1:5]
sigmasq <- foo[6:10] - mu^2
mu
sigmasq

mu.mcse <- apply(out$batch[ , 1:5], 2, sd) / sqrt(out$nbatch)
mu.mcse

u <- out$batch[ , 1:5]
v <- out$batch[ , 6:10]
ubar <- apply(u, 2, mean)
vbar <- apply(v, 2, mean)
deltau <- sweep(u, 2, ubar)
deltav <- sweep(v, 2, vbar)
foo <- sweep(deltau, 2, ubar, "*")
sigmasq.mcse <- sqrt(apply((deltav - 2 * foo)^2, 2, mean) / out$nbatch)
sigmasq.mcse


sqrt(mean(((v[ , 2] - vbar[2]) - 2 * ubar[2] * (u[ , 2] - ubar[2]))^2) /out$nbatch)


sigma <- sqrt(sigmasq)
sigma.mcse <- sigmasq.mcse / (2 * sigma)
sigma
sigma.mcse

##=========================== NEWTON RAPHSON ====================================##


f<-function(x){ 
  return((3*x^2+2*x+3)^2-10); 
} 


df<-function(x){ 
  2*(6*x+2)*(3*x^2+2*x+3) 
} 

ddf<-function(x){ 
  4*(2+6*x+9+12*x+27*x^2) 
  
} 

nf.method<-function(func,dfunc,x){ 
  if (abs(dfunc(x))<10*.Machine$double.eps){ 
    return (x); 
  }else{ 
    return(x-func(x)/dfunc(x)); 
  } 
} 
curve(f,from=-2,to=2); 

start<-locator(n=1)$x; 
iter=100; 
col=rainbow(20) 
xn<-NULL; 
xn<-c(xn,start); 
n=1; 

while (iter>0){ 
  n=n+1 
  xn<-c(xn,nf.method(df,ddf,xn[n-1])); 
  abline(a=f(xn[n])-df(xn[n])*xn[n],b=df(xn[n]),col=col[n-1]); 
  if(abs(xn[n]-xn[n-1])<100*.Machine$double.eps) break; 
  iter=iter-1; 
  Sys.sleep(1) 
}

##================================ GIBBS SAMPLER =======================================##

rbvn<-function (n, rho)  
{ 
  x <- rnorm(n, 0, 1) 
  y <- rnorm(n, rho * x, sqrt(1 - rho^2)) 
  cbind(x, y) 
} 

bvn<-rbvn(10000,0.98) 
par(mfrow=c(3,2)) 
plot(bvn,col=1:10000) 
plot(bvn,type="l") 
plot(ts(bvn[,1])) 
plot(ts(bvn[,2])) 
hist(bvn[,1],40) 
hist(bvn[,2],40) 
par(mfrow=c(1,1)) 




gibbs<-function (n, rho)  
{ 
  mat <- matrix(ncol = 2, nrow = n) 
  x <- 0 
  y <- 0 
  mat[1, ] <- c(x, y) 
  for (i in 2:n) { 
    x <- rnorm(1, rho * y, sqrt(1 - rho^2))
    y <- rnorm(1, rho * x, sqrt(1 - rho^2))
    mat[i, ] <- c(x, y) 
  } 
  mat 
}


bvn<-gibbs(10000,0.98) 
par(mfrow=c(3,2)) 
plot(bvn,col=1:10000) 
plot(bvn,type="l") 
plot(ts(bvn[,1])) 
plot(ts(bvn[,2])) 
hist(bvn[,1],40) 
hist(bvn[,2],40) 
par(mfrow=c(1,1)) 

##================================ RANDOMIZE =======================================##


hisrandom.unif <- function(m,x0,a,c,n){
  hasil <- c()
  for(i in 1:n){
    y <- a*x0+c
    y1 <- (y/m)-floor(y/m)
    x <- y1*m
    x0 <- x
    ui <- x/m
    hasil <- c(hasil,ui)
  }
  print(hasil)
}

random <- function(m,x0,a,c,n){
  hasil <- c()
  for(i in 1:n){
    y <- a*x0+c
    y1 <- (y/m)-floor(y/m)
    x <- y1*m
    x0 <- x
    hasil <- c(hasil,x)
  }
  print(hasil)
}

m<-6
ydat<-matrix(0,m,5)
ydat[,2]<-10
ydat[,3]<-5
for(i in 1:m){
  n<-10*(i+4)
  ydat[i,1]<-n
  y<-rnorm(n,10,sqrt(5))
  ydat[i,4]<-mean(y)
  ydat[i,5]<-var(y)
}
plot(ydat[,1],ydat[,4],type="b",xlab="n",ylab="ragam dan mean",ylim=c(3,12))
lines(ydat[,1],ydat[,2])
points(ydat[,1],ydat[,5])
lines(ydat[,1],ydat[,5])
lines(ydat[,1],ydat[,3])


m<-6
par(mfrow=c(2,3),mar=rep(3,4))
ydat<-matrix(0,m,5)
for(i in 1:m){
  n<-10*(i+4)
  ydat[i,1]<-n
  y<-rnorm(n,10,sqrt(5))
  ydat[i,4]<-mean(y)
  ydat[i,5]<-var(y)
  xd<-seq(min(y),max(y),0.1)
  yd<-dnorm(xd,10,sqrt(5))
  plot(density(y))
  lines(xd,yd) 
}

##================================ CART =======================================##

##Data yang digunakan

adult = read.csv("~/adult_train.csv")
head(adult)

names(adult)
plot(adult$Target)

#Data Train dan Test
#DAta dibagi menjadi data train untuk membentuk model dan  data test untuk mengevaluasi model dengan proporsi 80% dan 20%
validation_index <- createDataPartition(adult$Target, p=0.80, list=FALSE)
# select 20% of the data for validation
test <- adult[-validation_index,]
# use the remaining 80% of data to training and testing the models
train <- adult[validation_index,]

##LIbrary yang digunakan
library(caret)
library(hmeasure)
library(DescTools)
##Model yang digunakan
#Linear Discriminant Analysis
#Linear Discriminant Analysis (LDA) is most commonly used as dimensionality reduction technique in the pre-processing step for pattern-classification and machine learning applications. The goal is to project a dataset onto a lower-dimensional space with good class-separability in order avoid overfitting ("curse of dimensionality") and also reduce computational costs.
model_lda <- train(Target~., data=train, method="lda")
warnings()
model_lda

##Evaluasi Model

#prediksi
prediction_lda <- predict(model_lda, test)
cm_lda = confusionMatrix(prediction_lda, test$Target)
cm_lda$
  prediction_lda$posterior
perform_lda<-predict(model_lda, test,type="prob")

perform_lda<-perform_lda[,2]
result_lda<-HMeasure(test$Target,perform_lda)
summary(result_lda)
result_lda$metrics$Prec
BS_lda<-BrierScore(as.numeric(prediction_lda),as.numeric(test$Target))
PCC_lda<-mean(test$Target==prediction_lda)
eval_lda = cbind("LDA", PCC_lda, result_lda$metrics$AUC,result_lda$metrics$Gini, result_lda$metrics$H, BS_lda,result_lda$metrics$KS  )
eval_table[1,] = eval_lda
colnames(eval_table) = c("Model", "PCC", "AUC", "PG", "HMeasure", "BS","KS")


#Load Data
adult_train <- read.csv(file.choose(),sep=",", header=TRUE)
adult_test <- read.csv(file.choose(), sep=",", header=TRUE)

#Preprocessing
train<-adult_test[,1:14]
test<-adult_test[,1:14]


#Build the model
modelCART<-rpart(Target~Age+Workclass+fnlwgt+Education+Education_Num+Martial_Status+Occupation+Relationship+Race+Sex+Capital_Gain+Capital_Loss+Hours_per_week+Country,data=adult_train)

#Summarize the model
summary(modelCART)

#Predict using the model
pred_cart<-predict(modelCART,train,type="class")

#Accuracy of the model
mtab<-table(pred_cart,adult$Target)
confusionMatrix(mtab)

#Measure

library(caret)
library(hmeasure)
library(DescTools)

perform<-predict(modelCART,test,type="prob")
perform<-perform[,1]
result<-HMeasure(adult$Target,perform)
summary(result)
adult$Target<-as.numeric(adult$Target)
BS<-BrierScore(adult$Target,perform)
PCC<-mean(adult$Target==perform)
BS
PCC


model<-knn3(Target~Age+Workclass+fnlwgt+Education+Education_Num+Martial_Status+Occupation+Relationship+Race+Sex+Capital_Gain+Capital_Loss+Hours_per_week+Country,data=train,k=5)
summary(model)
pred<-predict(model,test,type="class")
mtab<-table(pred,test$Target)
confusionMatrix(mtab)

perform<-predict(model,test,type="prob")
perform<-perform[,1]
result<-HMeasure(test$Target,perform)
summary(result)
test$Target<-as.numeric(test$Target)
BS<-BrierScore(test$Target,perform)
PCC<-mean(test$Target==perform)
BS
PCC



