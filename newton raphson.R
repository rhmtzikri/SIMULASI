f<-function(x){
  return(4*x^3-15*x^2+17*x-6)
}

f1<-function(x){
  return(12*x^2-30*x+17)
}

simdat.newton.raphson <- function(x0,fx,f1x,error,iterasi=1000){
  xn<-x0
  fx<-f(xn)
  f1x<-f1(xn)
  tabel<-matrix(ncol=4,nrow=iterasi)
  colnames(tabel)<-c("xn","xn+1","f(xn)","f(xn+1)")
  for(i in 1:iterasi){
   xnplus1<-xn-(fx/f1x)
   fxnplus1<-f(xnplus1)
   tabel[i,]<-cbind(xn,xnplus1,fx,fxnplus1)
   if(abs(fxnplus1)<=error){
     return(list(xnplus1, tabel[!is.na(tabel[,1]),]))
     break()
   } else{
     xn<-xnplus1
     fx<-f(xn)
     f1x<-f1(xn)
   }
  }
}
simdat.newton.raphson(3,f,f1,error = 0.000,iterasi = 1000)
