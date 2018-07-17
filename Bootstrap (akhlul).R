# setwd("D:/House/kj-uas-simdat")
## Resampling -- Bootstrap
##
## best for confidence interval

## bootstrap simple
################################################################
# input : 
#   Yvar: data dalam c()
#   nB: jumlah bootstrap sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil bootstrap
#   [[2]]: rata-rata dari bootstrap
#   [[3]]: rata-rata dari aslinya
kj.bootstrap.mean.simple <- function(Yvar, nB=1000, verbose=T) {
  bootdf <- list()
  for (i in 1:nB) {
    bootdf[[i]] <- sample(Yvar, replace=TRUE)
  }
  
  real_mean<- mean(Yvar)
  bootdf_mean<- mean(sapply(bootdf, mean))
  
  # Interpretasi7
  if(verbose) { 
    cat("\n Resampling Bootstrap ( nB =", nB, ")", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (bootstrap) :", bootdf_mean
    )
    hist(sapply(bootdf, mean))
  }
  invisible(list(bootdf, bootdf_mean, real_mean))
}

## bootstrap by index
################################################################
# input : 
#   Yvar: data dalam "data.frame"
#   nB: jumlah bootstrap sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil bootstrap
#   [[2]]: rata-rata masing-masing kolom dari bootstrap
#   [[3]]: rata-rata masing-masing kolom dari aslinya
kj.bootstrap.mean.index <- function(Yvar, nB=1000, verbose=T) {
  bootdf <- list()
  for (i in 1:nB) {
    index_boot <- sample(1:nrow(Yvar), replace=TRUE)
    bootdf[[i]] <- Yvar[index_boot,]
  }
  real_mean<- sapply(Yvar, mean)
  bootdf_mean<- sapply(bootdf, function(x){ sapply(x, mean) })
  bootdf_mean<- matrix(sapply(bootdf_mean, mean), ncol = 2, byrow = T)
  bootdf_mean<- apply(bootdf_mean, 2, mean)
  
  # Interpretasi7
  if(verbose) { 
    
    cat("\n Resampling Bootstrap (nB =", nB, ")", 
        "\n  | each rata-rata (populasi)  : \n  | ", real_mean, 
        "\n  | each rata-rata (bootstrap) : \n  | ", bootdf_mean
    )
  }
  
  invisible(list(bootdf, bootdf_mean, real_mean))
}

## bootstrap parametric simple
################################################################
# input : 
#   Yvar: data dalam c()
#   nB: jumlah bootstrap sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil bootstrap
#   [[2]]: rata-rata dari bootstrap
#   [[3]]: rata-rata dari aslinya
kj.bootstrap.parametric.mean.simple <- function(Yvar, nB=1000, verbose=T) {
  mean.y <- mean(Yvar)
  sd.y <- sd(Yvar)
  
  bootdf <- list()
  for (i in 1:nB) {
    bootdf[[i]] <- rnorm(length(Yvar), mean.y, sd.y)
  }
  
  real_mean<- mean(Yvar)
  bootdf_mean<- mean(sapply(bootdf, mean))
  
  # Interpretasi7
  if(verbose) { 
    cat("\n Resampling Parametric Bootstrap ( nB =", nB, ")", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (bootstrap) :", bootdf_mean
    )
    hist(sapply(bootdf, mean))
  }
  invisible(list(bootdf, bootdf_mean, real_mean))
}