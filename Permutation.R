# setwd("D:/House/kj-uas-simdat")
## Resampling -- Permutation
##
## best for testing hypothesis

## permutation simple
################################################################
# input : 
#   Yvar: data dalam c()
#   nB: jumlah permutasi sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil permutasi
#   [[2]]: rata-rata dari permutasi
#   [[3]]: rata-rata dari aslinya
kj.permutation.mean.simple <- function(Yvar, nP=1000, verbose=T) {
  permutatedf <- list()
  for (i in 1:nP) {
    permutatedf[[i]] <- sample(Yvar, replace=F)
  }
  
  real_mean<- mean(Yvar)
  permutatedf_mean<- mean(sapply(permutatedf, mean))
  
  # Interpretasi7
  if(verbose) { 
    cat("\n Resampling Permutation ( nP =", nP, ")", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (bootstrap) :", permutatedf_mean
    )
    hist(sapply(permutatedf, mean))
  }
  invisible(list(permutatedf, permutatedf_mean, real_mean))
}

## permutation by index
################################################################
# input : 
#   Yvar: data dalam data.frame
#   nB: jumlah permutasi sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil permutasi
#   [[2]]: rata-rata masing-masing kolom dari permutasi
#   [[3]]: rata-rata masing-masing kolom dari aslinya
kj.permutation.mean.index <- function(Y, nP=1000, verbose=T) {
  permutatedf <- list()
  for (i in 1:nP) {
    temp <- list()
    for (j in 1:dim(Y)[2]) 
      temp[[j]] <- Y[sample(dim(Y)[1], replace = F) ,j]
    tempdf <- data.frame(temp)
    names(tempdf) <- names(Y)
    permutatedf[[i]] <- tempdf
  }
  
  real_mean<- sapply(Y, mean)
  permutatedf_mean<- sapply(permutatedf, function(x){ sapply(x, mean) })
  permutatedf_mean<- matrix(sapply(permutatedf_mean, mean), ncol = 2, byrow = T)
  permutatedf_mean<- apply(permutatedf_mean, 2, mean)
  
  # Interpretasi7
  if(verbose) { 
    cat("\n Resampling Permutation ( nP =", nP, ")", 
        "\n  | each rata-rata (populasi)  : \n  | ", real_mean, 
        "\n  | each rata-rata (permutasi)  : \n  | ", permutatedf_mean
    )
  }
  invisible(list(permutatedf, permutatedf_mean, real_mean))
}