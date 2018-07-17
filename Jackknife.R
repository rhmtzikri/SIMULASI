# setwd("D:/House/kj-uas-simdat")
## Resampling -- Jack Knife

################################################################
# input : 
#   Yvar: data dalam c()
#   nB: jumlah jackknife sampel
#   v: tampilkan output
# output :
#   [[1]]: data hasil jackknife
#   [[2]]: rata-rata dari jackknife
#   [[3]]: rata-rata dari aslinya
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
  jackdf_mean<- mean(sapply(jackdf, mean))
  # Interpretasi
  if(verbose) { 
    cat(" Resampling Jackknife", 
        "\n | rata-rata (populasi)  :", real_mean, 
        "\n | rata-rata (jackknife) :", jackdf_mean
    )
    hist(sapply(jackdf, mean))
  }
  invisible(list(jackdf, jackdf_mean, real_mean))
}