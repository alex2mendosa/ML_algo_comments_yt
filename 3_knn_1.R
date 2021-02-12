library(dplyr)
library(tibble)
n<-1000
sample_1<-tibble( x1=rnorm(n,10,2),
                  x2=runif(n, min = 100, max = 200) , 
                  x3=rexp(n,0.1) , 
                  x4=sample(0:1,n,replace = TRUE) , 
                  x5=rchisq(n, 5, ncp = 50) ,
                  x6=log( rnorm(n,10,2) ) )















