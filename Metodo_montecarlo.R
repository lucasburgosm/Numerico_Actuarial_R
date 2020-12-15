# metodo montecarlo




vector_aleatorio <- runif(1000000,max = 0.5)

integracion <- function(a) {
t <- 0
for(i in 1:length(a)) { 
b <- (a[i]^0.5)
t <- t + exp(1)^b
 }
return(t/length(a))
}

integracion(vector_aleatorio)


z <- c(0.1304730,0.2684891,0.1775968,0.9005566,0.6194398)

qpois(z,2)
#  0 1 1 4 2

qbeta(z,shape1 = 3,shape2 = 1)
# 0.5071933 0.6451225 0.5620976 0.9656884 0.8524450


?Beta


markovv_con_for <- function(a,b,t) { 
  h <- 0
  j <- 0
  for(i in 1:t){
    b <-  a %*% b
    if(rbinom(1,1,b[1,1])==1) {
      b <- matrix(data = c(1,0), nrow = 2)    
      h <- h + 1   
    }
    else{
      b <- matrix(data = c(0,1), nrow = 2)    
      j <- j + 1     
    } 
  }  
  print(h)
  print(j)
  return(b)  
}

markovv_con_for(a,b,10)



a <- matrix(data = c(1/2,1/2,0.9,0.1), nrow = 2)
b <- matrix(data = c(2/3,1/3), nrow = 2)


