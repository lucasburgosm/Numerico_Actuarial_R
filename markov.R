
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
  b <- matrix(data = c(1,0), nrow = 2)    
  j <- h + 1     
} 
}  
print(h)
print(j)
return(b)  
}

markovv_con_for(a,b,4)


markovvv <- function(a,b,t) {
z <- eigen(a)  
z_autovalores <- z$values
## saco auto valores
z_autovectores<- z[[2]] 
## saco matriz de autovectores
z_autovalores <- diag(z_autovalores)
## diagonal de autovalores
z_autovectores_inversa <- solve(z_autovectores)
## saco P^-1 inversa de autovectores
z_autovalores <- z_autovalores^t
## elevo a la t la matriz diagional de autovalores
b <- round(z_autovectores %*% z_autovalores%*% z_autovectores_inversa %*% b,10)
print("autovalores")
print(round(z$values,10)) 
print(round(z[[2]],10))   
return(b)      
}
#####
markovvv(a,b,10000)



# Ejercicio 1
a <- matrix(data = c(0,1/2,1/2,0.5,0,0.5,0.5,0.5,0), nrow = 3)
p_0 <- matrix(data = c(1,0,0), nrow = 3)

markovvv(a,p_0,2)
markovvv(a,p_0,10)
markovvv(a,p_0,5)
markovvv(a,p_0,1000)

####
# Ejercicio 2
b <-  matrix(data = c(1/4,3/4,0,3/4,1/4,0,0,0,1), nrow = 3)
b
p_0 <- matrix(data = c(1,0,0), nrow = 3)

markovvv(b,p_0,2)
markovvv(b,p_0,10)
markovvv(b,p_0,4)
markovvv(b,p_0,1000)

p_1 <- matrix(data = c(1/2,0,1/2), nrow = 3)

markovvv(b,p_1,2)
markovvv(b,p_1,10)
markovvv(b,p_1,4)
markovvv(b,p_1,1000)



####
# Ejercicio 2) B)

c <-  matrix(data = c(1/3,1/3,1/3,1/3,1/3,1/3,0,0,1), nrow = 3)

markovvv(c,p_0,2)
markovvv(c,p_0,10)
markovvv(c,p_0,4)
markovvv(c,p_0,1000)

markovvv(c,p_1,2)
markovvv(c,p_1,10)
markovvv(c,p_1,4)
markovvv(c,p_1,1000)

# Ejercicio 2) c)
d <-  matrix(data = c(0,1/2,1/2,0,1/2,0,0,1/2,1/2,0,0,1/2,0,1/2,1/2,0), nrow = 4)
p_2 <- matrix(data = c(1/2,0,0,1/2), nrow = 4)

markovvv(d,p_2,2)
markovvv(d,p_2,10)
markovvv(d,p_2,4)
markovvv(d,p_2,1001)


p_3 <- matrix(data = c(1/8,1/8,3/8,3/8), nrow = 4)

markovvv(d,p_3,2)
markovvv(d,p_3,10)
markovvv(d,p_3,4)
markovvv(d,p_3,1000)

p_4 <- matrix(data = c(1,0,0,0), nrow = 4)

markovvv(d,p_4,4)


##
## Ejercicio 4



# Ejercicio 5
markovvv(a,p_0,1)[2,1]   # P(Xt1= 2 / Xt0 = 1)

# b
p_t1 <- c(0,1,0)

markovvv(a,p_t1,1)[3,1]   # P(Xt2= 3 / Xt0 = 2)

# Prob (Xt2 = 3/ Xt1 = 2 y Xt0 = 1)
markovvv(a,p_0,1)[2,1] * markovvv(a,p_t1,1)[3,1] * 1

###

markov_tiempo_continuo <- function(a,b,t) {
  z <- eigen(a)  
  z_autovalores <- z$values
  ## saco auto valores
  z_autovectores<- z[[2]] 
 
   ## saco matriz de autovectores
  z_autovalores <- exp(1)^(z_autovalores * t)
  z_autovalores <- diag(z_autovalores)
  
  z_autovectores_inversa <- solve(z_autovectores)
  ## saco P^-1 inversa de autovectores
  
  b <- round(z_autovectores %*% z_autovalores%*% z_autovectores_inversa %*% b,10)
  print("autovalores")
  print(round(z$values,10)) 
  print(round(z[[2]],10))   
  return(b)      
}
#####
p <-  matrix(data = c(1.2,1.2,0), nrow = 3)

a <- matrix(data = c(-2,1,1,1,-2,1,1,1,-2), nrow = 3)

a <- matrix(data = c(1,-2,2,-4), nrow = 2)


z <- eigen(a)
auto_vectores <- z[[2]]


inversa_auto_vectores <- round(solve(auto_vectores),10)

auto_values <- round(z$values,10)

q <- diag(exp(1)^(auto_values*1))


auto_vectores %*% q %*% inversa_auto_vectores  

inversa_auto_vectores

