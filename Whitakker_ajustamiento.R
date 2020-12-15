

options(digits = 20)

# x para ver si son equispaciado
# u = vector de u, muestras
# m = vector cantidad de muestras
# z = (un numero si o si) cantidad de diferencias, con poner n-1 alcanza, despues depende de las H que quieras analizar(van coordinados) 
# H = vector o un solo numero, la funcion analiza por order coordinando con los z. Ejemplo z = 3, y H(0,0,1) Solo analiza z = 3. Si pones H(1,0,1) analiza z = 1 + z = 3 
# Y = numero con el cual queres rellenar el resto de las H faltantes, por default es 0. Ejemplo H(0,4,0) con y = 1 quedaria H(0,4,0,1,1,1,1,1, length(u)). Analizando z = 2, z = 4:length(u)
# P opcional, si True busca parte de la ecuacion del GCV aprovechando la matriz H^-1

whittakerTP4 <- function(x,u,m,z,H,Y=0,P=F)  { 
  n<-length(x)
  d<-x[2]-x[1]
  a<-seq(x[1],(x[1]+(n-1)*d),by=d)
  for(i in 1:length(x)){
    if(x[i]==a[i])
      p=0 else p=1} 
   if(p==1) { 
    return("Los argumentos no son equispaciados") 
    break   }  
  
  if(z >= length(u)) { 
      return("La longitud del vector U debe ser mas larga que la diferencia Z")  
      break}
  
# uso funcion adentro de funcion para no cargar de funciones el script
   function_diferencia <- function(z) {   
     k <- c()
       for(s in 0:z) {
              k[s+1] <- factorial(z)/(factorial(z-s)*factorial(s))*((-1)^(z-s))
                    } 
    return(k)         # k es el vector que se usa para crear la matriz k, es la formula para poner de forma matricial las diferencias. DElta^n = (E-1)^n     
                  }

   funcion_matrix_diferencia <- function(u,z) {  
     v <- length(u) - z
     k <- function_diferencia(z) 
     matrix_k <- matrix(nrow = v*2, ncol = (v+z)*2, data = 0)  # creo una matrix grande de 0, para trabajar mas comodo

     for(s in 1:v) {                          # Esto es lo mas complicado de ver aunque es sencillo. Puedo reemplezar un trozo de la matriz con un vector
       matrix_k[s,s:(length(k)+s-1)] <- k     # Los primeros datos de la matriz, ejemplo [1,1:3]. Reemplazo con K[1], k[2], k[3]
                                              # Elijo la primera fila, que despues con el for va bajando una a una.  
                }                             # Y tambien se va corriendo una columna. (Ver Caviezel pag 12)
   return(matrix_k[1:v,1:(v+z)])              # corto la matriz que se usa. matriz de  n-z*n   (n = largo de u)

                   }
KH <- 0                                       # creo el resto de las H, por defualt, sino especficas el resto de las H son 0. Pero se puede poner Y = 1. Para
A <- rep(Y,times = z)                         # que el resto o todas las H tengan ese valor.
H <- c(H, A)

for(g in 1:z) {                              # va guardando los KH a partir de los z, por eso los H y los Z tiene que estan coordinados.
tm <- funcion_matrix_diferencia(u,g)         # ej, si pones H(0,97,0), va a tomar solo el z = 2 porque hace 0 el resto de los z. De hecho con poner solo 
tt <- t(tm)                                  # un z-n, alcanza. Vas modificando los H que queres que cuenten.
k  <- H[g] * (tt %*% tm)  
KH <- KH + k  
       }
 
m <- m/mean(m)               # vector con la m ajustada por la media de u
m_matriz <- diag(m)          # matriz diagonal m

KH <- m_matriz + KH
WU <- m*u
WU <- matrix(WU,ncol = 1)
if(det(KH)==0){return("El sistema no tiene solucion , dado que KH no es invertible") }

c <- solve(KH,WU)
colnames(c)<-c("vi")

if(P == TRUE) {                                                          # esto es un extra para el GCV, ya que resolvemos KH dependiendo el H, en vez de devolver
return((length(x)%*%((1-((sum(diag(solve(KH)%*%m_matriz))))/n)^2))) }    # los vi, devuelve parte de la cuenta del GCV

return(c)
}



H <- c(0,0,1160)

z = 3 
x <- seq(from = 1989, to = 2009, by = 1)
u <- c(9.5,24.8,19.8,5.8,10.3,16.5,27.5,12.4,35.6,51.7,26.3,23.9,39.9,45.6,45.9,43.1,47.4,42.7,40.2,31.2,44.5)
m <- rep(1,21)


H <- c(0,0,1160)

whittakerTP4(x,u,m,z,H)
cbind(x,u,m,z,whittakerTP4(x,u,m,z,H))


H <- c(0,97,0)


whittakerTP4(x,u,m,z,H)
cbind(x,u,m,z,whittakerTP4(x,u,m,z,H))




registro=885627  
set.seed(registro)
x=1:20
m=runif(20,5,15)
u=round(100+x/3+x^2/40-x^3/600+rnorm(20,0,2),1)
H <- c(1,97,1160)

whittakerTP4(x,u,m,z,H)
cbind(x,u,m,z,whittakerTP4(x,u,m,z,H))








GCV <- c()
x <- seq(from = 1989, to = 2009, by = 1)
u <- c(9.5,24.8,19.8,5.8,10.3,16.5,27.5,12.4,35.6,51.7,26.3,23.9,39.9,45.6,45.9,43.1,47.4,42.7,40.2,31.2,44.5)
m <- rep(1,21)
z <- 5
n <- length(u)

for(g in 1:5000) {
H     <- c(0,g,0)    # al poner el resto de las H = 0. Solo toma Z = 2
vi    <- whittakerTP4(x,u,m,z,H)
GCV[g]<- t(vi-u) %*% diag(m/mean(m)) %*% (vi-u) / whittakerTP4(x,u,m,z,H,Y=0,TRUE)  # notar el true para diferenciar lo que devuelve la funcion de whittaker
}
  
landa <- 1:5000

mi_matrix <- cbind(landa,GCV)
mi_matrix <- mi_matrix[order(GCV),]
head(mi_matrix)


plot(x = landa,y = GCV, type = "lines")

 

GCV <- c()  
for(g in 1:5000) {
  H     <- c(0,0,g)  
  vi    <- whittakerTP4(x,u,m,z,H)
  GCV[g]<- t(vi-u) %*% diag(m/mean(m)) %*% (vi-u) / whittakerTP4(x,u,m,z,H,Y=0,TRUE)
  
}
mi_matrix <- cbind(landa,GCV)
mi_matrix <- mi_matrix[order(GCV),]
head(mi_matrix)


plot(x = landa,y = GCV, type = "lines")



# caso nocon y scott NASDAQ

x <- seq(from = 1, to = 20, by = 1) 
u <- c(12.02,10.87,12.56,16.47,17.62,17.13,21.25,22.14,23.6,22.27,24.47,23.53,23.02,23.02,26.97,23.78,21.41,19.61,20.91,23.13)
m <- rep(1,20)
z = 5

GCV <- c()
for(g in 1:100) {
  H     <- c(0,g,0)  
  vi    <- whittakerTP4(x,u,m,z,H)
  GCV[g]<- t(vi-u) %*% diag(m/mean(m)) %*% (vi-u) / whittakerTP4(x,u,m,z,H,Y=0,TRUE)
   
}
landa <- 1:100  
GCV <- GCV[1:100]
mi_matrix <- cbind(landa,GCV)
mi_matrix <- mi_matrix[order(GCV),]
head(mi_matrix)


plot(x = landa,y = GCV, type = "lines")




###
# Punto c


registro=885627  
set.seed(registro)
x=1:20
m=runif(20,5,15)
u=round(100+x/3+x^2/40-x^3/600+rnorm(20,0,2),1)


GCV <- c()
for(t in 250:8000) {
  H     <- c(0,t,0)    # al poner el resto de las H = 0. Solo toma Z = 2
  vi    <- whittakerTP4(x,u,m,z,H)
  GCV[t]<- t(vi-u) %*% diag(m/mean(m)) %*% (vi-u) / whittakerTP4(x,u,m,z,H,Y=0,TRUE)  # notar el true para diferenciar lo que devuelve la funcion de whittaker
}
GCV <- GCV[250:8000]
landa <- 250:8000

mi_matrix <- cbind(landa,GCV)
mi_matrix <- mi_matrix[order(GCV),]
head(mi_matrix)


plot(x = landa,y = GCV, type = "lines")




GCV <- c()
for(t in 1:10000) {
  H     <- c(0,0,t)    # al poner el resto de las H = 0. Solo toma Z = 2
  vi    <- whittakerTP4(x,u,m,z,H)
  GCV[t]<- t(vi-u) %*% diag(m/mean(m)) %*% (vi-u) / whittakerTP4(x,u,m,z,H,Y=0,TRUE)  # notar el true para diferenciar lo que devuelve la funcion de whittaker
}
GCV <- GCV[1000:10000]
landa <- 1000:10000

mi_matrix <- cbind(landa,GCV)
mi_matrix <- mi_matrix[order(GCV),]
head(mi_matrix)


plot(x = landa,y = GCV, type = "lines")



        