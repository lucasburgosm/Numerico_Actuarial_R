a <- expression(2*x +x)
D(a,"x")
x <- 10
eval(a)




e <- expression(x/5 - sin(x))

f_resolver <- function(x) { 
e <- eval(e)  
return(e)
}



biseccion <- function(a,b,n,TOL) { 
options(digits = 15)
i = 0
while(i<n)  { 
p <- (b+a)/2  
FP <- f_resolver(p)
        if(abs(FP) < TOL | FP == 0) { 
        print(round(FP,digits = 10)) # resultado
        print(p)  # raiz
        print(i)  # cantidad de iteraciones
        break }
     else if(FP* f_resolver(a) < 0) { 
          i = i + 1  
           b <- p                   }
     else            { 
           i = i + 1
           a <- p    }
      }
if(i == n) { 
return("metodo falló luego de N iteraciones") }
}

biseccion(3.145,3.145/2,50,0.000000005)

biseccion(-10,0,40,0.00005)












