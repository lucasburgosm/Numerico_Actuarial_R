                                    # FUNCION PARA DIFERENCIAS SIMPLES Y DIVIDIDAS 

diferencias_simples_divididas <- function(b,orden,divididas=F,) {     # b matrix de x y FX, orden = cantidad de diferencias # simple por default, 
  funcion_escalvo_dif <- function(m,p=1) {   # se ingresa una vector, FX)
    l <- length(m)                           # largo del vector
    c <- rep(0,l)                            # se llena de vector de 0
    for(i in 1:(l-p)) { 
      c[i] <- m[i+1] - m[i] 
    }
    return(c)
  }  
  k <- orden                                        #  
  x <- b[,1]                                        # vector de las x
  fx <- b[,2]                                       # vector de las fx
  if(k >= length(fx)){return("No se puede, revisar largo de vector") }       # verifico cantidad de dif con datos
  else { 
    names <- c("X", "FX")                           # creo vector nombre de las columnas
    for(i in 1:k) { 
      c <- funcion_escalvo_dif(fx,i)                # uso funcion para restar diferencias simples 
      if(divididas==T) {                       
        c <- c/funcion_escalvo_dif(x,i)  }      # si quiero diferencias div,las difencias simples, por la funcion esclavo
      b <- cbind(b,c)                               # agrego el nuevo vector a la matrix original                                                     
      fx <- b[,(i+2)]                               # igual al nuevo vector en el cual se hacen las diferencias 
      u <- "Dif"
      uu <- i                                       # genero nombres de las columnas y los uno con paste y colnames al final
      name<-paste(u,uu,sep = "", collapse = "")
      names <- c(names, name)
    }  
  }    
  colnames(b) <- names
  b[is.na(b)] <- 0                                  # saco los NA (que son los 0/0 de la diferencias divididas) 
   print(b)
   print("Vector Dif orden K")
  return(b[,k+2])
}  # si queres div poner T

x<-c(1,3,5,7,9,11,13)
fx<-c(144,56,35,22,78,3,17)
b<-cbind(x,fx)

diferencias_simples_divididas(b,5,T)



