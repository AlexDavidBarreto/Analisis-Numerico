#Punto 13 libro Python
f<-function(n,E,x)
{
  y<-0.5*(x+(n/x))
  repeat{
    x<-y
    y<-0.5*(x+(n/x))
    if(abs(x-y)>E)
      break
  }
  return(y) 
}
#Se va a evaluar la raiz de 81
cat(f(81,0.00000001,10))
