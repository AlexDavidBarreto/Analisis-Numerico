require(Matrix)
#Creacion de las matrices
A = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2), nrow=4, byrow=TRUE)
B = matrix(c(3, 9, 14,
             -3, -1, 0, 
             3/4, 0.43,2), nrow=3, byrow=TRUE)
C = matrix(c(-8.1, -7, 6.123, -2, -1, 4,
             -3, -1, 0, -1, -5, 0.6,
             -1, 0.33, 6, 1/2,1,2,3,4,5,6,7,8,9), nrow=5, byrow=TRUE)
#Metodo que suma los elem de una matriz
sumarElementosMatriz = function(A, tamMatriz)
{
  sum = 0
  resultados = c()
  
  for (i in 1:tamMatriz) # n recorridos para las filas
  {
    for (j in 1:tamMatriz) # n*n recorridos para las columnas
    {
      sum = sum + A[i,j]
    } 
  }
  
  iteraciones = i*j
  
  resultados[1] = sum
  resultados[2] = iteraciones
  
  return(resultados)
}
#Matrices diagonales superiores e inferiores
LA = tril(A,k=-1,diag = FALSE)
UA = triu(A,k=1,diag = FALSE)

LB = tril(B,k=-1,diag = FALSE)
UB = triu(B,k=1,diag = FALSE)

LC = tril(C,k=-1,diag = FALSE)
UC = triu(C,k=1,diag = FALSE)

#Punto A formato Suma/Iteraciones totales

sumarElementosMatriz(LA,4)
sumarElementosMatriz(LB,3)
sumarElementosMatriz(LC,5)

#Punto B formato Suma/Iteraciones totales
sumarElementosMatriz(A,4)
sumarElementosMatriz(B,3)
sumarElementosMatriz(C,5)

#Dado que debe recorrer todos los elementos de la matriz, para una matriz de N numeros, su orden de complejidad es O(n)


