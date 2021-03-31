install.packages("markovchain")
library(markovchain)
library(MASS)

#QUIZ 1 PROCESOS ESTOCASTICOS
#Integrantes del grupo: 
#Paula Valencia, Juan Diego Hernandez, Laura Viviana Vargas, Camilo Navarro

#PUNTOS A Y B: 
#Contruimos la matriz de transicion de la cadena y obtenemos los tiempos
#esperados de absorcion

#Proceso:

#Creamos la matriz de transicion
C1<-matrix(c(0.96, 0.04, rep(0,times=9), 0.9289, 0.0711,
             rep(0,times=9), 0.9067, 0.0933, rep(0,times=9),
             0.8933, 0.1067, rep(0,times=9), 0.8889, 0.1111,
             rep(0,times=9), 0.8933, 0.1067, rep(0,times=9),
             0.9067, 0.0933, rep(0,times=9), 0.9289, 0.0711,
             rep(0,times=9), 0.9600, 0.0400,rep(0,times=9),1), 
           nrow=10, byrow=TRUE)

#Imprimimos C1 para verificarla y visualizarla
print(C1)

#Para hayar el tiempo de absorcion, identficamos la matriz Q
Q<-C1[1:9,1:9]

#Creamos una matriz identidad de las dimensiones de Q
ID<-diag(9)

#Hayamos la matriz que se obtiene al restar I y Q
M1<-ID-Q

#Hayamos la inversa de la matriz arriba mencionada
M1INV<-solve(M1)

#Creamos un vector de una columna y tantas filas como filas tiene Q
UNO<-matrix(rep(1, times=9), nrow=9, byrow=TRUE)

#Hayamos la matriz de tiempos de absorcion multiplicando la inversa de la resta 
#de I y Q con el vector de unos
Tiemp<-M1INV%*%UNO

#Imprimimos el vector de tiempos de absorcion
print(Tiemp)


#PUNTO C
#¿Como cambian los tiempos de absorcion dependiendo de la probabilidad
#de que un amigo que no conoce el restaurante, vaya dado que se reunio
#con un amigo que ya conoce el restaurante?

#Llamamos "P" a esta probabilidad

#Los valores de la matriz C1 en el punto A y B fueron calculados usando
#una probabilidad de P=0.2 , donde tambien se uso la probabilidad
#de que una reunion entre un amigo que ya fue al restaurante y uno que no,
#suceda, dado que hay "m" amigos entre los 10 que ya fueron. Esta probabilidad
#la conocemos con una dist. Hipergeometrica P(Y=1), con m:{1;2;...;9}
#Entendemos entonces PM como una funcion de m -> PM(m)



#Llamamos "Pm" a P(Y=1) con m:{1;2;...;9}
PM<-c(0.200,0.3556,0.4667,0.5333,0.5556,0.5333,0.4667,0.3556,0.200) 

#Deducimos que el elemento 10,10 de la matriz es 1, ya que la prob. de ir
#de 10 a 10 es el 100%

#Para formular la matriz de transicion en funcion de P,
#primero haremos la matriz formulada con un valor de P=0.2
#para confirmar que la formulacion funcione

#Entendemos que la diagonal principal de la matriz de transicion contiene
#la probabilidad de ir de un estado i a un mismo estado i, es decir,
#que al siguiente dia de la reunion, el numero de amigos que ya fue al 
#restaurante sea el mismo del dia anterior

#Esta probabilidad se puede entender como el complemento de la probabilidad
#de ir de un estado i a un estado i+1, osea, que al siguiente dia de la 
#reunion, haya un amigo más en el grupo que conoció el restaurante


P<-0.2


#Probabilidades de la diagonal:

1-PM[i]*P

#Probabilidades a la derecha de la diagonal
PM[i]*P


#Ahora llenamos la matriz formulada
C2<-matrix(c(1-PM[1]*P, PM[1]*P, rep(0,times=9), 1-PM[2]*P, PM[2]*P,
             rep(0,times=9), 1-PM[3]*P, PM[3]*P, rep(0,times=9),
             1-PM[4]*P, PM[4]*P, rep(0,times=9), 1-PM[5]*P, PM[5]*P,
             rep(0,times=9), 1-PM[6]*P, PM[6]*P, rep(0,times=9),
             1-PM[7]*P, PM[7]*P, rep(0,times=9), 1-PM[8]*P, PM[8]*P,
             rep(0,times=9), 1-PM[9]*P, PM[9]*P,rep(0,times=9),1), 
           nrow=10, byrow=TRUE)


#Imprimimos C2 y la comparamos con C1
print(C1)
print(C2)

#Ahora, podemos crear una funcion cuyo input sea el valor de "P" y el output sea
#la matriz de transicion

F1<-function(P){
  C<-matrix(c(1-PM[1]*P, PM[1]*P, rep(0,times=9), 1-PM[2]*P, PM[2]*P,
              rep(0,times=9), 1-PM[3]*P, PM[3]*P, rep(0,times=9),
              1-PM[4]*P, PM[4]*P, rep(0,times=9), 1-PM[5]*P, PM[5]*P,
              rep(0,times=9), 1-PM[6]*P, PM[6]*P, rep(0,times=9),
              1-PM[7]*P, PM[7]*P, rep(0,times=9), 1-PM[8]*P, PM[8]*P,
              rep(0,times=9), 1-PM[9]*P, PM[9]*P,rep(0,times=9),1), 
            nrow=10, byrow=TRUE)
  return(C)}

#Confirmamos que la funcion funcione
F1(0.2)


#comparamos con C1
print(C1)

#Al coincidir, decimos que la funcion es correcta.

#Podemos hacer una funcion cuyo input sea el output de F1, y que de como output
#el tiempo de absorcion

  T1<-function(C){
   a<-solve(diag(9)-C[1:9, 1:9])%*%matrix(rep(1, times=9), nrow=9, byrow=TRUE)
    return(a)}
  
#Comparamos el valor de TI(F1(0.2)) con el tiempo de absorcion que ya habiamos
#encontrado y que habiamos llamado "Tiemp"
  T1(F1(0.2))
  print(Tiemp)

#Los vectores coinciden, la funcion es correcta

#Ahora, podemos hacer un vector que conntenga distintos valores posibles de P

V<-seq(0.1, 1, by=0.1)

#Por ultimo, relizamos un for que imprimirá las 10 matrices de tiempos de
#absorcion para cada uno de los valores de P que definimos en V

for(v in V){
  print(T1(F1(v)))
}

#Copiamos en excel y graficamos





