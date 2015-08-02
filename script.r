#Practica final AA

#Alumnos:
#Ahisahar Pretel Rodriguez 75937643S
#Diego José Granados Álvarez  77150626R




#Cargar las muestras desde la carpeta /datos
poker.hand.testing <- read.csv("~/practicafinalAA/poker-hand-training-true.data", header=FALSE)
poker.hand.training.true <- read.csv("~/datos/poker-hand-training-true.data.txt", header=FALSE)

#Asignar el nombre de cada atributo a cada columna
nombre.col = c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","PH")
colnames(poker.hand.testing)=nombre.col
colnames(poker.hand.training.true)=nombre.col

#Analisis de los datos
hist(poker.hand.training.true$PH)
cor(poker.hand.training.true)
colnames(poker.hand.testing)=nombre.col




#ordenacion de las bases de datos

#Creamos ambas matrices para training y test
tr <- matrix(nrow=1,ncol=11)
te <- matrix(nrow=1,ncol=11)

for (n in 1:length(poker.hand.training.true[,1]))
{
  #Almacenamos una fila en una variable auxiliar
  prueba.poker = poker.hand.training.true[n,]
  
  #La transformamos de tipo matriz a tipo vector
  prueba.poker = as.vector(prueba.poker,mode='numeric')
  
  #Añadimos un 14 para que la longitud sea multiplo de 2 y al ser
  #mayor que todos los posibles valores para el rango de una carta
  #siempre permanecera en la ultima posición en la ordenación
  prueba.poker[12] = 14 
  
  #Creamos una matriz donde cada columna representa una carta menos
  #la ultima que tiene el tipo de la baraja y un 14
  m <- matrix(prueba.poker,nrow=2,ncol=6)
  
  #Obtenemos un vector con los indices de los elementos de la matriz ordenados
  #de mayor a menor
  a = order(m[2,],decreasing = T)
  
  #Invertimos los indices para obtener los elementos de menor a mayor
  a = rev(a)
  
  #Creamos un nuevo vector con las cartas ordenadas a partir de los indices
  v <- c(m[,a[1]],m[,a[2]],m[,a[3]],m[,a[4]],m[,a[5]],m[,a[6]])
  
  #Eliminiamos el ultimo elemento que añadimos
  v <- v[-12]
  
  #Insertamos la nueva muestra en la base de datos nueva
  tr <- rbind(tr,v)
}

#Mismo procedimiento para los datos de test
for (n in 1:length(poker.hand.testing[,1]))
{
  prueba.poker = poker.hand.testing[n,]
  prueba.poker = as.vector(prueba.poker,mode='numeric')
  prueba.poker[12] = 14 #Para que la longitud sea multiplo de 2
  m <- matrix(prueba.poker,nrow=2,ncol=6)
  a = order(m[2,],decreasing = T)
  a = rev(a)
  v <- c(m[,a[1]],m[,a[2]],m[,a[3]],m[,a[4]],m[,a[5]],m[,a[6]])
  v <- v[-12]
  te <- rbind(te,v)
}

tr <- tr[-1,]
te <- te[-1,]

#Asignamos las cabeceras a los nuevos datos
nombre.col = c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","PH")
colnames(tr)=nombre.col


nombre.col = c("S1","C1","S2","C2","S3","C3","S4","C4","S5","C5","PH")
colnames(te)=nombre.col

rownames(tr) <- NULL
rownames(te) <- NULL

#fin ordenacion


#Balanceado de las clases


for (c in 1:10)
{
  
  for (i in 1:round(Nmax/table(tr$PH)[c])-1)
  {
    for (x in 1:length(tr[,1]))
    {
      if(tr[x,11]==c){
        
        trB <- rbind(trB,tr[x,])
        
      }
        
      
    }
    
  }
  
  
}


#Ejecutamos naive bayes
library(e1071)
nb = naiveBayes(as.factor(tr$PH)~., data = tr)
system.time(a<- predict(nb ,poker.hand.testing[,],type='raw'))

model <- svm( poker.hand.training.true$PH~., poker.hand.training.true )
set.seed(1)
res <- predict( model, newdata=poker.hand.training.true )

#SVM

#con la base de datos original
attach(poker.hand.training.true)
poker.hand.training.true$PH = factor(poker.hand.training.true$PH)
tune.out = tune(svm,PH~., data=poker.hand.training.true[1:2000,], kernel="radial", ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)))
#table(predict = predict(tune.out$best.model,poker.hand.testing),truth=poker.hand.testing)
svmpred = predict(tune.out$best.model,poker.hand.testing[1:25000,])
mean(svmpred==poker.hand.testing$PH)


#con la base de datos ordenada

attach(tr)
tr$PH = factor(tr$PH)
tune.out = tune(svm,PH~., data=tr[1:2000,], kernel="radial", ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)))
#table(predict = predict(tune.out$best.model,poker.hand.testing),truth=poker.hand.testing)
svmpred = predict(tune.out$best.model,te)
#svmpred = round(svmpred)
mean(svmpred==te$PH)
table(predict = svmpred , thuth = te$PH)
table(tune.out$best.model$fitted,tr$PH)

#NaiveBayes con los datos ordenados

nb = naiveBayes(as.factor(tr$PH)~., data = tr)
pred = predict(nb,te)
table(predict = pred, truth=te$PH)
mean(pred==te$PH)

#KNN con la base de datos ordenada

library("class")

set.seed(1)
knn.pred = knn(tr,te,tr$PH,k=1)
table(knn.pred==te$PH)
mean(knn.pred==te$PH)

set.seed(1)
knn.pred = knn(tr,te,tr$PH,k=5)
table(knn.pred==te$PH)
mean(knn.pred==te$PH)

set.seed(1)
knn.pred = knn(tr,te,tr$PH,k=10)
table(knn.pred==te$PH)
mean(knn.pred==te$PH)

set.seed(1)
knn.pred = knn(tr,te,tr$PH,k=15)
table(knn.pred==te$PH)
mean(knn.pred==te$PH)

