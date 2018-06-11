install.packages('C50');
install.packages('readxl');
install.packages("VIM");
install.packages("car")

library(readxl);
library (rpart.plot);
library (rpart);
library(car)

# Lectura de datos desde el archivo
df <- read.csv("C:/Users/Usuario-03/train.csv");

# ====CARACTERISTICAS DE LOS DATOS ===
# Dimensión del dataset
dim (df);

# Tipo de dato asignado a cada campo
sapply(df, function(x) class(x))

# Resumen de datos
summary(df);

# Eliminación de campos
df <- df[, -(7:11)];  # campos: SibSp,Parch,Ticket,Fare,Cabin
df <- df[, -(4)];     # campo Name
df <- df[, -(1)];     # campo PassengerId
dfaux=df;

# Datos Nulos
sapply(df, function(x) sum(is.na(x)));
df$Age[is.na(df$Age)];
df$Embarked[is.na(df$Embarked)];

# Inputación de valore nulos
suppressWarnings(suppressMessages(library(VIM)));
df$Age<-kNN(df)$Age;
df$Embarked<-kNN(df)$Embarked;

# Valores extremos
boxplot.stats(df$Survived)$out;
boxplot.stats(df$Pclass)$out;
boxplot.stats(df$Sex)$out;
boxplot.stats(df$Age)$out;
boxplot.stats(df$Embarked)$out;

# Gráfica de las Frecuencias de cada una de las variables del dataset
nf<-layout(matrix(c(1,2,3,4,5,5), 2, 3, byrow=TRUE),respect=TRUE);
barplot(prop.table(table(df$Sex)),ylim=c(0,0.7), main="(a) Sexo");
barplot(prop.table(table(df$Pclass)),ylim=c(0,0.7), main="(b) Clase");
barplot(prop.table(table(df$Survived)),ylim=c(0,0.7), main="(c) Sobrevivencia");
barplot(prop.table(table(df$Embarked)),ylim=c(0,1), main="(d) Embarque");
barplot(prop.table(table(df$Age)),ylim=c(0,0.05), main="(e) Edades");

# Comparación de varianzas
var <- fligner.test(df);
var;

# Se estudia el dataset
total_clase=aggregate(df$Pclass, by=list(clase=df$Pclass), FUN=function(x){NROW(x)});
total_sexo=aggregate(df$Pclass, by=list(sex=df$Sex, clase=df$Pclass), FUN=function(x){NROW(x)});
total_edad=aggregate(df$Pclass, by=list(edad=df$Age, clase=df$Pclass), FUN=function(x){NROW(x)});

m_Hombres<-subset(total_sexo, sex=='male');
m_Mujeres<-subset(total_sexo, sex=='female');

m_Hombres$porcentaje <- round(prop.table(m_Hombres$x),4)*100;
m_Mujeres$porcentaje <- round(prop.table(m_Mujeres$x),4)*100;

m_c1<-subset(total_edad, clase=='1');
m_c2<-subset(total_edad, clase=='2');
m_c3<-subset(total_edad, clase=='3');

nf<-layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow=TRUE),respect=TRUE);
barplot(m_c1$x, main="(a) Frecuencia 1ra Clase");
barplot(m_c2$x, main="(b) Frecuencia 2da Clase");
barplot(m_c3$x, main="(c) Frecuencia 3ra Clase");

hist(m_c1$edad, main="(d) Histograma 1ra Clase");
hist(m_c2$edad, main="(e) Histograma 2da Clase");
hist(m_c3$edad, main="(f) Histograma 3ra Clase");

qqnorm(m_c1$edad, main="(a) D. Normal 1ra Clase");
qqline(m_c1$edad);
qqnorm(m_c2$edad, main="(b) D. Normal 2da Clase");
qqline(m_c2$edad);
qqnorm(m_c3$edad, main="(c) D. Normal 1ra Clase");
qqline(m_c3$edad);

plot(density(m_c1$edad), main="(d) Densidad 1ra Clase");
plot(density(m_c2$edad), main="(e) Densidad 2da Clase");
plot(density(m_c3$edad), main="(f) Densidad 3ra Clase");

lblsH <- paste(m_Hombres$clase, " Clase: ",m_Hombres$porcentaje,"%",sep=""); 
lblsM <- paste(m_Mujeres$clase, " Clase: ",m_Mujeres$porcentaje,"%",sep=""); 

#Se dibuja los cada uno de las subconjuntos obtenidos
nf<-layout(matrix(c(1,2), 1, 2, byrow=TRUE),respect=TRUE);
pie(m_Hombres$x,labels = lblsH, col=rainbow(length(lblsH)),main="% Hombres-Clase");
pie(m_Mujeres$x,labels = lblsM, col=rainbow(length(lblsM)),main="% Mujeres-Clase");     
       
# ANÁLISIS ESTADISTICO
#==CREACION DEL ARBOL==
nf<-layout(matrix(c(1), 1, byrow=TRUE),respect=TRUE);
datos2p=as.data.frame(df);
datosp=datos2p[,2:5];
# El campo SURVIVED se convierte a tipo factor
datosp$Survived=as.factor(datos2p$Survived);
# Creación el árbol
arbol<- rpart(Survived ~., data=datosp, method="class");
# Gráfico del árbol
rpart.plot(arbol, type=3, extra=101, fallen.leaves=T);
# Resumen del árbol
arbol;

#==============CREACION DE LAS CLUSTER
library('cluster');
library('fpc');
library('NbClust');
library('ggplot2');
library('factoextra');
library('stringr');
library('discretization');
nf<-layout(matrix(c(1), 1, byrow=TRUE),respect=TRUE);

dfc=df;
dfc <- dfc[, -5]; 
dfc$Sex=str_replace_all(dfc$Sex,"female","1");
dfc$Sex=str_replace_all(dfc$Sex,"male","2");
dfc$Sex=as.integer(dfc$Sex);
#niño=1, adolecente=2, joven=3, adulto=4, mayor=5
dfc$Age=cut(dfc$Age, breaks = c(0.40,12,18,30,50,81), 
    labels = c(1, 2, 3, 4, 5))
dfc$Age=as.integer(dfc$Age);
summary(dfc);

#Se selecciona el número adecuado de grupos
vec_distancias=kmeans(dfc,centers=1)$betweenss;
vec_distancias2=kmeans(dfc,centers=1)$tot.withinss;
for (i in 1:10) {
  vec_distancias[i]<-kmeans(dfc,centers=i)$betweenss;
  vec_distancias2[i]<-kmeans(dfc,centers=i)$tot.withinss;
}
plot (vec_distancias2, type='b',xlab='Número de cluster', 
      ylab='Suma de cuadrados inter-grupos',col = 'blue');
lines(vec_distancias, type='b',col = 'green');

#Se crea el cluster
cl <- kmeans(dfc, 4);
#Se presenta las caracteristicas del cluster
cl;

cl$iter #Número de iteraciones para generar los grupos
cl$size #Tamaño de los grupos
disst <- daisy(dfc); #distancia entre observaciones
#Distancia inter-grupos
cluster.stats(disst,cl$cluster)$average.between;
#Distancia entre-grupos
cluster.stats(disst,cl$cluster)$average.within;

#Se grafica el cluster generado
clusplot(disst, cl$cluster,diss = TRUE,color=TRUE,col.p="black");
#Se grafica el cluster con la combinacion de sus variables
plot(dfc, col = cl$cluster);
#Se grafica el cluster de la Edad VS la clase del pasajero
plot(dfc$Pclass, df$Age,col=cl$cluster,xlab='Pclass',ylab='Age');
#Se grafica el cluster de la Edad VS si sobrevive o no
plot(dfc$Age, df$Survived,col=cl$cluster,xlab='Age',ylab='Survived');

#Se almacena el archivo de salida
write.csv(dfc, "C:/Users/Usuario-03/Titanic_data_clean.csv");  
