# PRA2-Limpieza y anÃ¡lisis de datos. Tipologia y ciclo de vida de los datos.Curso 2020-2021,semestre 1
# JABB-Jose Antonio Barranco Bernabe, DNI : 52096903V.

# 1. Descripcion del dataset.

# fichero Diabetes tomado de https://www.kaggle.com/uciml/pima-indians-diabetes-database 
# El conjunto de datos dispone de 8 medidas/columnas siendo estas las variables predictoras, 
# junto con una variable de clase u objetivo, para un total de 768 pacientes/filas,
# las medidas son:
  
# Pregnancies: N.º de veces embarazada. 
# Glucose: Concentración de glucosa en plasma a 2 horas en una prueba de tolerancia a la glucosa oral.
# BloodPressure/Presión sanguínea: Presión arterial 
# SkinThickness/Pliegue cutáneo: Espesor del pliegue cutáneo triceps.
# Insulin: Insulina suero-sérica de 2 horas.
# BMI/IMC: Índice de masa corporal.
# Pedigrí diabetes: Función del pedigrí de la diabetes.
# Age/Edad :Años.
# Outcome: Es resultado a predecir(0,1). 1 = Si, 0=No 


# 2. Integracion y seleccion de los datos de interes a analizar. 

# Solo tenemos un fichero--> pasamos a la lectura del fichero descargado sobre el dataframe/df:mydiab
mydiab <- read.csv("../data/Seccion 6/diabetes.csv", header = TRUE)
mydiab
# comprobamos tipo del objeto : "data.frame" 
class(mydiab)

# Realizamos una primera inspeccion de cabeceras y datos 
View(mydiab) 
# Estructura,tipos de las variables con str 
str(mydiab) 

# Renombramos la variable objetivo(Outcome) a diabetes
colnames(mydiab)[9] <- "diabetes"

# 3. Limpieza de los datos. 

# Comprobacion respecto a Datos no disponibles :
colSums(is.na(mydiab))
# igual mediante funcion colMeans,muestra que proporcion de datos no disponibles tenemos por columna.
sort(colMeans(is.na(mydiab)), decreasing = TRUE)
colSums(mydiab=="")

# Imputo NA Not Available si viene a 0 el valor de Glucose, si bien en primera instancia no lo realizo
# por problemas en analisis posteriores.
mydiab$Glucose[mydiab$Glucose == 0] <- NA
# hay 5 casos, o se visualizan con View
View(mydiab)
# u obteniendo sus estadisticas con summary()
summary(mydiab$Glucose)

# Identificacion y tratamiento de valores extremos

# para Embarazos:
boxplot(mydiab$Pregnancies,
        main = "Embarazos",
        boxwex = 0.5)
boxplot.stats(mydiab$Pregnancies)$out

# En este caso se verían para Embarazos pero en funcion de la Edad/Age
boxplot(Pregnancies ~ Age,
        data = mydiab,
        main = "Embarazos",
        boxwex = 0.5)$out

# variable Glucose
boxplot(mydiab$Glucose,
        main = "Glucosa,prueba de tolerancia",
        ylab = "Glucosa",
        col = "brown",
        border = "black"
)
boxplot.stats(mydiab$Glucose)$out

# variable Insuline
boxplot(mydiab$Insulin,
        main = "Insulina",
        ylab = "Insuline",
        col = "green",
        border = "red"
)
boxplot.stats(mydiab$Insulin)$out
summary(mydiab$Insulin)


# variable DiabetesPedigreeFunction
boxplot(mydiab$DiabetesPedigreeFunction,
        main = "Pedigree",
        ylab = "DiabetesPedigree",
        col = "red",
        border = "green"
)
boxplot.stats(mydiab$DiabetesPedigreeFunction)$out
## ojo posible transformación-imputacion a realizar en variable Pedigree
mydiab$DiabetesPedigreeFunction[mydiab$DiabetesPedigreeFunction >= 40] <- mydiab$DiabetesPedigreeFunction/1000
summary(mydiab$DiabetesPedigreeFunction)
View(mydiab$DiabetesPedigreeFunction)

## POR SI REALIZO LO MISMO CON GLUCOSE 
# Respecto de esta variable reemplazamos los valores >400( por no tener sentido), con su mediana :
#summary(mydiab$Insulin)
#mydiab$BMI[mydiab$Insulin >= 400] <- median(mydiab$Insulin)

# variable BMI-IMC
boxplot(mydiab$BMI,
        main = "BMI o Indice de masa corporal",
        ylab = "IMC",
        col = "green",
        border = "red"
)
boxplot.stats(mydiab$BMI)$out
mtext(c(boxplot.stats(mydiab$BMI)$out))

# Respecto de esta variable BMI-IMC reemplazamos los valores 0( por no tener sentido), con su mediana :
summary(mydiab$BMI)
mydiab$BMI[mydiab$BMI == 0] <- median(mydiab$BMI)
# Comprobamos :
View(mydiab$BMI)
summary(mydiab$BMI)


# 4. Analisis de los datos. 
# 4.1.Grupos de datos a analizar/comparar.

# guardamos en variable el num de filas por si se precisa en el análisis 
filas=dim(mydiab)[1]
filas

# Procedemos a intentar ver relacion/es mediante visualizacion como metodo.

#install.packages("dplyr") 
#library(dplyr)
library(ggplot2)

# Visualizamos la relacion entre las variables "edad" y "diabetes"
ggplot(data=mydiab[,],aes(x=Age,fill=saldiabetes))+geom_bar()
# parece haber mas Yes que No a partir de 40 de edad, aproximadamente.

#Visualizamos la relacion entre las variables "BMI" y "diabetes":
ggplot(data=mydiab[,],aes(x=BMI,fill=saldiabetes))+geom_bar()
# Va creciendo el num de Yes a partir de 33, aproximadamente.

#Visualizamos la relacion entre las variables "Pregnancies" y "diabetes":
ggplot(data=mydiab[,],aes(x=Pregnancies,fill=saldiabetes))+geom_bar()
summary(mydiab$Pregnancies)
# A partir de 7(el Q3 es 6 para la variable) hay mas Yes que No

#Visualizamos la relacion entre las variables "Glucose" y "diabetes":
ggplot(data=mydiab[,],aes(x=Glucose,fill=saldiabetes))+geom_bar()
# A partir de 125(entre media y Q3 para la variable) es claro que hay mas Yes que No

#Visualizamos la relacion entre las variables "Insulin" y "diabetes":
ggplot(data=mydiab[,],aes(x=Insulin,fill=saldiabetes))+geom_bar()

#Visualizamos la relacion entre las variables "Pedigree" y "diabetes":
ggplot(data=mydiab[,],aes(x=DiabetesPedigreeFunction,fill=saldiabetes))+geom_bar()

#Visualizamos la relacion entre las variables "Pedigree" y "Glucosa":
ggplot(data=mydiab[,],aes(x=DiabetesPedigreeFunction,fill=Glucose))+geom_bar()
ggplot(data=mydiab[1:filas,],aes(x=DiabetesPedigreeFunction,fill=saldiabetes))+geom_bar(position="fill")+facet_wrap(~Glucose)

#Visualizamos la relacion entre las variables "Pedigree" y "diabetes" segun edad
ggplot(data=mydiab[1:filas,],aes(x=DiabetesPedigreeFunction,fill=saldiabetes))+geom_bar(position="fill")+facet_wrap(~Age)
# hay algunos casos claros con prevalencia de Yes

#Visualizamos la relacion entre las variables "BMI" y "diabetes" segun edad
ggplot(data=mydiab[1:filas,],aes(x=BMI,fill=saldiabetes))+geom_bar(position="fill")+facet_wrap(~Age)
# hay algunos casos claros con prevalencia de Yes

#Visualizamos la relacion entre las variables "Glucose" y "diabetes" segun IMC-BMI
ggplot(data=mydiab[1:filas,],aes(x=BMI,fill=saldiabetes))+geom_bar(position="fill")+facet_wrap(~Glucose)
# hay casos con prevalencia de Yes a partir de IMC-BMI entre 25 y 35
summary(mydiab$Glucose)
## Aqui volveriamos a dejar a 0 Glucose si procede


#####################################################################################################
# 4.2 Comprobación de la normalidad y homogeneidad de la varianza.

set.seed(2020)
# Test de Shapiro-Wilk a las variables. Ppara todas ellas es menor que 0.05
shapiro.test(mydiab$Pregnancies)
shapiro.test(mydiab$Glucose) 
shapiro.test(mydiab$BloodPressure) 
shapiro.test(mydiab$SkinThickness)
shapiro.test(mydiab$Insulin)
shapiro.test(mydiab$BMI)
shapiro.test(mydiab$DiabetesPedigreeFunction)
shapiro.test(mydiab$Age)

# Test de Kolmogorov-Smirnow
ks.test(mydiab$Pregnancies, pnorm, mean(mydiab$Pregnancies), sd(mydiab$Pregnancies))
#D = 0.16243, p-value < 2.2e-16
ks.test(mydiab$Glucose, pnorm, mean(mydiab$Glucose), sd(mydiab$Glucose))
#D = 0.063998, p-value = 0.003705      
ks.test(mydiab$BloodPressure, pnorm, mean(mydiab$BloodPressure), sd(mydiab$BloodPressure))
# D = 0.16147, p-value < 2.2e-16
ks.test(mydiab$SkinThickness, pnorm, mean(mydiab$SkinThickness), sd(mydiab$SkinThickness))
#D = 0.19659, p-value < 2.2e-1
ks.test(mydiab$Insulin, pnorm, mean(mydiab$Insulin), sd(mydiab$Insulin))
#D = 0.24433, p-value < 2.2e-16
ks.test(mydiab$BMI, pnorm, mean(mydiab$BMI), sd(mydiab$BMI))
#D = 0.038759, p-value = 0.1988   FALSE solo BMI es mayor, comprobamos-ratificamos con q-q plot :
qqnorm(mydiab$BMI);qqline(mydiab$BMI, col = 2)
ks.test(mydiab$DiabetesPedigreeFunction, pnorm, mean(mydiab$DiabetesPedigreeFunction), sd(mydiab$DiabetesPedigreeFunction))
#D = 0.12397, p-value = 1.121e-10
ks.test(mydiab$Age, pnorm, mean(mydiab$Age), sd(mydiab$Age))
#D = 0.15643, p-value < 2.2e-16

## segun los resultados de ambos Test, podemos decir que los datos no siguen una distribucion normal.

## como no se tenía Normalidad,aplico test no parametrico  de Fligner-Killeen

fligner.test(Pregnancies ~ diabetes, data = mydiab)              # p-value 1.062e-07 < varianzas diferentes
fligner.test(Glucose ~ diabetes, data = mydiab)                  # p-value 6.276e-07 < varianzas diferentes 
fligner.test(BloodPressure ~ diabetes, data = mydiab)            # 0.3376            > varianzas homogeneas
fligner.test(SkinThickness ~ diabetes, data = mydiab)            # 1.01e-06          < varianzas diferentes
fligner.test(Insulin ~ diabetes, data = mydiab)                  # 0.3654            > varianzas homogeneas
fligner.test(BMI ~ diabetes, data = mydiab)                      # 0.135             > varianzas homogeneas
fligner.test(DiabetesPedigreeFunction ~ diabetes, data = mydiab) # 4.356e-06         < varianzas diferentes
fligner.test(Age ~ diabetes, data = mydiab)                      # 0.000347          < varianzas diferentes


########################################################################################################
# 4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos

## Contraste de hipotesis

## Podríamos realizar la aplicacion de wilcox.test al no tener normalidad y homocedasticidad.
wilcox.test(Pregnancies ~ diabetes, data = mydiab)
wilcox.test(Glucose ~ diabetes, data = mydiab)
wilcox.test(BloodPressure ~ diabetes, data = mydiab)
wilcox.test(SkinThickness ~ diabetes, data = mydiab)
wilcox.test(Insulin ~ diabetes, data = mydiab)        # > 0.05
wilcox.test(DiabetesPedigreeFunction ~ diabetes, data = mydiab)
wilcox.test(Age ~ diabetes, data = mydiab)
#dado que BMI tendría distribucion normal y varianzas homgeneas utilizamos la prueba t de Student
t.test(BMI ~ diabetes, data = mydiab)


## Comprobacion por correlación
cor(mydiab)

# Así, la correlación de Spearman aparece como una alternativa no paramétrica
# que mide el grado de dependencia entre dos variables. Este método no conlleva
# ninguna suposición sobre la distribución de los datos, aunque las variables a
# comparar deben medirse al menos en una escala ordinal.

cor.test(mydiab$Pregnancies,mydiab$diabetes, method="spearman")   # spearman 0.1986887 
cor.test(mydiab$Glucose,mydiab$diabetes, method="spearman")       # spearman 0.4757763 aprox alta 
cor.test(mydiab$BloodPressure,mydiab$diabetes, method="spearman") # spearman 0.1429207
cor.test(mydiab$SkinThickness,mydiab$diabetes, method="spearman") # spearman 0.08972776 
cor.test(mydiab$Insulin,mydiab$diabetes, method="spearman")       # spearman 0.06647165
cor.test(mydiab$DiabetesPedigreeFunction,mydiab$diabetes, method="spearman") # 0.1753535
cor.test(mydiab$Age,mydiab$diabetes, method="spearman")                      # 0.3090403 aprox media-alta

#aplico Pearson por seguir Normalidad y homogenidad varianzas
cor.test(mydiab$BMI,mydiab$diabetes, method="pearson")            # pearson   0.312249 aprox media-alta


## Realizamos regresion para tratar de aproximar relacion de dependencia lineal entre variable dependiente 
## y una (o una serie) de var.independientes
set.seed(2020)
# diabetes segun Glucose
modelo_simple <- lm(data = mydiab,formula = diabetes ~ Glucose)
#names(modelo_simple)
mean(mydiab$diabetes)  # vemos media de diabetes( realmente son solo valores 0 o 1)
summary(modelo_simple)

#Incluimos la variable Insuline a la Glucose
modelo_simple2 <- lm(data = mydiab,formula = diabetes ~ Glucose+Insulin)
summary(modelo_simple2)

#Incluimos la variable BMI a la Glucose
modelo_simple3 <- lm(data = mydiab,formula = diabetes ~ Glucose+BMI)
summary(modelo_simple3)

#Incluimos la variable Edad junto con BMI y la Glucose
modelo_simple4 <- lm(data = mydiab,formula = diabetes ~ Glucose+BMI+Age)
summary(modelo_simple4)

#Incluimos la variable PedigreeDiabetes junto con BMI, Edad y la Glucose
modelo_simple5 <- lm(data = mydiab,formula = diabetes ~ Glucose+BMI+Age+DiabetesPedigreeFunction)
summary(modelo_simple5)
# se incluye Insulin
modelo_simple6 <- lm(data = mydiab,formula = diabetes ~ Glucose+Insulin+BMI+Age+DiabetesPedigreeFunction)
summary(modelo_simple6)

# Ahora realizamos un modelo multiple, de la variable dependiente respecto del resto:
modelo_multiple <- lm(formula = diabetes ~ ., data = mydiab)
summary(modelo_multiple)


# Realizamos predicciones --PREDICT
newpacient <- data.frame(
  Glucose = 110,
  BMI = 25,
  Age = 40
)
predict(modelo_simple4,newpacient)

#Aumentamos valores para las mismas variables
newpacient50 <- data.frame(
  Glucose = 145,
  BMI = 34,
  Age = 51
)
predict(modelo_simple4,newpacient50)

# paciente de iguales datos añadiendo Pedigree
newpacient51 <- data.frame(
  Glucose = 145,
  BMI = 34,
  Age = 51,
  DiabetesPedigreeFunction = mean(mydiab$DiabetesPedigreeFunction)
)
predict(modelo_simple5,newpacient51)

# añadiendo Insulin
newpacient51i <- data.frame(
  Glucose = 145,
  Insulin = 75,
  BMI = 34,
  Age = 51,
  DiabetesPedigreeFunction = mean(mydiab$DiabetesPedigreeFunction)
)
predict(modelo_simple6,newpacient51i)

# la prediccion multiple

newpacient51total <- data.frame(
  Pregnancies = 3,
  Glucose = 145,
  BloodPressure = 68,
  SkinThickness = 20,
  Insulin = 70,
  BMI = 34,
  Age = 51,
  DiabetesPedigreeFunction = mean(mydiab$DiabetesPedigreeFunction)
)
predict(modelo_multiple,newpacient51total)



##############################################################################################
## 5.Representación de los resultados a partir de tablas y gráficas.

# pairplot
cols <- c('Glucose', 'Insulin', 'BMI', 'DiabetesPedigreeFunction', 'Age','diabetes')
pairs(mydiab[,cols], pch = 21)

# Finalmente, nos interesará ver qué conjuntos de variables estan relacionados entre sí. Para eso, 
# usaremos técnicas estadísticas de análisis multivariante.

# Una de las herramientas más útiles es calcular la matriz de correlación entre las variables. 
install.packages("corrplot")
library(corrplot)
corr.res<-cor(mydiab)
corrplot(corr.res,method="circle")
corrplot.mixed(corr.res,upper="circle",number.cex=.8,tl.cex=.9)


## Mediante mapa de calor, relacion entre variables:
library(ggplot2)
library(reshape2)

heatmapall <- mydiab[, c('Pregnancies','Glucose','BloodPressure','SkinThickness', 
                         'Insulin', 'BMI', 'DiabetesPedigreeFunction', 'Age')]

qplot(x=Var1, y=Var2, data=melt(cor(heatmapall, use="p")), fill=value, geom="tile") +
  theme(axis.text.x = element_text(angle = 90)) +
  coord_fixed()


# mapa de calor 
trainscaled <- as.matrix(scale(cor(heatmapall, use="p")))
heatmap(trainscaled, Colv=F, scale='none')

#########################
## tablas

# tabla respecto a Edad y diabetes
tabla_AD <- table(mydiab$Age, mydiab$diabetes)
prop.table(tabla_AD, margin = 1)  # de 43 a 55 de edad hay mas Yes

# BMI o IMC-diabetes
tabla_BD <- table(mydiab$BMI, mydiab$diabetes)
prop.table(tabla_BD, margin = 1)

# Glucosa-diabetes
tabla_GD <- table(mydiab$Glucose, mydiab$diabetes)
prop.table(tabla_GD, margin = 1)


# Pasamos aplicar estimación a partir de datos de entrenamiento, en este caso mediante
# método supervisado, clasificacion.

# Mediante trainControl() se puede especificar las caracteristicas del training y test, en este caso
# validacion cruzada de 10 folds(mutuamente exclusivos y de tamaños similares)
# La exactitud o accuracy es el nº total de clasificaciones correctas en k=10 iteraciones/nº total muestras 
set.seed(2020)
library(caret)
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)
metric <- "Accuracy"

# creamos un primer modelo de arbol
# Usamos la función rpart para entrenar nuestro modelo.Esta función nos pide una formula para especificar 
# la variable objetivo de la clasificación.
# La formula que usaremos es tipo ~ ., la cual expresa que intentaremos clasificar tipo, usando a todas las demás 
# variables como predictoras
model.cart <- train(as.factor(diabetes)~., data=mydiab, method="rpart", metric=metric, trControl=train_control)

# Medida del rendimiento.
# Mediante la matriz de confusion
confusionMatrix(model.cart)
print(model.cart)

# Graficamente respecto a accuracy
plot(model.cart)

# Graficamente importancia de las variables del objeto de entrenamiento
ctreeVarImp = varImp(model.cart)
plot(ctreeVarImp)

###########################################################################
# Finalización escribiendo fichero de datos

# Pasamos a factor dicha variable como otra columna
mydiab$saldiabetes <- as.factor(mydiab$diabetes)
levels(mydiab$saldiabetes) <- c("No","Yes")
# lo comprobamos (500=NO, 268=Yes)
str(mydiab) 
View(mydiab)

write.csv(mydiab, file = "../data/Seccion 6/diabetes_salida.csv")



