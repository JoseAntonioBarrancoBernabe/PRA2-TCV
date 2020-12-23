# PRA2-Limpieza y anÃ¡lisis de datos. Tipologia y ciclo de vida de los datos.Curso 2020-2021,semestre 1
# JABB-Jose Antonio Barranco Bernabe, DNI : 52096903V.

# 1. Descripcion del dataset.

# fichero Diabetes tomado de https://www.kaggle.com/uciml/pima-indians-diabetes-database 
# El conjunto de datos dispone de 8 medidas/columnas siendo estas las variables predictoras, 
# junto con 1 variable de clase u objetivo, para un total de 768 pacientes/filas,las medidas son:
  
# Pregnancies: N.º de veces embarazada. 
# Glucose: Concentración de glucosa en plasma a 2 horas en una prueba de tolerancia a la glucosa oral.
# BloodPressure/Presión sanguínea: Presión arterial 
# SkinThickness/Pliegue cutáneo: Espesor del pliegue cutáneo triceps.
# Insulin: Insulina suero-sérica de 2 horas.
# BMI/IMC: Índice de masa corporal.
# Pedigrí diabetes: Función del pedigrí de la diabetes.
# Age/Edad :Años.
# Outcome: Es resultado a predecir(0,1).  


# 2. Integracion y seleccion de los datos de interes a analizar. 

# realizamos la lectura del fichero descargado sobre el dataframe/df:mydiab
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
# Pasamos en primera instancia a factor dicha variable
mydiab$diabetes <- as.factor(mydiab$diabetes)
levels(mydiab$diabetes) <- c("No","Yes")
# lo comprobamos (500=NO, 268=Yes)
str(mydiab) 

# Seleccion--> Si quisieramos solo de algunas variables(si interesase):
# 1-Expresando cuales tomamos explicitamente 
mydiab-selecc <- mydiab[, c(2,3,5,6,7,8,9)]
# 2-O bien, en este caso,seria lo mismo,pero quitando columnas 1 y 4 :
mydiab-selecc <- mydiab[, -c(1,4)]

# Filtrado de filas, 
# por ejemplo,personas menores de 50 aÃ±os sin tomar aquellos de los que no tuviesemos la edad.
mydiab[mydiab$Age < 50 & !is.na(mydiab$Age),]


# 3. Limpieza de los datos. 

# Comprobacion respecto a Datos no disponibles :
colSums(is.na(mydiab))
# igual mediante funcion colMeans,muestra que proporcion de datos no disponibles tenemos por columna.
sort(colMeans(is.na(mydiab)), decreasing = TRUE)
colSums(mydiab=="")

# Imputo NA Not Available si viene a 0 el valor de Glucose, en primera instancia.
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
summary(mydiab$BMI)


# 4. Analisis de los datos. 
# 4.1.Grupos de datos a analizar/comparar.

# guardamos en variable el num de filas por si se precisa 
filas=dim(mydiab)[1]
filas

# Procedemos a intentar ver relacion/es mediante visualizacion como metodo.

#install.packages("dplyr") 
#library(dplyr)
library(ggplot2)

# Visualizamos la relacion entre las variables "edad" y "diabetes"
ggplot(data=mydiab[,],aes(x=Age,fill=diabetes))+geom_bar()
# parece haber mas Yes que No a partir de 40 de edad, aproximadamente.

#Visualizamos la relacion entre las variables "BMI" y "diabetes":
ggplot(data=mydiab[,],aes(x=BMI,fill=diabetes))+geom_bar()
# Va creciendo el num de Yes a partir de 33, aproximadamente.

#Visualizamos la relacion entre las variables "Pregnancies" y "diabetes":
ggplot(data=mydiab[,],aes(x=Pregnancies,fill=diabetes))+geom_bar()
# A partir de 6(el Q3 para la variable) hay mas Yes que No

#Visualizamos la relacion entre las variables "Glucose" y "diabetes":
ggplot(data=mydiab[,],aes(x=Glucose,fill=diabetes))+geom_bar()
# A partir de 125(entre media y Q3 para la variable) es claro que hay mas Yes que No

#Visualizamos la relacion entre las variables "Insulin" y "diabetes":
ggplot(data=mydiab[,],aes(x=Insulin,fill=diabetes))+geom_bar()

#Visualizamos la relacion entre las variables "Pedigree" y "diabetes":
ggplot(data=mydiab[,],aes(x=DiabetesPedigreeFunction,fill=diabetes))+geom_bar()

#Visualizamos la relacion entre las variables "Pedigree" y "diabetes" segun edad
ggplot(data=mydiab[1:filas,],aes(x=DiabetesPedigreeFunction,fill=diabetes))+geom_bar(position="fill")+facet_wrap(~Age)
# hay algunos casos claros con prevalencia de Yes

#Visualizamos la relacion entre las variables "BMI" y "diabetes" segun edad
ggplot(data=mydiab[1:filas,],aes(x=BMI,fill=diabetes))+geom_bar(position="fill")+facet_wrap(~Age)
# hay algunos casos claros con prevalencia de Yes

#Visualizamos la relacion entre las variables "Glucose" y "diabetes" segun IMC-BMI
ggplot(data=mydiab[1:filas,],aes(x=BMI,fill=diabetes))+geom_bar(position="fill")+facet_wrap(~Glucose)
# hay casos con prevalencia de Yes a partir de IMC-BMI entre 25 y 35

## AQUI LO VUELVO A DEJAR A CERO LA GLUCOSA
mydiab$Glucose[is.na(mydiab$Glucose)] <- 0
summary(mydiab$Glucose)

# Intento ver cuantos clusteres son los apropiados para establecer grupos de clasificación

#Metodo 1 Silueta
library(cluster)
d <- daisy(mydiab[,1:8]) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9))
{
        fit           <- kmeans(mydiab[,1:8], i)
        y_cluster     <- fit$cluster
        sk            <- silhouette(y_cluster, d)
        resultados[i] <- mean(sk[,3])
}
plot(2:9,resultados[2:9],type="o",col="blue",pch=0,xlab="Numero de clusters",ylab="Silueta")

# Metodo 2 tot.withinss
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9))
{
        fit           <- kmeans(mydiab[,1:8], i)
        resultados[i] <- fit$tot.withinss
}
plot(2:9,resultados[2:9],type="o",col="blue",pch=0,xlab="Numero de clusters",ylab="tot.tot.withinss")

#aprox 4 clusteres es donde la curva se estabiliza

fit2       <- kmeans(mydiab[,1:8], 2)
y_cluster2 <- fit2$cluster
y_cluster2
clusplot(mydiab, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#con estos dos se explicaría el 45.61% de la variabilidad

# Uso de tablas de contingencia
#table(y_cluster2,mydiab[,1:8]) da error quizas se podia haber dejado a 0 y 1 en lugar de
#convertirla a factor la diabetes
tabla4clusters <- kmeans(mydiab[,1:8], 4, nstart=50)
table(tabla4clusters$cluster,mydiab$diabetes)
# en los grupos 2 y 3 hay prevalencia de Yes sobre No
  
# edad-diabetes
tabla_AD <- table(mydiab$Age, mydiab$diabetes)
prop.table(tabla_AD, margin = 1)  # de 43 a 55 de edad hay mas Yes
plot(mydiab[c(8)], col=tabla4clusters$cluster)

# IMC-diabetes
tabla_BD <- table(mydiab$BMI, mydiab$diabetes)
tabla_BD
prop.table(tabla_BD, margin = 1)
plot(mydiab[c(6)], col=tabla4clusters$cluster)

# Glucosa-diabetes
tabla_GD <- table(mydiab$Glucose, mydiab$diabetes)
tabla_GD
prop.table(tabla_GD, margin = 1)

# Insulin-diabetes
tabla_ID <- table(mydiab$Insulin, mydiab$diabetes)
tabla_ID
prop.table(tabla_ID, margin = 1)
###########################################################################
# 4.2 Comprobación de la normalidad y homogeneidad de la varianza.

# Test de Shapiro-Wilk a las variables, para todas ellas es menor que 0.05
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
#alternative hypothesis: two-sided
ks.test(mydiab$Glucose, pnorm, mean(mydiab$Glucose), sd(mydiab$Glucose))
#D = 0.063998, p-value = 0.003705      
ks.test(mydiab$BloodPressure, pnorm, mean(mydiab$BloodPressure), sd(mydiab$BloodPressure))
# D = 0.16147, p-value < 2.2e-16
ks.test(mydiab$SkinThickness, pnorm, mean(mydiab$SkinThickness), sd(mydiab$SkinThickness))
#D = 0.19659, p-value < 2.2e-1
ks.test(mydiab$Insulin, pnorm, mean(mydiab$Insulin), sd(mydiab$Insulin))
#D = 0.24433, p-value < 2.2e-16
ks.test(mydiab$BMI, pnorm, mean(mydiab$BMI), sd(mydiab$BMI))
#D = 0.038759, p-value = 0.1988   FALSE solo BMI es mayor
ks.test(mydiab$DiabetesPedigreeFunction, pnorm, mean(mydiab$DiabetesPedigreeFunction), sd(mydiab$DiabetesPedigreeFunction))
#D = 0.12397, p-value = 1.121e-10
ks.test(mydiab$Age, pnorm, mean(mydiab$Age), sd(mydiab$Age))
#D = 0.15643, p-value < 2.2e-16

## segun los resultados de ambos Test, podemos decir que los datos no siguen una distribucion normal.

## como no es normal aplico el test de Fligner-Killeen

fligner.test(Pregnancies ~ diabetes, data = mydiab)              # p-value 1.062e-07 < varianzas diferentes
fligner.test(Glucose ~ diabetes, data = mydiab)                  # p-value 6.276e-07 < varianzas diferentes 
fligner.test(BloodPressure ~ diabetes, data = mydiab)            # 0.3376            > varianzas homogeneas
fligner.test(SkinThickness ~ diabetes, data = mydiab)            # 1.01e-06          < varianzas diferentes
fligner.test(Insulin ~ diabetes, data = mydiab)                  # 0.3654            > varianzas homogeneas
fligner.test(BMI ~ diabetes, data = mydiab)                      # 0.135             > varianzas homogeneas
fligner.test(DiabetesPedigreeFunction ~ diabetes, data = mydiab) # 4.29e-06          < varianzas diferentes
fligner.test(Age ~ diabetes, data = mydiab)                      # 0.000347          < varianzas diferentes


###########################################################################
# 4.3 Aplicación de pruebas estadísticas para comparar los grupos de datos

# realizamos comprobacion por correlación
cor(mydiab)

# pagina 39 teoria

# Así, la correlación de Spearman aparece como una alternativa no paramétrica
# que mide el grado de dependencia entre dos variables. Este método no conlleva
# ninguna suposición sobre la distribución de los datos, aunque las variables a
# comparar deben medirse al menos en una escala ordinal.

## Ver si Pearson o Spearman

cor.test(mydiab$Pregnancies,mydiab$diabetes, method="pearson")       # pearson --0.2218982

cor.test(mydiab$Glucose,mydiab$diabetes, method="pearson")           # pearson --0.4665814 alta
cor.test(mydiab$Glucose,mydiab$diabetes, method="spearman")          # spearman--0.4757763 alta también

cor.test(mydiab$BloodPressure,mydiab$diabetes, method="spearman")    # spearman--0.1429207

cor.test(mydiab$SkinThickness,mydiab$diabetes, method="pearson")     # pearson-- 0.07475223

cor.test(mydiab$Insulin,mydiab$diabetes, method="spearman")          # spearman--0.06647165
cor.test(mydiab$BMI,mydiab$diabetes, method="spearman")              # spearman--0.3073382 alta

cor.test(mydiab$DiabetesPedigreeFunction,mydiab$diabetes, method="pearson") # --0.1738441
cor.test(mydiab$Age,mydiab$diabetes, method="pearson")                      # pearson--0.238356


###Podríamos también realizar la aplicacion de wilcox.test 
## no acabado
wilcox.test(diabetes ~ Glucose, data = mydiab, subset = Age %in% c(35, 55))
wilcox.test(diabetes ~ BMI, data = mydiab)
wilcox.test(mydiabNormal[,'Outcome'], mydiabNormal[,'BMI'], paired = T)
wilcox.test(mydiabNormal[,'Outcome'], mydiabNormal[,'BMI'], paired = T)
#wilcox.test(numeric_var ~ two_level_group_var)

kruskal.test(diabetes ~ Age, data = mydiab)
kruskal.test(Outcome ~ Age, data = mydiabNormal)

kruskal.test(diabetes ~ Glucose, data = mydiab)
kruskal.test(diabetes ~ BloodPressure, data = mydiab)
# https://bookdown.org/dietrichson/metodos-cuantitativos/wilcoxon-signed-rank-test.html


##regresion

plot(mydiab)






# https://www.kaggle.com/mirichoi0218/classification-diabetes-or-not-with-basic-12ml


