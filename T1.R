library(ggplot2)
library(tidyr)
library(dplyr)
library(readxl)
library(corrgram)
library(glmnet)


####### PARTE 1 ########

#limpieza de la base de datos
BD_T1 <-read_excel("C:/Users/JavieraValentina/Desktop/T1mkt/BD_T1.xlsx")
#Limpieza de datos
str(BD_T1)
BD_T1_mod = BD_T1[1:58]
BD_T1_mod = BD_T1_mod[, -(54:57) ]
BD_T1_mod = BD_T1_mod[, -(31:50) ]

str(BD_T1_mod)
#reemplazo de los NA
for( i in 2:34){
  for(j in 1:length(BD_T1_mod[,i])){
    if(BD_T1_mod[j,i]=="NA") BD_T1_mod[j,i]<- NA
  }
}
#variables numericas
args(lapply)
BD_T1_mod[,5:24] = lapply(BD_T1_mod[,5:24],as.numeric)
BD_T1_mod[,29:32] = lapply(BD_T1_mod[,29:32],as.numeric)


BD_T1_mod <- na.omit(BD_T1_mod)


str(BD_T1_mod)
#eliminacion de outliers
bd<-BD_T1_mod

for(i in 5:24){
bd<- bd[bd[,i]<6&bd[,i]>0,]
}
for(i in 29:32){
  bd<- bd[bd[,i]<6&bd[,i]>0,]
}
bd<- bd[bd[,31]<3&bd[,31]>0,]
bd<- bd[bd[,23]<3&bd[,23]>0,]
bd<- bd[bd[,23]<5&bd[,23]>0,]

#matriz de correlacion

criterio_numerico = which(sapply(bd, is.numeric))
variables_numericas = bd[, criterio_numerico]
matriz_correlacion = cor(variables_numericas)
corrgram(variables_numericas, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Matriz de Correlación")


#histogramas por proporcion según matriz de correlacion
#1 grafico de la experiencia del consumidor
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$EXPERIENCIA, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿La experiencia de compra fue agradable?") + ylab("Proporción") + ggtitle("Experiencia del consumidor")
#2 grafico de disponibilidad de productos
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$DISPONIBILIDAD, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿El supermercado siempre tuvo los productos que buscaba") + ggtitle("Disponibilidad de productos")
#3 grafico de disposicion de cajeros
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$DISP_CAJEROS, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Los cajeros estuvieron dispuestos a ayudarle?") + ylab("Proporción") + ggtitle("Disposición de cajeros")
#4 grafico de disposicion de vendedores
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$DISP_VENDEDORES, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Los vendedores estuvieron dispuestos a ayudarle?") + ylab("Proporción") + ggtitle("Disposición vendedores")
#resto de los graficos correlacionados
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$CONFIANZA_PERSONAL, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Volvería a comprar en este supermercado?") + ylab("Frecuencia") + ggtitle("Confianza")
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$SEÑA_GONDOLAS, y= ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Volvería a comprar en este supermercado?") + ylab("Frecuencia") + ggtitle("Señas")
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$LIMPIEZA, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Volvería a comprar en este supermercado?") + ylab("Frecuencia") + ggtitle("Limpieza")
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$VARIEDAD, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Volvería a comprar en este supermercado?") + ylab("Frecuencia") + ggtitle("Variedad")
ggplot(data = bd) + geom_bar(mapping = aes(x = bd$CALIDAD_PROD, y = ..prop.. ,fill=bd$ID_CORTO),position="dodge") + xlab("¿Volvería a comprar en este supermercado?") + ylab("Frecuencia") + ggtitle("Productos")


####### PARTE 2 ########

#regresion de satisfaccion general P18, satisfaccion_total
reg_S_1=lm(data= bd, formula= SATISFACCION_TOTAL~ EXPERIENCIA + DISPONIBILIDAD + DISP_CAJEROS + DISP_VENDEDORES )
summary(reg_S_1)

reg_S_2=lm(data= bd, formula= SATISFACCION_TOTAL~ EXPERIENCIA + DISPONIBILIDAD + DISP_CAJEROS + CALIDAD_PROD + DISP_VENDEDORES + LIMPIEZA + VARIEDAD)
summary(reg_S_2)

#regresion de recomendacion

reg_R_1=lm(data= bd, formula= RECOMENDACION~ EXPERIENCIA + LIMPIEZA + VARIEDAD)
summary(reg_R_1)


#regresion con modelo automatico LASSO

#satisfaccion total
x_lasso1=as.matrix(bd[,c(5:21,23,24,29:32)])
y_lasso1=as.matrix(bd$SATISFACCION_TOTAL)
cvlasso1 = cv.glmnet(x_lasso1,y_lasso1)
summary(cvlasso1)
coef(cvlasso1, s = "lambda.1se")
cvlasso1$lambda.min

#satisfaccion total
x_lasso2=as.matrix(bd[,c(5:24,29,31,32)])
y_lasso2=as.matrix(bd$RECOMENDACION)
cvlasso2 = cv.glmnet(x_lasso2,y_lasso2)
summary(cvlasso2)
coef(cvlasso2, s = "lambda.1se")
cvlasso2$lambda.min

