#1. Cargo las librerías que considero necesarias.
library(dplyr)
library(tidyverse)
library(readr)
library(tidyverse)
library(MASS)
library(car)
library(lmtest)

#2. Cargo la base de datos nba.
nba <- read.csv("nba.csv")

#3. Hago una regresión con la variable "Salary" como variable dependiente, y omitiendo "Name" y
#   "tm" como regresores.
rnba <- lm(Salary~ NBA_Country + NBA_DraftNumber + Age + G + MP + PER + TS. + X3PAr + FTr + 
             ORB. + DRB. + TRB. + AST. + STL. + BLK. + TOV. + USG. + OWS + DWS + WS + 
             WS.48 + OBPM + DBPM + BPM + VORP, data = nba)
summary(rnba)

#4. Utilizo la funcion "stepAIC" para decidir que modelo usar.
stepAIC(rnba, direction = "both")

modelo_nba <- lm(formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
     X3PAr + ORB. + TRB. + USG. + WS + OBPM, data = nba)
summary(modelo_nba)

#5. Quiero crear un nuevo modelo que agrupo la variable "NBA_Country" por nacionales e internacionales, 
#   y que también agrupe la variable "NBA_DrafPick" en función de la ronda en la  que el jugador ha
#   sido seleccionado: Primera, segunda o undrafted.

nba <- mutate(nba,
              American = NBA_Country,
              Round_1 = NBA_DraftNumber,
              Round_2 = NBA_DraftNumber)

nba$American
nba$American[nba$American != "USA"] <- 0
nba$American[nba$American == "USA"] <- 1

nba$Round_1[nba$Round_1 <= 30  ] <- 1
nba$Round_1[31 <= nba$Round_1 ] <- 0

nba$Round_2[nba$Round_2 <= 30 |  61 <= nba$Round_2] <- 0
nba$Round_2[ nba$Round_2 != 0] <- 1

#6. Aplico el mismo proceso que en modelo original.
rnba2 <- lm(Salary~ American + Round_1 + Round_2 + Age + G + MP + PER + TS. + X3PAr + FTr + 
             ORB. + DRB. + TRB. + AST. + STL. + BLK. + TOV. + USG. + OWS + DWS + WS + 
             WS.48 + OBPM + DBPM + BPM + VORP, data = nba)
summary(rnba2)

stepAIC(rnba2, direction = "both")

modelo_nba2 <- lm(formula = Salary ~ Round_1 + Age + G + MP + PER + X3PAr + 
                    ORB. + TRB. + USG. + WS + OBPM + BLK., data = nba)
summary(modelo_nba2)

#7. Ejecuto predicciones con ambos modelos, incluyendo los resultados en la base de datos.
nba$prediccion1 <- predict(modelo_nba, newdata = nba)
nba$prediccion2 <- predict(modelo_nba2, newdata = nba)

#8. De cara a que modelo elegir, me guio por los criterios AIC y BIC. Como en ambos casos son menores
#   en el modelo original, me quedo con ese.
AIC(modelo_nba, modelo_nba2)
BIC(modelo_nba, modelo_nba2)

#9. Elimino la columna de la predicción con el modelo modificado.
nba$prediccion2 <- NULL
nba

#10. Estudio la Normalidad, Linealidad, Homocedasticidad y Multicolinealidad del modelo seleccionado.
#normalidad
qqPlot(modelo_nba, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
#linealidad
crPlots(modelo_nba)
#homocedasticidad
ncvTest(modelo_nba)
#multicolinealidad
vif(modelo_nba)


