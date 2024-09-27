###############################################################################
#                 Progretto Inferenza Statistica - Gruppo 48 
#   Alessandro Rossi - Emanuele Piccolo - Niccolò Singorelli - Federico Riva
#       Politecnino di Milano - Ingengneria Matematica - AA 2023/2024
###############################################################################

#### LIBRERIE #### 

library( ellipse )
library( faraway )
library( leaps )
library(car) # per il test di Levene
library(MASS)
library(ggplot2)
library( GGally)
library(rgl)
library(corrplot)
library(lmtest)
library(caret)
library(boot)

#### COMANDI INIZIALI #####
#fede
fede_f="C:/Users/Fede/OneDrive - Politecnico di Milano/MODELLI E METODI DELL'INFERENZA STATISTICA/Progetto Statistica/Secondo tentativo"
setwd(fede_f)
#fede_p="C:/Users/feder/OneDrive - Politecnico di Milano/MODELLI E METODI DELL'INFERENZA STATISTICA/Progetto Statistica/Secondo tentativo"
#setwd(fede_p)

# ale ---  

# pic ---

# nico
#nik_wd = "~/Desktop/Inferenza Statistica/Progetto/Secondo tentativo"
#setwd(nik_wd)

# Importiamo i dati
data0 = read.csv('dati_energia.csv', sep = ";")
#View(data0)

## Tolgo colonna inutile e aggiusto unità di misura
data1 = subset(data0, select = -c(Timestamp,Time))
str(data1)
data1$SquareM=data1$SquareFootage*0.093
data1 = subset(data1,select = -c(SquareFootage))
data1$Time=as.factor(data1$Time)
data1$HVACUsage = as.factor(data1$HVACUsage)
data1$LightingUsage = as.factor(data1$LightingUsage)
data1$DayOfWeek = as.factor(data1$DayOfWeek)
data1$Holiday = as.factor(data1$Holiday)
data1$Occupancy=as.factor(data1$Occupancy)
str(data1)
summary(data1)
dim(data1)
#View(data1)

set.seed(123)
train=sample(1000,700)
data_train=data1[train,]
dim(data_train)
data_test=data1[-train,]

## Modello Iniziale 
mod0 = lm( EnergyConsumption ~ ., data=data_train)
summarymod0=summary(mod0)
summarymod0

shapiro.test(mod0$residuals)
bptest(mod0)

## QUALI SONO LE COVARIATE NON INFLUENTI ?

attach(data_train)
## GG-PAIRS
ggpairs(data_train[,c('Temperature','Humidity','RenewableEnergy')], aes(col=as.factor(HVACUsage)))
ggpairs(data_train[,c('Temperature','Humidity','RenewableEnergy')], aes(col=as.factor(Occupancy)))
ggpairs(data_train[,c('Temperature','Humidity','RenewableEnergy')], aes(col=as.factor(LightingUsage)))
ggpairs(data_train[,c('Temperature','Humidity','RenewableEnergy')], aes(col=as.factor(DayOfWeek)))
ggpairs(data_train[,c('Temperature','Humidity','RenewableEnergy')], aes(col=as.factor(Holiday)))


## ANOVA ## 

## DayOfWeek ##
# omoschedasticità
bartlett.test(EnergyConsumption, DayOfWeek)
# Normalità nei gruppi
Ps = tapply( EnergyConsumption, DayOfWeek, function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( EnergyConsumption ~ DayOfWeek, xlab = 'DayOfWeek', ylab = 'EnergyConsumption',
         main = 'EnergyConsumption according to DayOfWeek' )
abline( h = mean( EnergyConsumption ), col='red' )
aov1=aov(EnergyConsumption ~ DayOfWeek)
summary(aov1)

data_train = subset(data_train, select = -DayOfWeek)
data_test = subset(data_test, select = -DayOfWeek)

## Occupancy ##
# omoschedasticità
bartlett.test(EnergyConsumption, Occupancy)

# Normalità nei gruppi
Ps = tapply( EnergyConsumption, Occupancy, function( x ) ( shapiro.test( x )$p ) )
Ps
# medie = tapply( EnergyConsumption, Occupancy, function( x ) (mean( x ) ) )
# plot(medie)

boxplot( EnergyConsumption ~ Occupancy, xlab = 'Occupancy', ylab = 'EnergyConsumption',
         main = 'EnergyConsumption according to Occupancy' )
abline( h = mean( EnergyConsumption ), col='red' )
aov2=aov(EnergyConsumption ~ Occupancy)
summary(aov2)


## HVACUsage ##
# omoschedasticità
bartlett.test(EnergyConsumption, HVACUsage)

# Normalità nei gruppi
Ps = tapply( EnergyConsumption, HVACUsage, function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( EnergyConsumption ~ HVACUsage, xlab = 'HVACUsage', ylab = 'EnergyConsumption',
         main = 'EnergyConsumption according to HVACUsage' )
abline( h = mean( EnergyConsumption ), col='red' )
aov3=aov(EnergyConsumption ~ HVACUsage)
summary(aov3)


## LightingUsage ##
# omoschedasticità
bartlett.test(EnergyConsumption, LightingUsage)

# Normalità nei gruppi
Ps = tapply( EnergyConsumption, LightingUsage, function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( EnergyConsumption ~ LightingUsage, xlab = 'LightingUsage', ylab = 'EnergyConsumption',
         main = 'EnergyConsumption according to LightingUsage' )
abline( h = mean( EnergyConsumption ), col='red' )
aov4=aov(EnergyConsumption ~ LightingUsage)
summary(aov4)

data_train = subset(data_train, select = -LightingUsage)  ## p-value alto rispetto all'altra che abbiamo tenuto
data_test = subset(data_test, select = -LightingUsage)


## Holiday ##
# omoschedasticità
bartlett.test(EnergyConsumption, Holiday)

# Normalità nei gruppi
Ps = tapply( EnergyConsumption, Holiday, function( x ) ( shapiro.test( x )$p ) )
Ps

boxplot( EnergyConsumption ~ Holiday, xlab = 'Holiday', ylab = 'EnergyConsumption',
         main = 'EnergyConsumption according to Holiday' )
abline( h = mean( EnergyConsumption ), col='red' )
aov5=aov(EnergyConsumption ~ Holiday)
summary(aov5)

data_train = subset(data_train, select = -Holiday)
data_test = subset(data_test, select = -Holiday)


## CREIAMO UN NUOVO MODELLO ##
mod1 = lm(EnergyConsumption ~ ., data = data_train)
summarymod1 = summary(mod1)
summarymod1

## Selezione delle Varibili del Modello 
ris=step(mod1,direction = 'backward' , trace=T)
ris$coefficients

## Rimuovere i Coefficienti Ininflueanti 
data_train = subset(data_train, select = -SquareM)
data_test =subset(data_test, select = - SquareM)


## Costruiamo il nuovo modello di regressione lineare senza le covariate superflue
mod1 = lm(EnergyConsumption ~ ., data = data_train)
summarymod1 = summary(mod1)
summarymod1

shapiro.test(mod1$residuals)
bptest(mod1)

## AIC
AIC(mod1)

## BIC
BIC(mod1)


## Cerchiamo Punti Leva, Residui, Residui Studentizzati e distanza di Cook's
## Troviamo i punti leva
lev=hatvalues(mod1)
p=mod1$rank
n=dim(data_train)[1]

plot(mod1$fitted.values,lev,ylab='Leverege', main ="Leverege Points")
abline(h=2*p/n,lty=2,col='red')
watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points( mod1$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16 )


## Residui Standardizzati 
res_std=mod1$residuals/summarymod1$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
watchout_rstd = res_std[ watchout_ids_rstd ]
watchout_rstd

plot( mod1$fitted.values, res_std, ylab = "Standardized Residuals", main = "Standardized Residuals" )
abline( h = c(-2,2), lty = 2, col = 'red' )
points( mod1$fitted.values[watchout_ids_rstd],
        res_std[watchout_ids_rstd], col = 'red', pch = 16 )
points( mod1$fitted.values[watchout_ids_lev],
        res_std[watchout_ids_lev], col = 'green', pch = 16 )
legend('bottomleft', col = c('red','green'),
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )

# Non essendoci differenza fra l'una o l'altra osservazione campionaria possimao eliminare
# tutte quelle osservazioni che sforano i limisti di st_Dev e Leverage senza interrogarci sul senso dell'osservazione

## Residui Studentizzati
stud=rstandard(mod1)
watchout_ids_stud = which( abs( stud ) > 2 )
watchout_stud = stud[ watchout_ids_stud ]
watchout_stud

plot( mod1$fitted.values, stud, ylab = "Studentized Residuals", main = "Studentized Residuals", pch = 16 )
points( mod1$fitted.values[watchout_ids_stud],
        stud[watchout_ids_stud], col = 'blue', pch = 16 )
points( mod1$fitted.values[watchout_ids_lev],
        stud[watchout_ids_lev], col = 'orange', pch = 16 )
abline( h = c(-2,2), lty = 2, col = 'blue' )
legend('bottomleft', col = c('blue','orange'),
       c('Studentized Residual', 'Leverages'), pch = rep( 16, 3 ), bty = 'n' )

## Distanza di Cook's
Cdist = cooks.distance( mod1 )
watchout_ids_Cdist = which( Cdist > 4/(n-p) )
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
watchout_Cdist

# Plot della distanza di Cook sui fitted values
plot( mod1$fitted.values, Cdist, pch = 16, xlab = 'Fitted values',
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( mod1$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ],col = 'green', pch = 16 )
abline(h= 4/(n-p),lty=2,col='green')

# Plot della distanza di Cook per la i-esima osservazione del campione
plot(mod1, which = 4)

## Grafico di Influenza
influencePlot(mod1,id.method = "identify", main = "influential Plot",
              sub = "Circle size is proportial to Cook's Distance" )

## Costruiamo il modello lineare senza i punti che influenzano significativamente la regressione

mod2 = lm(EnergyConsumption ~ ., data = data_train,subset = ( stud < 2 | res_std < 2 | Cdist < 4/(n-p)))
data_train_finale=data_train[stud < 2 | res_std < 2 | Cdist < 4/(n-p),]
detach(data_train)
attach(data_train_finale)
summarymod2=summary(mod2)
summarymod2

## Verifica della gaussianità ed omoschedasticità dei residui
shapiro.test(mod2$residuals)
bptest(mod2)

## motifichiamo i dataset tendendo solo le osservazioni con residui o risuidui studentizzati < 2 o Cdist < 4/(n-p)
dim(data_train_finale) 

## Verifichiamo le Hp di Omoschedatiscità e di Gaussianità per il nuovo modello più approfonditamente
plot(mod2$fitted.values,mod2$residuals,xlab='Fitted',ylab='Residuals',main='Residuals vs Fitted Values')
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
shapiro.test(mod2$residuals)
bptest(mod2)
hist(mod2$residuals)
qqnorm(mod2$residuals,pch=16)
qqline(mod2$residuals,col='red')

## Facciamo una Trasformazione Box-Cox per provare ad aumentare la gaussianità dei residui
b=boxcox(EnergyConsumption ~ ., data=data_train)
best_lambda_ind=which.max(b$y)
best_lambda=b$x[best_lambda_ind]
best_lambda

## Verifichiamo la collinearità delle Variabili 
vif(mod2)

# Verifichiamo che i coefficienti AIC e BIC siano effettivamente migliorati
## AIC
AIC(mod2)

## BIC
BIC(mod2)


## Verifichiamo la correlazione delle variabili numeriched
variabili=data_train[,-c(3,4,6)]
corrplot(cor(variabili),method='color')
corrplot(cor(variabili), method='number')


## CROSS-VALIDAZIONE 
# Cross Validazione del Modello: K-fold Crossvalidation
control <- trainControl(method = "cv", number = 6)  # CV a 6 fold
set.seed(123)
cv_model <- train(
  EnergyConsumption ~ .,
  data = data_train_finale,
  method = "lm",
  trControl = control
)
print(cv_model)
summary(cv_model)
summarycv=cv_model$finalModel


## STIME INTERVALLARI SUI PARAMETRI DEL MODELLO
alpha <- 0.05
p <- mod2$rank
n <- dim(data1)[1]
t_alpha2 <- qt(1 - alpha / 2, n - p)
# Inizializzo il vettore per salvare gli intervalli di confidenza
IC_betas <- vector("list", length(mod2$coefficients))
# Calcolo degli intervalli di confidenza per ogni coefficiente
for (i in 1:length(mod2$coefficients)) {
  beta_i <- mod2$coefficients[i]
  se_beta_i <- summarymod2$coefficients[i, "Std. Error"]
  IC_betas[[i]] <- c(beta_i - t_alpha2 * se_beta_i, beta_i + t_alpha2 * se_beta_i)
}
IC_betas=list2DF(IC_betas)
IC_betas

plot( ellipse( mod2, c( 2, 3 ) ), type = "l" )
points( 0, 0 )
points( mod2$coef[ 2 ] , mod2$coef[ 3 ] , pch = 18 )
abline( v = c( IC_betas[1,2], IC_betas[2,2] ), lty = 2 )
abline( h = c( IC_betas[1,3], IC_betas[2,3] ), lty = 2 )

plot( ellipse( mod2, c( 3, 9 ) ), type = "l" )
points( 0, 0 )
points( mod2$coef[ 3 ] , mod2$coef[ 9 ] , pch = 18 )
abline( v = c( IC_betas[1,3], IC_betas[2,3] ), lty = 2 )
abline( h = c( IC_betas[1,9], IC_betas[2,9] ), lty = 2 )

aux0=rbind(mod2$coefficients,IC_betas)

#aux0 in formato matriciale
data_matrix <- do.call(rbind, aux0)
# Creazione del vettore di indici per l'asse x
x <- 1:14

# Plottiamo le colonne della matrice
matplot(x, data_matrix, type = "b", pch = 16, col = 1:3,
        xlab = "Indici dei Beta", ylab = "Stime",
        main = "Plot degli intervalli di confidenza")

legend("topright", legend = c("Fit", "Lower Bound", "Upper Bound"),
       col = 1:3, pch = 16)
abline(h=0, col='blue')


## INTERVALLI DI PREVISIONE DELLA MEDIA DEI FITTED VALUES ( Teorema di Gauss-Markov)
# Costruiamo un intervallo di confidenza per la Media dei valori previsti 
rows=dim(data_train_finale)[1]
IP=rbind(rep(0,rows),rep(0,rows),rep(0,rows))
Z=model.matrix(mod2)
betas <- as.numeric(mod2$coefficients)
s=summarymod2$sigma
alpha=0.05
rk=mod2$rank
quant=qt(1-alpha/2,rows-rk)
for(i in 1:rows){
  Zi=as.numeric(Z[i,])
  IP[1,i]=Zi%*%betas
  IP[2,i]=IP[1,i]-s*sqrt(t(Zi) %*% solve(t(Z)%*%Z) %*% Zi)*quant
  IP[3,i]=IP[1,i]+s*sqrt(t(Zi) %*% solve(t(Z)%*%Z) %*% Zi)*quant
}
IP=t(IP)
## Illustrazione
aux <- cbind(data_train_finale$Temperature, IP)
pred_spline <- loess(aux[, 2] ~ aux[, 1], span = 0.3)
lower_spline <- loess(aux[, 3] ~ aux[, 1], span = 0.3)
upper_spline <- loess(aux[, 4] ~ aux[, 1], span = 0.3)
pred_values <- predict(pred_spline, aux[, 1])
lower_values <- predict(lower_spline, aux[, 1])
upper_values <- predict(upper_spline, aux[, 1])
plot(aux[, 1], aux[, 2], type = "n", xlab = "Temperature", ylab = "Predicted Values", 
     main = "IC per la media della risposta")
lines(aux[, 1], pred_values, col = 1, lty = 1)
lines(aux[, 1], lower_values, col = 'blue', lty = 2)
lines(aux[, 1], upper_values, col = 'blue', lty = 2)
legend("topright", legend = c("Prediction", "Lower Bound", "Upper Bound"), 
       lty = c(1, 2, 2), col = c(1, 'blue', 'blue'))

## INTERVALLO DI CONFIDENZA PER I FITTED VALUES

### PREVISIONE 
## PREVISIONE 1 - previsione manuale 
mod3 <- lm(EnergyConsumption ~ ., data = data_test)  
Test_Matrix <- model.matrix(mod3)  
betas <- as.numeric(mod2$coefficients) 
y_fitted_man <- Test_Matrix %*% betas  

# Calculate manually fitted values
mod3 <- lm(EnergyConsumption ~ ., data = data_test)
Test_Matrix <- model.matrix(mod3)
betas <- as.numeric(mod2$coefficients)
y_fitted_man <- Test_Matrix %*% betas

# Plot settings
plot(y_fitted_man, col = "red", pch = 10, ylab = "Prediction", xlab = "Index", main = "Real  vs Predicted ")
points(data_test$EnergyConsumption,col='black',pch=10)
legend("bottomleft", legend = c("Prediction","Reals"), col = c("red", "black"), pch = c(16,15))

## Confornto analitico
var(abs(data_test$EnergyConsumption-y_fitted_man))
mse_man = mean((data_test$EnergyConsumption-y_fitted_man)^2)
mse_man

### PREVISIONE 2 - previsione con codice R
predictions <- predict(mod2, newdata = data_test)
# Plot dei valori predetti vs quelli reali: 
plot(predictions, col = "orange", pch = 16, ylab = "Prediction", xlab = "Index", main = "Real  vs Predicted ")
points(data_test$EnergyConsumption,col='blue',pch=16)
legend("bottomleft",legend = c("Prediction","Real"), col = c("orange", "blue"), pch = c(16,16))
# Calcolo del Mean Squared Error
mse <- mean((data_test$EnergyConsumption - predictions)^2)
print(paste("Mean Squared Error:", mse))
# Calcolo del Mean Absolute Error (MAE)
mae <- mean(abs(data_test$EnergyConsumption - predictions))
print(paste("Mean Absolute Error:", mae))
# Calcolo dell'R-squared per il dataset di test
rss <- sum((data_test$EnergyConsumption - predictions)^2)
tss <- sum((data_test$EnergyConsumption - mean(data_test$EnergyConsumption))^2)
r_squared <- 1 - rss/tss
print(paste("R-squared:", r_squared))

## COSTRUZIONE DI MODELLI LINEARI PIù COMPLESSI 
# ELVAMENTO A POTENZA DELLE COVARIATE CONTINUE
MSE_vett <- rep(0, 7)
MSE_vett_1 <- rep(0, 7)

for (p in 2:8) {
  mod_aux <- lm(EnergyConsumption ~ poly(Temperature, p) + poly(Humidity, p) + poly(RenewableEnergy, p) + Occupancy + HVACUsage, data = data_train_finale)
  predictions_aux <- predict(mod_aux, newdata = data_test)
  MSE_vett[p - 1] <- mean((data_test$EnergyConsumption - predictions_aux)^2)
  MSE_vett_1[p - 1] <- mean((data_train_finale$EnergyConsumption - mod_aux$fitted.values)^2)
}

plot(2:8, MSE_vett, type = 'b', xlab = 'Degree of Polynomial', ylab = 'MSE', ylim = range(c(MSE_vett, MSE_vett_1)), main = "Degree of Polynomial vs MSE")
lines(2:8, MSE_vett_1, type = "b", col = "green")
legend("center", legend = c("Test MSE", "Train MSE"), col = c("black", "green"), lty = 1, pch = 1)

 #### FINE ####
