library (mlbench)
data(BostonHousing)
names(BostonHousing) #afficher les colonnes (les variables) du dataset
Reg1 =lm(crim~medv, data=BostonHousing) #regression linéaire simple
Reg1 #afficher reg1
summary(Reg1)

#Interpretation :  Y^ = -0.362 X + 11.7965
#Sa^ = -0.36316 et t = a^/Sa^= -9.46
# H0 "a=0" vs H1="a<>0"
# sous H0 : T=-9.46 
# t(alpha/2)(504) =  ?? table de student
# Residual standard error c'est racine s^2

plot(BostonHousing$medv, BostonHousing$crim,
     main = "Relation entre crim et medv",
     xlab ="medv",
     ylab ="crim",
     pch = 19,
     col = "orange")

abline(Reg1, col="blue", lwd=2)
     
     
     
new_data <- data.frame(medv =  28)
predicted_crim <- predict(Reg1, newdata = new_data)
print(paste("Prédiction pour medv = 28 :", predicted_crim))

Reg2 =lm(crim ~.,data=BostonHousing) #regression multiple
Reg2
summary(Reg2)
