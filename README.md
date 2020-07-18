# Regression-R
Assignment-Regression-R
#import the data

insurance<-read.csv("BC1insurance copie 2.csv", stringsAsFactors=TRUE)

str(insurance)

# la variable de prediction est charges 

# the prediction variable is charges 

summary(insurance$charges)

# the mean is greater than median so it is a right skewed distribution as confirmed by histogram 

hist(insurance$charges)

# matrice de correlation des variables entre elles

# correlation matrix

cor(insurance[c("age","bmi","children","charges")])

# la plus forte correlation est entre age et charges 

# the strongest correlation is between age and charges

pairs(insurance[c("age","bmi","children","charges")])

# install package psych

library(psych)

pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# building the model (linear regression)

insurance_model<-lm(charges~age+sex+smoker+region+bmi+children,insurance)

# or 

ins_model<-lm(charges~ .,insurance)

summary(ins_model)

# chaque coeff sous le nom de la variable indique l'increment de charge pour une augmentation

# de 1 de la variable ex pour age +1 = 256$ de charges supp 

# under each coefficient you get an indication how much increment of predicted variable correspond increment of 1 of the 

# of the variable influencing prediction

# now we seperate in two part the dataset we take 75% of lines for train, remaining for test

ins_model2<-insurance[1:1100,]

ins_test2<-insurance[1101:1338,]

ins_train2<-ins_model2

ins_model2<-lm(charges~age+sex+smoker+region+bmi+children,ins_train2)

ins_pred<-predict(ins_model2,ins_test2)

# we check model performance prediction and charges in test should be highly correlated if model is good

cor(ins_test2$charges,ins_pred)

head(ins_pred)

head(ins_test2$charges)

# to compare side by side prediction and real data in test

comparaison<-cbind(ins_test2$charges,ins_pred)

ins_model2
