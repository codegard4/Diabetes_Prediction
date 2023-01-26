Task 1

library(tidyverse)
diabetes <- read.csv("diabetes.csv")
write.csv(diabetes, "diabetes.csv")

insMod <- glm(Outcome ~ ., data=diabetes, family = "binomial")
summary(insMod)
step(insMod, test="LRT")
allMod <- glm(Outcome~Age+BloodPressure+Pregnancies+BMI+Glucose, 
              data = diabetes, family = "binomial")
summary(allMod)
insPred <- diabetes %>% 
  mutate(
    training_predictions = predict.glm(allMod, newdata=diabetes, type="response"),
    predicted_class = ifelse(training_predictions < 0.5, "Predicted Not Diabetic", "Predicted Diabetic"),
    ins_class = ifelse(Outcome == 0, "Is Not Diabetic","Is Diabetic")
  ) 
(bal_c_m <- table(insPred$ins_class, insPred$predicted_class))
bal_s_r <- sum(diag(bal_c_m)) / nrow(diabetes)
bal_s_r
diabetes$Outcome <- as.factor(diabetes$Outcome)
ggplot(data=diabetes, aes(x=Glucose)) +
  geom_density(aes(fill=as.factor(Outcome)), alpha=0.4)+
  labs(title = "Figure 1: Density Plot of Glucose")
ggplot(data = insPred, aes(x = Glucose, y = Outcome)) + 
  geom_point(color = "blue")+
  geom_smooth(aes(y = training_predictions),se = F)+
  labs(title = "Figure 2: Glucose Levels vs Outcome")

Using Deviance as our determining factor we eliminated SkinThickness and Insulin from our model. Glucose and BMI were the most accurate predictors in our model followed by Pregnancies, Blood Pressure and Age, again based on Both decreases in our Residual Deviance and p-values of the coefficients. Our model is shown below: 
$probability = e^(-7.96+.02*Age-.01*BloodPressure+.12*Pregnancies+.09*BMI+.03*Glucose)/(1+e^(-7.96+.02*Age-.01*BloodPressure+.12*Pregnancies+.09*BMI+.03*Glucose))$
Our first model for probability of diabetes is based on 5 factors. These factors are:
  -Age
  -Blood Pressure
  -Pregnancies
  -BMI
  -Glucose

Of these factors, Age, Pregnancies, BMI, and Glucose increase the probability that someone will be diabetic, while Blood Pressure decreases this probability. 
The number of pregnancies someone has is the factor that increases probability that someone will be diabetic the most, with each pregnancy increasing the chance a person is diabetic by about 1.3%. 
Even though the number of pregnancies has the highest coefficient, a persons BMI, Age and Glucose have an arguably greater impact. This is because pregnancy values will usually not exceed 3, while age, BMI and glucose 
all go much higher. A 70 year old who has never had kids is 12% more likely to be diabetic than a woman who has had 4 kids. 
Next we will perform a hypothesis test to determine if this model produces a success rate that is statistically significant

p = (437+63)/(437+63+158+110)
phat = .7747396
n = 768
ci = .95
A = 0.05
stddevphat = sqrt((p*(1-p))/n)  
standarderrornull = sqrt(p*(1-p)/n)
Z = (phat-p)/standarderrornull #standard error
#Z
pvalue = 1-pnorm(Z)
pvalue

  H0: p^ = p
  HA: p^ > p
  P-value: 3.191891e-13  
  The hypothesis test yielded a Z score of 7.2 and a p-value of 3.19*10^-13. Both of these figures
  give us enough information to reject our null hypothesis and conclude that our models improvement on the simple assumption that everyone in the dataset is not diabetic is statistically significant




Task 2

Part 1

library(tidyverse)
diabetes <- read.csv("diabetes2.csv")
write.csv(diabetes, "diabetes2.csv")
diabetesClean <- diabetes %>% 
  mutate(Smoking = ifelse(Smoking == "yes",1,0),
         Diabetic = ifelse(Diabetic == "yes",1,0),
         Alcohol = ifelse(Alcohol == "yes",1,0),
         FamilyDiabetes = ifelse(FamilyDiabetes=="yes",1,0),
         highBP = ifelse(highBP == "yes",1,0),
         PhysicallyActive = as.factor(PhysicallyActive),
         RegularMedicine = ifelse(RegularMedicine == "yes",1,0),
         JunkFood = as.factor(JunkFood),
         Stress = as.factor(Stress),
         BPLevel = as.factor(BPLevel),
         Age = as.factor(Age),
         UrinationFreq = as.factor(UriationFreq))         
taskMod <- glm(Diabetic ~ ., data=diabetesClean, family = "binomial")

summary(taskMod)
step(taskMod, test="LRT")
taskMod2 <- glm(Diabetic ~ RegularMedicine+Age+FamilyDiabetes+PhysicallyActive+BPLevel+SoundSleep+Pregancies+Smoking+Gender,data=diabetesClean, family = "binomial")
summary(taskMod2)
taskMod3 <- glm(Diabetic ~ RegularMedicine+Age+FamilyDiabetes+PhysicallyActive+SoundSleep+Pregancies+Smoking+Gender,data=diabetesClean, family = "binomial")
summary(taskMod3)
allMod <- taskMod3
diabetes <- diabetesClean
nrow(diabetesClean)
summary(diabetesClean)
insPred <- diabetes %>% 
  mutate(
    training_predictions = predict.glm(allMod, newdata=diabetes, type="response"),
    predicted_class = ifelse(training_predictions < 0.5, "Predicted Not Diabetic", "Predicted Diabetic"),
    ins_class = ifelse(Diabetic == 0, "Is Not Diabetic","Is Diabetic")
  ) 
(bal_c_m <- table(insPred$ins_class, insPred$predicted_class))
bal_s_r <- sum(diag(bal_c_m)) / nrow(diabetes)
bal_s_r
diabetes$Diabetic <- as.factor(diabetes$Diabetic)
ggplot(data = insPred, aes(x = BMI, y = Diabetic)) + 
  geom_jitter(color = "blue")+
  geom_smooth(aes(y = training_predictions),se = F)+
  labs(title = "Figure 3: BMI vs Diabetes")

Success Rate: 85.8%
$probability = e^(-4.55+2.6*Med-1.91*Age1+.37*Age2+1.86*Age3+1.28*FD+.75*PA1+.52*PA2+1.39*PA3+.13*Sleep+.33*Pregnancies+1.38*Smoking+.46*Gender)/$
  $(1+e^(-4.55+2.6*Med-1.91*Age1+.37*Age2+1.86*Age3+1.28*FD+.75*PA1+.52*PA2+1.39*PA3+.13*Sleep+.33*Pregnancies+1.38*Smoking+.46*Gender))$  
Our Final Model Uses 8 of the 17 Variables in the dataset. Descriptions for these variables are below:  
  Med: Regular Medicine Usage  
  Age1: <40 Years Old  
  Age2: 50-59 Years Old  
  Age3: 60+ Years Old  
  FD: Family History of Diabetes  
  PA1: Not Physically Active  
  PA2: Physically Active More Than 30 Minutes  
  PA3: Physically Active More Than 1 Hour  
  Sleep: Hours of Sound Sleep Nightly  
  Pregnancies: Number of Pregnancies  
  Smoking: Do they Smoke?  
  Gender: Are they Male?  
  
-Why variables excluded/included
-engineered features (if applicable)
-Visualizations with color
-probability or Odds with explanation of coefficients
-confusion matrix & success rate
-hypothesis test for significance


Task 3
Part 1
library(tidyverse)
diabetes3 <- read.csv(file.choose())
write.csv(diabetes3, "diabetesTask3Given.csv")
diabetes4 <- diabetes3 %>% mutate(
  HighBP = as.factor(HighBP),
  HighChol = as.factor(HighChol),
  Sex = as.factor(Sex),
  Education = as.factor(Education),
  Income = as.factor(Income),
  age = age)
diabetes5 = diabetes4 %>% mutate(
  ageNew = ifelse(age == '30-39',35,ifelse(age == '40-49',45,ifelse(age == '50-59',55,ifelse(age == '60-69',65,ifelse(age == '70-79',75,85))))),
  GeneralHealthNew = ifelse(GeneralHealth == 'excellent',4,ifelse(GeneralHealth == 'very good',3, ifelse(GeneralHealth == 'good',2,ifelse(GeneralHealth == 'fair',1,0)))),
  SexNew = ifelse(Sex == 'female',0,1),
  Income = ifelse(Income == "Less than $15K",15,ifelse(Income == "Less than $20K",20,ifelse(Income == "Less Than $25K",25,ifelse(Income == "Less than $35K",35,ifelse(Income == "Less than $50K",50, ifelse(Income == "Less than $75K",75,100))))))
  ) %>% select(-GeneralHealth, -age,-Sex)
summary(diabetes5)
set.seed(123)
trainingRows <- sample( 1:nrow(diabetes3), size = floor(0.80*nrow(diabetes3)),replace=FALSE)
training <- diabetes5[trainingRows, ]
testing <- diabetes5[-trainingRows, ]
summary(training)
taskMod <- glm(Diabetes_binary ~ ., data=training, family = "binomial")

summary(taskMod)
step(taskMod, test="LRT")
taskMod2 <- glm(Diabetes_binary ~GeneralHealthNew+BMI+HighBP+HighChol+ageNew+CholCheck+SexNew+HeartDiseaseorAttack+Income+HvyAlcoholConsump+PhysHlth,data=training, family = "binomial")
summary(taskMod2)
allMod <- taskMod2
diabetes <- training
nrow(training)
insPred <- training %>% 
  mutate(
    training_predictions = predict.glm(allMod, newdata=training, type="response"),
    predicted_class = ifelse(training_predictions < 0.5, "Predicted Not Diabetic", "Predicted Diabetic"),
    ins_class = ifelse(Diabetes_binary == 0, "Is Not Diabetic","Is Diabetic")
  ) 
(bal_c_m <- table(insPred$ins_class, insPred$predicted_class))
bal_s_r <- sum(diag(bal_c_m)) / nrow(training)
bal_s_r
ggplot(data = insPred, aes(x = BMI, y = Diabetes_binary))+ 
  geom_point(aes(color = as.factor(HeartDiseaseorAttack))) + 
  geom_smooth(aes(y = training_predictions),se = F)+
  labs(color = "Heart Disease/Attack", y = "Diabetes",x = "BMI",title = "Figure 6: BMI vs Diabetes With Model")

  Our Final Dataset was narrowed down to 11 variables out of the original 21. We once again narrowed down variables by reductions in residual deviance. We also modified Income, age, general health, and sex to be numeric variables. The result was easier coefficients to quantify, with these four predictors now having one coefficient.    

  Testing Success Rate: 72.1%  
  Training Success Rate: 74.2%  

  $Probability = e^(-2.09-.57*GH+.07*BMI-.93*NHBP-.66*NHC+.02*Age+1.22*CC+.27*Sex+.33*HDOA-.004*Income-.55*HAC-.01*PhysHlth)/$
  $(1+e^(-2.09-.57*GH+.07*BMI-.93*NHBP-.66*NHC+.02*Age+1.22*CC+.27*Sex+.33*HDOA-.004*Income-.55*HAC-.01*PhysHlth))$

    Our final model is above and descriptions for the variable names are below:  
    GH-Numerical variable for General health 0 being poor 4 being excellent
  BMI-Body Mass Index
  NHBP-Not High Blood Pressure
  NHC-Not High Cholesterol
  Age-Age as a number (30-39 age group as 35, 40-49 as 45, etc.)
  CC-Cholesterol Check
  Sex-1 = Male, 0 = Female
  HDOA - Heart Disease or a Heart Attack
  Income - Number for income level (less than 15k as 15, less than 35k as 35, over 75k as 100, etc.)
  HAC-Heavy Alcohol Consumption (14+drinks/wk for men 7+drinks/wk for women)
  PhysHlth-How many days in the past 30 was your Physical Health not good?

Next we will test our model training and testing success rates for significance

  
-hypothesis test for significance


Part 2
-Predictions on new data

library(tidyverse)
diabetes3 <- read.csv("diabetesTask3Given.csv")
diabetes4 <- diabetes3 %>% mutate(
  HighBP = as.factor(HighBP),
  HighChol = as.factor(HighChol),
  Sex = as.factor(Sex),
  Education = as.factor(Education),
  Income = as.factor(Income),
  age = age)
diabetes5 = diabetes4 %>% mutate(
  ageNew = ifelse(age == '30-39',35,ifelse(age == '40-49',45,ifelse(age == '50-59',55,ifelse(age == '60-69',65,ifelse(age == '70-79',75,85))))),
  GeneralHealthNew = ifelse(GeneralHealth == 'excellent',4,ifelse(GeneralHealth == 'very good',3, ifelse(GeneralHealth == 'good',2,ifelse(GeneralHealth == 'fair',1,0)))),
  SexNew = ifelse(Sex == 'female',0,1),
  Income = ifelse(Income == "Less than $15K",15,ifelse(Income == "Less than $20K",20,ifelse(Income == "Less Than $25K",25,ifelse(Income == "Less than $35K",35,ifelse(Income == "Less than $50K",50, ifelse(Income == "Less than $75K",75,100))))))
) %>% select(-GeneralHealth, -age,-Sex)

set.seed(123)
trainingRows <- sample( 1:nrow(diabetes3), size = floor(0.80*nrow(diabetes3)),replace=FALSE)
training <- diabetes5[trainingRows, ]
testing <- diabetes5[-trainingRows, ]
taskMod2 <- glm(Diabetes_binary ~GeneralHealthNew+BMI+HighBP+HighChol+ageNew+CholCheck+SexNew+HeartDiseaseorAttack+Income+HvyAlcoholConsump+PhysHlth,data=diabetes5, family = "binomial")

diabetesP <- read.csv("diabetesTask3Mystery.csv")
diabetesP2 <- diabetesP %>% mutate(
  HighBP = as.factor(HighBP),
  HighChol = as.factor(HighChol),
  Sex = as.factor(Sex),
  Education = as.factor(Education),
  Income = as.factor(Income),
  age = age)
diabetesP3 = diabetesP2 %>% mutate(
  ageNew = ifelse(age == '30-39',35,ifelse(age == '40-49',45,ifelse(age == '50-59',55,ifelse(age == '60-69',65,ifelse(age == '70-79',75,85))))),
  GeneralHealthNew = ifelse(GeneralHealth == 'excellent',4,ifelse(GeneralHealth == 'very good',3, ifelse(GeneralHealth == 'good',2,ifelse(GeneralHealth == 'fair',1,0)))),
  SexNew = ifelse(Sex == 'female',0,1),
  Income = ifelse(Income == "Less than $15K",15,ifelse(Income == "Less than $20K",20,ifelse(Income == "Less Than $25K",25,ifelse(Income == "Less than $35K",35,ifelse(Income == "Less than $50K",50, ifelse(Income == "Less than $75K",75,100))))))
) %>% select(-GeneralHealth, -age,-Sex)

allMod <- taskMod2
diabetes <- diabetesP3
insPred <- diabetes %>% 
  summarize(
    ID = ID,
    PredictionProb = predict.glm(allMod, newdata=diabetes, type="response"),
    PredictionBinary = ifelse(PredictionProb >=.5,1,0),
    PredictionCategory = ifelse(PredictionProb < 0.5, "predicted not diabetes", "predicted diabetes")
  ) 
write.csv(insPred, "RMSE_Master_Predictions")


Part 3
Compare all three models

