library(readxl)
library(corrplot)
library(corpcor)
library(GGally)
library(mctest)
library(ppcor)
library(ggplot2)
library(car)
library(gvlma)


#Enr_Class8_Dataset <- Enr_Grade8_IND
Enr_Grade8_IND <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Final_Dataset2.xlsx", 
                             sheet = "Grade 8")
Enr_Class8_Dataset <- Enr_Grade8_IND
View(Enr_Class8_Dataset)
Enr_Class8_Dataset <- Enr_Class8_Dataset[,-c(22:28)]
#View(Enr_Class8_Dataset)
str(Enr_Class8_Dataset)
summary(Enr_Class8_Dataset)
par(mfrow=c(1,1))
# Removing population data from data set
# Removing NA for generating correlation matrix
Enr_Class8_Dataset <- na.omit(Enr_Class8_Dataset)
s1<- Enr_Class8_Dataset[,-c(1,2,3,4)]
str(s1)
head(s1)

# Correlation Matrix
library(Hmisc)
CM1 <- rcorr(as.matrix(s1))
CM1$r

# Visualize correlation matrix
#install.packages("corrplot")
par(mfrow=c(1,1))
library(corrplot)
corrplot(as.matrix(CM1$r), bg="black", order="hclust", type = "upper", method="circle", tl.srt=35, tl.cex = 0.65)
corrplot(as.matrix(CM1$r), bg="black", order="hclust", type = "upper", method="number", tl.srt=35, tl.cex = 0.65, number.cex = 0.6)

s1<-s1[,-c(18)] # remove DV for PCA
str(s1)

#cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,as.numeric(Enr_Class8_Dataset$acadyear))
#Pearson correlation Test Class 1 # replace Grade for each run for 
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$nb_blocks, method="pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$nb_clusters,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$nb_villages,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$nb_schools,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,as.numeric(Enr_Class8_Dataset$acadyear),method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$nb_classrooms,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$cls1_school,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$tch1_school,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$gtoilet_sch,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$water_sch,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,Enr_Class8_Dataset$Nb_Teachers_Govt,method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$Nb_teachers_pvt, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$Nb_Teachers_UR, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$`Nb_schools_PTR>30_pri`, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$`Nb_schools_SCR>30_pri`, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`, method = "pearson")
cor(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, Enr_Class8_Dataset$`Area(SQKM)`, method = "pearson")


# from the correlation matrix we can see that Class 1 Enrolment is less dependant on
#
#Dropping variables with correlation less than 0.6

#gtoilet_sch -7
#Cls1_sch - 5
#Tch1_sch - 6
#Area(SQKm) -21
#Nb_Teachers_UR - 16
#Nb_blocks - 8
#Nb_Schools_PTR>30_pri  - 17
#Nb_Clusters -9

## Linear Model after removing variables with less correlation - relavancy check 
set.seed(100)
describe(Enr_)
Enr.fit0 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                 Enr_Class8_Dataset$nb_schools+
                 Enr_Class8_Dataset$water_sch +
                 Enr_Class8_Dataset$nb_villages+
                 Enr_Class8_Dataset$Nb_Teachers_Govt+
                 Enr_Class8_Dataset$Nb_teachers_pvt+
                 Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`+
                 Enr_Class8_Dataset$`Nb_schools_SCR>30_pri`+
                 Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`,
                 data=Enr_Class8_Dataset)
summary(Enr.fit0)
AIC(Enr.fit0)
vif(Enr.fit0)

EnrPred0 <- predict(Enr.fit0, data=Enr_Class8_Dataset)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPred0
Error0 <- actual-EnrPred0
#Error0

library(ModelMetrics)
rmse(actual,EnrPred0)
mae(actual,EnrPred0)


# Visualizing Data
boxplot(Enr_Class8_Dataset$Nb_Students_Enr_Grade8)
qplot(Enr_Class8_Dataset$Nb_Students_Enr_Grade8,colodata=Enr_Class8_Dataset, ylab="No students", binwidth=10000, main="Class VIII Enrollment", fill=I("orange"),col=I("grey"))
qplot(Enr_Class8_Dataset$nb_schools,colodata=Enr_Class8_Dataset, ylab="No of schools", binwidth=500, fill=I("orange"),col=I("grey"))
qplot(Enr_Class8_Dataset$nb_classrooms,colodata=Enr_Class8_Dataset, ylab="No of classrooms", fill=I("orange"),col=I("grey"))


## # Validating Normality of data  - Qnorm plot 
par(mfrow=c(2,2))
qqnorm(Enr_Class8_Dataset$nb_classrooms, main =  "Normality check No of classrooms")
qqnorm(Enr_Class8_Dataset$nb_schools, main =  "Normality check No of schools")
qqnorm(Enr_Class8_Dataset$Nb_Teachers_Govt, main =  "Normality check No of teachers in Govt schools")
qqnorm(Enr_Class8_Dataset$water_sch, main =  "Normality check No of schools with drinking water facility")
qqnorm(Enr_Class8_Dataset$nb_villages, main =  "Normality check No of Villages")
qqnorm(Enr_Class8_Dataset$Nb_teachers_pvt, main =  "Normality check No of teachers in Pvt schools")
qqnorm(Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, main =  "Normality check No of schools with PTR>35 u pri")
qqnorm(Enr_Class8_Dataset$`Nb_schools_SCR>30_pri`, main =  "Normality check No of schools with SCR>30 pri")
qqnorm(Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`, main =  "Normality check No of schools with SCR>35 u pri")
qqnorm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, main="Normality check for no of students enroled in class 8")
#### 

library(gvlma)

gvlma::gvlma(Enr.fit0)
plot(Enr.fit0)

# Skewness of DV
library(moments)
skewness(Enr_Class8_Dataset$Nb_Students_Enr_Grade8)

### Identifying Multi-collinearity

### Checking for Multicollenearity

X1 <- Enr_Class8_Dataset[,c(-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-16,-17,-21)]
summary(X1)
X1 <- na.omit(X1)

library(mctest)
omcdiag(X1,X1$Nb_Students_Enr_Grade8)
imcdiag(X1,X1$Nb_Students_Enr_Grade8, vif=10, corr = TRUE)
pcor(X1,method = "pearson")

# Keeping only relevant IV after collinearity check
# 1. Nb of classrooms
# 2.Nb_schools_SCR>35_up_Pri 
# 3. Nb_schools_PTR>35_Upri 
# 4. Nb_Teachers_Govt              
# 5. Nb_teachers_pvt



#Treatment before Linear Regression:

# Removing Outliers
# Enrolment in class 8
quantiles0 <- quantile(Enr_Class8_Dataset$Nb_Students_Enr_Grade8, probs = c(.25, .75))
quantiles0
range0 <- 3 * IQR(Enr_Class8_Dataset$Nb_Students_Enr_Grade8)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$Nb_Students_Enr_Grade8 > (quantiles0[1] - range0) &
                                  Enr_Class8_Dataset$Nb_Students_Enr_Grade8 < (quantiles0[2] + range0))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew 


# # No of classrooms
quantiles1 <- quantile(Enr_Class8_Dataset$nb_classrooms, probs = c(.25, .75))
quantiles1
range1 <- 3 * IQR(Enr_Class8_Dataset$nb_classrooms)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$nb_classrooms > (quantiles1[1] - range1) &
                                  Enr_Class8_Dataset$nb_classrooms < (quantiles1[2] + range1))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew 
#Skwewness in Classrooms
library(moments)
skewness(Enr_Class8_Dataset$nb_classrooms)

##Nb_Teachers_Govt

quantiles2 <- quantile(Enr_Class8_Dataset$Nb_Teachers_Govt, probs = c(.25, .75))
quantiles2
range2 <- 3 * IQR(Enr_Class8_Dataset$Nb_Teachers_Govt)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$Nb_Teachers_Govt > (quantiles2[1] - range2) & 
                                  Enr_Class8_Dataset$Nb_Teachers_Govt < (quantiles2[2] + range2))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew 
library(moments)
skewness(Enr_Class8_Dataset$Nb_Teachers_Govt)


# `Nb_teaches_pvt`

quantiles3 <- quantile(Enr_Class8_Dataset$Nb_teachers_pvt, probs = c(.25, .75))
quantiles3
range3 <- 3 * IQR(Enr_Class8_Dataset$Nb_teachers_pvt)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$Nb_teachers_pvt > (quantiles3[1] - range3) & 
                                  Enr_Class8_Dataset$Nb_teachers_pvt < (quantiles3[2] + range3))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew

#Skwewness 
library(moments)
skewness(Enr_Class8_Dataset$Nb_teachers_pvt)


# Nb_schools_SCR>35_up_Pri

quantiles4 <- quantile(Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`, probs = c(.25, .75))
quantiles4
range4 <- 3 * IQR(Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri` > (quantiles4[1] - range4) & 
                                  Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri` < (quantiles4[2] + range4))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew 

#Skwewness Nb_schools_SCR>35_up_Pri
library(moments)
skewness(Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`)

# Nb_Schools_PTR>35_Upri

quantiles5 <- quantile(Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, probs = c(.25, .75))
quantiles5
range5 <- 3 * IQR(Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`)

Enr_Class8_Datasetnew <- subset(Enr_Class8_Dataset,
                                Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri` > (quantiles5[1] - range5) & 
                                  Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri` < (quantiles4[2] + range5))

Enr_Class8_Dataset <- Enr_Class8_Datasetnew 

library(moments)
skewness(Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`)

### Removing NA values
Enr_Class8_Dataset <- na.omit(Enr_Class8_Dataset)
### All India Regression eqn
par(mfrow=c(2,2))



# Corr plot training 

par(mfrow=c(1,1))
library(corrplot)
corrplot(as.matrix(CMtrain$r), bg="black", order="hclust", type = "upper", method="circle", tl.srt=35, tl.cex = 0.65)
corrplot(as.matrix(CMtrain$r), bg="black", order="hclust", type = "upper", method="number", tl.srt=35, tl.cex = 0.65, number.cex = 0.6)


# Corr plot test
corrplot(as.matrix(CMtest$r), bg="black", order="hclust", type = "upper", method="circle", tl.srt=35, tl.cex = 0.65)
corrplot(as.matrix(CMtest$r), bg="black", order="hclust", type = "upper", method="number", tl.srt=35, tl.cex = 0.65, number.cex = 0.6)


###############################

#Simple Linear Model using X1
##################################

# Model with IV - Nb of classrooms
set.seed(100)
m_x1 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms)
summary(m_x1)
AIC(m_x1)
BIC(m_x1)
EnrPredmX1 <- predict(m_x1)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPredmX1
Error <- actual-EnrPredmX1
#Error
library(ModelMetrics)
rmse(actual,EnrPredmX1)
mae(actual,EnrPredmX1)


# Model with IV - No of Teachers _Govt
set.seed(100)
m_x2 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$Nb_Teachers_Govt)
summary(m_x2)
AIC(m_x2)
BIC(m_x2)
EnrPredmX2 <- predict(m_x2)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPredmX2
Error <- actual-EnrPredmX2
#Error
rmse(actual,EnrPredmX2)
mae(actual,EnrPredmX2)

# Model with IV - No of Pvt Teachers
set.seed(100)
m_x3 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$Nb_teachers_pvt)
summary(m_x3)
AIC(m_x3)
BIC(m_x3)
EnrPredmX3 <- predict(m_x3)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPredmX3
Error <- actual-EnrPredmX3
#Error
rmse(actual,EnrPredmX3)
mae(actual,EnrPredmX3)


# Model with IV - Nb_Schools_PTR>35_Upri
set.seed(100)
m_x4 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`)
summary(m_x4)
AIC(m_x4)
BIC(m_x4)
EnrPredmX4 <- predict(m_x4)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPredmX4
Error <- actual-EnrPredmX4
#Error
rmse(actual,EnrPredmX4)
mae(actual,EnrPredmX4)

# Model with IV - Nb-Schools_SCR>35_upri

set.seed(100)
m_x5 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`)
summary(m_x5)
AIC(m_x5)
BIC(m_x5)
EnrPredmX5 <- predict(m_x5)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPredmX5
Error <- actual-EnrPredmX5
#Error
rmse(actual,EnrPredmX5)
mae(actual,EnrPredmX5)


#*******************
set.seed(100)

Enr.fit1 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                 Enr_Class8_Dataset$Nb_Teachers_Govt+Enr_Class8_Dataset$Nb_teachers_pvt+
                 Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`+Enr_Class8_Dataset$`Nb_schools_SCR>35_up_Pri`, data=Enr_Class8_Dataset)

summary(Enr.fit1)

#Enr.fit1 <- lm(log(Enr_Class8_Dataset$Nb_Students_Enr_Grade8)~log(Enr_Class8_Dataset$nb_classrooms)+
 #                log(Enr_Class8_Dataset$Female_Pop)+log(Enr_Class8_Dataset$Total_Pop)+
  #               log(Enr_Class8_Dataset$Literate_pop)+log(Enr_Class8_Dataset$Nb_of_students_Age_13), data=Enr_Class8_Dataset)

summary(Enr.fit1)
gvlma::gvlma(Enr.fit1)

plot(Enr.fit1)
vif(Enr.fit1)
AIC(Enr.fit1)
### Prediction using fit1
EnrPred1 <- predict(Enr.fit1)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPred1
Error1 <- actual-EnrPred1
#Error1
rmse(actual,EnrPred1)
mae(actual,EnrPred1)

#####
set.seed(100)

Enr.fit2 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
              Enr_Class8_Dataset$Nb_Teachers_Govt+Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`+
              Enr_Class8_Dataset$Nb_teachers_pvt, data=Enr_Class8_Dataset)

summary(Enr.fit2)
par(mfrow=c(2,2))
plot(Enr.fit2)
library(car)
vif(Enr.fit2)
AIC(Enr.fit2)

EnrPred2 <- predict(Enr.fit2)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPred2
Error2 <- actual-EnrPred1
#Error2
rmse(actual,EnrPred2)
mae(actual,EnrPred2)
####### ####


set.seed(100)
Enr.fit3 <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                 Enr_Class8_Dataset$Nb_Teachers_Govt+ 
                 Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, data=Enr_Class8_Dataset)

summary(Enr.fit3)
plot(Enr.fit3)
library(car)
vif(Enr.fit3)
AIC(Enr.fit3)

EnrPred3 <- predict(Enr.fit3)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPred3
Error3 <- actual-EnrPred3
#Error3
rmse(actual,EnrPred3)
mae(actual,EnrPred3)

# In sample accuracy

actuals_preds3 <- data.frame(cbind(actuals=Enr_Class8_Dataset$Nb_Students_Enr_Grade8, predicteds=EnrPred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds3)  
correlation_accuracy # 91.12% - test Sample 0.933
min_max_accuracy <- mean(apply(actuals_preds3, 1, min) / apply(actuals_preds3, 1, max))  
min_max_accuracy # => 80.22%, min_max accuracy - using test data - 0.872
mape <- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)  
mape # => 0.2592 using test data - > 2.3749, 0.1642?


#######

## influence diagnostics:

library(car)
influence.measures(Enr.fit3)
influenceIndexPlot(Enr.fit3, id.n = 3)
influencePlot(Enr.fit3, id.n = 3)


#Prediction with Test Data
Enr.fit3T <- lm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                 Enr_Class8_Dataset$Nb_Teachers_Govt+
                 Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, data=Enr_Class8_Dataset)

summary(Enr.fit3T)
AIC(Enr.fit3T)
par(mfrow=c(2,2))
plot(Enr.fit3T)
EnrPred3T <- predict(Enr.fit3T,tt1.val)

actuals_preds3T <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade8, predicteds=EnrPred3T))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds3T)  
correlation_accuracy # 93.6% - test Sample 0.44

min_max_accuracy <- mean(apply(actuals_preds3T, 1, min) / apply(actuals_preds3T, 1, max))  
min_max_accuracy # => 87.31%, min_max accuracy - using test data - 0.4922

mape <- mean(abs((actuals_preds3T$predicteds - actuals_preds3T$actuals))/actuals_preds3T$actuals)  
mape # => 0.1637 using test data - > 2.741, 

actual <- tt1.val$Nb_Students_Enr_Grade8
#EnrPred3
Error <- actual-EnrPred3T
#Error3
rmse(actual,EnrPred3T)
mae(actual,EnrPred3T)

#### Using RLM Function

## entire dataset
Enr.fit3R <- rlm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                    Enr_Class8_Dataset$Nb_Teachers_Govt+
                    Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`, data=Enr_Class8_Dataset)


summary(Enr.fit3R)
plot(Enr.fit3R)
#Enr.fit3R
vif(Enr.fit3R)
AIC(Enr.fit3R)

EnrPred3R <- predict(Enr.fit3R,Enr_Class8_Dataset)

actuals_preds <- data.frame(cbind(actuals=Enr_Class8_Dataset$Nb_Students_Enr_Grade8, predicteds=EnrPred3R))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 93.6% - test Sample 0.42%

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy # => 80.29%, min_max accuracy - using test data - 0.49

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # => 0.1637 using test data - > 2.71, 0.1642?

actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8

Error <- actual-EnrPred3R
#Error3
rmse(actual,EnrPred3R)
mae(actual,EnrPred3R)
m2 <- mean(Enr.fit3R$residuals)
#mean of residuals
m2
#####

set.seed(100)
tt1n<-Enr_Class8_Dataset
tt1 <- tt1n

tt1$random <- runif(nrow(Enr_Class8_Dataset), 0, 1);
tt1 <- tt1[order(tt1$random),]
tt1.dev <- tt1[which(tt1$random <= 0.70),]
summary(tt1.dev)


tt1.val <- tt1[which(tt1$random > 0.30),]
summary(tt1.val)
c(nrow(tt1.dev), nrow(tt1.val))



#######

Enr.fit3TR <- rlm(tt1.dev$Nb_Students_Enr_Grade8~tt1.dev$nb_classrooms+
                  tt1.dev$Nb_Teachers_Govt+
                  tt1.dev$`Nb_schools_PTR>35_Upri`, data=tt1.dev)


summary(Enr.fit3TR)
plot(Enr.fit3TR)
#Enr.fit3TR
vif(Enr.fit3TR)
AIC(Enr.fit3TR)

EnrPred3TR <- predict(Enr.fit3TR,tt1.val)

actuals_preds <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade8, predicteds=EnrPred3TR))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 93.6% - test Sample 0.42%

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy # => 87.31%, min_max accuracy - using test data - 0.49

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # => 0.1637 using test data - > 2.71, 0.1642?

actual <- tt1.val$Nb_Students_Enr_Grade8

Error <- actual-EnrPred3TR
#Error3
rmse(actual,EnrPred3TR)
mae(actual,EnrPred3TR)
m2 <- mean(Enr.fit3TR$residuals)
#mean of residuals
m2

#######

##library(gvlma)
gvlma::gvlma(Enr.fit3TR)
plot(Enr.fit3)

###



set.seed(100)
Enr.fit4 <- rlm(Enr_Class8_Dataset$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$nb_classrooms+
                 Enr_Class8_Dataset$nb_villages+ 
                 Enr_Class8_Dataset$`Nb_schools_PTR>35_Upri`+Enr_Class8_Dataset$Nb_Teachers_Govt, data=Enr_Class8_Dataset)

summary(Enr.fit4)
plot(Enr.fit4)
library(car)
vif(Enr.fit4)
AIC(Enr.fit4)

EnrPred4 <- predict(Enr.fit4)
actual <- Enr_Class8_Dataset$Nb_Students_Enr_Grade8
#EnrPred3
Error3 <- actual-EnrPred4
#Error3
rmse(actual,EnrPred4)
mae(actual,EnrPred4)



##*************************************
###Running for Rajasthan 
###***************************************

Raj_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Rajasthan_2011-15.xlsx", 
                                  sheet = "Grade8")
str(Raj_EnrG1)
EnrRJ.fit3 <- lm(Raj_EnrG1$Nb_Students_Enr_Grade8~Raj_EnrG1$nb_classrooms+
                 Raj_EnrG1$`Nb_schools_PTR>30_pri`+
                 Raj_EnrG1$Nb_of_students_Age_6, data=Raj_EnrG1)

summary(EnrRJ.fit3)
plot(EnrRJ.fit3)
AIC(EnrRJ.fit3)

EnrPredRJ <- predict(Enr.fit3)
actual <- Raj_EnrG1$Nb_Students_Enr_Grade8
str(EnrPredRJ)
ErrorRJ <- actual-EnrPredRJ
ErrorRJ
rmse(actual,EnrPredRJ)
mae(actual,EnrPredRJ)

#*************************************
###Running for Maharashtra 
###************************************

Mah_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Maharashtra_2011_15.xlsx", 
                        sheet = "Grade8")
str(Mah_EnrG1)
EnrA.fit3 <- lm(Mah_EnrG1$Nb_Students_Enr_Grade8~Mah_EnrG1$nb_classrooms+
                   Mah_EnrG1$`Nb_schools_PTR>30_pri`+
                   Mah_EnrG1$Nb_of_students_Age_6, data=Mah_EnrG1)

summary(EnrMH.fit3)
plot(EnrMH.fit3)
AIC(EnrMH.fit3)

EnrPredMH <- predict(EnrMH.fit3)
actual <- Mah_EnrG1$Nb_Students_Enr_Grade8
EnrPredMH
Error <- actual-EnrPredMH
Error
rmse(actual,EnrPredMH)
mae(actual,EnrPredMH)


#*************************************
###Running for Assam 
###************************************
  
As_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Assam_2011_15.xlsx", 
                          sheet = "Grade8")
str(As_EnrG1)
EnrAS.fit3 <- lm(As_EnrG1$Nb_Students_Enr_Grade8~As_EnrG1$nb_classrooms+
                   As_EnrG1$`Nb_schools_PTR>30_pri`+
                   As_EnrG1$Nb_of_students_Age_6, data=As_EnrG1)

summary(EnrAS.fit3)
plot(EnrAS.fit3)
AIC(EnrAS.fit3)

EnrPredAS <- predict(EnrAS.fit3)
actual <- As_EnrG1$Nb_Students_Enr_Grade8
EnrPredAS
Error <- actual-EnrPredAS
Error
rmse(actual,EnrPredAS)
mae(actual,EnrPredAS)


#*************************************
###Running for Gujarat 
###************************************

GJ_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Gujarat_2011_15.xlsx", 
                       sheet = "Grade8")
str(GJ_EnrG1)
EnrGJ.fit3 <- lm(GJ_EnrG1$Nb_Students_Enr_Grade8~GJ_EnrG1$nb_classrooms+
                   GJ_EnrG1$`Nb_schools_PTR>30_pri`+
                   GJ_EnrG1$Nb_of_students_Age_6, data=GJ_EnrG1)

summary(EnrGJ.fit3)
plot(EnrGJ.fit3)
AIC(EnrGJ.fit3)

EnrPredGJ <- predict(EnrGJ.fit3)
actual <- GJ_EnrG1$Nb_Students_Enr_Grade8
EnrPredGJ
Error <- actual-EnrPredGJ
Error
rmse(actual,EnrPredGJ)
mae(actual,EnrPredGJ)

#*************************************
###Running for Madhya Pradesh 
###************************************

MP_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Madhya_Pradesh_2011_15.xlsx", 
                       sheet = "Grade8")
str(MP_EnrG1)
EnrMP.fit3 <- lm(MP_EnrG1$Nb_Students_Enr_Grade8~MP_EnrG1$nb_classrooms+
                   MP_EnrG1$`Nb_schools_PTR>30_pri`+
                   MP_EnrG1$Nb_of_students_Age_6, data=MP_EnrG1)

summary(EnrMP.fit3)
plot(EnrMP.fit3)
AIC(EnrMP.fit3)

EnrPredMP <- predict(EnrMP.fit3)
actual <- MP_EnrG1$Nb_Students_Enr_Grade8
EnrPredMP
Error <- actual-EnrPredMP
Error
rmse(actual,EnrPredMP)
mae(actual,EnrPredMP)

#*************************************
###Running for Bihar
###************************************

BH_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Bihar_2011_15.xlsx", 
                       sheet = "Grade8")
str(BH_EnrG1)
EnrBH.fit3 <- lm(BH_EnrG1$Nb_Students_Enr_Grade8~BH_EnrG1$nb_classrooms+
                   BH_EnrG1$`Nb_schools_PTR>30_pri`+
                   BH_EnrG1$Nb_of_students_Age_6, data=BH_EnrG1)

summary(EnrBH.fit3)
plot(EnrBH.fit3)
AIC(EnrBH.fit3)

EnrPredBH <- predict(EnrBH.fit3)
actual <- BH_EnrG1$Nb_Students_Enr_Grade8
EnrPredBH
Error <- actual-EnrPredBH
Error
rmse(actual,EnrPredBH)
mae(actual,EnrPredBH)

#*************************************
###Running for Kerala
###************************************

KL_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Kerala_2011_15.xlsx", 
                       sheet = "Grade8")
str(KL_EnrG1)
EnrKL.fit3 <- lm(KL_EnrG1$Nb_Students_Enr_Grade8~KL_EnrG1$nb_classrooms+
                   KL_EnrG1$`Nb_schools_PTR>30_pri`+
                   KL_EnrG1$Nb_of_students_Age_6, data=KL_EnrG1)

summary(EnrKL.fit3)
plot(EnrKL.fit3)
AIC(EnrKL.fit3)

EnrPredKL <- predict(EnrKL.fit3)
actual <- KL_EnrG1$Nb_Students_Enr_Grade8
EnrPredKL
Error <- actual-EnrPredKL
Error
rmse(actual,EnrPredKL)
mae(actual,EnrPredKL)
#*************************************
###Running for Uttar Pradesh
###************************************
  
UP_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Uttar_Pradesh_2011_15.xlsx", 
                       sheet = "Grade8")
str(UP_EnrG1)
EnrUP.fit3 <- lm(UP_EnrG1$Nb_Students_Enr_Grade8~UP_EnrG1$nb_classrooms+
                   UP_EnrG1$`Nb_schools_PTR>30_pri`+
                   UP_EnrG1$Nb_of_students_Age_6, data=UP_EnrG1)

summary(EnrUP.fit3)
plot(EnrUP.fit3)
AIC(EnrUP.fit3)

EnrPredUP <- predict(EnrUP.fit3)
actual <- UP_EnrG1$Nb_Students_Enr_Grade8
EnrPredUP
Error <- actual-EnrPredUP
Error
rmse(actual,EnrPredUP)
mae(actual,EnrPredUP)

#*************************************
###Running for ODISSHA
###************************************

OR_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Odissa_2011_15.xlsx", 
                       sheet = "Grade8")
str(OR_EnrG1)
EnrOR.fit3 <- lm(OR_EnrG1$Nb_Students_Enr_Grade8~OR_EnrG1$nb_classrooms+
                   OR_EnrG1$`Nb_schools_PTR>30_pri`+
                   OR_EnrG1$Nb_of_students_Age_6, data=OR_EnrG1)

summary(EnrOR.fit3)
plot(EnrOR.fit3)
AIC(EnrOR.fit3)

EnrPredOR <- predict(EnrOR.fit3)
actual <- OR_EnrG1$Nb_Students_Enr_Grade8
EnrPredOR
Error <- actual-EnrPredOR
Error
rmse(actual,EnrPredOR)
mae(actual,EnrPredOR)

#*************************************
###Running for PUNJAB
###************************************

PB_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Punjab_2011_15.xlsx", 
                       sheet = "Grade8")
str(PB_EnrG1)
EnrPB.fit3 <- lm(PB_EnrG1$Nb_Students_Enr_Grade8~PB_EnrG1$nb_classrooms+
                   PB_EnrG1$`Nb_schools_PTR>30_pri`+
                   PB_EnrG1$Nb_of_students_Age_6, data=PB_EnrG1)

summary(EnrPB.fit3)
plot(EnrPB.fit3)
AIC(EnrPB.fit3)

EnrPredPB <- predict(EnrPB.fit3)
actual <- PB_EnrG1$Nb_Students_Enr_Grade8
EnrPredPB
Error <- actual-EnrPredPB
Error
rmse(actual,EnrPredPB)
mae(actual,EnrPredPB)


#************************************************
#*************************************
###Running for TAMIL NADU
###************************************

TN_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/TamilNadu_2011_15.xlsx", 
                       sheet = "Grade8")
str(TN_EnrG1)
EnrTN.fit3 <- lm(TN_EnrG1$Nb_Students_Enr_Grade8~TN_EnrG1$nb_classrooms+
                   TN_EnrG1$`Nb_schools_PTR>30_pri`+
                   TN_EnrG1$Nb_of_students_Age_6, data=TN_EnrG1)

summary(EnrTN.fit3)
plot(EnrTN.fit3)
AIC(EnrTN.fit3)

EnrPredTN <- predict(EnrTN.fit3)
actual <- TN_EnrG1$Nb_Students_Enr_Grade8
EnrPredTN
Error <- actual-EnrPredTN
Error
rmse(actual,EnrPredTN)
mae(actual,EnrPredTN)


#*************************************
###Running for MANIPUR
###************************************

MN_EnrG1 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Manipur_2011_15.xlsx", 
                       sheet = "Grade8")
str(MN_EnrG1)
EnrMN.fit3 <- lm(MN_EnrG1$Nb_Students_Enr_Grade8~MN_EnrG1$nb_classrooms+
                   MN_EnrG1$`Nb_schools_PTR>30_pri`+
                   MN_EnrG1$Nb_of_students_Age_6, data=MN_EnrG1)

summary(EnrMN.fit3)
plot(EnrMN.fit3)
AIC(EnrMN.fit3)

EnrPredMN <- predict(EnrMN.fit3)
actual <- MN_EnrG1$Nb_Students_Enr_Grade8
EnrPredMN
Error <- actual-EnrPredMN
Error
rmse(actual,EnrPredMN)
mae(actual,EnrPredMN)


#************************************************

#RMSE calculation

library(dplyr)
RMSE=(exp(predicted)-ttr.val$PM2.5)**2 %>% 
  mean() %>% 
  sqrt()
RMSE
mean(abs(((exp(predicted)-ttr.val$PM2.5)/ttr.val$PM2.5)))*100
mean(abs((model$pred[,1]-model$pred[,2])/model$pred[,2])*100)
par(mfrow = c(1,2))
plot(ttr.val$PM2.5, exp(predicted) ,xlab= " PM2.5-Actual", ylab = " PM2.5-predicted" , main = " RKPuram -MLR Model Fit
     W/o PD_PM2.5",col = "red")
abline(0, 1)

plot(ttr.val$PM2.5, exp(predicted) ,xlab= " PM2.5-Actual", ylab = " PM2.5-predicted" , main = " RKPuram -MLR Model Fit
     With PD_PM2.5",col = "red")
abline(0, 1)



###
ggplot(Enr_Class8_Dataset,aes(x= Enr_Class8_Dataset$acadyear, y=Enr_Class8_Dataset$Nb_Students_Enr_Grade8)) + 
  geom_boxplot(outlier.colour = "blue") +  
  scale_y_continuous( breaks =c(seq(0 , ceiling(max(Enr_Class8_Dataset$Nb_Students_Enr_Grade8))+1000 , ceiling(max(Enr_Class8_Dataset$Nb_Students_Enr_Grade8/10))))) +
  labs( x = "Academic Year", y = "No of students", title = "Enrasthan - Class 1 enrollment (2011-2015)")

str(Enr_Class8_Dataset)



library(car)
vif(m1)
outlierTest(m1)
# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 

cutoff <- 4/((nrow(Enr_Class8_Dataset)-length(m1$coefficients)-2)) 
plot(m1, which=4, cook.levels=cutoff)

## Validating the model on other data set
library(readxl)
Bihar_Enr_Grade8 <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Bihar_Enr_Grade8.xlsx")
#View(Bihar_Enr_Grade8)
str(Bihar_Enr_Grade8)
Bihar_Enr_Grade8 <- Bihar_Enr_Grade8[,-c(1,2,3,4)]
str(Bihar_Enr_Grade8)

Enr_Grade8_Predicted <- predict(m1,data = Bihar_Enr_Grade8)


## Based on the correlation test, (Relevancy check) we will drop nb_blocks, nb_clusters, nb_villages, cls1_school,
#tch1_school,`Area(SQKM)`, ST_Pop and run the Linear Regression model

Enr_Class8_Dataset_n1 <- Enr_Class8_Dataset[,-c(5,6,9,10,11,12,16,18,21,25)]
str(Enr_Class8_Dataset_n1)


# # Model with all IVs

m1 <- lm(tt1.dev$Nb_Students_Enr_Grade8~tt1.dev$gtoilet_sch+tt1.dev$water_sch+tt1.dev$nb_classrooms+tt1.dev$Nb_Teachers_Govt+tt1.dev$Nb_teachers_pvt+
           tt1.dev$`Nb_schools_PTR>30_pri`+tt1.dev$`Nb_schools_SCR>30_pri`+tt1.dev$`Nb_schools_SCR>35_up_Pri`+
           tt1.dev$Nb_of_students_Age_6+tt1.dev$SC_Pop+tt1.dev$Literate_pop+tt1.dev$Female_Pop+tt1.dev$Female_Lit_Pop+
           tt1.dev$Total_Pop+tt1.dev$, data=tt1.dev)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
AIC(m1)
BIC(m1)
vif(m1)


# Model with IV - `Nb_schools_PTR>30_pri`
m_x6 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$`Nb_schools_PTR>30_pri`)
summary(m_x6)
AIC(m_x6)
BIC(m_x6)


# Model with IV - `Nb_schools_SCR>30_pri`
m_x7 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$`Nb_schools_SCR>30_pri`)
summary(m_x7)
AIC(m_x7)
BIC(m_x7)


# Model with IV - `Nb_schools_SCR>35_up_Pri`
m_x8 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$`Nb_schools_SCR>35_up_Pri`)
summary(m_x8)
AIC(m_x8)
BIC(m_x8)

# Model with IV - Nb_of_students_Age_6
m_x9 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$Nb_of_students_Age_6)
summary(m_x9)
AIC(m_x9)
BIC(m_x9)


# Model with IV - SC_Pop
m_x10 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$SC_Pop)
summary(m_x10)
AIC(m_x10)
BIC(m_x10)

# Model with IV - Literate_pop
m_x11 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$Literate_pop)
summary(m_x11)
AIC(m_x11)
BIC(m_x11)

# Model with IV - Female_Pop
m_x12 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$Female_Pop)
summary(m_x12)
AIC(m_x12)
BIC(m_x12)

# Model with IV - Female_Lit_Pop
m_x13 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$Female_Lit_Pop)
summary(m_x13)
AIC(m_x13)
BIC(m_x13)

# Model with IV - Total_Pop
m_x14 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$Total_Pop)
summary(m_x14)
AIC(m_x14)
BIC(m_x14)

# model X9 and X3
m_x9x3 <- lm(Enr_C)

# Model with all IVs

m1 <- lm(Enr_Class8_Dataset_n1$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n1$gtoilet_sch+Enr_Class8_Dataset_n1$water_sch+Enr_Class8_Dataset_n1$nb_classrooms+Enr_Class8_Dataset_n1$Nb_Teachers_Govt+Enr_Class8_Dataset_n1$Nb_teachers_pvt+
           Enr_Class8_Dataset_n1$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n1$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n1$`Nb_schools_SCR>35_up_Pri`+
           Enr_Class8_Dataset_n1$Nb_of_students_Age_6+Enr_Class8_Dataset_n1$SC_Pop+Enr_Class8_Dataset_n1$Literate_pop+Enr_Class8_Dataset_n1$Female_Pop+Enr_Class8_Dataset_n1$Female_Lit_Pop+
           Enr_Class8_Dataset_n1$Total_Pop, data=Enr_Class8_Dataset_n1)
summary(m1)
par(mfrow=c(2,2))
plot(m1)
AIC(m1)
BIC(m1)
vif(m1)


# Still model accuracy is low

# We will noow remove addditional multi collinear variables

###Check multicollinearity in independent variables


# Reducing Number of variables - Female_Lit_Pop , water_sch, Literate_Pop  coefficient(s) are non-significant may be due to multicollinearity
View(Enr_Class8_Dataset_n1)
Enr_Class8_Dataset_n2 <- Enr_Class8_Dataset_n1[,-c(6,12,17,18)]
View(Enr_Class8_Dataset_n2)

# Model with reduced variables  - Linear regression

m2 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$gtoilet_sch+Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+Enr_Class8_Dataset_n2$Nb_teachers_pvt+
           Enr_Class8_Dataset_n2$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+
           Enr_Class8_Dataset_n2$Nb_of_students_Age_6+Enr_Class8_Dataset_n2$SC_Pop+Enr_Class8_Dataset_n2$Literate_pop+Enr_Class8_Dataset_n2$Total_Pop, data=Enr_Class8_Dataset_n2)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
AIC(m2)
BIC(m2)
vif(m2)


m3 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$gtoilet_sch+Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+Enr_Class8_Dataset_n2$Nb_teachers_pvt+
           Enr_Class8_Dataset_n2$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n2$Nb_of_students_Age_6+
           Enr_Class8_Dataset_n2$SC_Pop+Enr_Class8_Dataset_n2$Total_Pop, data=Enr_Class8_Dataset_n2)
summary(m3)
par(mfrow=c(2,2))
plot(m3)
AIC(m3)
BIC(m3)
vif(m3)

m4 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$gtoilet_sch+Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+Enr_Class8_Dataset_n2$Nb_teachers_pvt+
           Enr_Class8_Dataset_n2$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n2$Nb_of_students_Age_6+
           Enr_Class8_Dataset_n2$SC_Pop, data=Enr_Class8_Dataset_n2)
summary(m4)
#par(mfrow=c(2,2))
#plot(m4)
AIC(m4)
BIC(m4)
vif(m4)
accuracy(m4)

m5 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+Enr_Class8_Dataset_n2$Nb_teachers_pvt+
           Enr_Class8_Dataset_n2$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n2$Nb_of_students_Age_6+
           Enr_Class8_Dataset_n2$SC_Pop, data=Enr_Class8_Dataset_n2)
summary(m5)
#par(mfrow=c(2,2))
#plot(m5)
AIC(m5)
BIC(m5)
vif(m5)
abs.error.pred(lp=fitted(m5),y=Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8)

# Model 6

m6 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$gtoilet_sch+Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+
           Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n2$Nb_of_students_Age_6,
         data=Enr_Class8_Dataset_n2)
summary(m6)
#par(mfrow=c(2,2))
#plot(m5)
AIC(m6)
BIC(m6)
vif(m6)
abs.error.pred(lp=fitted(m6),y=Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8)


### Model with 4 IV
m7 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$nb_classrooms+Enr_Class8_Dataset_n2$Nb_Teachers_Govt+
           Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+Enr_Class8_Dataset_n2$Nb_of_students_Age_6,
         data=Enr_Class8_Dataset_n2)
summary(m7)
#par(mfrow=c(2,2))
#plot(m7)
AIC(m7)
BIC(m7)
vif(m7)
abs.error.pred(lp=fitted(m7),y=Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8)


m8 <- lm(Enr_Class8_Dataset_n2$Nb_Students_Enr_Grade8~Enr_Class8_Dataset_n2$Nb_Teachers_Govt+
           Enr_Class8_Dataset_n2$Nb_teachers_pvt+Enr_Class8_Dataset_n2$`Nb_schools_PTR>30_pri`+Enr_Class8_Dataset_n2$`Nb_schools_SCR>30_pri`+
           Enr_Class8_Dataset_n2$Nb_of_students_Age_6+Enr_Class8_Dataset_n2$SC_Pop+Enr_Class8_Dataset_n2$Total_Pop, data=Enr_Class8_Dataset_n2)
summary(m8)
AIC(m8)
BIC(m8)

gvlma::gvlma(m8)
plot(m8)


##### Model Evaluation
set.seed(100)
tt1<-Enr_Class8_Dataset_n2
tt1$random <- runif(nrow(Enr_Class8_Dataset_n2), 0, 1);
tt1 <- tt1[order(tt1$random),]
tt1.dev <- tt1[which(tt1$random <= 0.85),]
tt1.dev
tt1.val <- tt1[which(tt1$random > 0.85),]
tt1.val

### Linear Regression after removing variables - water_sch, Nb_schools_SCR>35_up_pri, Female_Lit_Pop  
m7 <- lm(tt1.dev$Nb_Students_Enr_Grade8~tt1.dev$nb_classrooms+tt1.dev$Nb_Teachers_Govt+
           tt1.dev$`Nb_schools_SCR>30_pri`+tt1.dev$Nb_of_students_Age_6,
         data=tt1.dev)
summary(m7)

AIC(m7)
BIC(m7)
vif(m7)


##Validating Assumptions of Linear Regression

# Assumption 1: The mean of residuals is zero
Mean_Res <- mean(m7$residuals)
Mean_Res


# Assumption 2: The homoscedasticity of residuals or equal variance is true:
par(mfrow=c(2,2))
plot(m7)

# Predicted Values of Enrolment
Enr_Class1_predicted1 <- round(predict(m7, data =tt1.val),digits=0)

actuals_preds <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade8, predicteds=Enr_Class1_predicted1))  # make actuals_predicteds dataframe.
actuals_preds
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)

###
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
min_max_accuracy
# => 64%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape
# => 51%, mean absolute percentage deviation

### Linear Regression after removing variables:

### Linear Regression after removing variables - Nb_schools_PTR>30_pri, gtoilet_sch,water_sch, Nb_schools_SCR>30_pri,Nb_classrooms,  
#fit1 <- lm(tt1.dev$Nb_Students_Enr_Grade8~Enr_Class8_Dataset$cls1_school+Enr_Class8_Dataset$Nb_Teachers_Govt+Enr_Class8_Dataset$Nb_teachers_pvt+Enr_Class8_Dataset$Nb_Teachers_UR+Enr_Class8_Dataset$SC_Pop+Enr_Class8_Dataset$ST_Pop+Enr_Class8_Dataset$`Area(SQKM)`+Enr_Class8_Dataset$Total_Pop, data= Enr_Class8_Dataset)
#fit2 <- lm(tt1.dev$Nb_Students_Enr_Grade8~tt1.dev$Nb_of_students_Age_6+tt1.dev$Nb_Teachers_Govt+tt1.dev$Nb_teachers_pvt+tt1.dev$Nb_Teachers_UR, data=tt1.dev)

fit2 <- lm(tt1.dev$Nb_Students_Enr_Grade8~+tt1.dev$nb_schools+tt1.dev$Nb_of_students_Age_6+tt1.dev$Nb_Teachers_Govt+tt1.dev$Nb_teachers_pvt+tt1.dev$Nb_Teachers_UR+tt1.dev$Literate_pop+tt1.dev$nb_blocks, data= tt1.dev)
summary(fit2)
#par(mfrow=c(2,2))
plot(fit2)
AIC(fit2)
BIC(fit2)

