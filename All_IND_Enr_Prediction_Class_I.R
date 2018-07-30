library(readxl)
#install.packages("corrplot")
library(corrplot)
install.packages("corpcor")
library(corpcor)
install.packages("GGally")
library(GGally)
library(mctest)
library(ppcor)
library(ggplot2)
library(car)
library(gvlma)


# Import Class 1 Enrolment Data

#Enr_Class1_Dataset <- Enr_Grade1_IND
#Final_Dataset2 <- read_excel("C:/Users/500007624/Desktop/Great Learning/Project/FinalData/Final_Dataset2.xlsx", 
#                             +     sheet = "Grade1")

Enr_Grade1_IND <- read_excel("E:/Personal/PGPBABI/capstone/Elementry_Education_India/01District_Level/Final_Dataset2.xlsx", 
                             sheet = "Grade1")
Enr_Class1_Dataset <- Enr_Grade1_IND
View(Enr_Class1_Dataset)
Enr_Class1_Dataset <- Enr_Class1_Dataset[,-c(22:28)]
View(Enr_Class1_Dataset)
str(Enr_Class1_Dataset)
summary(Enr_Class1_Dataset)
par(mfrow=c(1,1))
# Removing population data from data set
# Removing NA for generating correlation matrix
Enr_Class1_Dataset <- na.omit(Enr_Class1_Dataset)
s1<- Enr_Class1_Dataset[,-c(1,2,3,4)]
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

# from the correlation matrix we can see that Class 1 Enrolment is less dependant on
#
#Dropping variables with correlation less than 0.6
# cls1_school	  1
# tch1_school	  2
# gtoilet_sch 	3
# nb_blocks	  5
# nb_clusters	  6
# Nb_teachers_pvt 	11
#Nb_Teachers_UR  	12
#Area(SQKM)	  17

# Creating a new dataset after removing the non related variables
View(s1)
Enr_Class1_Dataset_1<-s1[,-c(1,2,3,5,6,11,12,17)]
View(Enr_Class1_Dataset_1)

## Linear Model after removing variables with less correlation - relavancy check 
set.seed(100)
describe(Enr_Class1_Dataset_1)
Enr.fit0 <- lm(Enr_Class1_Dataset_1$Nb_Students_Enr_Grade1~., data = Enr_Class1_Dataset_1)
summary(Enr.fit0)
AIC(Enr.fit0)
vif(Enr.fit0)
#Removing one of the two large VIF variables based on correlation
#VIF for nb_schools- 12.892456 & Nb_schools_PTR>30_pri - 9.630221
#removing nb_schools / column 3

Enr_Class1_Dataset_1<-Enr_Class1_Dataset_1[,-c(3)]
View(Enr_Class1_Dataset_1)

EnrPred0 <- predict(Enr.fit0, data=Enr_Class1_Dataset_1)
actual <- Enr_Class1_Dataset_1$Nb_Students_Enr_Grade1
#EnrPred0
Error0 <- actual-EnrPred0
#Error0

library(ModelMetrics)
rmse(actual,EnrPred0)
mae(actual,EnrPred0)

library(gvlma)
gvlma::gvlma(Enr.fit0)
plot(Enr.fit0)
#Since all assumptions are not satisfied, treating them one by one
#Checking for Skweness& Kurtosis

View(Enr_Class1_Dataset_1)
# Skewness of DV
library(moments)
skewness(Enr_Class1_Dataset_1)
kurtosis(Enr_Class1_Dataset_1)

#converting to log data
Enr_Class1_Dataset_1_log<-log(Enr_Class1_Dataset_1)
View(Enr_Class1_Dataset_1_log)

#Checking for NAN values
summary(is.nan(Enr_Class1_Dataset_1_log$water_sch))
summary(is.nan(Enr_Class1_Dataset_1_log$nb_villages))
summary(is.nan(Enr_Class1_Dataset_1_log$nb_classrooms))
summary(is.nan(Enr_Class1_Dataset_1_log$Nb_Teachers_Govt))
summary(is.nan(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>30_pri`))
summary(is.nan(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>35_Upri`))
summary(is.nan(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>30_pri`))
summary(is.nan(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>35_up_Pri`))
summary(is.nan(Enr_Class1_Dataset_1_log$Nb_Students_Enr_Grade1))


#Checking Infinity values
summary(is.infinite(Enr_Class1_Dataset_1_log$water_sch))
summary(is.infinite(Enr_Class1_Dataset_1_log$nb_villages))
summary(is.infinite(Enr_Class1_Dataset_1_log$nb_classrooms))
summary(is.infinite(Enr_Class1_Dataset_1_log$Nb_Teachers_Govt))
summary(is.infinite(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>30_pri`))
summary(is.infinite(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>35_Upri`))
summary(is.infinite(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>30_pri`))
summary(is.infinite(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>35_up_Pri`))
summary(is.infinite(Enr_Class1_Dataset_1_log$Nb_Students_Enr_Grade1))

#Removing Inf
Enr_Class1_Dataset_1_log$water_sch[which(!is.finite(Enr_Class1_Dataset_1_log$water_sch))]<- NA
Enr_Class1_Dataset_1_log$Nb_Teachers_Govt[!is.finite(Enr_Class1_Dataset_1_log$Nb_Teachers_Govt)]<-NA
Enr_Class1_Dataset_1_log$`Nb_schools_PTR>30_pri`[!is.finite(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>30_pri`)]<-NA
Enr_Class1_Dataset_1_log$`Nb_schools_PTR>35_Upri`[!is.finite(Enr_Class1_Dataset_1_log$`Nb_schools_PTR>35_Upri`)]<-NA
Enr_Class1_Dataset_1_log$`Nb_schools_SCR>30_pri`[!is.finite(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>30_pri`)]<-NA
Enr_Class1_Dataset_1_log$`Nb_schools_SCR>35_up_Pri`[!is.finite(Enr_Class1_Dataset_1_log$`Nb_schools_SCR>35_up_Pri`)]<-NA

#Creating lm function
Enr.fit_lg0 <- lm(Enr_Class1_Dataset_1_log$Nb_Students_Enr_Grade1~.,data = Enr_Class1_Dataset_1_log)
gvlma::gvlma(Enr.fit_lg0)

#Still the assumptions are not satisfied
#Checking normality
Enr.fit_lg0_Plot <- rstandard(Enr.fit_lg0)
qqnorm(Enr.fit_lg0_Plot)
qqline(Enr.fit_lg0_Plot)


#Treatment before Linear Regression:


#Missing Value treatment
#Calculating Missing Values

summary(is.na(Enr_Class1_Dataset_1_log))

#Imputing Missingvalues
#Using MICE funtion for missing value imputation using MICE
install.packages("missForest")
library(missForest)
#seed 10% missing values
Enr_Class1_Dataset_1_log_mis <- prodNA(Enr_Class1_Dataset_1_log, noNA = 0.1)
summary(Enr_Class1_Dataset_1_log_mis)

install.packages("mice")
library(mice)


md.pattern(Enr_Class1_Dataset_1_log_mis)
install.packages("VIM")
library(VIM)
mice_plot <- aggr(Enr_Class1_Dataset_1_log_mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Enr_Class1_Dataset_1_log_mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
imputed_Data <- mice(Enr_Class1_Dataset_1_log_mis, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(imputed_Data)

#get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)

summary(is.na(completeData))



# Removing Outliers
# Enrolment in class 1
quantiles0 <- quantile(completeData$Nb_Students_Enr_Grade1, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$Nb_Students_Enr_Grade1)

completeData_New <- subset(completeData,
                           completeData$Nb_Students_Enr_Grade1 > (quantiles0[1] - range0) &
                             completeData$Nb_Students_Enr_Grade1 < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New 


# # No of water_Sch
quantiles0 <- quantile(completeData$water_sch, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$water_sch)

completeData_New <- subset(completeData,
                           completeData$water_sch > (quantiles0[1] - range0) &
                             completeData$water_sch < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of nb_villages
quantiles0 <- quantile(completeData$nb_villages, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$nb_villages)

completeData_New <- subset(completeData,
                           completeData$nb_villages > (quantiles0[1] - range0) &
                             completeData$nb_villages < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of nb_classrooms
quantiles0 <- quantile(completeData$nb_classrooms, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$nb_classrooms)

completeData_New <- subset(completeData,
                           completeData$nb_classrooms > (quantiles0[1] - range0) &
                             completeData$nb_classrooms < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of Nb_Teachers_Govt
quantiles0 <- quantile(completeData$Nb_Teachers_Govt, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$Nb_Teachers_Govt)

completeData_New <- subset(completeData,
                           completeData$Nb_Teachers_Govt > (quantiles0[1] - range0) &
                             completeData$Nb_Teachers_Govt < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of `Nb_schools_PTR>30_pri`
quantiles0 <- quantile(completeData$`Nb_schools_PTR>30_pri`, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$`Nb_schools_PTR>30_pri`)

completeData_New <- subset(completeData,
                           completeData$`Nb_schools_PTR>30_pri` > (quantiles0[1] - range0) &
                             completeData$`Nb_schools_PTR>30_pri` < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of `Nb_schools_PTR>35_Upri`
quantiles0 <- quantile(completeData$`Nb_schools_PTR>35_Upri`, probs = c(.25, .75))
quantiles0
range0 <- 1.5 * IQR(completeData$`Nb_schools_PTR>35_Upri`)

completeData_New <- subset(completeData,
                           completeData$`Nb_schools_PTR>35_Upri` > (quantiles0[1] - range0) &
                             completeData$`Nb_schools_PTR>35_Upri` < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of `Nb_schools_PTR>35_Upri`
quantiles0 <- quantile(completeData$`Nb_schools_PTR>35_Upri`, probs = c(.25, .75))
quantiles0
range0 <- 1.5* IQR(completeData$`Nb_schools_PTR>35_Upri`)

completeData_New <- subset(completeData,
                           completeData$`Nb_schools_PTR>35_Upri` > (quantiles0[1] - range0) &
                             completeData$`Nb_schools_PTR>35_Upri` < (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New

# # No of `Nb_schools_SCR>30_pri`
quantiles0 <- quantile(completeData$`Nb_schools_SCR>30_pri`, probs = c(.25, .75))
quantiles0
range0 <- 1.5* IQR(completeData$`Nb_schools_SCR>30_pri`)

completeData_New <- subset(completeData,
                           completeData$`Nb_schools_SCR>30_pri` > (quantiles0[1] - range0) &
                             completeData$`Nb_schools_SCR>30_pri`< (quantiles0[2] + range0))
View(completeData_New)
completeData <- completeData_New 

# # No of `Nb_schools_SCR>35_up_Pri`
quantiles0 <- quantile(completeData$`Nb_schools_SCR>35_up_Pri`, probs = c(.25, .75))
quantiles0
range0 <- 1.5* IQR(completeData$`Nb_schools_SCR>35_up_Pri`)

completeData_New <- subset(completeData,
                           completeData$`Nb_schools_SCR>35_up_Pri`> (quantiles0[1] - range0) &
                             completeData$`Nb_schools_SCR>35_up_Pri`< (quantiles0[2] + range0))
View(completeData_New)

completeData <- completeData_New 

#Skewness after outlier treatment                   
library(moments)
skewness(completeData)

#Changing Dataset Name
Enr_Class1_Dataset<-completeData

### cheking/ Removing NA values

summary(is.na(Enr_Class1_Dataset))

### All India Regression eqn
par(mfrow=c(2,2))

set.seed(100)
tt1n<-Enr_Class1_Dataset
tt1 <- tt1n

tt1$random <- runif(nrow(Enr_Class1_Dataset), 0, 1);
tt1 <- tt1[order(tt1$random),]
tt1.dev <- tt1[which(tt1$random <= 0.85),]
summary(tt1.dev)
tt1.val <- tt1[which(tt1$random > 0.85),]
summary(tt1.val)
c(nrow(tt1.dev), nrow(tt1.val))


###############################
View(Enr_Class1_Dataset)
#Handling multi collinearity
mmc<-lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset)
vif(mmc)
View(Enr_Class1_Dataset)
#Removing Water_sch
Enr_Class1_Dataset_mc<-Enr_Class1_Dataset[,-c(1)]

#View(Enr_Class1_Dataset_mc)

mmc<-lm(Enr_Class1_Dataset_mc$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset_mc)
vif(mmc)
#Removing `Nb_schools_PTR>30_pri`
Enr_Class1_Dataset_mc<-Enr_Class1_Dataset_mc[,-c(4)]
#View(Enr_Class1_Dataset_mc)

mmc<-lm(Enr_Class1_Dataset_mc$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset_mc)
vif(mmc)


#Removing `Nb_schools_SCR>30_pri`
Enr_Class1_Dataset_mc4<-Enr_Class1_Dataset_mc[,-c(6)]

mmc<-lm(Enr_Class1_Dataset_mc4$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset_mc4)
vif(mmc)


#using nb_villages, nb_classrooms, Nb_Teachers_Govt, `Nb_schools_PTR>35_Upri`,`Nb_schools_PTR>35_Upri` & `Nb_schools_SCR>30_up_Pri` for prediction
View(Enr_Class1_Dataset_mc4)


#Changing Data Name
Enr_Class1_Dataset<- Enr_Class1_Dataset_mc4
#Simple Linear Model using X1
##################################
str(Enr_Class1_Dataset)
# Model with IV - Nb of classrooms
set.seed(100)
m_x1 <- lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~Enr_Class1_Dataset$nb_classrooms)
summary(m_x1)
AIC(m_x1)
BIC(m_x1)
EnrPredmX1 <- predict(m_x1)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPredmX1
Error <- actual-EnrPredmX1
#Error
library(ModelMetrics)
rmse(actual,EnrPredmX1)
mae(actual,EnrPredmX1)


# Model with IV - No of Villages
set.seed(100)
m_x2 <- lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~Enr_Class1_Dataset$nb_villages)
summary(m_x2)
AIC(m_x2)
BIC(m_x2)
EnrPredmX2 <- predict(m_x2)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPredmX2
Error <- actual-EnrPredmX2
#Error
rmse(actual,EnrPredmX2)
mae(actual,EnrPredmX2)

# Model with IV - `Nb_schools_PTR>35_Upri`
set.seed(100)
m_x3 <- lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~Enr_Class1_Dataset$`Nb_schools_PTR>35_Upri`)
summary(m_x3)
AIC(m_x3)
BIC(m_x3)
EnrPredmX3 <- predict(m_x3)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPredmX3
Error <- actual-EnrPredmX3
#Error
rmse(actual,EnrPredmX3)
mae(actual,EnrPredmX3)
View(Enr_Class1_Dataset)

# Model with IV - `Nb_schools_SCR>30_pri`
set.seed(100)
m_x4 <- lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~ Enr_Class1_Dataset$`Nb_schools_SCR>30_pri`)
summary(m_x4)
AIC(m_x4)
BIC(m_x4)
EnrPredmX4 <- predict(m_x4)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPredmX4
Error <- actual-EnrPredmX4
#Error
rmse(actual,EnrPredmX4)
mae(actual,EnrPredmX4)


#*******************
# With all the variables

set.seed(100)
Enr.fit1 <- lm(Enr_Class1_Dataset_mc$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset_mc)

summary(Enr.fit1)
gvlma::gvlma(Enr.fit1)

plot(Enr.fit1)
vif(Enr.fit1)
AIC(Enr.fit1)
### Prediction using fit1
EnrPred1 <- predict(Enr.fit1)
actual <- Enr_Class1_Dataset_mc$Nb_Students_Enr_Grade1
#EnrPred1
Error1 <- actual-EnrPred1
#Error1
rmse(actual,EnrPred1)
mae(actual,EnrPred1)

#####
set.seed(100)

Enr.fit2 <- lm(Enr_Class1_Dataset$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset)

summary(Enr.fit2)
par(mfrow=c(2,2))
plot(Enr.fit2)
library(car)
vif(Enr.fit2)
AIC(Enr.fit2)

EnrPred2 <- predict(Enr.fit2)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPred2
Error2 <- actual-EnrPred1
#Error2
rmse(actual,EnrPred2)
mae(actual,EnrPred2)

####### ####
#Further removing MC by removing   non significant Nb_Teachers_Govt
Enr_Class1_Dataset_2<-Enr_Class1_Dataset[,-c(3)]


set.seed(100)
Enr.fit3 <- lm(Enr_Class1_Dataset_2$Nb_Students_Enr_Grade1~., data=Enr_Class1_Dataset_2)
summary(Enr.fit3)
plot(Enr.fit3)
library(car)
vif(Enr.fit3)
AIC(Enr.fit3)

EnrPred3 <- predict(Enr.fit3)
actual <- Enr_Class1_Dataset$Nb_Students_Enr_Grade1
#EnrPred3
Error3 <- actual-EnrPred3
#Error3
rmse(actual,EnrPred3)
mae(actual,EnrPred3)

# In sample accuracy with Model Enr.fit2

actuals_preds2 <- data.frame(cbind(actuals=Enr_Class1_Dataset$Nb_Students_Enr_Grade1, predicteds=EnrPred2))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds2)  
correlation_accuracy # 0.9410667 
min_max_accuracy <- mean(apply(actuals_preds2, 1, min) / apply(actuals_preds2, 1, max))  
min_max_accuracy # => 98.42%, 
mape <- mean(abs((actuals_preds2$predicteds - actuals_preds2$actuals))/actuals_preds2$actuals)  
mape # => 0.01590646

# In sample accuracy with Model Enr.fit3

actuals_preds3 <- data.frame(cbind(actuals=Enr_Class1_Dataset$Nb_Students_Enr_Grade1, predicteds=EnrPred3))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds3)  
correlation_accuracy # 0.9398773 
min_max_accuracy <- mean(apply(actuals_preds3, 1, min) / apply(actuals_preds3, 1, max))  
min_max_accuracy # => 98.42%, 
mape <- mean(abs((actuals_preds3$predicteds - actuals_preds3$actuals))/actuals_preds3$actuals)  
mape # => 0.01599737


#######

## influence diagnostics:

library(car)
influence.measures(Enr.fit3)
influenceIndexPlot(Enr.fit3, id.n = 3)
influencePlot(Enr.fit3, id.n = 3)

#splitting the data
set.seed(100)
tt1n<-Enr_Class1_Dataset
tt1 <- tt1n

tt1$random <- runif(nrow(Enr_Class1_Dataset), 0, 1);
tt1 <- tt1[order(tt1$random),]
tt1.dev <- tt1[which(tt1$random <= 0.85),]
summary(tt1.dev)
tt1.val <- tt1[which(tt1$random > 0.85),]
summary(tt1.val)
c(nrow(tt1.dev), nrow(tt1.val))
tt1.dev_1<-tt1.dev[,-c(7)]
View(tt1.dev_1)



#Prediction with Test Data for Model Enr.fit3T


Enr.fit3T <- lm(tt1.dev$Nb_Students_Enr_Grade1~., data = tt1.dev)

summary(Enr.fit3T)
AIC(Enr.fit3T)
par(mfrow=c(2,2))
plot(Enr.fit3T)
EnrPred3T <- predict(Enr.fit3T,tt1.val)

actuals_preds3T <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade1, predicteds=EnrPred3T))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds3T)  
correlation_accuracy # 94.01% / 0.9436742

#Exporting Data
export<- exp(actuals_preds3T)
View(export)
write.csv(export, "Actual_Predicted_Data.csv")
getwd()
min_max_accuracy <- mean(apply(actuals_preds3T, 1, min) / apply(actuals_preds3T, 1, max))  
min_max_accuracy # =>  0.9850532
mape <- mean(abs((actuals_preds3T$predicteds - actuals_preds3T$actuals))/actuals_preds3T$actuals)  
mape # => 0.01510236

actual <- tt1.val$Nb_Students_Enr_Grade1
#EnrPred3
Error <- actual-EnrPred3T
#Error3
rmse(actual,EnrPred3T)
mae(actual,EnrPred3T)
tt1.dev_1<-tt1.dev[,-c(7)]
View(tt1.dev)

View(tt1.dev_1)
str(tt1.dev_1)


#Prediction with Test Data for Model Enr.fit2T
View(tt1.dev)
tt1.dev_2<-tt1.dev[,-c(3,7)]
Enr.fit2T <- lm(tt1.dev_2$Nb_Students_Enr_Grade1~., data = tt1.dev_2)

summary(Enr.fit2T)
AIC(Enr.fit2T)
par(mfrow=c(2,2))
plot(Enr.fit2T)
EnrPred2T <- predict(Enr.fit2T,tt1.val)

actuals_preds2T <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade1, predicteds=EnrPred2T))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds2T)  
correlation_accuracy # 94.01% / 0.9436742

View(exp(actuals_preds3T))

min_max_accuracy <- mean(apply(actuals_preds2T, 1, min) / apply(actuals_preds2T, 1, max))  
min_max_accuracy # => 0.9835308 / 0.9850532
mape <- mean(abs((actuals_preds3T$predicteds - actuals_preds3T$actuals))/actuals_preds3T$actuals)  
mape # => 0.01668615 / 0.01510236

actual <- tt1.val$Nb_Students_Enr_Grade1
#EnrPred3
Error <- actual-EnrPred2T
#Error3
rmse(actual,EnrPred2T)
mae(actual,EnrPred2T)
tt1.dev_1<-tt1.dev[,-c(7)]
View(tt1.dev)

View(tt1.dev_1)
str(tt1.dev_1)

# Entire Data set
Enr.fit3TR <- rlm(Enr_$Nb_Students_Enr_Grade1~tt1.dev$nb_classrooms
                  +tt1.dev_1$`Nb_schools_PTR>35_Upri`
                  +tt1.dev_1$`Nb_schools_SCR>30_pri` ,data = tt1.dev_1)


summary(Enr.fit3TR)

#### Using RLM Function
Enr.fit3TR <- rlm(tt1.dev_1$Nb_Students_Enr_Grade1~tt1.dev_1$nb_classrooms
                  +tt1.dev_1$`Nb_schools_PTR>35_Upri`
                  +tt1.dev_1$`Nb_schools_SCR>30_pri` ,data = tt1.dev_1)


summary(Enr.fit3TR)
plot(Enr.fit3TR)
#Enr.fit3TR
vif(Enr.fit3TR)
AIC(Enr.fit3TR)

EnrPred3TR <- predict(Enr.fit3TR,tt1.val)
#EnrPred3TR
actuals_preds <- data.frame(cbind(actuals=tt1.val$Nb_Students_Enr_Grade1, predicteds=EnrPred3TR))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy # 93.6% 

#Exporting Data
export<- exp(actuals_preds)
View(export)
write.csv(export, "Actual_Predicted_Data.csv")
getwd()

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy # => 91.18%, 

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape # => 0.09569448 

actual <- tt1.val$Nb_Students_Enr_Grade1

Error <- actual-EnrPred3TR
#Error3
rmse(actual,EnrPred3TR)
mae(actual,EnrPred3TR)
m2 <- mean(Enr.fit3TR$residuals)
#mean of residuals
m2

#######


