library(corrgram)
library(scorer)
library(corrplot)
library(caTools)
library(rpart)
library(rpart.plot)

#Dataset selection phase
# Read CSV, note the delimiter (sep)
fifa_df <- read.csv('fifa18.csv')
head(fifa_df)
summary(fifa_df)

#Preprocessing phase - ?ata cleaning and transformation
#Checking if NA or NULL Values exist in the dataset
any(is.na(fifa_df))

#Removing irrelevant columns
irrelevant_columns <- c("X", "ID","Photo","Flag", "Club.Logo", "Preferred.Positions")
fifav1 <- fifa_df[,! names(fifa_df) ?in% irrelevant_columns, drop = F]
fifav1 = fifav1[-c(50:75)]
fifav1 = fifav1[-c(44:49)]
fifav1$Value <- gsub("â,¬", "", fifav1$Value)
fifav1$Wage <- gsub("â,¬", "", fifav1$Wage)
fifav1$Wage <- as.numeric(gsub("K", "", fifav1$Wage))
fifav1$Value <- as.numer?c(gsub("M", "", fifav1$Value))
fifav1$Value <- fifav1$Value * 1000


for (i in c(10:43))
  fifav1[,i]<- as.numeric(gsub("- | +", ".", fifav1[,i],fixed=TRUE))
fifav1 = na.omit(fifav1)
features = str(fifav1)

fifa_model = fifav1[, c('Reactions', 'Composure',?'Overall', 'Potential', 'Value', 'Special')]
cor = cor(fifav1[, num.cols])
num.cols <- sapply(fifav1, is.numeric)
cor.data = cor(fifav1[, num.cols])
fifav1 = fifav1[, num.cols]


#Feature selection
fifav1 = fifav1[, c('Wage', 'Reactions', 'Overall', 'Poten?ial', 'Value')]
cor.data = cor(fifav1)
corrplot(cor.data, method = 'color' )
features = str(fifav1)


#Splitting data into test and train set
set.seed(101) 
sample <- sample.split(fifav1$Wage, SplitRatio = 0.78)
#Training Data
train = subset(fifav1, sample?== TRUE)
#Testing Data
test = subset(fifav1, sample == FALSE)


#Applying Decision tree regressor on training data
rpart.control<-rpart.control(minbucket = 2,cp = 0.01,maxcompete = 3, maxsurrogate = 4, usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdept? = 10) 
#training the dtr model
dtr<-rpart(Wage~., data=train, control=rpart.control, method='anova', cp=0.01)
dtr
#Plotting the decision tree
rpart.plot(dtr, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)





options(warn=-1)
set.seed(576?)
#cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#cross validation pred
dtr_CV_predict<-train(Wage~.,data=train,method='rpart',trControl=train.control)
dtr_CV_predict
#Cross validation prediction plot
residuals<-resid?dtr_CV_predict)
plot(train,residuals, main='Cross validation plot')
abline(0,0)



#Applying trained model on test data set
set.seed(7882)
dtr_predict<-predict(dtr,test)
head(dtr_predict,5)

#Model evaluation
#Grab Residuals
res1 <- residuals(dtr)
# Conver? to DataFrame for gglpot
res1 <- as.data.frame(res1)
head(res1)
#plotting model residuals
ggplot(res1,aes(res1)) +  geom_histogram(fill='red',alpha=0.8)

#Applying trained model on test data set
Wage_predictions <- predict(dtr,test)
results123 <- cbind(Wag?_predictions,test$Wage)
colnames(results123) <- c('Predicted','Real')
results123 <- as.data.frame(results123)

library(scorer)
mean_absolute_error(test$Wage,Wage_predictions)
mean_squared_error(test$Wage,Wage_predictions)
