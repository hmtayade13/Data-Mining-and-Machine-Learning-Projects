# Read CSV, note the delimiter (sep)
fifa_df <- read.csv('fifa18.csv')

head(fifa_df)

summary(fifa_df)

#Checking if NA or NULL Values exist in the dataset
any(is.na(fifa_df))

irrelevant_columns <- c("X", "ID","Photo","Flag", "Club.Logo", "Preferred.Posi?ions")



fifav1 <- fifa_df[,! names(fifa_df) %in% irrelevant_columns, drop = F]

fifav1 = fifav1[-c(50:75)]

fifav1 = fifav1[-c(44:49)]


fifav1$Value <- gsub("â,¬", "", fifav1$Value)
fifav1$Wage <- gsub("â,¬", "", fifav1$Wage)

fifav1$Wage <- as.numeric(?sub("K", "", fifav1$Wage))

fifav1$Value <- as.numeric(gsub("M", "", fifav1$Value))

fifav1$Value <- fifav1$Value * 1000




for (i in c(10:43))
  fifav1[,i]<- as.numeric(gsub("- | +", ".", fifav1[,i],fixed=TRUE))

fifav1 = fifav1[complete.cases(fifav1), ]?fifav1 = na.omit(fifav1)

features = str(fifav1)

fifa_model = fifav1[, c('Reactions', 'Composure', 'Overall', 'Potential', 'Value', 'Special')]

cor = cor(fifav1[, num.cols])



num.cols <- sapply(fifav1, is.numeric)

cor.data = cor(fifav1[, num.cols])

f?fav1 = fifav1[, num.cols]

fifav1 = fifav1[, c('Wage', 'Reactions', 'Overall', 'Potential', 'Value')]
cor.data = cor(fifav1)


library(corrplot)

install.packages('corrgram')
library(corrgram)

corrplot(cor.data, method = 'color' )


features = str(fifav1)?


# Import Library
library(caTools)
# Set a random see so your "random" results are the same as this notebook
set.seed(101) 

# Split up the sample, basically randomly assigns a booleans to a new column "sample"
sample <- sample.split(fifav1$Wage, SplitRa?io = 0.78) # SplitRatio = percent of sample==TRUE

# Training Data
train = subset(fifav1, sample == TRUE)

# Testing Data
test = subset(fifav1, sample == FALSE)


set.seed(1234)
#load the randomForest library
library(randomForest)
#training the model
rf_mo?el<-randomForest(Wage~., train,importance=TRUE,ntree=200)
rf_model

library(rpart.plot)
rpart.plot(rf_model, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)
summary(rf_model)




options(warn=-1)
set.seed(1234)
library(randomForest)
#load th? ranger library for random forest CV
install.packages("ranger")
library(ranger)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
rf_CV_predict<-train(Wage~., train, method='ranger',trControl?train.control)
rf_CV_predict



set.seed(1234)
#Predicting the model
rf_predict<-predict(rf_model,test)
head(rf_predict,5)

rf_model$rsq

RMSE(rf_predict, obs = test$Wage)

library(scorer)
set.seed(1234)

mean_squared_error(test$Wage, rf_predict)
mean_abso?ute_error(test$Wage, rf_predict)



Wage_predictions=data.frame(test,rf_predict)
Wage_predictions
