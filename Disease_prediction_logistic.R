#install.packages("RColorBrewer")
library(caTools)
library(ggplot2) 
library(readr)
library(dplyr)
library(corrgram)
library(corrplot)
library(scorer)
library(caret)

#Data selection phase
cardio <- read.csv2("E:\\Data mining\\cardio_train.csv", sep = ";")
summary(cardio)



#Preprocessing phase - Cleaning and transformation
df<-data.frame(cardio)
df = df[-1]

#Number of Duplicate rows
paste("Number of duplicate rows in the dataset = ", sum(duplicated(df)))
#Number of na values per column
sapply(cardio, function(x)sum(duplicated(x)))
duplicate_rows = df[duplicated(df),]
head(duplicate_rows, 2)
df$age = df$age / 365
df$weight = as.numeric(df$weight)
df$cardio = as.factor(df$cardio)
#Outlier Detection
OutVals = boxplot(df)$out

#Visualizations
library(ggplot2)
g_age = ggplot(data = df, aes(x = age))
g_age + geom_histogram(col="yellow", aes(fill = ..count..)) + ggtitle("Distribution based on Age")
ggplot(df,aes(gluc)) + geom_bar(aes(fill = factor(gluc))) + ggtitle("Distribution of glucose level in the population sample")
ggplot(data= df, aes(x=age, fill= factor(cardio)))+geom_histogram()+
  labs(title =  "Risk of cardiovascular disease based on age") + theme(plot.title=element_text(hjust = 0.5))
ggplot(data= df, aes(x=age, y=height))+geom_point(aes(fill = age), color = 'steelblue4')+
  labs(title =  "Distribution of age and height of the sample population") + theme(plot.title=element_text(hjust = 0.5))



#Feature selection
df = df[, c('age', 'weight', 'gluc', 'cholesterol', 'cardio')]
corr.data = cor(df)
corrplot(corr.data, method = 'color' )
hist(corr.data)
heatmap(corr.data, annot = TRUE)
hist(corr.data)


set.seed(1234)
split = sample.split(df$cardio, SplitRatio = 0.82) 
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
training_set[, 1:2] = scale(training_set[,1:2])
test_set[, 1:2] = scale(test_set[,1:2])

boxplot(training_set)

#Applying logistic regression
classifier = glm(formula = cardio ~ .,
                 family = binomial,
                 data = training_set)

#Applying trained model on test data
prob_pred = predict(classifier, type = 'response', newdata = test_set[-5])
y_pred = ifelse(prob_pred > 0.5, 1, 0)


##Model evaluation
cm = table(test_set[,5], y_pred)
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(1951, 3219, 2946, 1684)
cm_frame <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  cm_frame, mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f",Y)), vjust = 1) +
  scale_fill_gradient(low = "steelblue", high = "red")


accuracy_Test <- sum(diag(cm)) / sum(cm)
confusionMatrix(table(test_set[,5], y_pred))


install.packages('pROC')
library(pROC)
predTestwithProb = predict(classifier, test_set[-5], type= 'response')
predTestwithProb=as.character(predTestwithProb)
predTestwithProb = as.numeric(predTestwithProb)
plot(roc(test_set$cardio,predTestwithProb))
auc<-(auc(test_set$cardio,predTestwithProb))
table(test_set$cardio)