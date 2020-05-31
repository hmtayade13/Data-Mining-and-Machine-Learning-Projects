#install.packages("gridExtra")
#install.packages('tidyverse')
install.packages("RColorBrewer")
library(RColorBrewer)
library(tidyverse)
library(MASS)
library(gridExtra)
library(ggplot2)
library(randomForest)
library(caTools)
library(caret)
library(corrgram)
library(corrplot)
library(rpart)
library(rpart.plot)
#Data selection phase
# Read CSV, note the delimiter (sep)
accidents_data <- read.csv('accidents_2012_to_2014.csv')
head(accidents_data)
summary(accidents_data)
accidents_data =  accidents_data %>% filter(Year == 2013)


#Visualization - EDA process
accidents_data_eda  = accidents_data[, c('Accident_Severity', 'Urban_or_Rural_Area', 'X1st_Road_Class', 'Road_Type', 'Pedestrian_Crossing.Physical_Facilities', 'Did_Police_Officer_Attend_Scene_of_Accident', 'Speed_limit', 'Number_of_Vehicles')]


plot1 <- ggplot(accidents_data_eda)+geom_bar(aes(Accident_Severity, fill =factor(Road_Type))) +  labs(title = "Accident Severity Based on Road type")
grid.arrange(plot1, nrow = 1)

col <- brewer.pal(3,"Set3")
ggplot(accidents_data_eda,aes(factor(Accident_Severity),Speed_limit)) + geom_boxplot(fill = col) + theme_classic() +
  labs(title = "Accident severity based on speed limit") +
  scale_x_discrete(labels = c("Low","Moderate","High"))

#Data cleaning and transformation phase
accidents_data$Accident_Severity = as.factor(accidents_data$Accident_Severity)
accidents_data$Urban_or_Rural_Area = as.factor(accidents_data$Urban_or_Rural_Area)
accidents_data$Day_of_Week = as.factor(accidents_data$Day_of_Week)
accidents_data$X1st_Road_Class = as.factor(accidents_data$X1st_Road_Class)

#Encoding categorical variables
accidents_data$Road_Type = factor(accidents_data$Road_Type,
                                  levels = c('Single carriageway', 'One way street', 'Roundabout', 'Dual carriageway', 'Unknown', 'Slip road'),
                                  labels = c(1,2,3,4,5,6))


accidents_data$Junction_Control =  factor(accidents_data$Junction_Control,
                                          levels = c('Giveway or uncontrolled', 'Automatic traffic signal', 'Authorised person ', 'Dual carriageway', 'Stop Sign', '  '),
                                          labels = c(1,2,3,4,5,6))


accidents_data$Pedestrian_Crossing.Human_Control = factor(accidents_data$Pedestrian_Crossing.Human_Control,
                                                          levels = c('None within 50 metres', 'Control by other authorised person', 'Control by school crossing patrol'),
                                                          labels = c(1,2,3))


accidents_data$Light_Conditions = factor(accidents_data$Light_Conditions,
                                         levels = c('Daylight: Street light present', 'Darkness: Street lighting unknown', 'Darkness: Street lights present and lit', 'Darkness: Street lights present and lit', 'Darkeness: No street lighting'),
                                         labels = c(1,2,3,4,5))


accidents_data$Weather_Conditions = factor(accidents_data$Weather_Conditions,
                                           levels = c('Raining without high winds', 
                                                      'Fine without high winds ',
                                                      'Raining with high winds',
                                                      'Unknown',
                                                      'Fine with high winds',
                                                      'Other',
                                                      'Snowing without high winds',
                                                      'Fog or mist',
                                                      'Snowing with high winds'),
                                           labels = c(1,2,3,4,5,6,7,8,9))


accidents_data$Road_Surface_Conditions = factor(accidents_data$Road_Surface_Conditions,
                                                levels = c('Wet/Damp', 'Dry', 'Flood (Over 3cm of water)', 'Frost/Ice', 'Snow'),
                                                labels = c(1,2,3,4,5))


accidents_data$Did_Police_Officer_Attend_Scene_of_Accident = factor(accidents_data$Did_Police_Officer_Attend_Scene_of_Accident,
                                                                    levels = c('No', 'Yes'),
                                                                    labels = c(0,1))


accidents_data$Pedestrian_Crossing.Physical_Facilities = factor(accidents_data$Pedestrian_Crossing.Physical_Facilities,
                                                                levels = c('No physical crossing within 50 meters',
                                                                           'Pedestrian phase at traffic signal junction',
                                                                           'Zebra crossing',
                                                                           'Central refuge',
                                                                           'non-junction pedestrian crossing',
                                                                           'Footbridge or subway'),
                                                                labels = c(1,2,3,4,5,6))


accidents_data_v1 = accidents_data[, c("Police_Force", 
                                       "Accident_Severity",
                                       "Number_of_Vehicles",
                                       "Number_of_Casualties",
                                       "Day_of_Week",
                                       "X1st_Road_Class",
                                       "Road_Type",
                                       "Speed_limit",
                                       "Junction_Control",
                                       "Pedestrian_Crossing.Human_Control",
                                       "Pedestrian_Crossing.Physical_Facilities",
                                       "Light_Conditions",
                                       "Weather_Conditions",
                                       "Road_Surface_Conditions",
                                       "Urban_or_Rural_Area",
                                       "Did_Police_Officer_Attend_Scene_of_Accident")]

accidents_data_v1$Day_of_Week = as.numeric(accidents_data_v1$Day_of_Week)

#Feature selection phase
#Evaluating relation beteen two categorical variables using chi-squared test
tbl1 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Day_of_Week)
tbl2 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Urban_or_Rural_Area)
tbl3 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$X1st_Road_Class)
tbl4 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Road_Type)
tbl5 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Junction_Control)
tbl6 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Pedestrian_Crossing.Human_Control)
tbl7 = table(accidents_data_v1$Accident_Severity ,accidents_data_v1$Pedestrian_Crossing.Physical_Facilities)
tbl8 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Light_Conditions)
tbl9 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Weather_Conditions)
tbl10 = table(accidents_data_v1$Accident_Severity, accidents_data_v1$Road_Surface_Conditions)
tbl11 = table(accidents_data_v1$Accident_Severity ,accidents_data_v1$Did_Police_Officer_Attend_Scene_of_Accident)


chisq.test(tbl1)
chisq.test(tbl2)
chisq.test(tbl3)
chisq.test(tbl4)
chisq.test(tbl5)
chisq.test(tbl6)
chisq.test(tbl7)
chisq.test(tbl8)
chisq.test(tbl9)
chisq.test(tbl10)
chisq.test(tbl11)



#Feature selection
accidents_data_v4  = accidents_data_v1[, c('Accident_Severity', 'Urban_or_Rural_Area', 'X1st_Road_Class', 'Road_Type', 'Pedestrian_Crossing.Physical_Facilities', 'Did_Police_Officer_Attend_Scene_of_Accident', 'Speed_limit', 'Number_of_Vehicles')]
accidents_data_v4 = accidents_data_v4[complete.cases(accidents_data_v4), ]


#Splitting dataset into train and test set
set.seed(123)
split = sample.split(accidents_data_v4$Accident_Severity, SplitRatio = 0.70) 
training_set = subset(accidents_data_v4, split == TRUE)
test_set = subset(accidents_data_v4, split == FALSE)
training_set$Number_of_Vehicles = scale(training_set$Number_of_Vehicles)
training_set$Speed_limit = scale(training_set$Speed_limit)
test_set$Number_of_Vehicles = scale(test_set$Number_of_Vehicles)
test_set$Speed_limit = scale(test_set$Speed_limit)

prop.table(table(training_set$Accident_Severity))
prop.table(table(test_set$Accident_Severity))

#Applying random forest classifier on train set
model2 <- randomForest(Accident_Severity ~ ., data = training_set, ntree = 500, mtry = 6, importance = TRUE)
model2
predTrain <- predict(model2, training_set, type = "class")
table(predTrain, training_set$Accident_Severity)  
rpart.plot(model2, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)

#Applying model on test set
predValid <- predict(model2, test_set, type = "class")
table(predValid, test_set$Accident_Severity)

#Model evaluation
mean(predValid == test_set$Accident_Severity)
confusionMatrix(predValid, test_set$Accident_Severity)
